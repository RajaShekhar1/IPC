from decimal import Decimal

from taa.core import TAAFormError
from taa.services.products.payment_modes import get_payment_modes
from taa.services import RequiredFeature, LookupService
from taa.services.enrollments.enrollment_record_parser import EnrollmentRecordParser
from taa.services.enrollments.enrollment_import_processor import EnrollmentProcessor

class EnrollmentImportService(object):
    case_service = RequiredFeature("CaseService")
    file_import_service = RequiredFeature("FileImportService")
    product_service = RequiredFeature("ProductService")
    soh_service = RequiredFeature("StatementOfHealthQuestionService")

    def process_enrollment_data(self, data, data_format, data_source, case_token=None, auth_token=None, user_href=None):
        processor = EnrollmentProcessor()
        try:
            processor.process_enrollment_import_request(
                data,
                data_format,
                data_source=data_source,
                case_token=case_token,
                auth_token=auth_token,
            )
        except TAAFormError:
            #TODO: There might be some logic needed here
            pass

        if user_href:
            processor.send_status_email(user_href)
        return processor

    def standardize_imported_data(self, data, method='api_import'):

        from taa.services.enrollments import EnrollmentRecordParser
        from taa.services.cases import RiderService
        rider_service = RiderService()

        out_data = {
            "employee": {},
            "spouse": {},
            "children": [],
            "child_coverages": [],
            "replacement_policies": [],
        }

        code = data.get("product_code")
        products = self.product_service.get_products_by_codes([code])
        product = products[0]
        state = data.get("signed_at_state")

        # Pass through all the original data to start
        out_data.update(data)

        out_data["did_decline"] = data.get("did_decline") or False

        out_data["enrollCity"] = data.get("signed_at_city")
        out_data["enrollState"] = data.get("signed_at_state")
        out_data["product_type"] = data.get("product_code")

        out_data["payment_mode"] = int(data["payment_mode"])
        out_data["payment_mode_text"] = get_payment_modes(single=int(data["payment_mode"]))[0].get("name").lower()

        out_data["existing_insurance"] = bool_from_answer(data.get("existing_insurance"))
        out_data["replacing_insurance"] = bool_from_answer(data.get("replacing_insurance"))
        out_data["replacement_read_aloud"] = bool_from_answer(data.get("replacement_read_aloud"))
        out_data["replacement_is_terminating"] = bool_from_answer(data.get("replacement_is_terminating"))
        out_data["replacement_using_funds"] = bool_from_answer(data.get("replacement_using_funds"))

        out_data["is_employee_actively_at_work"] = bool_from_answer(data.get("actively_at_work"))

        # These two are like the health question answers
        out_data["has_spouse_been_treated_6_months"] = standardize_answer(data.get("sp_treated_6_months"))
        out_data["has_spouse_been_disabled_6_months"] = standardize_answer(data.get("sp_disabled_6_months"))

        for i in range(1, EnrollmentRecordParser.MAX_POLICIES):
            if data.get("replacement_policy{}_name".format(i)):
                out_data["replacement_policies"].append(dict(
                    name=data.get("replacement_policy{}_name".format(i)),
                    policy_number=data.get("replacement_policy{}_number".format(i)),
                    insured=data.get("replacement_policy{}_insured".format(i)),
                    replaced_or_financing=data.get("replacement_policy{}_replaced_or_financing".format(i)),
                    replacement_reason=data.get("replacement_policy{}_reason".format(i))
                ))

        out_data["is_spouse_address_same_as_employee"] = self.do_addresses_match(data)

        out_data["is_spouse_email_same_as_employee"] = (data.get('emp_email') == data.get('sp_email'))

        out_data["employee"].update(build_person(data, "emp", product, state, self.soh_service))
        out_data["spouse"].update(build_person(data, "sp", product, state, self.soh_service))

        # Beneficiaries
        out_data['employee_beneficiary'] = 'other'
        out_data['spouse_beneficiary'] = 'other'
        out_data['employee_contingent_beneficiary_type'] = 'other'
        out_data['spouse_contingent_beneficiary_type'] = 'other'
        out_data.update(build_beneficiary_data(data, "employee", "emp"))
        out_data.update(build_beneficiary_data(data, "spouse", "sp"))
        out_data.update(build_beneficiary_data(data, "employee_contingent", "emp_cont"))
        out_data.update(build_beneficiary_data(data, "spouse_contingent", "sp_cont"))

        out_data["employee_coverage"] = self.format_coverage(data.get("emp_coverage"), data.get("emp_premium"))
        out_data["spouse_coverage"] = self.format_coverage(data.get("sp_coverage"), data.get("sp_premium"))

        for num in range(1, EnrollmentRecordParser.MAX_CHILDREN + 1):
            if data.get("ch{}_first".format(num)):
                out_data["children"].append(build_person(data, "ch{}".format(num), product, state, self.soh_service))
                out_data["child_coverages"].append(
                    self.format_coverage(
                        face_value=data.get("ch{}_coverage".format(num)),
                        premium=data.get("ch{}_premium".format(num)),
                    )
                )

        # identityToken is date of hire
        out_data['identityToken'] = val_or_blank(data, 'emp_date_of_hire')
        out_data['identityType'] = 'Date of Hire'

        # Source / method of the enrollment.
        out_data['method'] = method
        if method == 'api_import':
            out_data['is_third_party'] = True

        # Owner
        out_data['employee_owner'] = 'self'
        out_data['employee_other_owner_name'] = ''
        out_data['employee_other_owner_ssn'] = ''
        out_data['spouse_owner'] = 'employee'
        out_data['spouse_other_owner_name'] = ''
        out_data['spouse_other_owner_ssn'] = ''

        # Product
        out_data['product_data'] = {'id': product.id}

        # Initials
        out_data['emp_initials_txt'] = val_or_blank(data, data.get('emp_initials_txt'))
        out_data['agent_initials_txt'] = val_or_blank(data, data.get('agent_initials_txt'))
        out_data['rider_data'] = {
            'emp': [],
            'sp': []
        }

        for prefix, long_prefix in [('emp', 'employee'), ('sp', 'spouse')]:
            for rider in RiderService.default_riders:
                has_rider = data.get('{}_rider_{}'.format(prefix, rider.code.lower()))
                if(has_rider):
                    out_data['rider_data'][prefix].append(rider.to_json())

        return out_data

    def do_addresses_match(self, data):
        emp_address = (data.get("emp_street"), data.get("emp_street2"), data.get("emp_city"), data.get("emp_state"),
                       data.get("emp_zipcode"))
        sp_address = (data.get("sp_street"), data.get("sp_street2"), data.get("sp_city"), data.get("sp_state"),
                      data.get("sp_zipcode"))
        return (emp_address == sp_address)

    def format_coverage(self, face_value, premium):
        return dict(
            face_value=int(face_value) if face_value else None,
            premium=Decimal(premium) if premium else None,
        )

    def standardize_wizard_data(self, wizard_data):
        output = wizard_data.copy()

        # If not replacing insurance, clear any replacement policies / questions provided.
        #  TODO: do this in the wizard so this isn't necessary to clear these here.
        if not output.get('replacing_insurance'):
            output['replacement_is_terminating'] = False
            output['replacement_using_funds'] = False
            output['replacement_policies'] = []

        # Update beneficiary data to new format
        output.update(
            standardize_wizard_beneficiaries(wizard_data, "employee")
        )
        output.update(
            standardize_wizard_beneficiaries(wizard_data, "spouse")
        )
        output.update(
            standardize_wizard_contingent_beneficiaries(wizard_data, "employee_contingent")
        )
        output.update(
            standardize_wizard_contingent_beneficiaries(wizard_data, "spouse_contingent")
        )
        return output


# Utility function for cleaning up data.
def val_or_blank(data, name):
    if data.get(name) is None:
        return ''
    return data.get(name, '')


def build_beneficiary_data(data, out_prefix, prefix):


    if data.get("{}_bene_name".format(prefix)):
        return standardize_legacy_beneficiaries(data, out_prefix, prefix)
    else:
        return standardize_multi_beneficiaries(data, out_prefix, prefix)


def standardize_legacy_beneficiaries(data, out_prefix, prefix):
    out_data = {}

    # Legacy format with only one beneficiary per type at 100%
    out_data["{}_beneficiary1_name".format(out_prefix)] = val_or_blank(data, "{}_bene_name".format(prefix))
    out_data["{}_beneficiary1_relationship".format(out_prefix)] = val_or_blank(data,
                                                                                   "{}_bene_relationship".format(
                                                                                       prefix))
    out_data["{}_beneficiary1_dob".format(out_prefix)] = val_or_blank(data, "{}_bene_birthdate".format(prefix))
    out_data["{}_beneficiary1_ssn".format(out_prefix)] = val_or_blank(data, "{}_bene_ssn".format(prefix))
    out_data["{}_beneficiary1_percentage".format(out_prefix)] = 100

    return out_data


def standardize_wizard_beneficiaries(data, out_prefix):
    out_data = {}

    # Wizard contingent beneficiaries are in a different input format
    out_data["{}_beneficiary1_name".format(out_prefix)] = data.get('{}_beneficiary_name'.format(out_prefix), '')
    out_data["{}_beneficiary1_relationship".format(out_prefix)] = data.get('{}_beneficiary_relationship'.format(out_prefix), '')
    out_data["{}_beneficiary1_dob".format(out_prefix)] = data.get('{}_beneficiary_dob'.format(out_prefix), '')
    out_data["{}_beneficiary1_ssn".format(out_prefix)] = data.get('{}_beneficiary_ssn'.format(out_prefix), '')

    # For consistency, put a percentage in too.
    out_data["{}_beneficiary1_percentage".format(out_prefix)] = 100
    return out_data

def standardize_wizard_contingent_beneficiaries(data, out_prefix):
    out_data = {}

    bene_data = data.get("{}_beneficiary".format(out_prefix))
    if not bene_data:
        return {}

    # Wizard contingent beneficiaries are in a different input format
    out_data["{}_beneficiary1_name".format(out_prefix)] = bene_data.get('name', '')
    out_data["{}_beneficiary1_relationship".format(out_prefix)] = bene_data.get('relationship', '')
    out_data["{}_beneficiary1_dob".format(out_prefix)] = bene_data.get('date_of_birth', '')
    out_data["{}_beneficiary1_ssn".format(out_prefix)] = bene_data.get('ssn', '')
    # For consistency, put a percentage in too.
    out_data["{}_beneficiary1_percentage".format(out_prefix)] = 100

    return out_data

def standardize_multi_beneficiaries(data, out_prefix, prefix):
    out_data = {}
    # Multiple beneficiaries allowed with percentages provided
    for i in range(1, EnrollmentRecordParser.MAX_BENEFICIARY_COUNT + 1):
        out_data["{}_beneficiary{}_name".format(out_prefix, i)] = val_or_blank(data,
                                                                               "{}_bene{}_name".format(prefix, i))
        out_data["{}_beneficiary{}_relationship".format(out_prefix, i)] = val_or_blank(data,
                                                                                       "{}_bene{}_relationship".format(
                                                                                           prefix, i))
        out_data["{}_beneficiary{}_dob".format(out_prefix, i)] = val_or_blank(data,
                                                                              "{}_bene{}_birthdate".format(prefix,
                                                                                                           i))
        out_data["{}_beneficiary{}_ssn".format(out_prefix, i)] = val_or_blank(data,
                                                                              "{}_bene{}_ssn".format(prefix, i))
        out_data["{}_beneficiary{}_percentage".format(out_prefix, i)] = val_or_blank(data,
                                                                                     "{}_bene{}_percentage".format(
                                                                                         prefix, i))
    return out_data


def standardize_answer(answer):
    answers = dict(Y="Yes", N="No", y="Yes", n="No")
    return answers.get(answer)

def bool_from_answer(answer):
    return str(answer).lower() in ['y', 'yes']

def build_person(data, prefix, product, state, soh_service):
    genders = dict(m="male", M="male", f="female", F="female")
    base_dict = dict(
        first=val_or_blank(data, "{}_first".format(prefix)),
        last=val_or_blank(data, "{}_last".format(prefix)),
        birthdate=val_or_blank(data, "{}_birthdate".format(prefix)),
        ssn=val_or_blank(data, "{}_ssn".format(prefix)),
        gender=genders.get(val_or_blank(data, "{}_gender".format(prefix))),
        phone=val_or_blank(data, "{}_phone".format(prefix)),
        email=val_or_blank(data, "{}_email".format(prefix)),
        address1=val_or_blank(data, "{}_street".format(prefix)),
        address2=val_or_blank(data, "{}_street2".format(prefix)),
        city=val_or_blank(data, "{}_city".format(prefix)),
        state=val_or_blank(data, "{}_state".format(prefix)),
        zip=val_or_blank(data, "{}_zipcode".format(prefix)),
        height=val_or_blank(data, "{}_height_inches".format(prefix)),
        weight=val_or_blank(data, "{}_weight_pounds".format(prefix)),
        is_smoker=bool_from_answer("{}_smoker".format(prefix)),
        soh_questions=[]
    )

    questions = soh_service.get_health_questions(product, state)
    for q_num in range(1, len(questions)+1):
        answer = data.get("{}_question_{}_answer".format(prefix, q_num))
        if answer:
            base_dict["soh_questions"].append(dict(
                question=questions[q_num-1].question,
                answer=standardize_answer(answer)
            ))
    return base_dict
