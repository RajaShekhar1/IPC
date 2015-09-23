from decimal import Decimal

from taa.core import TAAFormError
from taa.services.products.payment_modes import get_payment_modes
from taa.services import RequiredFeature
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
        def val_or_blank(name):
            if data.get(name) is None:
                return ''
            return data.get(name, '')

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

        from taa.services.enrollments import EnrollmentRecordParser
        def build_beneficiary_data(prefix, out_prefix):
            if data.get("{}_bene_name".format(prefix)):
                out_data["{}_beneficiary_name{}".format(out_prefix, 1)] = val_or_blank("{}_bene_name".format(prefix))
                out_data["{}_beneficiary_ssn{}".format(out_prefix, 1)] = val_or_blank("{}_bene_ssn".format(prefix))
                out_data["{}_beneficiary_dob{}".format(out_prefix, 1)] = val_or_blank("{}_bene_birthdate".format(prefix))
                out_data["{}_beneficiary_relationship{}".format(out_prefix, 1)] = val_or_blank("{}_bene_relationship".format(prefix))
                out_data["{}_beneficiary_percentage{}".format(out_prefix, 1)] = 100
            else:
                for i in range(1, EnrollmentRecordParser.MAX_BENEFICIARY_COUNT+1):
                    out_data["{}_beneficiary_name{}".format(out_prefix, i)] = val_or_blank("{}_bene_name{}".format(prefix, i))
                    out_data["{}_beneficiary_ssn{}".format(out_prefix, i)] = val_or_blank("{}_bene_ssn{}".format(prefix, i))
                    out_data["{}_beneficiary_dob{}".format(out_prefix, i)] = val_or_blank("{}_bene_birthdate{}".format(prefix, i))
                    out_data["{}_beneficiary_relationship{}".format(out_prefix, i)] = val_or_blank("{}_bene_relationship{}".format(prefix, i))
                    out_data["{}_beneficiary_percentage{}".format(out_prefix, i)] = val_or_blank("{}_bene_percentage{}".format(prefix, i))

        def build_contingent_beneficiary_data(prefix, out_prefix):
            out_data["{}_contingent_beneficiary".format(out_prefix)] = {
                'name': val_or_blank("{}_cont_bene_name".format(prefix)),
                'ssn': val_or_blank("{}_cont_bene_ssn".format(prefix)),
                'date_of_birth': val_or_blank("{}_cont_bene_birthdate".format(prefix)),
                'relationship':val_or_blank("{}_cont_bene_relationship".format(prefix)),
            }

        def standardize_answer(answer):
            answers = dict(Y="Yes", N="No", y="Yes", n="No")
            return answers.get(answer)

        def build_person(prefix):
            genders = dict(m="male", M="male", f="female", F="female")
            base_dict = dict(
                first=val_or_blank("{}_first".format(prefix)),
                last=val_or_blank("{}_last".format(prefix)),
                birthdate=val_or_blank("{}_birthdate".format(prefix)),
                ssn=val_or_blank("{}_ssn".format(prefix)),
                gender=genders.get(val_or_blank("{}_gender".format(prefix))),
                phone=val_or_blank("{}_phone".format(prefix)),
                email=val_or_blank("{}_email".format(prefix)),
                address1=val_or_blank("{}_street".format(prefix)),
                address2=val_or_blank("{}_street2".format(prefix)),
                city=val_or_blank("{}_city".format(prefix)),
                state=val_or_blank("{}_state".format(prefix)),
                zip=val_or_blank("{}_zipcode".format(prefix)),
                height=val_or_blank("{}_height_inches".format(prefix)),
                weight=val_or_blank("{}_weight_pounds".format(prefix)),
                is_smoker=val_or_blank("{}_smoker".format(prefix)) in ["Y", "y"],
                soh_questions=[]
            )

            questions = self.soh_service.get_health_questions(product, state)
            for q_num in range(1, len(questions)+1):
                answer = data.get("{}_question_{}_answer".format(prefix, q_num))
                if answer:
                    base_dict["soh_questions"].append(dict(
                        question=questions[q_num-1].question,
                        answer=standardize_answer(answer)
                    ))
            return base_dict

        # Pass through all the original data to start
        out_data.update(data)

        out_data["did_decline"] = data.get("did_decline") or False

        out_data["enrollCity"] = data.get("signed_at_city")
        out_data["enrollState"] = data.get("signed_at_state")
        out_data["product_type"] = data.get("product_code")

        out_data["payment_mode"] = int(data["payment_mode"])
        out_data["payment_mode_text"] = get_payment_modes(single=int(data["payment_mode"]))[0].get("name").lower()

        out_data["existing_insurance"] = data.get("existing_insurance") in ["Y","y"]
        out_data["replacing_insurance"] = data.get("replacing_insurance") in ["Y","y"]
        out_data["replacement_read_aloud"] = data.get("replacement_read_aloud") in ["Y","y"]
        out_data["replacement_is_terminating"] = data.get("replacement_is_terminating") in ["Y","y"]
        out_data["replacement_using_funds"] = data.get("replacement_using_funds") in ["Y","y"]

        out_data["is_employee_actively_at_work"] = data.get("actively_at_work") in ["Y","y"]

        # These two are like the health question answers
        out_data["has_spouse_been_treated_6_months"] = standardize_answer(data.get("sp_treated_6_months"))
        out_data["has_spouse_been_disabled_6_months"] = standardize_answer(data.get("sp_disabled_6_months"))

        if data.get("replacement_policy1_name"):
            out_data["replacement_policies"].append(dict(
                name=data.get("replacement_policy1_name"),
                policy_number=data.get("replacement_policy1_number"),
                insured=data.get("replacement_policy1_insured"),
                replaced_or_financing=data.get("replacement_policy1_replaced_or_financing"),
                replacement_reason=data.get("replacement_policy1_reason")
            ))

        emp_address = (data.get("emp_street"), data.get("emp_street2"), data.get("emp_city"), data.get("emp_state"), data.get("emp_zipcode"))
        sp_address = (data.get("sp_street"), data.get("sp_street2"), data.get("sp_city"), data.get("sp_state"), data.get("sp_zipcode"))
        out_data["is_spouse_address_same_as_employee"] = (emp_address == sp_address)

        out_data["is_spouse_email_same_as_employee"] = (data.get('emp_email') == data.get('sp_email'))

        out_data["employee"].update(build_person("emp"))
        out_data["spouse"].update(build_person("sp"))

        # Beneficiaries
        out_data['employee_beneficiary'] = 'other'
        out_data['spouse_beneficiary'] = 'other'
        build_beneficiary_data("emp", "employee")
        build_beneficiary_data("sp", "spouse")

        out_data['employee_contingent_beneficiary_type'] = 'other'
        out_data['spouse_contingent_beneficiary_type'] = 'other'
        build_contingent_beneficiary_data("emp", "employee")
        build_contingent_beneficiary_data("sp", "spouse")

        out_data["employee_coverage"] = self.format_coverage(data.get("emp_coverage"), data.get("emp_premium"))
        out_data["spouse_coverage"] = self.format_coverage(data.get("sp_coverage"), data.get("sp_premium"))

        for num in range(1, EnrollmentRecordParser.MAX_CHILDREN + 1):
            if data.get("ch{}_first".format(num)):
                out_data["children"].append(build_person("ch{}".format(num)))
                out_data["child_coverages"].append(
                    self.format_coverage(
                        face_value=data.get("ch{}_coverage".format(num)),
                        premium=data.get("ch{}_premium".format(num)),
                    )
                )

        # identityToken is date of hire
        out_data['identityToken'] = val_or_blank('emp_date_of_hire')
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

        return out_data

    def format_coverage(self, face_value, premium):
        return dict(
            face_value=int(face_value) if face_value else None,
            premium=Decimal(premium) if premium else None,
        )
