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

    def process_enrollment_data(self, data, data_format, case_token=None, auth_token=None, email_errors=False):
        processor = EnrollmentProcessor()
        try:
            processor.process_enrollment_import_request(
                data,
                data_format,
                case_token=case_token,
                auth_token=auth_token
            )
        except TAAFormError:
            if email_errors:
                processor.send_errors_email()
        return processor

    def standardize_imported_data(self, data, method='api_import'):

        def valOrBlank(val):
            if not val:
                return ""
            return val

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

        def build_beneficiary_data(prefix, out_prefix):
            out_data["{}_beneficiary".format(out_prefix)] = valOrBlank(data.get("{}_bene_relationship".format(prefix)))
            out_data["{}_beneficiary_name".format(out_prefix)] = valOrBlank(data.get("{}_bene_name".format(prefix)))
            out_data["{}_beneficiary_ssn".format(out_prefix)] = valOrBlank(data.get("{}_bene_ssn".format(prefix)))
            out_data["{}_beneficiary_birthdate".format(out_prefix)] = valOrBlank(data.get("{}_bene_birthdate".format(prefix)))
            out_data["{}_beneficiary_relationship".format(out_prefix)] = valOrBlank(data.get("{}_bene_relationship".format(prefix)))

        def build_person(prefix):
            genders = dict(m="male", M="male", f="female", F="female")
            base_dict = dict(
                first=valOrBlank(data.get("{}_first".format(prefix))),
                last=valOrBlank(data.get("{}_last".format(prefix))),
                birthdate=valOrBlank(data.get("{}_birthdate".format(prefix))),
                ssn=valOrBlank(data.get("{}_ssn".format(prefix))),
                gender=genders.get(data.get("{}_gender".format(prefix))),
                phone=valOrBlank(data.get("{}_phone".format(prefix))),
                email=valOrBlank(data.get("{}_email".format(prefix))),
                address1=valOrBlank(data.get("{}_street".format(prefix))),
                address2=valOrBlank(data.get("{}_street2".format(prefix))),
                city=valOrBlank(data.get("{}_city".format(prefix))),
                state=valOrBlank(data.get("{}_state".format(prefix))),
                zip=valOrBlank(data.get("{}_zipcode".format(prefix))),
                height=valOrBlank(data.get("{}_height_inches".format(prefix))),
                weight=valOrBlank(data.get("{}_weight_pounds".format(prefix))),
                is_smoker=data.get("{}_smoker".format(prefix)) in ["Y", "y"],
                soh_questions=[]
            )
            answers = dict(Y="Yes", N="No", y="Yes", n="No")
            questions = self.soh_service.get_health_questions(product, state)
            for q_num in range(1, len(questions)+1):
                answer = data.get("{}_question_{}_answer".format(prefix, q_num))
                if answer:
                    base_dict["soh_questions"].append(dict(
                        question=questions[q_num-1].question,
                        answer=answers.get(answer)
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

        out_data["has_spouse_been_treated_6_months"] = data.get("sp_treated_6_months") in ["Y","y"]
        out_data["has_spouse_been_disabled_6_months"] = data.get("sp_disabled_6_months") in ["Y","y"]

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
        out_data["is_spouse_address_same_as_employee"] = emp_address == sp_address

        out_data["employee"].update(build_person("emp"))
        out_data["spouse"].update(build_person("sp"))

        build_beneficiary_data("emp", "employee")
        build_beneficiary_data("sp", "spouse")

        out_data["employee_coverage"] = self.format_coverage(data.get("emp_coverage"), data.get("emp_premium"))
        out_data["spouse_coverage"] = self.format_coverage(data.get("sp_coverage"), data.get("sp_premium"))

        from taa.services.enrollments import EnrollmentRecordParser
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
        out_data['identityToken'] = data.get('emp_date_of_hire', '')
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

