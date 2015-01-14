import datetime
import json

from taa.core import DBService
from taa.core import db
from models import Enrollment, EnrollmentCoverage

from taa.services.cases import CaseService
from taa.services.products import ProductService
case_service = CaseService()
product_service = ProductService()

class EnrollmentService(DBService):
    
    __model__ = Enrollment

    def __init__(self, *args, **kwargs):
        super(EnrollmentService, self).__init__(*args, **kwargs)

        self.coverages_service = EnrollmentCoverageService()

    def save_enrollment_data(self, data, census_record):
        
        # Update the census record data with the new data
        case_service.update_census_record_from_enrollment(census_record, data)
        
        # Store extra enrollment data on the enrollment
        enrollment = self._create_enrollment(census_record, data)
        
        # Save coverages
        self._save_coverages(enrollment, data)
        
        return enrollment
        
        
    def _create_enrollment(self, census_record, data):
        
        # Link to census record and case, if it exists
        if census_record:
            case_id = census_record.case_id
            census_record_id = census_record.id
        else:
            case_id = None
            census_record_id = None
            
        enrollment_data = dict(
            case_id = case_id,
            census_record_id = census_record_id,

            # TODO: Decline 

            # TODO: mode, method

            # Signing info
            signature_time = datetime.datetime.now(),
            signature_city = data['enrollCity'],
            signature_state = data['enrollState'],
            identity_token = data['identityToken'],
            identity_token_type = data['identityType'],
            
            # Owner
            is_employee_owner = data['employee_owner'] != "other",
            employee_other_owner_name = data['employee_other_owner_name'],
            employee_other_owner_ssn = data['employee_other_owner_ssn'],
            is_spouse_owner = data['spouse_owner'] != "other",
            spouse_other_owner_name = data['spouse_other_owner_name'],
            spouse_other_owner_ssn = data['spouse_other_owner_ssn'],
            
            # emp beneficiary
            is_employee_beneficiary_spouse = data['employee_beneficiary'] != 'other',
            employee_beneficiary_name = data['employee_beneficiary_name'],
            employee_beneficiary_ssn = data['employee_other_owner_ssn'],
            employee_beneficiary_relationship = data['employee_beneficiary_relationship'],
            employee_beneficiary_birthdate = data['employee_beneficiary_dob'],
            
            # spouse beneficiary
            is_spouse_beneficiary_employee = data['spouse_beneficiary'] != 'other',
            spouse_beneficiary_name = data['spouse_beneficiary_name'],
            spouse_beneficiary_ssn = data['spouse_beneficiary_ssn'],
            spouse_beneficiary_relationship = data['spouse_beneficiary_relationship'],
            spouse_beneficiary_birthdate = data['spouse_beneficiary_dob'],
        )
        
        return self.create(**enrollment_data)
        
    
    def _save_coverages(self, enrollment, data):
        
        product_data = data['product_data']
        product = product_service.get(product_data['id'])
        
        if data['employee_coverage']:
            emp_coverage = self.coverages_service.create_coverage(enrollment, product, 
                                                 data, data['employee'], data['employee_coverage'], 
                                                 EnrollmentCoverage.APPLICANT_TYPE_EMPLOYEE)
        if data['spouse_coverage']:
            sp_coverage = self.coverages_service.create_coverage(enrollment, product,
                                                 data, data['spouse'], data['spouse_coverage'],
                                                 EnrollmentCoverage.APPLICANT_TYPE_SPOUSE)
        
        if data['child_coverages']:
            ch_coverage = self.coverages_service.create_coverage(enrollment, product,
                                                data, data['children'][0], data['child_coverages'][0],
                                                EnrollmentCoverage.APPLICANT_TYPE_SPOUSE)

        db.session.commit()
        
    
class EnrollmentCoverageService(DBService):
    
    __model__ = EnrollmentCoverage

    def create_coverage(self, enrollment, product, data, applicant_data, applicant_coverage, applicant_type):
        return self.create(**dict(
            enrollment_id=enrollment.id,
            product_id=product.id,
            applicant_type=applicant_type,

            height_inches=applicant_data['height'],
            weight_pounds=applicant_data['weight'],
            is_smoker=applicant_data['is_smoker'],

            coverage_face_value=applicant_coverage['face_value'],
            weekly_premium=applicant_coverage['weekly_premium'],
            soh_answers=json.dumps(dict(
                existing_insurance=data['existing_insurance'],
                replacing_insurance=data['replacing_insurance'],
            ))
        ))