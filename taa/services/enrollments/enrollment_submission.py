import json
from taa.services import RequiredFeature
from taa.services.docusign.docusign_envelope import \
    EnrollmentDataWrap,\
    create_envelope_recipients,\
    create_fpp_envelope_components


class EnrollmentSubmissionService(object):
    def submit_imported_enrollment(self, enrollment_record):

        processor = EnrollmentSubmissionProcessor()
        processor.submit_to_docusign(enrollment_record)

        return processor


class EnrollmentSubmissionProcessor(object):

    pdf_generator_service = RequiredFeature('ImagedFormGeneratorService')

    def submit_to_docusign(self, enrollment_record):

        data_wrap = EnrollmentDataWrap(json.loads(enrollment_record.standardized_data),
                                       census_record=enrollment_record.census_record,
                                       case=enrollment_record.case)
        employee_recip, recipients = create_envelope_recipients(enrollment_record.case, data_wrap)
        components = create_fpp_envelope_components(data_wrap, recipients, should_use_docusign_renderer=False)

        # Create PDF

        import datetime
        #with open('test_pdfs/test_output-{}.pdf'.format(datetime.datetime.now()), 'wb+') as f:
        #    f.write(pdf_bytes)

        # Submit to DocuSign
