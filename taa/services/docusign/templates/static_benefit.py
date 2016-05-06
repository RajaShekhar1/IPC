from taa.services import LookupService
from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_template_id


class StaticBenefitTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, use_docusign_renderer, enrollment_application):
        """
        :param recipients:
        :param enrollment_data:
         :type enrollment_data: taa.services.docusign.docusign_service.EnrollmentDataWrap
        :param use_docusign_renderer:
        """
        product_id = enrollment_data.get_product().id
        state = enrollment_data["enrollState"]
        template_id = get_template_id('Static Benefit', state, product_id)
        self.application = enrollment_application

        DocuSignServerTemplate.__init__(self, template_id, recipients, use_docusign_renderer)

        self.data = enrollment_data

    def generate_tabs(self, recipient, purpose):
        tabs = super(StaticBenefitTemplate, self).generate_tabs(recipient, purpose)

        application_service = LookupService('EnrollmentApplicationService')
        census_export = application_service.get_export_dictionary(self.application.census_record, self.application)

        tabs += (DocuSignTextTab(k, v) for k, v in census_export.iteritems())

        if recipient.is_employee():
            tabs += self.make_general_tabs()

        return tabs

    def make_general_tabs(self):
        premium = self.data.get_formatted_employee_premium() if self.data.did_employee_select_coverage() else ''
        tabs = [
            DocuSignTextTab('eeName', self.data.get_employee_name()),
            DocuSignTextTab('eePremium', premium)
        ]

        return tabs

    def should_include_bank_draft(self):
        return self.data.should_include_bank_draft()
