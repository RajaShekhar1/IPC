

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_replacement_template_id


class FPPReplacementFormTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data):

        product_type = enrollment_data["product_type"]
        state = enrollment_data["agent_data"]["state"]
        template_id = get_replacement_template_id(product_type, state)

        DocuSignServerTemplate.__init__(self, template_id, recipients)

        self.data = enrollment_data

    def generate_tabs(self, recipient):

        if not recipient.is_employee():
            return {}

        # Has to be at least one. Additional, if any, will go on the attachment document.
        policy = self.data['replacement_policies'][0]

        tabs = [
            DocuSignRadioTab('read_aloud', 'yes' if self.data['replacement_read_aloud'] else 'no'),
            DocuSignRadioTab('considering_terminating_existing', 'yes' if self.data['replacement_is_terminating'] else 'no'),
            DocuSignRadioTab('considering_using_funds', 'yes' if self.data['replacement_using_funds'] else 'no'),
            DocuSignTextTab('policy_insurer_name', policy['name']),
            DocuSignTextTab('policy_number', policy['policy_number']),
            DocuSignTextTab('policy_insured', policy['insured']),
            DocuSignTextTab('policy_replaced_or_financing', 'R' if policy['replaced_or_financing'] == 'replaced' else 'F'),
            DocuSignTextTab('policy_reason', policy['replacement_reason']),

            DocuSignTextTab('eeName', self.data.get_employee_name())
        ]

        # Format tabs for docusign
        ds_tabs = {}
        for tab in tabs:
            tab.add_to_tabs(ds_tabs)

        return ds_tabs