

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_replacement_template_id


class FPPReplacementFormTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, should_use_docusign_renderer):

        product_type = enrollment_data["product_type"]
        state = enrollment_data["enrollState"]
        template_id = get_replacement_template_id(product_type, state)

        DocuSignServerTemplate.__init__(self, template_id, recipients, should_use_docusign_renderer)

        self.data = enrollment_data

    def generate_tabs(self, recipient):
        tabs = super(FPPReplacementFormTemplate, self).generate_tabs(recipient)

        if not recipient.is_employee():
            return tabs

        tabs += [
            DocuSignRadioTab('read_aloud', 'yes' if self.data['replacement_read_aloud'] else 'no'),
            DocuSignRadioTab('considering_terminating_existing', 'yes' if self.data['replacement_is_terminating'] else 'no'),
            DocuSignRadioTab('considering_using_funds', 'yes' if self.data['replacement_using_funds'] else 'no'),
            DocuSignTextTab('eeName', self.data.get_employee_name()),
            DocuSignTextTab('agentSignName', self.data.get_agent_signing_name()),
        ]

        if len(self.data['replacement_policies']) == 1:
            # Has to be at least one. Additional, if any, will go on the attachment document.
            policy = self.data['replacement_policies'][0]

            if policy['replaced_or_financing'] == 'replaced':
                replaced_or_financing = 'R'
            elif not policy['replaced_or_financing']:
                replaced_or_financing = ''
            else:
                replaced_or_financing = 'F'

            tabs += [
                DocuSignTextTab('policy_insurer_name', policy['name']),
                DocuSignTextTab('policy_number', policy['policy_number']),
                DocuSignTextTab('policy_insured', policy['insured']),
                DocuSignTextTab('policy_replaced_or_financing', replaced_or_financing),
                DocuSignTextTab('policy_reason', policy['replacement_reason']),
            ]
        elif len(self.data['replacement_policies']) > 1:
            tabs.append(DocuSignTextTab('additionalPoliciesNotice', 'SEE ATTACHED'))

        return tabs
