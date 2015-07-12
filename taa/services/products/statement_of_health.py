
from .states import states_by_statecode
from product_forms import ProductFormService

product_form_service = ProductFormService()

def get_template_id_for_product_state(base_product_code, statecode):

    form = product_form_service.form_for_product_code_and_state(base_product_code, statecode)

    if not form.docusign_template_id:
        raise Exception("Form %s for product %s state %s has no template ID"%(form.label, base_product_code, statecode))

    return form.docusign_template_id


class StatementOfHealthQuestionService(object):

    def get_health_questions(self, product, state):
        form = product_form_service.form_for_state(product, state)
        return [question for question in form.questions]

    def get_spouse_questions(self, product, state):
        if not product.is_fpp():
            return []
        else:
            return product_form_service.get_spouse_questions()

    def get_states_with_forms_for_product(self, product):

        code = product.get_base_product_code()
        
        enabled_statecodes = set()
        for form in product_form_service.get_all_application_forms().get(code, []):
            if not form.docusign_template_id:
                # Do not allow enrollment in a form if the template ID is not set
                continue

            for statecode in form.statecodes:
                enabled_statecodes.add(statecode)

        # Transform the set into a list of state objects sorted alphabetically by statecode.
        return sorted([states_by_statecode[sc] for sc in enabled_statecodes], key=lambda x: x['statecode'])

    def get_all_forms_used_for_product(self, product):
        code = product.get_base_product_code()
        return product_form_service.get_all_application_forms().get(code, [])

    def get_all_category_labels_for_product(self, product):

        category_labels = []
        category_labels += self._add_spouse_questions(product)
        category_labels += self._add_health_questions(product)
        return category_labels

    def _add_spouse_questions(self, product):
        if product.is_fpp():
            return [q.label for q in product_form_service.get_spouse_questions()]
        else:
            return []

    def _add_health_questions(self, product):
        "We want to keep them in the same order (mostly) as seen in the forms, but not included more than once"
        labels = []
        used_category_labels = set()
        for form in self.get_all_forms_used_for_product(product):
            for index, question in enumerate(form.questions):
                if question.label in used_category_labels:
                    continue
                else:
                    used_category_labels.add(question.label)

                # Keep it in the same order as outlined
                if len(labels) <= index:
                    labels.append(question.label)
                else:
                    labels.insert(index, question.label)

        return labels