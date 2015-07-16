
from reportlab.platypus import Paragraph, Spacer, Table, TableStyle
from reportlab.lib.units import inch
from reportlab.graphics.shapes import Line, String
from reportlab.lib import colors
from reportlab.lib.colors import black

from utils import style, bold_style2, NumberedCanvas, create_attachment_header, create_signature_line
from taa.services.docusign.service import (
    BasePDFDoc,
    DocuSignSigTab,
)

class ChildAttachmentForm(BasePDFDoc):
    def __init__(self, recipients, enrollment_data):
        BasePDFDoc.__init__(self, recipients)

        self.data = enrollment_data

        self.children = []

        # Map recipient signers to the coords of the signature line so we can attach tabs to those lines.
        self.sig_coords = {}

    def add_child(self, child_data):
        """
        Expects a dict of data with the following keys:
            first, last, ssn, gender (male or female), dob, soh_answers (dict with question, answer keys),
            coverage (int), premium (decimal)
        """
        self.children.append(child_data)

    def generate(self):
        flowables = []

        flowables += self.draw_header()

        flowables += self.draw_children_info_table()

        # Statement of Health Questions and Answers for the additional children
        flowables += self.draw_soh_question_and_answers()

        # Signature line and name
        flowables += self.draw_signature_line()

        # Generate the document using reportlab's PLATYPUS layout api.
        self._doc.build(flowables, canvasmaker=NumberedCanvas)

    def get_spacer(self, size=.2 * inch):
        return Spacer(0, size)

    def draw_header(self):
        return create_attachment_header(u"<u>Supplemental Form:  Children\u2019s Information</u>", self.data)

    def draw_children_info_table(self):
        flowables = [
            self.get_spacer(),
            Paragraph("Information and Coverage", bold_style2),
        ]

        child_table_style = TableStyle([
            # Put a box around each cell
             ('GRID', (0,0), (-1,-1), 0.25, colors.black),
            # Align first column
             ('ALIGN', (0,1), (0,-1), 'CENTER'),
            # Align Gender col
             ('ALIGN', (3,1), (3,-1), 'CENTER'),
            # Right-Align money columns
             ('ALIGN', (5,1), (5,-1), 'RIGHT'),
             ('ALIGN', (6,1), (6,-1), 'RIGHT'),
        ])
        child_table_data = [
            [
                "Child #",
                "Name",
                "SSN",
                "Gender",
                "Birth Date",
                "Coverage",
                "Premium",
            ]
        ]
        for num, child in enumerate(self.children):
            if child.get('gender'):
                gender = "M" if child["gender"].lower()[0] == "m" else "F"
            else:
                gender = ""

            row = [
                str(num + 3),
                Paragraph("%s %s"%(child['first'], child['last']), style),
                child.get("ssn", ""),
                gender,
                child['birthdate'],
                "$%s"%child['coverage'],
                "$%s"%child['premium'],
            ]
            child_table_data.append(row)

        flowables += [
            Table(child_table_data, style=child_table_style, hAlign='LEFT')
        ]

        return flowables

    def draw_soh_question_and_answers(self):
        """
        Draws the text of the SOH question and below it a table with each recipient's answer to the question
        """

        question_data = self.group_soh_answers_by_question()

        flowables = [
            self.get_spacer(),
            Paragraph("Statement of Health", bold_style2),
        ]
        for question in question_data:
            # Draw the question.
            flowables.append(Paragraph(question, style))

            # Draw the answer table.
            answer_table_data = []
            for child_name, answer in question_data[question].iteritems():
                if answer is None:
                    answer = "GI"
                answer_table_data.append(
                    ["", Paragraph(child_name, style), Paragraph(answer, style)],
                )
            flowables.append(Table(answer_table_data, colWidths=[.2*inch, 1.5*inch, None]))

        return flowables

    def group_soh_answers_by_question(self):
        """
        We get as input a mapping of all the questions and answers for each child.

        This just groups the data into a dictionary with keys being the questions and the value being a dictionary of the
        children's answers, keyed on the child's first name.
        """

        question_data = {}
        for child in self.children:
            child_name = child['first']
            for soh_data in child['soh_questions']:
                if soh_data.get('is_spouse_only'):
                    continue

                if soh_data['question'] not in question_data:
                    question_data[soh_data['question']] = {}

                question_data[soh_data['question']][child_name] = soh_data['answer']

        return question_data


    def draw_signature_line(self):
        return create_signature_line(self.page_width, self.sig_coords, self.get_signer_recipients())


    def generate_tabs(self, recipient):
        tabs = {}

        if self.is_recipient_signer(recipient):
            # Add a signature tab to the last page

            pdf_x, pdf_y = self.sig_coords[recipient.name]
            pix_x = pdf_x

            pix_y = (self.page_height - pdf_y)
            print("Converted %s, %s to %s %s"%(pdf_x, pdf_y, pix_x, pix_y))
            tab = DocuSignSigTab(x=pix_x, y=pix_y, document_id="1", page_number=str(self.get_num_pages()))
            tab.add_to_tabs(tabs)

        return tabs

    def is_recipient_signer(self, recipient):
        return recipient.is_employee()

    def get_signer_recipients(self):
        return [r for r in self.recipients if r.is_employee()]




if __name__ == "__main__":
    # Test drive the code

    from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient
    from taa.services.docusign.docusign_envelope import EnrollmentDataWrap

    agent = AgentDocuSignRecipient(name="Zachary Mason", email="zmason@delmarsd.com")
    employee = EmployeeDocuSignRecipient(name="Joe Tester", email="zach@zachmason.com")
    test_recipients = [
        agent,
        employee,
    ]

    child_attachment_form = ChildAttachmentForm(test_recipients, enrollment_data=EnrollmentDataWrap(dict(
        agent_data=dict(company_name="DelMar SD"),
        employee=dict(first="Test", last="Employee", ssn="123-12-1234")
    ), None, None))

    child_attachment_form.add_child(dict(
        first="Joe",
        last="Johnson",
        birthdate="12/01/2010",
        gender="male",
        ssn='123-12-1234',
        soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="no")],
        coverage=10000,
        premium='10.50')
    )
    child_attachment_form.add_child(dict(
        first="Susie",
        last="Johnson",
        birthdate="12/01/2012",
        gender="female",
        ssn='111-12-2222',
        soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="no")],
        coverage=10000,
        premium='10.50')
    )
    child_attachment_form.add_child(dict(
        first="Christy",
        last="Johnson",
        birthdate="12/01/2014",
        gender="female",
        ssn='444-12-4321',
        soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="GI")],
        coverage=10000,
        premium='5.25')
    )

    child_attachment_form.generate()
    f = open('test.pdf', 'w+')
    f.write(child_attachment_form._pdf_data.getvalue())
    f.close()
    print("Wrote PDF to test.pdf")

    print("%s pages in document."%child_attachment_form.get_num_pages())