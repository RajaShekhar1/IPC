from reportlab.platypus import Paragraph, Table, TableStyle
from reportlab.lib.units import inch
from reportlab.lib import colors

from PDFAttachment import PDFAttachment
from utils import style, bold_style2, NumberedCanvas, create_attachment_header


class ChildAttachmentForm(PDFAttachment):
    def __init__(self, recipients, enrollment_data, starting_child_num=3):
        PDFAttachment.__init__(self, recipients, enrollment_data)

        self.children = []
        self.starting_child_num = starting_child_num

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
                str(num + self.starting_child_num),
                Paragraph(u"{} {}".format(child['first'], child['last']), style),
                child.get("ssn", ""),
                gender,
                child['birthdate'],
                u"${}".format(child['coverage']),
                u"${}".format(child['premium']),
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
        
        # Outer loop is over the ordered question list of the first child
        for soh_data in self.children[0]['soh_questions']:
            if soh_data.get('is_spouse_only'):
                continue
            
            question = soh_data['question']
            
            # Draw the question.
            flowables.append(Paragraph(question, style))

            # Draw the answer table with an answer for each child.
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


if __name__ == "__main__":
    # Test drive the code
    pass
    # from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient
    # from taa.services.docusign.docusign_envelope import EnrollmentDataWrap
    # from mock import Mock
    #
    # case = Mock(company_name='DelMar SD')
    # agent = AgentDocuSignRecipient(name="Zachary Mason", email="zmason@delmarsd.com")
    # employee = EmployeeDocuSignRecipient(name="Joe Tester", email="zach@zachmason.com")
    # test_recipients = [
    #     agent,
    #     employee,
    # ]
    #
    # child_attachment_form = ChildAttachmentForm(test_recipients, enrollment_data=EnrollmentDataWrap(dict(
    #     agent_data=dict(company_name="DelMar SD"),
    #     employee=dict(first="Test", last="Employee", ssn="123-12-1234")
    # ), None, case))
    #
    # child_attachment_form.add_child(dict(
    #     first="Joe",
    #     last="Johnson",
    #     birthdate="12/01/2010",
    #     gender="male",
    #     ssn='123-12-1234',
    #     soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="no")],
    #     coverage=10000,
    #     premium='10.50')
    # )
    # child_attachment_form.add_child(dict(
    #     first="Susie",
    #     last="Johnson",
    #     birthdate="12/01/2012",
    #     gender="female",
    #     ssn='111-12-2222',
    #     soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="no")],
    #     coverage=10000,
    #     premium='10.50')
    # )
    # child_attachment_form.add_child(dict(
    #     first="Christy",
    #     last="Johnson",
    #     birthdate="12/01/2014",
    #     gender="female",
    #     ssn='444-12-4321',
    #     soh_questions=[dict(question="Have you ever eaten a lollipop?", answer="GI")],
    #     coverage=10000,
    #     premium='5.25')
    # )
    #
    # child_attachment_form.generate()
    # f = open('test.pdf', 'w+')
    # f.write(child_attachment_form._pdf_data.getvalue())
    # f.close()
    # print("Wrote PDF to test.pdf")
    #
    # print("%s pages in document."%child_attachment_form.get_num_pages())