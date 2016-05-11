import json
from io import BytesIO

from reportlab.platypus import Paragraph, Table, TableStyle, Frame, Image as ReportLabImage
from reportlab.lib.units import inch
from reportlab.lib import colors
from reportlab.lib.styles import _baseFontNameB, _baseFontName, ParagraphStyle
from PIL import Image as PILImage
from reportlab.platypus.flowables import Spacer

from PDFAttachment import PDFAttachment
from utils import style, bold_style2, NumberedCanvas, create_attachment_header


class CoverSheetAttachment(PDFAttachment):
    def __init__(self, recipients, enrollment_data, all_enrollments):
        PDFAttachment.__init__(self, recipients, enrollment_data)

        self.case = enrollment_data.case

        # This is a summary document so we need all the enrollment data.
        self.all_enrollments = all_enrollments

    def generate(self):
        flowables = []

        flowables += self.draw_header()

        flowables += self.draw_summary_table()

        #flowables += self.draw_legal_text()

        # Signature line and name
        flowables += self.draw_signature_line()

        # Generate the document using reportlab's PLATYPUS layout api.
        self._doc.leftMargin = .4 * inch
        self._doc.build(flowables, canvasmaker=NumberedCanvas)

    def draw_header(self):

        table_style = TableStyle([
            # Put a box around each cell
            #('GRID', (0, 0), (-1, -1), 0.25, colors.black),
            # Align logo column
            ('ALIGN', (0, 0), (0, -1), 'CENTER'),
            ('VALIGN', (0, 0), (0, -1), 'MIDDLE'),

            # Top vertical align the rest of the row Employee Info
            ('VALIGN', (1, 0), (-1, -1), 'TOP'),

            # Align Gender col
            # ('ALIGN', (3, 1), (3, -1), 'CENTER'),
            # # Right-Align money columns
            # ('ALIGN', (5, 1), (5, -1), 'RIGHT'),
            # ('ALIGN', (6, 1), (6, -1), 'RIGHT'),
        ])

        header_style = ParagraphStyle(name='HeadingCustom',
                                    parent=style,
                                    fontName=_baseFontNameB,
                                    fontSize=16,
                                    leading=14,
                                    spaceBefore=6,
                                    spaceAfter=6)
        header_style2 = ParagraphStyle(name='HeadingCustom3',
                                     parent=style,
                                     fontName=_baseFontName,
                                     fontSize=12,
                                     leading=14,
                                     spaceBefore=0,
                                     spaceAfter=0)

        emp_first = self.data.get_employee_first()
        emp_last = self.data.get_employee_last()
        emp_ssn_last = self.data.get_employee_ssn_last_digits()
        emp_street = data.get_employee_street()
        emp_citystatezip = data.get_employee_city_state_zip()

        summary_flowables = [
            Paragraph("Benefit Election Summary", header_style),
            Spacer(0, .05 * inch),
            Paragraph(u"{} {}".format(emp_first, emp_last), header_style2),
            Paragraph(u"{}".format(emp_street), header_style2),
            Paragraph(u"{}".format(emp_citystatezip), header_style2),

            Paragraph(u"SSN: ###-##-{}".format(emp_ssn_last), header_style2),
        ]

        if self.case.has_logo():
            image_flowable = self.get_reportlab_image_for_case()
            header_row = [image_flowable, Paragraph("", style), summary_flowables]
        else:
            header_row = [summary_flowables, Paragraph("", style), Paragraph('', style)]

        table_data = [
            header_row
        ]

        return [
            Table(table_data, style=table_style, hAlign='LEFT', colWidths=[
                    3.25*inch,
                    .5*inch,
                    3.5*inch
            ])
        ]

    def get_reportlab_image_for_case(self):
        logo_file = BytesIO(self.case.logo_image_data)
        image = PILImage.open(logo_file)
        print("Image size is: ", image.size)
        img_width, img_height = image.size
        w = min(img_width, 200)
        scale = img_height / img_width
        h = max(w * scale, 100)
        image_flowable = ReportLabImage(logo_file, w, h)
        return image_flowable

    def draw_summary_table(self):
        flowables = [
            self.get_spacer(),
            Paragraph("Enrolled / Declined Benefits", bold_style2),
        ]

        table_data = [
            [Paragraph("Benefit Name", bold_style2), "", "", "", "", ""],
            [Paragraph("Insured", bold_style2),
             Paragraph("Relationship to Applicant", bold_style2),
             Paragraph("Coverage / Tier", bold_style2),
             Paragraph("Effective Date", bold_style2),
             Paragraph("Premium Frequency", bold_style2),
             Paragraph("Premium", bold_style2)
             ],
        ]
        styles = [
            # Put a box around the whole table
             ('BOX', (0,0), (-1,-1), 0.5, colors.black),
             ('BACKGROUND', (0, 0), (-1, 0), colors.lightgrey),
             ('GRID', (0, 1), (-1, 1), 0.5, colors.black),
            # Table Spans
            ('SPAN',(0,0),(-1,0)),
            # Align first column
            #  ('ALIGN', (0,1), (0,-1), 'CENTER'),
            # # Align Gender col
            #  ('ALIGN', (3,1), (3,-1), 'CENTER'),
            # # Right-Align money columns
            #  ('ALIGN', (5,1), (5,-1), 'RIGHT'),
            #  ('ALIGN', (6,1), (6,-1), 'RIGHT'),
        ]

        for raw_product_data, i in enumerate(self.all_enrollments):
            product_data = EnrollmentDataWrap(raw_product_data, self.data.case, self.data.enrollment_record)
            table_data.append([
                Paragraph(product_data.get_product().name, bold_style2), "", "", "", "", ""
            ])

            table_styles.append([])


        # for num, child in enumerate(self.children):
        #     if child.get('gender'):
        #         gender = "M" if child["gender"].lower()[0] == "m" else "F"
        #     else:
        #         gender = ""
        #
        #     row = [
        #         str(num + self.starting_child_num),
        #         Paragraph(u"{} {}".format(child['first'], child['last']), style),
        #         child.get("ssn", ""),
        #         gender,
        #         child['birthdate'],
        #         u"${}".format(child['coverage']),
        #         u"${}".format(child['premium']),
        #     ]
        #     table_data.append(row)

        flowables += [
            Table(table_data, style=TableStyle(styles), hAlign='LEFT')
        ]

        return flowables



if __name__ == "__main__":

    from taa import db
    from taa.services.cases import Case
    from taa.services.enrollments import EnrollmentApplication, EnrollmentApplicationService
    from taa.services.docusign.service import EnrollmentDataWrap

    #case = db.session.query(Case).get(1)
    enrollment = db.session.query(EnrollmentApplication).order_by(db.desc(EnrollmentApplication.id)).all()[1]
    case = enrollment.case
    enrollment_data = json.loads(enrollment.standardized_data)
    data = EnrollmentDataWrap(enrollment_data[0], case, enrollment)

    doc = CoverSheetAttachment([], data, enrollment_data)

    doc.generate()
    f = open('test.pdf', 'w+')
    f.write(doc._pdf_data.getvalue())
    f.close()
    print("Wrote PDF to test.pdf")

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