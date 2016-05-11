import decimal
import json
from io import BytesIO

from reportlab.platypus import Paragraph, Table, TableStyle, Frame, Image as ReportLabImage
from reportlab.lib.units import inch
from reportlab.lib import colors
from reportlab.lib.styles import _baseFontNameB, _baseFontName, ParagraphStyle
from PIL import Image as PILImage
from reportlab.platypus.flowables import Spacer
from taa import db
from taa.services.enrollments import EnrollmentApplication, EnrollmentApplicationService
from taa.services.docusign.service import EnrollmentDataWrap

from PDFAttachment import PDFAttachment
from utils import style, bold_style2, NumberedCanvas, create_attachment_header, bold_style

small_style = ParagraphStyle(name='smallLegal',
                             parent=style,
                             fontName=_baseFontName,
                             fontSize=9,
                             leading=11,
                             spaceBefore=4,
                             spaceAfter=4)


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

        flowables += self.draw_legal_text()

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
        emp_street = self.data.get_employee_street()
        emp_citystatezip = self.data.get_employee_city_state_zip()

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
            ('GRID', (0, 1), (-1, -1), 0.5, colors.black),

            # Table Spans
            ('SPAN',(0,0),(-1,0)),

            ('ALIGN', (3, 3), (3, -1), 'CENTER'),
            ('ALIGN', (4, 3), (4, -1), 'CENTER'),
            #
            # # right-align premium column
            ('ALIGN', (2, 3), (2, -1), 'RIGHT'),
            # ('ALIGN', (5, 3), (5, 3), 'RIGHT'),
            #  ('ALIGN', (6,1), (6,-1), 'RIGHT'),
        ]

        row_count = 2

        total_premium = decimal.Decimal('0.00')

        for i, raw_product_data in enumerate(self.all_enrollments):


            product_data = EnrollmentDataWrap(raw_product_data, self.data.case, self.data.enrollment_record)
            table_data += [
                [
                    Paragraph(product_data.get_product().name, style), "", "", "", "", ""
                ]

            ]

            applicants = product_data.get_applicant_data()

            for applicant in applicants:
                table_data += [
                    [
                        Paragraph(applicant['name'], style),
                        Paragraph(applicant['relationship'], style),
                        applicant['coverage'],
                        applicant['effective_date'],
                        applicant['mode'],
                        applicant['formatted_premium'],
                    ]
                ]

                total_premium += applicant['premium']

            styles += [
                ('BACKGROUND', (0, row_count), (-1, row_count), colors.lightgrey),
                ('SPAN', (0, row_count), (-1, row_count)),
            #    ('GRID', (0, row_count+1), (-1, row_count+1), 0.5, colors.black),
            ]

            # The current row index is the
            row_count += (1 + len(applicants))

        # Total row
        table_data += [
            [
                "Total Premium Deduction / Draft Amount *",
                "","","","",
                "$ {}".format(self.data.format_money(total_premium)),
            ]
        ]
        styles += [
            ('BACKGROUND', (0, row_count), (-1, row_count), colors.lightgrey),
            ('SPAN', (0, row_count), (-2, row_count)),

            ('ALIGN', (5, 0), (5, row_count), 'RIGHT'),
            ('ALIGN', (0, row_count), (4, row_count), 'RIGHT'),
        ]

        flowables += [
            Table(table_data, style=TableStyle(styles), hAlign='LEFT', colWidths=[
                2 * inch,
                1.1 * inch,
                inch,
                inch,
                inch,
                inch,
            ])
        ]

        return flowables

    def draw_legal_text(self):

        return [
            Paragraph("<font size='8'>*</font> <font size='9'>(Actual amounts may very slightly due to rounding)</font>", small_style),
            Spacer(0, .5 * inch),
            Paragraph("""I agree that my compensation will be reduced by the amount shown above as my required contribution for the policies I have elected under the Plan, continuing for each pay period until this agreement is amended or terminated. I understand that the reduction in my cash compensation under this agreement will be in addition to any reductions under other agreements or benefit plans. In addition, pre-tax premiums, if any, paid under this Salary Redirection Agreement may reduce my future Social Security benefits, as my compensation for Social Security tax purposes has been reduced. I understand that insurance claim payments under certain health and medical coverage may be subject to Federal and State taxes when the premiums are paid for on a pre-tax basis.""",
                      small_style),
            Spacer(0, .1 * inch),
            Paragraph("""I have been informed of the differences between existing and proposed new policies. I understand the terms of coverage are not an exact match. I understand that health conditions which I may presently have, (so called pre-existing conditions), may not be immediately or fully covered under a new policy. This could result in denial or delay of a claim for benefits under this new policy, whereas a similar claim might have been payable under my present policy. Pre-existing Condition mean a condition(s) for which a Covered Person has been medically diagnosed, treated by, or sought advice from, or consulted with, a Doctor before his/her effective date of coverage (or waiting period start date) under a Policy. The pre-existing condition time frame (often 12 months) is specified in each policy.""",
                      small_style),
        ]


if __name__ == "__main__":



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
