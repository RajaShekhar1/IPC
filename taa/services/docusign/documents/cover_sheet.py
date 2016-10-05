import decimal
import json
from io import BytesIO
from itertools import ifilter

from PIL import Image as PILImage
from reportlab.platypus import Paragraph, Table, TableStyle, Frame, Image as ReportLabImage
from reportlab.lib.units import inch
from reportlab.lib import colors
from reportlab.lib.styles import _baseFontNameB, _baseFontName, ParagraphStyle
from reportlab.platypus.flowables import Spacer

from taa import db
from taa.services import LookupService
from taa.services.products.riders import RiderService
from taa.services.enrollments import EnrollmentApplication
from taa.services.docusign.service import EnrollmentDataWrap
from PDFAttachment import PDFAttachment
from utils import style, bold_style2, NumberedCanvas

small_style = ParagraphStyle(name='smallLegal',
                             parent=style,
                             fontName=_baseFontName,
                             fontSize=9,
                             leading=11,
                             spaceBefore=4,
                             spaceAfter=4)


class CoverSheetAttachment(PDFAttachment):
    def __init__(self, recipients, enrollment_data, all_enrollments, enrollment_application=None):
        PDFAttachment.__init__(self, recipients, enrollment_data)

        self.case = enrollment_data.case
        self.enrollment_data = enrollment_data
        self.enrollment_application = enrollment_application

        # This is a summary document so we need all the enrollment data.
        self.all_enrollments = all_enrollments

    def generate(self):
        flowables = []

        flowables += self.draw_header()

        flowables += self.draw_summary_table()

        flowables += self.draw_legal_text()

        # Signature line and name
        # Removing this for now to simplify the emailing of this sheet.
        #flowables += self.draw_signature_line()

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
        emp_street = self.data.get_employee_street()
        emp_citystatezip = self.data.get_employee_city_state_zip()

        summary_flowables = [
            Paragraph("Benefit Election Summary", header_style),
            Spacer(0, .05 * inch),
            Paragraph(u"{} {}".format(emp_first, emp_last), header_style2),
            Paragraph(u"{}".format(emp_street), header_style2),
            Paragraph(u"{}".format(emp_citystatezip), header_style2),
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
        orig_scale = float(img_height) / float(img_width)
        h = min(w * orig_scale, 100)
        if h == 100:
            print("Reducing width from '{}' to '{}'".format(w, w * (h / (w * orig_scale))))

            # also decrease the width to fit
            w *= h / (w * orig_scale)

        print("Resized: ({}, {})".format(w, h))
        image_flowable = ReportLabImage(logo_file, w, h)
        return image_flowable

    def draw_summary_table(self):
        flowables = [
            self.get_spacer(),
            Paragraph("Enrolled / Waived Benefits", bold_style2),
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

        # Iterate through the case products to identify declines.
        case_service = LookupService('CaseService')
        product_service = LookupService('ProductService')
        rider_service = RiderService()
        if self.enrollment_application:
            product_ids = self.enrollment_application.get_enrolled_product_ids()
            product_options = [product_service.get(product_id) for product_id in product_ids]
        else:
            product_options = case_service.get_ordered_products_for_case(self.data.case)
        state = self.enrollment_data.get('enrollState')
        declined_products = self.get_declined_products()
        for i, product in enumerate(product_service.filter_products_by_enrollment_state(product_options, state)):

            product_data = self.get_wrapped_enrollment_data_for_product(product)

            if not product_data:
                product_header = '{} - INELIGIBLE'.format(product.get_brochure_name() if product.get_brochure_name() else product.get_base_product().name)
                applicants = []

            elif product in declined_products:
                # Show Decline
                product_header = '{} - WAIVED'.format(product.get_brochure_name() if product.get_brochure_name() else product.get_base_product().name)
                applicants = []
            else:
                # Show product name, also tier if a simple_coverage option, and riders if riders are included.
                product_header = product.get_brochure_name() if product.get_brochure_name() else product.get_base_product().name
                if product.is_simple_coverage():
                    simple_cov_map = dict(
                        EE='Employee Only',
                        ES='Employee + Spouse',
                        EC='Employee + Children',
                        EF='Family',
                    )
                    coverage_tier = product_data.get_employee_coverage_tier()
                    product_header += ' - {}'.format(simple_cov_map.get(coverage_tier, coverage_tier))

                if product.is_fpp():
                    riders = rider_service.get_case_level_riders_for_product(product_data.case, product)
                    if riders:
                        product_header += " (with {})".format(', '.join([rider.user_facing_name for rider in riders]))

                applicants = product_data.get_applicant_data()

            table_data += [
                [
                    Paragraph(product_header, style), "", "", "", "", ""
                ]
            ]

            for applicant in applicants:
                # Also include applicant-level rider data if provided
                applicant_riders = rider_service.get_applicant_level_riders_for_product(
                        applicant, product_data.case, product)

                name = applicant['name']
                if applicant_riders:
                    name += " (with {})".format(', '.join([rider.user_facing_name
                                                           for rider in applicant_riders]))

                table_data += [
                    [
                        Paragraph(name, style),
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

    def get_enrolled_products(self):
        return [d.get_product() for d in self.get_wrapped_enrollment_data()]

    def get_declined_products(self):
        declined_products = []
        for data in self.get_wrapped_enrollment_data():
            if data['did_decline']:
                declined_products.append(data.get_product())
        return declined_products


    def get_wrapped_enrollment_data(self):
        return [EnrollmentDataWrap(raw_product_data, self.data.case, self.data.enrollment_record)
                for raw_product_data in self.all_enrollments]

    def draw_legal_text(self):

        return [
            Paragraph("<font size='8'>*</font> <font size='9'>(Actual amounts may vary slightly due to rounding)</font>", small_style),
            Spacer(0, .5 * inch),
            Paragraph("""Please take a moment to review and confirm that these elections are correct and that the individuals you wish to have covered under these plans are noted above.  This enrollment confirmation is not a replacement of any policies or certificates of coverage, which will be delivered either electronically or via US Mail service to the address noted at the top of this form.  The summary above is for informational purposes only and any applications may be subject to underwriting, pursuant to the terms of the Plan.""",
                      small_style),
            Spacer(0, .1 * inch),
            Paragraph("""I understand that access to these plans may be limited to specified open enrollment periods and that I may be required to provide evidence of insurability for any program that I have elected to waive at this time, should I chose to participate at a future point in time.  I agree that my premiums for these elected benefits will be submitted to the plan administrator by the amount shown above as my required contribution for the policies, according to the terms of my withholding agreement, until this agreement is amended or terminated.""",
                      small_style),
        ]

    def get_wrapped_enrollment_data_for_product(self, product):
        return next(ifilter(lambda d: d.get_product() == product, self.get_wrapped_enrollment_data()), None)


if __name__ == "__main__":

    pass
    #
    # #case = db.session.query(Case).get(1)
    # enrollment = db.session.query(EnrollmentApplication).order_by(db.desc(EnrollmentApplication.id)).all()[0]
    # case = enrollment.case
    # enrollment_data = json.loads(enrollment.standardized_data)
    # data = EnrollmentDataWrap(enrollment_data[0], case, enrollment)
    #
    # doc = CoverSheetAttachment([], data, enrollment_data)
    #
    # doc.generate()
    # f = open('test.pdf', 'w+')
    # f.write(doc._pdf_data.getvalue())
    # f.close()
    # print("Wrote PDF to test.pdf")
