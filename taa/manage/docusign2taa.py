#!/usr/bin/env python
"""
Convert exported DocuSign XML templates to proprietery TAA format that can be
used to generate PDFs internally without relying on the DocuSign service.
"""

import base64
import sys
import xml.etree.ElementTree as ET

import dateutil.parser
import dateutil.tz
from flask_script import Command, Option

from ..models import db
from taa.services.docusign.service import DocuSignTextTab, DocuSignRadioTab
from taa.services.enrollments import ImagedFormGeneratorService
from taa.services.enrollments.models import FormTemplate, FormTemplateTabs


TEST_ENROLLMENT_DATA = [
    DocuSignRadioTab('existingInsAgent', 'no'),
    DocuSignRadioTab('replaceAgent', 'yes'),

    DocuSignTextTab('AgentSignature', 'Agent Signature'),
    DocuSignTextTab('Signature 5', 'Signature'),
    DocuSignTextTab('Signature 179', 'Signature'),
    DocuSignTextTab('Date Signed', 'Date Signed'),
    # Dupe vv; currently no way to differentiate (must change DocuSign template)
    DocuSignTextTab('Date Signed', 'Date Signed'),

    DocuSignTextTab('employer', 'DelMar Test Case'),
    DocuSignTextTab('group_number', 'GRP-NUM-EX123'),
    DocuSignTextTab('eeName', 'John Smith'),
    DocuSignTextTab('eeFName', 'John'),
    DocuSignTextTab('eeLName', 'Smith'),
    DocuSignTextTab('eeDOB', '10/10/1981'),
    DocuSignTextTab('eeSSN', '123-12-4124'),
    DocuSignTextTab('eeAddress', '123 Sesame Apt L6'),
    DocuSignTextTab('eeCity', 'Chicago'),
    DocuSignTextTab('eeState', 'IL'),
    DocuSignTextTab('eeZip', '12345'),
    DocuSignTextTab('eePhone', '(999), 888-7777'),
    DocuSignTextTab('eeEmail', 'john@johnsmith.com'),
    DocuSignRadioTab('payment_mode', 'monthly'),
    DocuSignTextTab('eeEnrollCityState', 'Lansing, MI'),
    DocuSignTextTab('eeEnrollCity', 'Lansing'),
    DocuSignTextTab('eeEnrollState', 'MI'),
    DocuSignTextTab('date_of_hire', '01/01/2010'),
    DocuSignTextTab('agentCode', '55117'),
    DocuSignTextTab('agentSignName', 'Testing Agent'),
    DocuSignTextTab('Employer', 'DelMar Test Case'),
    DocuSignTextTab('eeOtherOwnerName', 'OTHER OWNER'),
    DocuSignTextTab('eeOtherOwnerName2', 'OTHER OWNER'),
    DocuSignTextTab('eeOtherOwnerSSN', '000-99-9888'),
    DocuSignTextTab('spouse_owner_notice', 'SPOUSE POLICY OWNER: OTHER SPOUSE OWNER, 092-03-7489'),
    DocuSignTextTab('eeEmailPart1', 'john'),
    DocuSignTextTab('eeEmailPart2', 'johnsmith.com'),
    DocuSignRadioTab('actively_at_work', 'no'),
    DocuSignRadioTab('eeSOH1', 'no'),
    DocuSignRadioTab('eeSOH2', 'no'),
    DocuSignRadioTab('eeSOH3', 'no'),
    DocuSignRadioTab('eeSOH4', 'no'),
    DocuSignRadioTab('eeSOH5', 'no'),
    DocuSignRadioTab('eeSOH6', 'no'),
    DocuSignRadioTab('eeSOH7', 'no'),
    DocuSignTextTab('eeSOH1gi', ''),
    DocuSignTextTab('eeSOH2gi', ''),
    DocuSignTextTab('eeSOH3gi', ''),
    DocuSignTextTab('eeSOH4gi', ''),
    DocuSignTextTab('eeSOH5gi', ''),
    DocuSignTextTab('eeSOH6gi', ''),
    DocuSignTextTab('eeSOH7gi', ''),
    DocuSignTextTab('spName', 'Jane Smith'),
    DocuSignTextTab('spFName', 'Jane'),
    DocuSignTextTab('spLName', 'Smith'),
    DocuSignTextTab('spDOB', '12/12/1980'),
    DocuSignTextTab('spSSN', '328-79-3408'),
    DocuSignTextTab('sp_address_same_as_employee', 'SAME AS EMPLOYEE'),
    DocuSignTextTab('sp_email_same_as_employee', 'SAME AS EMPLOYEE'),
    DocuSignRadioTab('spouse_hospital_six_months', 'yes'),
    DocuSignRadioTab('spouse_disability_six_months', 'no'),
    DocuSignRadioTab('spSOH1', 'no'),
    DocuSignRadioTab('spSOH2', 'no'),
    DocuSignRadioTab('spSOH3', 'no'),
    DocuSignRadioTab('spSOH4', 'no'),
    DocuSignRadioTab('spSOH5', 'no'),
    DocuSignRadioTab('spSOH6', 'no'),
    DocuSignRadioTab('spSOH7', 'no'),
    DocuSignTextTab('spSOH1gi', ''),
    DocuSignTextTab('spSOH2gi', ''),
    DocuSignTextTab('spSOH3gi', ''),
    DocuSignTextTab('spSOH4gi', ''),
    DocuSignTextTab('spSOH5gi', ''),
    DocuSignTextTab('spSOH6gi', ''),
    DocuSignTextTab('spSOH7gi', ''),
    DocuSignTextTab('eeCoverage', '125,000'),
    DocuSignTextTab('eePremium', '52.35'),
    DocuSignTextTab('spCoverage', '50,000'),
    DocuSignTextTab('spPremium', '24.75'),
    DocuSignTextTab('eePremiumTotal', '52.35'),
    DocuSignTextTab('spPremiumTotal', '24.75'),
    DocuSignTextTab('childPremiumTotal', '14.94'),
    DocuSignTextTab('totalAllPremium', '92.04'),
    DocuSignTextTab('eeBeneFullName', 'EMP BENEFICIARY'),
    DocuSignTextTab('eeBeneAge', '16'),
    DocuSignTextTab('eeBeneRelationship', 'EMP BENE REL'),
    DocuSignTextTab('eeBeneDOB', '01/10/1999'),
    DocuSignTextTab('eeBeneSSN', '019-01-0101'),
    DocuSignTextTab('eeContBeneFullName', 'EMP CONT NAME'),
    DocuSignTextTab('eeContBeneAge', '27'),
    DocuSignTextTab('eeContBeneRelationship', 'EMP CONT REL'),
    DocuSignTextTab('eeContBeneDOB', '12/11/1987'),
    DocuSignTextTab('eeContBeneSSN', '881-82-8181'),
    DocuSignTextTab('spBeneFullName', 'SP BENE NAME'),
    DocuSignTextTab('spBeneAge', '14'),
    DocuSignTextTab('spBeneRelationship', 'SP BENE REL'),
    DocuSignTextTab('spBeneDOB', '12/12/2000'),
    DocuSignTextTab('spBeneSSN', '998-98-9898'),
    DocuSignTextTab('spContBeneFullName', 'SP CONT BENE NAME'),
    DocuSignTextTab('spContBeneAge', '36'),
    DocuSignTextTab('spContBeneRelationship', 'SP CONT BENE REL'),
    DocuSignTextTab('spContBeneDOB', '03/03/1979'),
    DocuSignTextTab('spContBeneSSN', '333-33-3333'),
    DocuSignTextTab('child1Name', 'Susie Smith'),
    DocuSignTextTab('child1DOB', '12/13/1999'),
    DocuSignTextTab('child1SSN', '879-23-8479'),
    DocuSignTextTab('child1Coverage', '10,000'),
    DocuSignTextTab('child1Premium', '4.98'),
    DocuSignRadioTab('child1Gender', 'female'),
    DocuSignRadioTab('c1SOH1', 'no'),
    DocuSignRadioTab('c1SOH2', 'no'),
    DocuSignRadioTab('c1SOH3', 'no'),
    DocuSignRadioTab('c1SOH4', 'no'),
    DocuSignRadioTab('c1SOH5', 'no'),
    DocuSignRadioTab('c1SOH6', 'no'),
    DocuSignRadioTab('c1SOH7', 'no'),
    DocuSignTextTab('c1SOH1gi', ''),
    DocuSignTextTab('c1SOH2gi', ''),
    DocuSignTextTab('c1SOH3gi', ''),
    DocuSignTextTab('c1SOH4gi', ''),
    DocuSignTextTab('c1SOH5gi', ''),
    DocuSignTextTab('c1SOH6gi', ''),
    DocuSignTextTab('c1SOH7gi', ''),
    DocuSignTextTab('child2Name', 'Johnny Smith'),
    DocuSignTextTab('child2DOB', '12/14/2000'),
    DocuSignTextTab('child2SSN', '232-32-2323'),
    DocuSignTextTab('child2Coverage', '10,000'),
    DocuSignTextTab('child2Premium', '4.98'),
    DocuSignRadioTab('child2Gender', 'male'),
    DocuSignRadioTab('c2SOH1', 'no'),
    DocuSignRadioTab('c2SOH2', 'no'),
    DocuSignRadioTab('c2SOH3', 'no'),
    DocuSignRadioTab('c2SOH4', 'no'),
    DocuSignRadioTab('c2SOH5', 'no'),
    DocuSignRadioTab('c2SOH6', 'no'),
    DocuSignRadioTab('c2SOH7', 'no'),
    DocuSignTextTab('c2SOH1gi', ''),
    DocuSignTextTab('c2SOH2gi', ''),
    DocuSignTextTab('c2SOH3gi', ''),
    DocuSignTextTab('c2SOH4gi', ''),
    DocuSignTextTab('c2SOH5gi', ''),
    DocuSignTextTab('c2SOH6gi', ''),
    DocuSignTextTab('c2SOH7gi', ''),
    DocuSignTextTab('child3Name', 'Robert Smith'),
    DocuSignTextTab('child3DOB', '12/11/2001'),
    DocuSignTextTab('child3SSN', '777-77-7777'),
    DocuSignTextTab('child3Coverage', '10,000'),
    DocuSignTextTab('child3Premium', '4.98'),
    DocuSignRadioTab('child3Gender', 'male'),
    DocuSignRadioTab('c3SOH1', 'no'),
    DocuSignRadioTab('c3SOH2', 'no'),
    DocuSignRadioTab('c3SOH3', 'no'),
    DocuSignRadioTab('c3SOH4', 'no'),
    DocuSignRadioTab('c3SOH5', 'no'),
    DocuSignRadioTab('c3SOH6', 'no'),
    DocuSignRadioTab('c3SOH7', 'no'),
    DocuSignTextTab('c3SOH1gi', ''),
    DocuSignTextTab('c3SOH2gi', ''),
    DocuSignTextTab('c3SOH3gi', ''),
    DocuSignTextTab('c3SOH4gi', ''),
    DocuSignTextTab('c3SOH5gi', ''),
    DocuSignTextTab('c3SOH6gi', ''),
    DocuSignTextTab('c3SOH7gi', ''),
    DocuSignTextTab('extra_children_notice', 'SEE ATTACHED FOR ADDITIONAL CHILDREN'),
    DocuSignRadioTab('enrollType', 'assist'),
    DocuSignRadioTab('productType', 'FPPTI'),
    DocuSignRadioTab('existingIns', 'no'),
    DocuSignRadioTab('existingInsAgent', 'no'),
    DocuSignRadioTab('replaceAgent', 'yes'),
    DocuSignRadioTab('replace', 'yes'),
    DocuSignRadioTab('eeGender', 'male'),
    DocuSignRadioTab('eeSmoking', 'nonsmoker'),
    DocuSignRadioTab('eeOwner', 'other'),
    DocuSignRadioTab('spGender', 'female'),
    DocuSignRadioTab('spSmoking', 'nonsmoker'),
    DocuSignRadioTab('spOwner', 'other'),
]


class DocusignImportCommand(Command):
    option_list = (
        Option('-f', '--filename', dest='filename', required=True,
               help="Input XML file"),
        Option('-s', '--safe', dest='safe', required=False,
               help="Safe mode (don't overwrite)", action='store_true'),
        Option('-t', '--test', dest='test_id', required=False,
               help="Write PDF populated with data from specified "
                    "enrollment ID for testing"),
    )

    def run(self, filename, safe=False, test_id=None):
        with open(filename, 'r') as f:
            doc = DocusignDocument(filename)
        template_id = doc.meta['template_id']
        print("Importing Template ID '{}' from '{}'".format(template_id,
                                                            filename))
        if len(doc.doclist) > 1:
            print("ERROR: Multiple documents in one XML file aren't supported.")
            sys.exit(1)
        old = FormTemplate.query.filter_by(template_id=template_id).first()
        if old is not None:
            if safe:
                print("ERROR: Template ID '{}' exists in the database; "
                      "aborting.".format(template_id))
                sys.exit(1)
            else:
                print("WARNING: Template ID '{}' exists in the database; "
                      "overwriting.".format(template_id))
                try:
                    db.session.query(FormTemplateTabs).filter(
                        FormTemplateTabs.form_template_id == old.id).delete()
                except Exception:
                    # No tabs to delete; ignore
                    pass
                db.session.delete(old)
                db.session.commit()
        docdata = next(iter(doc.doclist.values()))
        new = FormTemplate(
            template_id=template_id,
            name=doc.meta['name'],
            description=doc.meta['description'],
            modified_at=doc.meta['modified_at'],
            data=docdata['data'],
            pages=doc.meta['pages'])
        db.session.add(new)
        db.session.commit()
        verify = FormTemplate.query.filter_by(template_id=template_id)
        if verify is None:
            print("ERROR: Import failed for an unknown reason.")
            sys.exit(1)
        print("Saved with ID={}".format(verify.first().id))
        for tab in docdata['tabs']:
            if tab['custom_type'] == 'Radio':
                # Radio buttons use `label`.`name` for the column name
                # to remain unique
                label = '{}.{}'.format(tab['label'], tab['name'])
            else:
                label = tab['label']
            newtab = FormTemplateTabs(
                template=new,
                page=tab['page'],
                x=tab['x'],
                y=tab['y'],
                name=tab['name'],
                type_=tab['type'],
                label=label,
                is_bold=tab['is_bold'],
                is_italic=tab['is_italic'],
                is_underline=tab['is_underline'],
                custom_type=tab['custom_type'],
                width=tab['width'],
                height=tab['height'],
                font=tab['font'],
                font_size=tab['font_size'],
                font_color=tab['font_color'],
            )
            db.session.add(newtab)
        db.session.commit()
        if test_id is not None:
            # Write test enrollment
            outpath = '/tmp/pdf/enrollment-{}-TEST.pdf'.format(template_id)
            writer = ImagedFormGeneratorService()
            writer.generate_form_pdf(template_id, TEST_ENROLLMENT_DATA,
                                     path=outpath)


class DocusignDocument(object):
    tree = None
    root = None
    ns = None

    def __init__(self, path):
        self._parse(path)
        self.meta = self._get_metadata()
        self.doclist = self._parse_envelope()

    def _write_form(self, path, data):
        with open(path, 'wb') as o:
            o.write(data)

    def _parse(self, path):
        self.tree = ET.parse(path)
        self.root = self.tree.getroot()
        self.ns = {'ds': self.root.tag[1:self.root.tag.find('}')]}

    def _xpath(self, xpath, root=None, multi=False):
        if root is None:
            root = self.root
        if multi:
            return root.findall('.//ds:{}'.format(xpath), self.ns)
        else:
            return root.find('.//ds:{}'.format(xpath), self.ns)

    def _get_metadata(self):
        root = self._xpath('EnvelopeTemplateDefinition')
        return {
            'template_id': self._xpath('TemplateID', root).text,
            'name': self._xpath('Name', root).text,
            'description': self._xpath('TemplateDescription', root).text,
            'modified_at':
                dateutil.parser.parse(self._xpath('LastModified', root).text,
                                      tzinfos=dateutil.tz.tzutc),
            'pages': int(self._xpath('PageCount', root).text),
        }

    def _parse_envelope(self):
        env = self._xpath('Envelope')
        docs = self._xpath('Documents', root=env)
        doclist = {}
        for doc in self._xpath('Document', root=docs, multi=True):
            id_ = self._xpath('ID', root=doc).text
            doclist[id_] = {
                'id': id_,
                'name': self._xpath('Name', root=doc).text,
                'data': base64.b64decode(self._xpath('PDFBytes',
                                                     root=doc).text.strip()),
                'tabs': [],
            }
        tabs = self._xpath('Tabs', root=env)
        for tab in self._xpath('Tab', root=tabs, multi=True):
            id_ = self._xpath('DocumentID', root=tab).text
            doc = doclist[id_]
            data = {
                'page': int(self._xpath('PageNumber', root=tab).text),
                'x': int(self._xpath('XPosition', root=tab).text),
                'y': int(self._xpath('YPosition', root=tab).text),
                'name': self._xpath('Name', root=tab).text,
                'type': self._xpath('Type', root=tab).text,
                'label': self._xpath('TabLabel', root=tab).text,
                'is_bold': self._get_or_none('Bold', tab,
                                             lambda x:
                                             True if x == 'true' else False),
                'is_italic': self._get_or_none('Italic', tab,
                                               lambda x:
                                               True if x == 'true' else False),
                'is_underline': self._get_or_none('Underline', tab,
                                                  lambda x:
                                                  True if x == 'true' else False),
                'custom_type':
                    self._get_or_none('CustomTabType', tab),
                'width': self._get_or_none('CustomTabWidth', tab, int),
                'height': self._get_or_none('CustomTabHeight', tab, int),
                'font': self._get_or_none('Font', root=tab),
                'font_size':
                    self._get_or_none('FontSize', tab,
                                      lambda x: int(''.join([c for c in x
                                                             if c.isdigit()]))),
                'font_color': self._get_or_none('FontColor', root=tab),
            }
            doc['tabs'].append(data)
        return doclist

    def _get_or_none(self, tag, root, f=None):
        if self._xpath(tag, root=root) is not None:
            value = self._xpath(tag, root=root).text
            return value if f is None else f(value)
        return None
