#!/usr/bin/env python
"""
Convert exported DocuSign XML templates to proprietery TAA format that can be
used to generate PDFs internally without relying on the DocuSign service.
"""
import os
import base64
import sys
import xml.etree.ElementTree as ET

import dateutil.parser
import dateutil.tz
from flask_script import Command, Option

from ..models import db
from taa.services.enrollments.models import FormTemplate, FormTemplateTabs


class DocusignImportCommand(Command):
    """Imports DocuSign XML templates to the database"""
    option_list = (
        Option('-f', '--filename', dest='filename_or_dirname', required=True,
               help="Input XML file or directory of files"),
        Option('-s', '--safe', dest='safe', required=False,
               help="Safe mode (don't overwrite)", action='store_true'),
    )

    def run(self, filename_or_dirname, safe=False):
        if os.path.isdir(filename_or_dirname):
            filenames = self.get_file_names(filename_or_dirname)
        else:
            filenames = [filename_or_dirname]

        for filename in filenames:
            self.import_xml_file(filename, safe)

    def get_file_names(self, dirname):
        return [
            os.path.join(dirname, fname)
            for fname in os.listdir(dirname)
            if not fname.startswith('.') and not os.path.isdir(os.path.join(dirname, fname))
        ]

    def import_xml_file(self, filename, safe):
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
                label = u'{}.{}'.format(tab['label'], tab['name'])
            else:
                label = tab['label']
            new_tab = FormTemplateTabs(
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
                recipient_role=tab['recipient_role'],
                custom_tab_required=tab['custom_tab_required'],
                custom_tab_locked=tab['custom_tab_locked'],
                template_tab_required=tab['template_tab_required'],
                template_tab_locked=tab['template_tab_locked']
            )
            db.session.add(new_tab)
        db.session.commit()




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
        recipient_roles = {}
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

        recipients = self._xpath('Recipients', root=env)
        for recip in self._xpath('Recipient', root=recipients, multi=True):
            recip_id = int(self._xpath('ID', root=recip).text)
            role = self._xpath('RoleName', root=recip).text
            recipient_roles[recip_id] = role

        tabs = self._xpath('Tabs', root=env)
        for tab in self._xpath('Tab', root=tabs, multi=True):
            id_ = self._xpath('DocumentID', root=tab).text
            doc = doclist[id_]
            recip_id = int(self._xpath('RecipientID', root=tab).text)
            recip_role = recipient_roles.get(recip_id, '')


            def convert_to_bool(val):
                return True if val == "true" else False

            data = {
                'page': int(self._xpath('PageNumber', root=tab).text),
                'x': int(self._xpath('XPosition', root=tab).text),
                'y': int(self._xpath('YPosition', root=tab).text),
                'name': self._xpath('Name', root=tab).text,
                'type': self._xpath('Type', root=tab).text,
                'label': self._xpath('TabLabel', root=tab).text,
                'is_bold': self._get_or_none('Bold', tab, convert_to_bool),
                'is_italic': self._get_or_none('Italic', tab, convert_to_bool),
                'is_underline': self._get_or_none('Underline', tab, convert_to_bool),
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
                'recipient_role':recip_role,
                'custom_tab_required': self._get_or_none('CustomTabRequired', tab, convert_to_bool),
                'custom_tab_locked':self._get_or_none('CustomTabLocked', tab, convert_to_bool),
                'template_tab_required':self._get_or_none('TemplateTabRequired', tab, convert_to_bool),
                'template_tab_locked':self._get_or_none('TemplateTabLocked', tab, convert_to_bool),
            }
            doc['tabs'].append(data)
        return doclist


    def _get_or_none(self, tag, root, f=None):
        if self._xpath(tag, root=root) is not None:
            value = self._xpath(tag, root=root).text
            return value if f is None else f(value)
        return None
