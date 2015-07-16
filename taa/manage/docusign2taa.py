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
from taa.services.enrollments.models import FormTemplate, FormTemplateTabs


class DocusignImportCommand(Command):
    option_list = (
        Option('-f', '--filename', dest='filename', required=True,
               help="Input XML file"),
        Option('-s', '--safe', dest='safe', required=False,
               help="Safe mode (don't overwrite)", action='store_true'),
    )

    def run(self, filename, safe=False):
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
            )
            db.session.add(newtab)
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
            }
            doc['tabs'].append(data)
        return doclist

    def _get_or_none(self, tag, root, f=None):
        if self._xpath(tag, root=root) is not None:
            value = self._xpath(tag, root=root).text
            return value if f is None else f(value)
        return None
