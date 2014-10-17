"""
AGENT pages and DOCUSIGN inbox
"""
import os
import csv

from flask import render_template, redirect, url_for, flash, request
from flask.ext.stormpath import login_required
from werkzeug import secure_filename

from taa import app
from taa.services.docusign.docu_console import console_url
from taa.model.DocuSign_config import sessionUserApprovedForDocusign
from taa.model.Enrollment import get_product_states, get_product_choices, get_all_states

@app.route("/inbox", methods =['GET'])
@login_required
def inbox():
#    return redirect( console_url())
    if sessionUserApprovedForDocusign():
        return render_template('agent/agent-inbox.html',
                               inboxURL = console_url())
    else:
        flash("You are not yet authorized for signing applications.  Please see your Regional Director for assistance.")
        return redirect(url_for("home"))

@app.route("/manage_cases")
@login_required
def manage_cases():
    
    vars = {'agent_cases':[], 'all_states': get_all_states(),'product_states': get_product_states()} 
    return render_template('agent/manage_cases.html', **vars)


@app.route("/manage_case", defaults={'case_id':None})
@app.route("/manage_case/<case_id>")
@login_required
def manage_case(case_id):
    
    case = Database().get_case(case_id) if case_id else None
    if not case:
        case = Case(id=None, statecode=None, company_name=None, products=[])
    
    vars = {'case':case}
    
    vars['product_choices'] = get_product_choices()
    vars['product_states'] = get_product_states()
    vars['all_states'] = get_all_states()
    
    vars['census_records'] = [
        dict(first="Joe", last="Smith", completed_enrollment=False, elected_coverage=False),
        dict(first="John", last="Smith", completed_enrollment=False, elected_coverage=False),
        dict(first="Jane", last="Smith", completed_enrollment=False, elected_coverage=False),
        dict(first="Jack", last="Johnson", completed_enrollment=False, elected_coverage=False),

    ]
    
    return render_template('agent/case.html', **vars)

CSV_FIELD_FORMAT = ["EMP_SSN", "EMP_FIRST", "EMP_LAST", "EMP_BIRTHDATE", 
                    "SP_SSN", "SP_FIRST", "SP_LAST", "SP_BIRTHDATE", ]

@app.route("/save-case", methods=['POST'], defaults={'case_id':None})
@app.route("/save-case/<case_id>", methods=['POST'])
@login_required
def save_case(case_id):
    
    case_company_name = request.form['company_name']
    case_product = request.form['case_product']
    case_statecode = request.form['case_state']
    is_active = request.form['case_active']
    
    case = Database().get_case(case_id) if case_id else None
    if not case:
        product = Database().get_product_by_code(case_product)
        if not product:
            raise Exception("Bad Product: %s"%case_product)
        
        case = Case(
            id=None,
            company_name=case_company_name,
            statecode=case_statecode,
            products=[product],
            is_active=is_active
        )
        Database().save_case(case)
    
    
    
    file_obj = request.files['csv-file']
    if file_obj and has_csv_extension(file_obj.filename):
        csv_reader = csv.DictReader(file_obj.stream, restkey="extra")
        
        if request.form['upload_type'] == "merge":
            merge_census_data(case_id, csv_reader)
        else:
            replace_census_data(case_id, csv_reader)
    
    return redirect(url_for('manage_case', case_id=case_id))
    
def save_uploaded_file(case_id, file_obj):
    folder_path = os.path.join(app.config['UPLOAD_FOLDER'], case_id)
    if not os.path.exists(folder_path):
        os.makedirs(folder_path)
    path = os.path.join(folder_path, secure_filename(file_obj.filename))
    
    file_obj.save(path)


def merge_census_data(case_id, census_data):
    pass

def replace_census_data(case_id, census_data):
    pass

def has_csv_extension(filename):
    return '.' in filename and filename.lower().rsplit('.', 1)[1] == 'csv'
