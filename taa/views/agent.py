"""
AGENT pages and DOCUSIGN inbox
"""

from flask import render_template, redirect, url_for, flash
from flask.ext.stormpath import login_required

from taa import app

from taa.docu_console import console_url
from taa.model.DocuSign_config import sessionUserApprovedForDocusign

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
    vars = {
        'agent_cases': [
            dict(id=1, company="XYZ, Inc.", state="IN", product="ABC Insurance Product"),
            dict(id=2, company="DelMar SD Inc.", state="IN", product="ABC Insurance Product"),
            dict(id=3, company="Blah Blah Inc.", state="WI", product="ABC Insurance Product"),
            dict(id=4, company="Orange Computers", state="MN", product="ABC Insurance Product"),
            dict(id=5, company="Red Bull, Inc.", state="NM", product="ABC Insurance Product"),
            dict(id=6, company="Monsters, Inc.", state="AZ", product="ABC Insurance Product"),
            
                        ],
    }
    return render_template('agent/manage_cases.html', **vars)

@app.route("/manage_case", defaults={'case_id':None})
@app.route("/manage_case/<case_id>")
@login_required
def manage_case(case_id):
    
    return render_template('agent/manage_case.html')

