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
        'agent_cases': [dict(company="XYZ, Inc.", state="IN", product="ABC Insurance Product")],
    }
    return render_template('agent/manage_cases.html', **vars)

@app.route("/manage_case")
@login_required
def manage_case():
    return render_template('agent/manage_case.html')