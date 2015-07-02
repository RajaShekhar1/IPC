# Celery tasks

import celery
from flask import render_template

from taa import db
from taa.services.enrollments import SelfEnrollmentEmailService, SelfEnrollmentLinkService, SelfEnrollmentEmailLog
from taa.services.agents import AgentService
from taa.services.cases import CensusRecordService, SelfEnrollmentService, SelfEnrollmentSetup

self_enrollment_email_service = SelfEnrollmentEmailService()
self_enrollment_link_service = SelfEnrollmentLinkService()
self_enrollment_service = SelfEnrollmentService
agent_service = AgentService()
census_record_service = CensusRecordService()


app = celery.Celery('tasks')
app.config_from_object('taa.config_defaults')

@app.task
def send_email(email_log_id):
    email_log = self_enrollment_email_service.get(email_log_id)
    link = email_log.link
    census_record = email_log.census_record
    case = census_record.case
    setup = case.self_enrollment_setup

    #agent = agent_service.get(agent_id)
    #link = self_enrollment_link_service.get(link_id)
    #record = census_record_service.get(record_id) if record_id else None

    if email_log.status != SelfEnrollmentEmailLog.STATUS_PENDING:
        print("Background task found email no longer in pending state, aborting")
        return

    # Render the actual email body.
    email_body = render_template(
        "emails/enrollment_email.html",
        custom_message=setup.email_message,
        greeting=build_email_greeting(census_record, setup),
        enrollment_url=link.url,
        company_name=case.company_name,
    )

    # Send the email
    success = self_enrollment_email_service._send_email(
            from_email=email_log.email_from_address,
            from_name=email_log.email_from_name,
            to_email=email_log.email_to_address,
            to_name=email_log.email_to_name,
            subject=email_log.email_subject,
            body=email_body,
    )

    if success:
        email_log.success = True
        email_log.status = SelfEnrollmentEmailLog.STATUS_SUCCESS
    else:
        email_log.success = False
        email_log.status = SelfEnrollmentEmailLog.STATUS_FAILURE

    db.session.commit()



def build_email_greeting(record, setup):
    salutation = ''
    if setup.email_greeting_salutation:
        salutation = '{} '.format(setup.email_greeting_salutation)
    greeting_end = ''
    if setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_FIRST_NAME:
        greeting_end = "{},".format(record.employee_first)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_FULL_NAME:
        greeting_end = "{} {},".format(record.employee_first, record.employee_last)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_LAST_NAME:
        greeting_end = "{},".format(record.employee_last)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_TITLE_LAST:
        if not record.employee_gender:
            title = 'Mr./Ms.'
        elif record.employee_gender.lower()[0] == 'm':
            title = 'Mr.'
        else:
            title = 'Mrs.'

        greeting_end = "{} {},".format(title, record.employee_last)
    elif setup.email_greeting_type == SelfEnrollmentSetup.EMAIL_GREETING_BLANK:
        greeting_end = ''
    greeting = "{}{}".format(salutation, greeting_end)
    return greeting