# Celery tasks

import celery
from flask import render_template

from taa import db
from taa.services.enrollments import SelfEnrollmentEmailService, SelfEnrollmentLinkService, SelfEnrollmentEmailLog
from taa.services.agents import AgentService
from taa.services.cases import CensusRecordService, SelfEnrollmentService

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

    #agent = agent_service.get(agent_id)
    #link = self_enrollment_link_service.get(link_id)
    #record = census_record_service.get(record_id) if record_id else None

    if email_log.status != SelfEnrollmentEmailLog.STATUS_PENDING:
        print("Background task found email no longer in pending state, aborting")
        return

    # Send the email
    success = self_enrollment_email_service._send_email(
            from_email=email_log.email_from_address,
            from_name=email_log.email_from_name,
            to_email=email_log.email_to_address,
            to_name=email_log.email_to_name,
            subject=email_log.email_subject,
            body=email_log.email_body,
    )

    if success:
        email_log.success = True
        email_log.status = SelfEnrollmentEmailLog.STATUS_SUCCESS
    else:
        email_log.success = False
        email_log.status = SelfEnrollmentEmailLog.STATUS_FAILURE

    db.session.commit()



