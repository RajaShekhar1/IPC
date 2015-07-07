# Celery tasks

import celery

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

    if email_log.status != SelfEnrollmentEmailLog.STATUS_PENDING:
        print("Background task found email no longer in pending state, aborting")
        return

    # Send the email
    success = self_enrollment_email_service._send_email(
            from_email=email_log.batch.email_from_address,
            from_name=email_log.batch.email_from_name,
            subject=email_log.batch.email_subject,
            to_email=email_log.email_to_address,
            to_name=email_log.email_to_name,
            # use the log's body, which has the rendered email
            body=email_log.email_body,
    )

    if success:
        email_log.success = True
        email_log.status = SelfEnrollmentEmailLog.STATUS_SUCCESS
    else:
        email_log.success = False
        email_log.status = SelfEnrollmentEmailLog.STATUS_FAILURE

    db.session.commit()



