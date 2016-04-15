from taa.tasks import process_hi_acc_enrollments
from apscheduler.schedulers.blocking import BlockingScheduler

#scheduler = BlockingScheduler()


#@scheduler.scheduled_job('cron', hour=0)
def schedule_hi_acc_enrollment_task():
    process_hi_acc_enrollments.delay()

schedule_hi_acc_enrollment_task()

#scheduler.start()
