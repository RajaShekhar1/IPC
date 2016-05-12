import taa.tasks as tasks
import sys


def schedule_hi_acc_enrollment_task():
    tasks.process_hi_acc_enrollments.delay()

if __name__ == '__main__':
    if len(sys.argv) == 1:
        schedule_hi_acc_enrollment_task()
    command = sys.argv[1].lower()
    if command == 'dell-export':
        tasks.process_hi_acc_enrollments.delay()
    elif command == 'paylogix-export':
        tasks.process_paylogix_csv_generation.delay()
