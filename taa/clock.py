import taa.tasks as tasks
import sys
import datetime


# Index of Wednesday in python's datetime package
WEEKDAY_WEDNESDAY = 2


if __name__ == '__main__':
    if len(sys.argv) == 1:
        tasks.process_hi_acc_enrollments.delay()
    command = sys.argv[1].lower()
    if command == 'dell-export':
        tasks.process_hi_acc_enrollments.delay()
    elif command == 'paylogix-export' and datetime.datetime.today().weekday() == WEEKDAY_WEDNESDAY:
        tasks.process_paylogix_csv_generation.delay()
