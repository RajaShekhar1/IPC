import taa.tasks as tasks
import sys
import datetime


# Index of Thursday in python's datetime package.
#  We want to run the paylogix script a little after midnight on Thursday morning.
WEEKDAY_THURSDAY = 3


if __name__ == '__main__':
    if len(sys.argv) == 1:
        tasks.process_hi_acc_enrollments.delay()
    command = sys.argv[1].lower()
    if command == 'dell-export':
        tasks.process_hi_acc_enrollments.delay()
    elif command == 'paylogix-export' and datetime.datetime.today().weekday() == WEEKDAY_THURSDAY:
        tasks.process_paylogix_csv_generation.delay()
