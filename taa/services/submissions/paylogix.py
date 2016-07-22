from taa.config_defaults import PAYLOGIX_FTP_HOSTNAME, PAYLOGIX_FTP_PASSWORD, PAYLOGIX_FTP_USERNAME, \
    PAYLOGIX_PGP_KEY_ID, PAYLOGIX_FTP_DIRECTORY
from taa.services import LookupService
from datetime import datetime


def upload_paylogix_file(data):
    ftp_service = LookupService('FtpService')
    filename = 'five_star_paylogix_export_{0}.csv.pgp'.format(datetime.now().strftime('%Y-%m-%d'))
    ftp_service.send_file(ftp_service.get_paylogix_server(), filename, data)
