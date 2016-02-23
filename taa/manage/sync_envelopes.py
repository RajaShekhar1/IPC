# Functions to synchronize DocuSign envelopes with our enrollment records
from datetime import timedelta
from itertools import ifilter

from flask_script import Command
from dateutil.parser import parse as dateutil_parse
from taa.services.cases import CaseCensus

from taa.services.enrollments import EnrollmentApplication, EnrollmentApplicationCoverage

from taa.services.docusign.service import DocusignEnvelope, get_docusign_transport
from taa.services.agents import AgentService
from taa.models import db

agent_service = AgentService()


class SyncEnvelopesCommand(Command):
    """Pull agents from stormpath and add them to the DB"""

    def run(self):
        match_envelopes()


def match_envelopes():

    out_for_sig = get_all_out_for_signature_envelopes()
    for envelope_data in out_for_sig:
        eid = envelope_data['envelopeId']

        # Need to filter only envelopes that are waiting on agent signature.
        env = DocusignEnvelope(envelope_data['envelopeUri'], fetch_tabs=True)
        agent_signer = env.get_agent_signing_status()
        if not agent_signer:
            print("Envelope {} did not have an agent signer!".format(eid, ))
            print(envelope_data)
            continue

        if not agent_signer.get('signedDateTime') and not is_linked(env):
            # Agent has not signed
            #print("Found envelope where agent did not sign: {}".format(eid))
            link_envelope(env)


    # Match all completed envelopes next
    for envelope_data in get_all_completed_envelopes():
        env = DocusignEnvelope(envelope_data['envelopeUri'], fetch_tabs=True)

        if not is_linked(env):
            link_envelope(env)


    # Voided?

def is_linked(envelope):

    linked_enrollment = EnrollmentApplication.query.filter(EnrollmentApplication.docusign_envelope_id == envelope.uri)

def link_envelope(envelope):
    status = envelope.get_envelope_status()
    start = parse_utc_date(status['createdDateTime']) + timedelta(hours=-6)
    end = parse_utc_date(status['createdDateTime']) + timedelta(hours=+6)

    emp_status = envelope.get_employee_signing_status()
    if emp_status:
        ee_ssn = find_text_tab(emp_status, 'eeSSN')
        if not ee_ssn:
            print("COULD NOT LINK ENVELOPE: No eeSSN found")
            return None

        ee_ssn = ee_ssn.replace('-', '').strip()
        ee_ssn_masked = "***-**-" + ee_ssn[-4:]

        ee_cov = find_text_tab(emp_status, 'eeCoverage')
        sp_cov = find_text_tab(emp_status, 'spCoverage')
        ch_cov = find_text_tab(emp_status, 'child1Coverage')

        if ee_cov == 'None' or ee_cov == 'NONE':
            ee_cov = None
        if sp_cov == 'None' or sp_cov == 'NONE':
            sp_cov = None
        if ch_cov == 'None' or ch_cov == 'NONE':
            ch_cov = None


        matching_enrollment = EnrollmentApplication.query.filter(EnrollmentApplication.signature_time >= start
            ).filter(EnrollmentApplication.signature_time < end
            ).filter(EnrollmentApplication.docusign_envelope_id == None
            ).options(db.eagerload('coverages')
            ).join('census_record'
            ).filter(CaseCensus.employee_ssn == ee_ssn
            ).first()

        if matching_enrollment:

            # Make sure coverages match too
            matching_ee_cov = find_coverage_for_applicant(ee_cov, matching_enrollment, EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE)
            matching_sp_cov = find_coverage_for_applicant(sp_cov, matching_enrollment, EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE)
            matching_ch_cov = find_coverage_for_applicant(ch_cov, matching_enrollment, EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD)

            if (ee_cov and not matching_ee_cov) or (sp_cov and not matching_sp_cov) or (ch_cov and not matching_ch_cov):
                print("Invalid coverage match on '{}' '{}' {}/{}/{} got {}/{}/{}".format(start.strftime('%F'), ee_ssn_masked, ee_cov, sp_cov, ch_cov,
                     matching_ee_cov.coverage_face_value if matching_ee_cov else '',
                     matching_sp_cov.coverage_face_value if matching_sp_cov else '',
                     matching_ch_cov.coverage_face_value if matching_ch_cov else ''))

            else:
                print("FOUND matching enrollment(s) for envelope: {} to {} '{}' '{}' {}/{}/{}".format(envelope.uri, matching_enrollment.id, start.strftime('%F'), ee_ssn, ee_cov, sp_cov, ch_cov))
                # Link it up
                matching_enrollment.docusign_envelope_id = envelope.uri
                db.session.flush()
                db.session.commit()
        else:
            print("No match on '{}' '{}' {}/{}/{}".format(start.strftime('%F'), ee_ssn_masked, ee_cov, sp_cov, ch_cov))
    else:

        # Must be call center test?
        agent_status = envelope.get_agent_signing_status()
        print("No employee on envelope; got agent signing status: {}".format(agent_status))


def find_coverage_for_applicant(cov_to_match, matching_enrollment, applicant_type):
    return next(ifilter(
        lambda c: c.applicant_type == applicant_type and c.coverage_face_value == cov_to_match.replace(',', ''),
        matching_enrollment.coverages), None)


def find_text_tab(recip_status, tab_name):
    text_tabs = recip_status.get('tabs', {}).get('textTabs', [])
    tab = next(ifilter(lambda t: t['tabLabel'] == tab_name, text_tabs), None)
    if not tab:
        return None
    else:
        return tab['value']


def get_all_completed_envelopes():
    return fetch_envelopes_for_status("Completed")


def get_all_out_for_signature_envelopes():
    return fetch_envelopes_for_status("Signed,Delivered,Sent")


def get_declined_envelopes():
    return fetch_envelopes_for_status('Declined')


def get_voided_envelopes():
    return fetch_envelopes_for_status('Voided')


def fetch_envelopes_for_status(status):
    transport = get_docusign_transport()
    data = transport.get('envelopes?from_date=2013-01-01&status={}'.format(status))
    total = data['resultSetSize']
    print("Processing {} envelopes...".format(total))

    if total == 0:
        return []
    return data['envelopes']

    # # num_results = data['resultSetSize']
    # next_uri = data.get('nextUri')
    # while next_uri:
    #     data = transport.get('search_folders/{}'.format(status))
    #
    #     envelope_data += data['folderItems']
    #     next_uri = data.get('nextUri')
    # return [envelope_data]


def parse_utc_date(val):
    utc_datetime = dateutil_parse(val)

    # Docusign datetimes are UTC and include TZ info. We are storing localtimes for now on the server, so convert it.
    from dateutil.tz import tzlocal
    from datetime import datetime

    # First add the local timezone offset to the UTC date.
    local_utc_offset = datetime.now(tzlocal()).utcoffset()
    local_datetime_with_tz = utc_datetime + local_utc_offset

    # Strip off the timezone info by parsing a format without TZ info.
    #  (We don't want to store the TZ info in the database)
    return dateutil_parse(local_datetime_with_tz.strftime("%FT%T"))


if __name__ == "__main__":
    match_envelopes()