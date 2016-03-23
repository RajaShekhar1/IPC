# Functions to synchronize DocuSign envelopes with our enrollment records
import hashlib
import time
from datetime import timedelta, datetime
from itertools import ifilter

import sys
from flask_script import Command, Option
from dateutil.parser import parse as dateutil_parse
from taa.services.cases import CaseCensus

from taa.services.enrollments import EnrollmentApplication, EnrollmentApplicationCoverage

from taa.services.docusign.service import DocusignEnvelope, get_docusign_transport
from taa.services.agents import AgentService
from taa.models import db

agent_service = AgentService()


class SyncEnvelopesCommand(Command):
    """Pull agents from stormpath and add them to the DB"""
    option_list = (
        Option(dest='sync_type', choices=['pending', 'complete'],
               help="One of 'pending' or 'complete'"),
        Option('-r', '--rate-limit', dest='rate_limit', required=False, type=int,
               help="Set the API rate limiter in requests/hr"),
        Option('-s', '--skip', dest='skip_count', required=False, type=int,
               help="Skip over the first skip_count envelopes"),

    )

    def run(self, sync_type, rate_limit=None, skip_count=None):

        envelope_sync = EnvelopeSync(skip_count=skip_count)
        if sync_type == "pending":
            envelope_sync.sync_out_for_sig()
        elif sync_type == "complete":
            rate_limiter = APIRateLimiter(rate_limit)
            envelope_sync.sync_completed(rate_limiter)
        else:
            print("Unknown sync_type '{}'".format(sync_type))

        envelope_sync.print_results_summary()


class EnvelopeSync(object):
    def __init__(self, skip_count=None):
        self.num_linked = 0
        self.num_already_linked = 0
        self.num_skipped = 0
        self.num_total = 0
        self.skip_count = skip_count

        self.progress_count = 0

    def match_envelopes(self):

        self.sync_out_for_sig()

        self.sync_completed()

        # TODO: Link up voided

        self.print_results_summary()

    def print_results_summary(self):
        print("Linked {}".format(self.num_linked))
        print("Already linked: {}".format(self.num_already_linked))
        print("Skipped: {}".format(self.num_skipped))

    def sync_out_for_sig(self):
        out_for_sig = self.get_all_out_for_signature_envelopes()
        self.num_total = len(out_for_sig)


        for envelope_data in out_for_sig:
            self.progress_count += 1

            eid = envelope_data['envelopeId']

            # Need to filter only envelopes that are waiting on agent signature.
            env = DocusignEnvelope(envelope_data['envelopeUri'], fetch_tabs=True)
            agent_signer = env.get_agent_signing_status()
            if not agent_signer:
                print("Envelope {} did not have an agent signer!".format(eid))
                print(envelope_data)
                continue

            if not agent_signer.get('signedDateTime') and not self.is_linked(env):
                # Agent has not signed
                # print("Found envelope where agent did not sign: {}".format(eid))
                did_link = self.link_envelope(env)
                if did_link:
                    self.num_linked += 1
                else:
                    self.num_skipped += 1
            elif self.is_linked(env):
                self.num_already_linked += 1

            # Replace agent account with embedded agent signing if necessary
            # print("Progress: {}".format(self.progress_count))
            # if not agent_signer.get('clientUserId'):
            #     enrollment = self.is_linked(env)
            #     clientUserId = hashlib.sha256("agent-{}".format(enrollment.agent_id)).hexdigest()
            #     new_recip_id = unicode(int(agent_signer['recipientId']) + 10)
            #     print("clientUserId {}".format(clientUserId))
            #
            #     print("REPLACING AGENT ON ENVELOPE {}".format(eid))
            #     print("Agent: {}".format(agent_signer))
            #
            #     tab_types = ['textTabs', 'signHereTabs', 'radioGroupTabs', 'initialHereTabs', 'dateSignedTabs']
            #
            #     updated_tabs = {}
            #     for tt in tab_types:
            #         updated_tabs[tt] = []
            #
            #         if agent_signer['tabs'].get(tt):
            #             for tab in agent_signer['tabs'][tt]:
            #                 tab['recipientId'] = new_recip_id
            #                 updated_tabs[tt].append(tab)
            #
            #     new_recip = {
            #         'recipientId': new_recip_id,
            #         'routingOrder': u'2',
            #         'roleName': u'Agent',
            #         'name': agent_signer['name'],
            #         'email': agent_signer['email'],
            #         'tabs': updated_tabs,
            #         'clientUserId': clientUserId,
            #         'templateRequired': False,
            #     }
            #
            #     transport = get_docusign_transport()
            #     print("Adding embedded recip...")
            #     transport.post('{}/recipients'.format(env.get_envelope_base_url()),
            #                    {"signers": [new_recip]})
            #     print("Deleting recip...")
            #     transport.delete('{}/recipients'.format(env.get_envelope_base_url()), {"signers": [{"recipientId": agent_signer['recipientId']}]})
            #
            #     # For some reason need to post tabs again
            #     print("Posting tabs...")
            #     transport.post('{}/recipients/{}/tabs'.format(env.get_envelope_base_url(), new_recip_id), updated_tabs)
            #     print("Finished agent ")

    def sync_completed(self, rate_limiter=None):
        completed = self.get_all_completed_envelopes()
        self.num_total = len(completed)
        self.progress_count = 0
        # Match all completed envelopes next
        num_requests = 0
        start_time = datetime.now()
        current_wait = 8


        if not rate_limiter:
            rate_limiter = APIRateLimiter(500)


        if not self.skip_count:
            self.skip_count = 0

        self.progress_count += self.skip_count

        envelopes_to_process = reversed(completed[self.skip_count:])

        for i, envelope_data in enumerate(envelopes_to_process):
            current_time = datetime.now()

            env = DocusignEnvelope(envelope_data['envelopeUri'], fetch_tabs=True)

            if not self.is_linked(env):
                did_link = self.link_envelope(env)
                if did_link:
                    self.num_linked += 1
                    print("{}/{} Linked".format(i + 1, len(completed)))
                else:
                    print("{}/{} Skipped".format(i + 1, len(completed)))
                    self.num_skipped += 1

                rate_limiter.wait(num_requests=2)

            else:
                self.num_already_linked += 1
                print("{}/{} Already Linked".format(i + 1, len(completed)))

    def is_linked(self, envelope):
        return EnrollmentApplication.query.filter(EnrollmentApplication.docusign_envelope_id == envelope.uri).first()

    def link_envelope(self, envelope):
        emp_status = envelope.get_employee_signing_status()
        if emp_status:
            return self.match_enrollment(emp_status, envelope)
        else:
            # Must be call center test?
            agent_status = envelope.get_agent_signing_status()
            print("No employee on envelope; got agent signing status: {}".format(agent_status))
            return False

    def match_enrollment(self, emp_status, envelope):

        ee_ssn = self.find_text_tab(emp_status, 'eeSSN')
        if not ee_ssn:
            print("COULD NOT LINK ENVELOPE: No eeSSN found")
            return None
        ee_name = self.find_text_tab(emp_status, 'eeName')
        if not ee_name:
            ee_first = self.find_text_tab(emp_status, 'eeFName')
            ee_last = self.find_text_tab(emp_status, 'eeLName')
            ee_name = u'{} {}'.format(ee_first, ee_last)

        ee_ssn = ee_ssn.replace('-', '').strip()
        ee_ssn_masked = "***-**-" + ee_ssn[-4:]
        ee_cov = self.find_text_tab(emp_status, 'eeCoverage')
        sp_cov = self.find_text_tab(emp_status, 'spCoverage')
        ch_cov = self.find_text_tab(emp_status, 'child1Coverage')
        if ee_cov == 'None' or ee_cov == 'NONE':
            ee_cov = None
        if sp_cov == 'None' or sp_cov == 'NONE':
            sp_cov = None
        if ch_cov == 'None' or ch_cov == 'NONE':
            ch_cov = None

        envelope_status = envelope.get_envelope_status()
        created_time = parse_utc_date(envelope_status['createdDateTime'])
        start = created_time + timedelta(hours=-6)
        end = created_time + timedelta(hours=+6)
        matching_enrollment = EnrollmentApplication.query.filter(EnrollmentApplication.signature_time >= start
            ).filter(EnrollmentApplication.signature_time < end
            ).filter(EnrollmentApplication.docusign_envelope_id == None
            ).options(db.eagerload('coverages')
            ).join('census_record'
            ).filter(CaseCensus.employee_ssn == ee_ssn
            ).first()
        if matching_enrollment:
            return self.match_coverages(ch_cov, ee_cov, ee_ssn_masked, envelope, matching_enrollment, sp_cov, created_time, ee_name)
        else:
            print(u"No match on envelope '{}' '{}' '{}' '{}' {}/{}/{}".format(envelope.uri,
                start.strftime('%F%T'), ee_name, ee_ssn_masked, ee_cov, sp_cov,
                                                               ch_cov))
            return False


    def match_coverages(self, ch_cov, ee_cov, ee_ssn_masked, envelope, matching_enrollment, sp_cov, created_time, ee_name):
        # Make sure coverages match too
        matching_ee_cov = self.find_coverage_for_applicant(ee_cov, matching_enrollment,
                                                      EnrollmentApplicationCoverage.APPLICANT_TYPE_EMPLOYEE)
        matching_sp_cov = self.find_coverage_for_applicant(sp_cov, matching_enrollment,
                                                      EnrollmentApplicationCoverage.APPLICANT_TYPE_SPOUSE)
        matching_ch_cov = self.find_coverage_for_applicant(ch_cov, matching_enrollment,
                                                      EnrollmentApplicationCoverage.APPLICANT_TYPE_CHILD)
        if (ee_cov and not matching_ee_cov) or (sp_cov and not matching_sp_cov) or (ch_cov and not matching_ch_cov):
            print(
                u"Invalid coverage match on '{}' '{}' {} {}/{}/{} got {}/{}/{}".format(created_time.strftime('%F%T'), ee_ssn_masked, ee_name,
                                                                                   ee_cov, sp_cov, ch_cov,
                                                                                   matching_ee_cov.coverage_face_value if matching_ee_cov else '',
                                                                                   matching_sp_cov.coverage_face_value if matching_sp_cov else '',
                                                                                   matching_ch_cov.coverage_face_value if matching_ch_cov else ''))
            return False

        else:
            print(u"FOUND matching enrollment(s) for envelope: {} to {} '{}' '{}' {}/{}/{}".format(
                envelope.uri, matching_enrollment.id, created_time.strftime('%F%T'), ee_name, ee_ssn_masked, ee_cov, sp_cov, ch_cov))

            # Link it up
            matching_enrollment.docusign_envelope_id = envelope.uri
            db.session.flush()
            db.session.commit()

            # Sync the enrollment
            envelope.enrollment_record = matching_enrollment
            envelope.update_enrollment_status()

            return True

    def find_coverage_for_applicant(self, cov_to_match, matching_enrollment, applicant_type):
        if not cov_to_match:
            cov_to_match = ''
        return next(ifilter(
            lambda c: c.applicant_type == applicant_type and c.coverage_face_value == cov_to_match.replace(',', ''),
            matching_enrollment.coverages), None)

    def find_text_tab(self, recip_status, tab_name):
        text_tabs = recip_status.get('tabs', {}).get('textTabs', [])
        tab = next(ifilter(lambda t: t['tabLabel'] == tab_name, text_tabs), None)
        if not tab:
            return None
        else:
            return tab['value']

    def get_all_completed_envelopes(self):
        return self.fetch_envelopes_for_status("Completed")

    def get_all_out_for_signature_envelopes(self):
        return self.fetch_envelopes_for_status("Signed,Delivered,Sent")

    def get_declined_envelopes(self):
        return self.fetch_envelopes_for_status('Declined')

    def get_voided_envelopes(self):
        return self.fetch_envelopes_for_status('Voided')

    def fetch_envelopes_for_status(self, status):
        transport = get_docusign_transport()
        data = transport.get('envelopes?from_date=2015-06-01&status={}'.format(status))
        total = data['resultSetSize']
        print("Processing {} envelopes...".format(total))

        if total == 0:
            return []
        return data['envelopes']


class APIRateLimiter(object):
    def __init__(self, limit):
        self.num_requests = 0
        self.current_wait = 8
        self.hourly_limit = limit
        self.start_time = datetime.now()

    def wait(self, num_requests):
        self.num_requests += num_requests

        # Limit our rate of requests to under req_limit/hr
        one_hour_in_seconds = 3600.0

        diff = datetime.now() - self.start_time
        seconds = diff.total_seconds()
        avg_freq = num_requests / seconds
        expected_freq = self.hourly_limit / one_hour_in_seconds
        drift = (avg_freq - expected_freq)
        if drift > 1 or drift < -1:
            self.current_wait += drift

        time.sleep(self.current_wait)


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

