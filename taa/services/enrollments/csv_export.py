import csv
import json

import dateutil.parser

from taa.services import LookupService
from taa.services.cases import AgentSplitsSetup
from taa.services.products.plan_codes import PLAN_CODES_SIMPLE, PLAN_CODES_ACC_CLASS, PLAN_CODES_HI_CLASS

__all__ = ['export_hi_acc_enrollments']

DEFAULT_EXPORT_TARGETS = PLAN_CODES_SIMPLE
AGENT_SPACES = 4
DEPENDENT_SPACES = 4


def export_hi_acc_enrollments_between(start_time=None, end_time=None):
    """
    :param start_time: Start time to filter by
    :type start_time: datetime
    :param end_time: End time to filter by
    :type end_time: datetime
    :rtype: str
    """

    application_service = LookupService('EnrollmentSubmissionService')
    """ :type : taa.services.enrollments.enrollment_submission.EnrollmentSubmissionService"""
    applications = application_service.get_enrollment_applications_between(start_time, end_time)

    return export_hi_acc_enrollments(applications)


def export_hi_acc_enrollments(enrollments, export_targets=None):
    """
    Create a CSV export from the specified enrollments
    :param enrollments: Enrollments to create the CSV for
    :type enrollments: list[EnrollmentApplication]
    :param export_targets: Target base product types
    :type export_targets: list[str]
    :return:
    """
    if export_targets is None:
        export_targets = DEFAULT_EXPORT_TARGETS
    output = csv.StringIO()
    w = csv.writer(output)
    # Member coverage info header
    header = [
        'Group Name',
        'Group Number',
        'Location',
        'Payment_Mode',
        'Effective Date',
        'Enrollment Action: A - Add/Enroll; C - Change of Name or Coverage; T - Terminate',
        'Signature State',
        'SignatureDate',
        'Occupation Class',
        'Rate Level',
        'Owner/Payor/Insured First Name',
        'Owner/Payor/Insured Middle Initial',
        'Owner/Payor/Insured Last Name',
        'Owner/Payor/Insured Address',
        'Owner/Payor/Insured Address City',
        'Owner/Payor/Insured State',
        'Owner/Payor/Insured Zip',
        'Owner/Payor/Insured Phone',
        'Owner/Payor/Insured Social Security Number',
        'Owner/Payor/Insured Date Of Birth',
        'Owner/Payor/Insured Gender',
        'Insured Smoker?',
        'Relation To Primary Insured',
        'Primary Beneficiary Name',
        'PrimaryBenefRelationship',
        'Accident',
        'Hospital Indemnity',
    ]
    for index in range(1, AGENT_SPACES + 1):
        header.extend([
            'Agent_{}'.format(index),
            'Commission Agent_{:02}'.format(index),
            'Subcount Code_{:02}'.format(index)
        ])

    # Dependent info header
    for index in range(1, DEPENDENT_SPACES + 1):
        header.extend([
            'Dependent {} First Name'.format(index),
            'Dependent {} Middle Initial'.format(index),
            'Dependent {} Last Name'.format(index),
            'Dependent {} Gender'.format(index),
            'Dependent {} Relationship to Member'.format(index),
            'Dependent {} DOB'.format(index),
            'Dependent {} Handicapped'.format(index),
        ])
    w.writerow(header)

    products_service = LookupService('ProductService')
    """:type : taa.services.products.ProductService"""
    case_service = LookupService('CaseService')
    """:type : taa.services.cases.case_service.CaseService"""

    # Data rows
    for enrollment in enrollments:
        case = enrollment.case
        json_data = json.loads(enrollment.standardized_data or '{}')
        if isinstance(json_data, dict):
            json_data = [json_data]
        for data in json_data:
            # Skip over anyways
            if data.get('did_decline', True):
                continue
            if data.get('product_id', None) is None:
                continue
            product = products_service.get(data['product_id'])
            
            # Skip non HI-ACC products
            if product.get_base_product_code() not in export_targets:
                continue
            
            employee = data.get('employee')
            spouse = data.get('spouse')
            children = data.get('children')
            coverage = next(c for c in enrollment.coverages if c.product_id == data.get('product_id'))
            agents = [case.owner_agent]
            agents.extend(case.partner_agents)
            if employee is None:
                continue

            # TODO: should be able to pull this from the enrollment data
            occ_class = data.get('occupation_class') if data.get('occupation_class') else enrollment.census_record.occupation_class
            rate_level = case_service.get_classification_for_label(occ_class, case, int(data['product_id']))

            # Member coverage info
            row = [
                case.company_name.upper(),
                case.group_number.upper(),
                case.format_location().upper(),
                case.payment_mode if case.payment_mode >= 0 else '',
                coverage.effective_date.strftime('%m%d%Y') if hasattr(coverage, 'effective_date') and coverage.effective_date else enrollment.signature_time.strftime("%m%d%Y"),
                'A',
                data.get('signed_at_state') if data.get('signed_at_state') else data.get('enrollState'),
                enrollment.signature_time.date().strftime('%m%d%Y'),
                occ_class,
                rate_level,
                employee.get('first', '').upper(),
                # No middle initial
                '',
                employee.get('last', '').upper(),
                employee.get('address1', '').upper(),
                employee.get('city', '').upper(),
                employee.get('state', '').upper(),
                employee['zip'],
                escape_phone(employee['phone']),
                employee['ssn'].strip().replace('-', ''),
                dateutil.parser.parse(employee['birthdate']).strftime('%m%d%Y'),
                employee.get('gender', '').upper(),
                'Y' if employee.get('is_smoker', False) else 'N',
                'SELF',
                data.get('employee_beneficiary1_name').upper(),
                data.get('employee_beneficiary1_relationship').upper(),
                coverage.coverage_selection if coverage.product.code in PLAN_CODES_ACC_CLASS else '',
                coverage.coverage_selection if coverage.product.code in PLAN_CODES_HI_CLASS else '',
            ]
            # Agent(s) info
            agent_splits = [s for s in case_service.get_agent_splits_setup(case) if s.product_id == product.id]
            writing_agent_split = get_writing_agent_split_for_product(agent_splits, product, enrollment)
            writing_agent = get_writing_agent_for_case(case, product, agents, agent_splits, enrollment)
            agent_spaces = 4

            def is_valid_agent_for_split(agent):
                split = next((s for s in agent_splits if s.agent_id == agent.id), None)
                return split is not None and split.split_percentage is not None and split.split_percentage > 0

            split_agents = filter(is_valid_agent_for_split, agents)
            
            if writing_agent_split.split_percentage:
                row.extend(get_agent_cells(writing_agent, writing_agent_split, product, case))
                agent_spaces -= 1
                
            for idx in range(agent_spaces):
                if idx >= len(split_agents):
                    row.extend(['', '', ''])
                    continue
                agent = split_agents[idx]
                split = next((s for s in agent_splits if s.agent_id == agent.id), None)
                if split:
                    row.extend(get_agent_cells(agent, split, product, case))
                else:
                    row.extend([agent.agent_code, '', ''])

            # Dependent info
            dep_spaces = 4
            if coverage.coverage_selection in ['ES', 'EF']:
                # Spouse is first dependent
                row.extend([
                    spouse.get('first', '').upper(),
                    '',
                    spouse.get('last', '').upper(),
                    spouse.get('gender', '').upper() if spouse.get('gender', None) is not None else '',
                    'SPOUSE',
                    dateutil.parser.parse(spouse['birthdate']).strftime('%m/%d/%Y'),
                    # TODO: Determine how to populate 'handicapped' field
                    '',
                ])
                dep_spaces -= 1
            if coverage.coverage_selection in ['EF', 'EC']:
                # Children fill up remaining dependent space(s)
                for index in range(dep_spaces):
                    if index >= len(children):
                        # Not enough children to continue
                        break
                    child = children[index]
                    row.extend([
                        child.get('first', '').upper(),
                        '',
                        child.get('last', '').upper(),
                        child.get('gender').lower() if child.get('gender') is not None else '',
                        'CHILD',
                        dateutil.parser.parse(child['birthdate']).strftime('%m/%d/%Y'),
                        # TODO: Determine how to populate 'handicapped' field
                        '',
                    ])
                    dep_spaces -= 1
            row.extend([''] * dep_spaces * 7)
            w.writerow(row)
    return output.getvalue()

def escape_phone(phone):
    if not phone:
        return ''
    
    return ''.join([c for c in phone if c.isdigit()])
    
        

def get_agent_cells(agent, split, product, case):
    from taa.services.enrollments.xml_export import get_agent_subcount_code
    return [agent.agent_code, split.split_percentage, get_agent_subcount_code(agent.id, case, product.id)]


def get_writing_agent_for_case(case, product, agents, agent_splits, enrollment):
    """
    Get the writing agent for the specified case, product, and agent_splits
    :param case: Case to get the agent for
    :type case: taa.services.cases.models.Case
    :param product: Product to get the writing agent for
    :type product: taa.services.products.models.Product
    :param agents: All agents on the case
    :type agents: list[taa.services.agents.models.Agent]
    :param agent_splits: Agent splits for the case
    :type agent_splits: list[taa.services.cases.models.AgentSplitsSetup]
    :return: Writing Agent for the specified product
    :rtype: taa.services.agents.models.Agent
    """
    split = get_writing_agent_split_for_product(agent_splits, product, enrollment)
    if split is None:
        return None
    agent_id = split.agent_id if split.agent_id is not None else case.agent_id
    return next((a for a in agents if a.id == agent_id), None)


def get_writing_agent_split_for_product(agent_splits, product, enrollment):
    """
    :type agent_splits: list[AgentSplitsSetup]
    :type product: taa.services.products.Product
    :type enrollment: taa.services.enrollments.EnrollmentApplication
    :rtype: AgentSplitsSetup
    """
    agent_split = next((s for s in agent_splits if s.product_id == product.id and s.agent_id is None), None)
    if agent_split is None:
        # noinspection PyArgumentList
        agent_split = AgentSplitsSetup(
            split_percentage='',
            agent_id=enrollment.agent_id,
            product_id=product.id,
            commission_subcount_code=''
        )
    return agent_split
