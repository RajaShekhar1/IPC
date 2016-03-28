import csv
import json
from taa.services import LookupService

import dateutil.parser

__all__ = ['export_acc_hi']

DEFAULT_EXPORT_TARGETS = ['ACC', 'HI']
AGENT_SPACES = 4
DEPENDENT_SPACES = 4


def export_acc_hi(enrollments, export_targets=None):
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
        'Issue_Date',
        'Enrollment Action: A - Add/Enroll; C - Change of Name or Coverage; T - Terminate',
        'Signature State',
        'SignatureDate',
        'Occupation Class',
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
            'Subcount Code_{:02}'.format(index),
        ])
    header.extend([
        'Owner/Payor/Insured First Name',
        'Owner/Payor/Insured Middle Initial',
        'Owner/Payor/Insured Last Name',
        'Owner/Payor/Insured Social Security Number',
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
    case_service = LookupService('CaseService')

    # Data rows
    for enrollment in enrollments:
        case = enrollment.case
        json_data = json.loads(enrollment.standardized_data or '{}')
        if isinstance(json_data, dict):
            json_data = [json_data]
        for data in json_data:
            product = products_service.get(data['product_id'])

            employee = data.get('employee')
            spouse = data.get('spouse')
            children = data.get('children')
            coverage = next(c for c in enrollment.coverages if c.product_id == data.get('product_id'))
            agents = [case.owner_agent]
            agents.extend(case.partner_agents)
            if employee is None:
                continue

            rate_level = case_service.get_classification_for_label(data.get('occupation_class'), case,
                                                                    int(data['product_id']))

            # Member coverage info
            row = [
                case.company_name.upper(),
                case.group_number.upper(),
                case.format_location().upper(),
                case.payment_mode if case.payment_mode >= 0 else '',
                case.issue_date.strftime('%m%d%Y') if hasattr(case, 'issue_date') else '',
                'A',
                data.get('signed_at_state'),
                enrollment.signature_time.date().strftime('%m%d%Y'),
                # TODO: this function needs to be written
                rate_level,
                employee['first'].upper(),
                '',
                employee['last'].upper(),
                employee['address1'].upper(),
                employee['city'].upper(),
                employee['state'].upper(),
                employee['zip'],
                employee['phone'],
                employee['ssn'],
                dateutil.parser.parse(employee['birthdate']).strftime('%m%d%Y'),
                employee['gender'].upper(),
                'Y' if employee['is_smoker'] else 'N',
                'SELF',
                data.get('employee_beneficiary1_name').upper(),
                data.get('employee_beneficiary1_relationship').upper(),
                coverage.coverage_selection if coverage.product.code == 'ACC' else '',
                coverage.coverage_selection if coverage.product.code == 'HI' else '',
            ]
            # Agent(s) info
            agent_splits = [s for s in case_service.get_agent_splits_setup(case) if s.product_id == product.id]
            writing_agent_split = get_writing_agent_split_for_product(agent_splits, product)
            writing_agent = get_writing_agent_for_case(case, product, agents, agent_splits)
            agent_spaces = 4

            split_agents = [s for s in agents if s.id is not None]

            if writing_agent is not None:
                row.extend([writing_agent.agent_code, writing_agent_split.split_percentage,
                            writing_agent_split.commission_subcount_code])
            else:
                row.extend(['', '', ''])
            for agent in split_agents:
                split = next(s for s in agent_splits if s.agent_id == agent.id)
                row.extend([agent.agent_code, split.split_percentage, split.commission_subcount_code])
            row.extend([''] * (agent_spaces - len(agents)) * 2)

            # Dependent info
            row.extend([
                employee['first'].upper(),
                '',
                employee['last'].upper(),
                employee['ssn'],
            ])
            dep_spaces = 4
            if coverage.coverage_selection in ['ES', 'EF']:
                # Spouse is first dependent
                row.extend([
                    spouse['first'].upper(),
                    '',
                    spouse['last'].upper(),
                    spouse['gender'].upper(),
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
                        child['first'].upper(),
                        '',
                        child['last'].upper(),
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


def get_writing_agent_for_case(case, product, agents, agent_splits):
    split = get_writing_agent_split_for_product(agent_splits, product)
    if split is None:
        return None
    agent_id = split.agent_id if split.agent_id is not None else case.agent_id
    return next(a for a in agents if a.id == agent_id)


def get_writing_agent_split_for_product(agent_splits, product):
    return next((s for s in agent_splits if s.product_id == product.id and s.agent_id is None), None)
