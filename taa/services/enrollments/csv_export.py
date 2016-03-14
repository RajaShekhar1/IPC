import csv
import json

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
    header =[
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

    # Data rows
    for enrollment in enrollments:
        case = enrollment.case
        data = json.loads(enrollment.standardized_data or '{}')
        employee = data.get('employee')
        spouse = data.get('spouse')
        children = data.get('children')
        coverages = enrollment.coverages
        agents = [case.owner_agent]
        agents.extend(case.partner_agents)
        if employee is None:
            continue
        for coverage in coverages:
            if (coverage.product.code not in export_targets or
                    coverage.applicant_type != 'employee'):
                continue
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
            data.get('occupation_class'),
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
        agent_spaces = 4
        commission_per = 100 / min(len(agents), agent_spaces)
        for agent in agents[:agent_spaces]:
            row.extend([agent.agent_code, commission_per])
            agent_spaces -= 1
        row.extend([''] * agent_spaces * 2)

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
                    child['gender'].upper(),
                    'CHILD',
                    dateutil.parser.parse(child['birthdate']).strftime('%m/%d/%Y'),
                    # TODO: Determine how to populate 'handicapped' field
                    '',
                ])
                dep_spaces -= 1
        row.extend([''] * dep_spaces * 7)
        w.writerow(row)
    return output.getvalue()
