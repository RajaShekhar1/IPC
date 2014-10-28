


# enrollments = Table('enrollments', metadata,
#     Column('id', Integer, primary_key=True),
#     Column('case_id', Integer, ForeignKey('cases.id'), nullable=False),
#     Column('application_date', DateTime),
#     Column('employee_first', String, nullable=False),
#     Column('employee_last', String, nullable=False),
#     Column('employee_ssn', String(9), nullable=True),
#     Column('employee_email', String, nullable=False),
#     # The token is a special string submitted with the application (like a driver's license number)
#     Column('identity_token', String),
#     Column('census_record_id', Integer, ForeignKey('census_employees.id'), nullable=True)
# )
# enrollment_requests = Table('enrollment_requests', metadata,
#     Column('id', Integer, primary_key=True),
#     Column('enrollment_id', Integer, ForeignKey('enrollments.id'), nullable=False),
#     Column('token', String, nullable=False),
#     Column('expiration', Date, nullable=False),
# )
# 
# enrollment_elections = Table('enrollment_elections', metadata,
#     Column('id', Integer, primary_key=True),
#     Column('enrollment_id', Integer, ForeignKey('enrollments.id')),
#     Column('product_id', Integer, ForeignKey('products.id')),
#     
#     Column('coverage_taken', Boolean),
#     
#     Column('total_annual_premium', Numeric),
#     Column('employee_coverage', Numeric),
#     Column('employee_annual_premium', Numeric),
#     Column('spouse_coverage', Numeric),
#     Column('spouse_annual_premium', Numeric),
# 
#     Column('children_coverage', Numeric),
#     Column('children_annual_premium', Numeric),
#     
# )