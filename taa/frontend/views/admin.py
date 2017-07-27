import traceback
import urllib
from flask import (
    render_template,
    url_for,
    flash,
    json,
    redirect,
    request,
    session, jsonify)
from flask_login import current_user, login_required


from taa import app, groups_required
from nav import get_nav_menu
from taa.models import db
from taa.old_model.Registration import TAA_UserForm, AdminNewUserForm
from taa.old_model.Enrollment import AgentActivationEmail, NotifyAdminEmail
from taa.services import LookupService
from taa.services.users.UserService import search_stormpath_accounts, UserService

agent_service = LookupService('AgentService')
api_token_service = LookupService('ApiTokenService')


@app.route('/admin', methods=['GET', 'POST'])
@login_required
@groups_required(['admins', 'home_office'], all=False)
def admin():
    accounts = []
    for agent in agent_service.get_sorted_agents():

        accounts.append({
            'fname': agent.first,
            'lname': agent.last,
            'email': agent.email,
            'email_url': urllib.quote_plus(agent.email),
            'agency': agent.agency,
            'agent_code': agent.agent_code,
            'signing_name': agent.signing_name,
            'status': agent.get_status(),
        })

    return render_template('admin/admin.html', accounts=accounts, nav_menu=get_nav_menu(), is_user_admin=agent_service.is_user_admin(current_user))


@app.route('/new-user', methods=['GET', 'POST'])
@login_required
@groups_required(['admins', 'home_office'], all=False)
def new_user():
    form = AdminNewUserForm()
    if form.validate_on_submit():
        data = form.data

        from taa.services.agents import OktaService
        okta_service = OktaService()
        if okta_service.get_user_by_email(data['email']):
            flash("This email is already associated with an account in this organization.")
            render_template('admin/update-user.html',
                            form=form,
                            group_membership=[],
                            groups=get_group_options(),
                            token="",
                            nav_menu=get_nav_menu(),
                            is_new=True,
                            )

        if data['password'] != data['repassword']:
            flash("Passwords don't match.")
            return redirect(request.url)

        # Attempt to create the user's account in Okta.
        try:
            okta_user_data = okta_service.create_user(dict(
                profile=dict(
                    firstName=data['fname'],
                    lastName=data['lname'],
                    email=data['email'],
                    login=data['email'],
                    agent_code=data['agent_code'].upper(),
                    agency=data['agency'],
                    signing_name=data['signing_name'],
                ),
                credentials=dict(
                    password=dict(value=data['password'])
                )
            ))

            if not okta_user_data:
                flash(okta_service.last_error_message)
            else:
                # Create the user account
                agent = agent_service.create_user(
                    email = data['email'],
                    password = data['password'],
                    given_name = data['fname'],
                    middle_name = data.get('middle_name'),
                    surname = data['lname'],
                    signing_name=data['signing_name'],
                    agent_code=data['agent_code'].upper(),
                    agency=data['agency'],
                    activated=True,
                    okta_id = okta_user_data['id'],
                )

                # Add to the agents group
                agent.add_group("agents")

                # Add the group in okta too
                OktaService().set_user_groups(agent, ['agents'])

                session['registered_name'] = data['fname']

                db.session.commit()

                # Go to the edit user page
                flash("User created successfully!")

                return redirect(url_for('updateUser', user=data['email']))
        except Exception as err:
            flash('There was a problem creating the user.')
            from taa.tasks import send_admin_error_email
            send_admin_error_email("Admin create user error:<br>{}".format(err.message), [traceback.format_exc()])



    return render_template('admin/update-user.html',
                           form=form,
                           group_membership=[],
                           groups=get_group_options(),
                           token="",
                           nav_menu=get_nav_menu(),
                           is_new=True,
                           )


@app.route('/edituser', methods=['GET', 'POST'])
@groups_required(['admins', 'home_office'], all=False)
def updateUser():
    user_email = request.args['user']
    form = TAA_UserForm()
    group_membership = []
    token = None

    # initially pre-populate the form with account values
    if request.method == 'GET':
        user = agent_service.find(email=user_email).first()
        if not user:
            flash('Failed to find user ' + user_email)
            return redirect(url_for('admin'))

        form.fname.data = user.first
        form.lname.data = user.last
        form.email.data = user.email

        group_membership = [g.group for g in user.groups]

        token = api_token_service.get_token_by_sp_href(user.okta_id)

        if not user.custom_data:
            user.custom_data = {}

        form.agency.data = user.agency
        form.agent_code.data = user.agent_code
        form.signing_name.data = user.signing_name
        form.activated.data = user.activated

        # Agents only
        if 'agents' in group_membership:
            # Load agent-specific items.
            form.is_restricted_to_licensed_states.data = user.is_restricted_to_licensed_states
            form.licensed_states.data = user.licensed_states

    if form.validate_on_submit():
        try:
            user = agent_service.find(email=user_email).first()
            if not user:
                flash('Failed to find user ' + user_email)
                return redirect(request.url)
            else:
                data = form.data

                # Ensure new email address is not already being used
                if (user_email != data['email'] and
                        not is_email_available(data['email'])):
                    flash("The email address '{}' is already in use by "
                          "someone else".format(data['email']))
                    return redirect(request.url)

                # Update the database user
                user.first = data['fname']
                user.last = data['lname']
                user.email = data['email']

                user.agency = data['agency']
                user.agent_code = data['agent_code'].upper()
                user.signing_name = data['signing_name']
                user.activated = data['activated']           

                groups = request.values.getlist('groups')
                if 'agents' in groups and ('home_office' in groups or
                                           'admins' in groups):
                    flash("User cannot be in the 'agents' and 'admin'/"
                          "'home_office' groups at the same time")
                    return redirect(request.url)

                token = api_token_service.get_token_by_sp_href(user.okta_id)

                # "api_users" is not a real group, we just pass it along with
                # the groups to indicate if the user has an active api token
                # or not.
                if 'api_users' not in groups:
                    if token:
                        token.activated = False
                        db.session.commit()
                else:
                    if token:
                        token.activated = True
                        db.session.commit()
                    else:
                        full_name = u"{} {}".format(user.first, user.last)
                        api_token_service.create_new_token(full_name, user.okta_id, activated=True)
                        db.session.commit()

                # If the account has a group membership that is not in the
                # posted data, delete it
                for group_name in [g.group for g in user.groups]:
                    if group_name not in groups:
                        user.remove_group(group_name)

                # For each group name that is in the posted data,
                #  add a membership link between the account and the group
                # TODO: See if there is a way to get rid of these list comprehension (something like a .get() function from Stormpath)
                for group in groups:
                    matching_groups = [n
                                       for n in UserService().get_groups()
                                       if n.name == group]
                    if not matching_groups:
                        print("No group {}".format(group))
                        continue

                    if not user.is_in_group(group):
                        # Add this user to the group.
                        user.add_group(group)

                # Update the Okta user
                from taa.services.agents import OktaService
                OktaService().update_user(user.okta_id, dict(
                    profile=dict(
                        firstName=data['fname'],
                        lastName=data['lname'],
                        email=data['email'],
                        login=data['email'],
                        agent_code=data['agent_code'].upper(),
                        agency=data['agency'],
                        signing_name=data['signing_name'],
                    ),
                    #credentials=dict(
                    #    password=dict(value=data['password'])
                    #)
                ))

                # Update the Okta groups
                service = OktaService()
                service.set_user_groups(user, groups)

                current_email = (user_email if data['email'] == user_email
                                 else "{} (formerly {})".format(data['email'],
                                                                user_email))
                flash('User {} updated successfully!'.format(current_email))


                db.session.commit()

                # if we've just activated a user, then send a notice
                if data['activated'] and data['send_notice']:
                    try:
                        AgentActivationEmail().send_activation_notice(data['email'], data['fname'], url_for('home', _external=True))
                        flash('Activation email sent.')
                    except:
                        flash('>> Problem sending activation email <<')
                        print('>> Problem sending activation email <<')

            return redirect(url_for('admin'))
        except Exception as err:
            flash(err.message)



    return render_template('admin/update-user.html',
                           form=form,
                           group_membership=group_membership,
                           groups=get_group_options(),
                           token=token,
                           nav_menu=get_nav_menu()
                           )


def get_group_options():
    return [
        dict(label='Enrollment Importer', value='enrollment_importers', info="User is allowed to upload enrollments.",
             exclusive_with=[]),
        dict(label='Agent', value='agents',
             info='Allowed to enroll cases to which access is granted. Mutually exclusive with Home Office or Admin rights.',
             exclusive_with=['admins', 'home_office']),
        dict(label='Case Creator and Manager', value='case_admins',
             info="In addition to enrolling, can also create new cases and manage owned cases.",
             exclusive_with=['admins', 'home_office'],
             depends_on=['agents']),
        dict(label='Home Office', value='home_office',
             info="Create, manage users and cases, and enroll cases. Mutually exclusive with Agent rights.",
             exclusive_with=['agents', 'case_admins']),
        dict(label='Admin', value='admins',
             info='An admin has access to all pages, including some debugging information. Mutually exclusive with Agent rights.',
             exclusive_with=['agents', 'case_admins']),
    ]


@app.route('/enrollment-import-batches', methods=['GET'])
@groups_required(['admins'])
def view_import_batches():
    return render_template('admin/enrollment_batches.html', nav_menu=get_nav_menu())


def create_application_dictionary_for_submissions_view(application):
    """
    Create a dictionary for use in JSON Serialization for a submission item
    :param application: Application to create the dictionary for
    :type application: taa.services.enrollments.models.EnrollmentApplication
    :rtype: dict
    """
    return {
        'id': application.id,
        'case': application.case,
        'census_record': application.census_record,
        'writing_agent': {'id': application.agent_id, 'name': application.agent_name, 'code': application.agent_code},
    }


def create_submission_dictionary_for_submissions_view(submission):
    """
    Create a dictionary for use in JSON Serialization for a submission item
    """
    return {
        'id': submission.id,
        'enrollment_applications': map(create_application_dictionary_for_submissions_view,
                                       submission.enrollment_applications),
        'created_at': submission.created_at,
        'submission_logs': submission.submission_logs,
        #'data': submission.data,
        'product':submission.product.name if submission.product else '',
        'submission_type': submission.submission_type,
        'status':submission.status,
    }


@app.route('/enrollment-submissions', methods=['GET'])
@groups_required(['admins', 'home_office'], all=False)
def view_submission_logs():

    return render_template('admin/enrollment_submissions.html', nav_menu=get_nav_menu())


def is_email_available(email):
    account = search_stormpath_accounts(filter_email=email)
    if account:
        return False
    return True


@app.route('/user', methods=['DELETE'])
@groups_required(['admins', 'home_office'], all=False)
def delete_user():
    user_email = request.args['email']
    account = search_stormpath_accounts(filter_email=user_email)
    if account:
        agent = agent_service.ensure_agent_in_database(account)
        agent_service.update(agent, **{
            'is_deleted': True
        })
        from taa.services.agents import OktaService
        OktaService().delete_user(account.id)
        db.session.commit()
        return (json.dumps({'success': True}), 200,
                {'ContentType': 'application/json'})
    return (json.dumps({'success': False}), 400,
            {'ContentType': 'application/json'})

@app.route('/sync_okta', methods=['POST'])
@login_required
@groups_required(['admins', 'home_office'], all=False)
def sync_okta():
    from taa.tasks import sync_okta
    sync_okta.delay(current_user.email)

    return jsonify({})
