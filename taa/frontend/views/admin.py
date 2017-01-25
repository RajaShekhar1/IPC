from flask import (
    render_template,
    url_for,
    flash,
    redirect,
    request,
)
from flask.ext.stormpath import groups_required, StormpathError, current_user
from stormpath.error import Error

from taa import app
from nav import get_nav_menu
from taa.models import db
from taa.old_model.Registration import TAA_UserForm
from taa.old_model.Enrollment import AgentActivationEmail
from taa.services import LookupService
from taa.services.users.UserService import search_stormpath_accounts, get_stormpath_application
from datetime import date, timedelta, datetime

agent_service = LookupService('AgentService')
api_token_service = LookupService('ApiTokenService')


@app.route('/admin', methods=['GET', 'POST'])
@groups_required(['admins', 'home_office'], all=False)
def admin():
    accounts = []
    for agent in agent_service.get_sorted_agents():
        # Skip over deleted users, they have no stormpath entry so there is no
        # use showing them here (clicking would error)
        if agent.get_status() == 'Deleted':
            continue

        accounts.append({
            'fname': agent.first,
            'lname': agent.last,
            'email': agent.email,
            'agency': agent.agency,
            'agent_code': agent.agent_code,
            'signing_name': agent.signing_name,
            'status': agent.get_status(),
        })

    # for acc in search_stormpath_accounts():
    #     accounts.append(
    #         {'fname': acc.given_name,
    #          'lname': acc.surname,
    #          'email': acc.email,
    #          'agency': acc.custom_data.get('agency'),
    #          'agent_code': acc.custom_data.get('agent_code'),
    #          'signing_name': acc.custom_data.get('signing_name'),
    #          'status': "Activated" if acc.custom_data.get('activated') else "Not Activated",
    #      })

    return render_template('admin/admin.html', accounts=accounts, nav_menu=get_nav_menu(), is_user_admin=agent_service.is_user_admin(current_user))


@app.route('/edituser', methods=['GET', 'POST'])
@groups_required(['admins', 'home_office'], all=False)
def updateUser():
    user_email = request.args['user']
    form = TAA_UserForm()
    group_membership = []
    token = None

    # initially pre-populate the form with account values
    if request.method == 'GET':
        accounts = search_stormpath_accounts(filter_email=user_email)
        if not accounts:
            flash('Failed to find user ' + user_email)
            return redirect(url_for('admin'))
        else:
            account = accounts[0]

            custom_data = account.custom_data
            keyset = custom_data.keys()
            form.fname.data = account.given_name
            form.lname.data = account.surname
            form.email.data = account.email

            group_membership = account.group_memberships

            token = api_token_service.get_token_by_sp_href(account.href)

            form.agency.data = custom_data['agency'] if 'agency' in keyset else ""
            form.agent_code.data = custom_data['agent_code'] if 'agent_code' in keyset else ""
            form.ds_apikey.data = custom_data['ds_apikey'] if 'ds_apikey' in keyset else ""
            form.signing_name.data = custom_data['signing_name'] if 'signing_name' in keyset else ""
            form.activated.data = custom_data['activated'] if 'activated' in keyset else False
            # form.status.data = "Activated" if custom_data['activated'] else "Not Activated"

    sp_app = get_stormpath_application()

    if form.validate_on_submit():
        try:
            accounts = search_stormpath_accounts(filter_email=user_email)
            if not accounts:
                flash('Failed to find user ' + user_email)
            else:
                account = accounts[0]
                data = form.data

                # Ensure new email address is not already being used
                if (user_email != data['email'] and
                        not is_email_available(data['email'])):
                    flash("The email address '{}' is already in use by "
                          "someone else".format(data['email']))
                    return redirect(request.url)

                # edit some custom data fields
                account.given_name = data['fname']
                account.surname = data['lname']
                account.email = data['email']

                account.custom_data['agency'] = data['agency']
                account.custom_data['agent_code'] = data['agent_code']
                account.custom_data['signing_name'] = data['signing_name']
                account.custom_data['ds_apikey'] = data['ds_apikey']
                account.custom_data['activated'] = data['activated']

                groups = request.values.getlist("groups")
                if 'agents' in groups and ('home_office' in groups or
                                           'admins' in groups):
                    flash("User cannot be in the 'agents' and 'admin'/"
                          "'home_office' groups at the same time")
                    return redirect(request.url)

                token = api_token_service.get_token_by_sp_href(account.href)

                # "api_users" is not a real group, we just pass it along with the groups to indicate if the user
                #  has an active api token or not.
                if "api_users" not in groups:
                    if token:
                        token.activated = False
                        db.session.commit()
                else:
                    if token:
                        token.activated = True
                        db.session.commit()
                    else:
                        full_name = u"{} {}".format(account.given_name, account.surname)
                        api_token_service.create_new_token(full_name, account.href, activated=True)
                        db.session.commit()

                # If the account has a group membership that is not in the posted data, delete it
                for gms in account.group_memberships:
                    if gms.group.name not in groups:
                        gms.delete()

                # For each group name that is in the posted data,
                #  add a membership link between the account and the group
                # TODO: See if there is a way to get rid of these list comprehension (something like a .get() function from Stormpath)
                for group in groups:
                    matching_groups = [item
                                       for item in sp_app.groups.items
                                       if item.name == group]
                    if not matching_groups:
                        continue
                    sp_group = matching_groups[0]
                    #existing_membership = [acct_mem
                    #                       for acct_mem in sp_group.account_memberships
                    #                       if acct_mem.account.email == account.email]
                    existing_membership = [membership for membership in account.group_memberships if membership.group.name == sp_group.name]
                    if not existing_membership:
                        # Add this account to the group
                        sp_group.add_account(account)

                # save your changes
                account.save()
                current_email = (user_email if data['email'] == user_email
                                 else "{} (formerly {})".format(data['email'],
                                                                user_email))
                flash('User {} updated successfully!'.format(current_email))

                # Update in database also
                if 'agents' in groups:
                    agent = agent_service.ensure_agent_in_database(account)
                    agent_service.update(agent, **{
                        'email': data['email'],
                        'first': data['fname'],
                        'last': data['lname'],
                        'agent_code': data['agent_code'],
                        'activated': data['activated']
                    })
                db.session.commit()

                # if we've just activated a user, then send a notice
                if data['activated'] and data['send_notice']:
                    try:
                        AgentActivationEmail().send_activation_notice(data['email'], data['fname'], url_for('home'))
                        flash('Activation email sent.')
                    except:
                        flash('>> Problem sending activation email <<')
                        print('>> Problem sending activation email <<')

            return redirect(url_for('admin'))
        except Error as err:
            flash(err.message)


    all_group_names = [g.name for g in sp_app.groups]

    return render_template('admin/update-user.html',
                           form=form,
                           group_membership=[g.group.name for g in group_membership],
                           groups=all_group_names,
                           token=token,
                           nav_menu=get_nav_menu()
                           )


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
    accounts = search_stormpath_accounts(filter_email=email)
    if accounts and len(accounts) > 0:
        return False
    return True
