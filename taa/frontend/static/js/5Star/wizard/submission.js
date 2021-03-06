function build_results() {
  return _.map(window.vm.coverage_vm.product_coverage_viewmodels(), function (product_cov) {
    return build_wizard_results_for_product_coverage(product_cov);
  });
}

function submit_application() {

  // Don't allow duplicate submissions.
  window.vm.is_submitting(true);
  window.vm.submission_error("");

  var results = build_results();

  var please_wait_dialogue = bootbox.dialog({
    message: "Preparing application for submission. Please wait, this may take a minute...",
    buttons: {
      //"success": {
      //  "label": "Close",
      //  "className": "btn-sm btn-primary"
      //}
    }
  });

  _send_wizard_results(results);
}

var POLL_WAIT = 5000;
var MAX_POLL_COUNT = 25;
var poll_count = 0;

function _send_wizard_results(wizard_results) {

  // Send to server
  ajax_post("/submit-wizard-data", {"wizard_results": wizard_results}, function (resp) {
    if (resp.error) {
      window.vm.is_submitting(false);

      bootbox.dialog({
        message: "There was a problem generating the application form (" + resp.error + ").  Please contact the enrollment system administrator.",
        buttons: {
          "success": {
            "label": "OK",
            "className": "btn-sm btn-primary"
          }
        }
      });
    } else if (resp.redirect_url) {
      // A redirect URL was returned immediately.
      window.location.href = resp.redirect_url
    } else {
      // We need to poll until the redirect URL is ready.
      setTimeout(function () {
        poll_envelope_result(resp.poll_url, wizard_results);
      }, POLL_WAIT);
    }

  }, function (req) {
    window.vm.is_submitting(false);
    handle_error_and_retry(req, wizard_results);
  }, true);
}

function poll_envelope_result(url, wizard_results) {

  poll_count += 1;
  if (poll_count > MAX_POLL_COUNT) {
    alert("Sorry, an error occurred generating the signature page. Please check your Agent Inbox to sign this application.");
    window.location.href = urls.get_manage_case_url(window.vm.options.case_data.id) + '#enrollment';
  }

  $.get(url).success(function (resp) {
    if (resp.status === 'ready' || resp.status === 'declined') {
      window.location.href = resp.redirect_url;
    } else {
      setTimeout(function () { poll_envelope_result(url, wizard_results); }, POLL_WAIT);
    }
  }).error(function (resp) {
    handle_error_and_retry(resp, wizard_results)
  })
}

function handle_error_and_retry(resp, wizard_results) {
  handle_remote_error_with_retry(resp, function retry_callback(success) {
    // We allow re-authentication if they have timed out; if successful, submit again immediately.
    if (success) {
      _send_wizard_results(wizard_results);
    }
  });
}

var SUBMISSION_RETRIES_ALLOWED = 3;
var submission_retry_count = 0;

function handle_remote_error_with_retry(response, retry_callback) {
  window.vm.is_submitting(false);
  if (response.status === 401) {
    // TODO: add this reauthentication logic again
    //if (ui.account_href != null) {
    //  prompt_login(retry_callback);
    //} else {
    //  // The user wasn't logged in, so just restart our session
    //  login_reauth(null, null, retry_callback);
    //}
  } else if (response.status === 503) {
    // Heroku Timeout. Maintain a counter that allows us to retry up to a certain number of times.
    //  This is a temporary shortcut to get around the Heroku timeout.
    //if (submission_retry_count < SUBMISSION_RETRIES_ALLOWED) {
    //  submission_retry_count += 1;
    //  // Wait for a few seconds before retrying.
    //  setTimeout(function() {
    //    retry_callback(true);
    //  }, 25000);
    //} else {
    alert("Sorry, an error occurred communicating with the server. Please check your Agent Inbox to sign this application.");
    window.location = urls.get_manage_case_url(window.vm.options.case_data.id) + '#enrollment';
    //}

  } else if (response.status === 500) {
    alert("Sorry, the server encountered an error processing this request.");
  } else {
    alert("Sorry, an error occurred communicating with the server. Please check your connection to the internet and try again.");
  }

}


function build_wizard_results_for_product_coverage(product_cov) {
  var root = window.vm;

  var health_questions = vm.get_product_health_questions(product_cov);

  var did_decline = (root.coverage_vm.has_multiple_products())? product_cov.did_decline() : root.did_decline();

  var occupation = Array.isArray(root.selected_occupation())? root.selected_occupation()[0].label : undefined;

  var bank_info = {
    account_holder_name: root.bank_account_holder_name(),
    account_type: root.selected_account_type(),
    account_number: root.account_number(),
    routing_number: root.routing_number(),
    bank_name: root.bank_name(),
    address_one: root.billing_street_one(),
    address_two: root.billing_street_two(),
    billing_city: root.billing_city(),
    billing_state: root.billing_state(),
    billing_zip: root.billing_zip(),
    city_state_zip: root.bank_city_state_zip()
  };

  var backup_bank_info = {
    account_holder_name: $("#bank-account-holder-name").val(),
    account_type: $("#bank-account-type").val(),
    account_number: $("#bank-account-number").val(),
    routing_number: $("#bank-routing-number").val(),
    bank_name: $("#bank-name").val(),
    address_one: $("#bank-street-one").val(),
    address_two: $("#bank-street-two").val(),
    billing_city: $("#billing-city").val(),
    billing_state: $("#billing-state").val(),
    billing_zip: $("#billing-zip").val(),
    city_state_zip: $("#bank-city-state-zip").val()
  };

  var wizard_results = {
    product_id: product_cov.product.product_data.id,
    case_id: root.enrollment_case.id,
    enrollCity: root.enrollCity(),
    enrollState: root.enrollState(),
    payment_mode: root.coverage_vm.payment_mode().frequency,
    payment_mode_text: root.coverage_vm.payment_mode().label,
    occupation_class: occupation,

    method: (root.is_in_person_application())? 'in_person' : 'self_enroll_email',
    did_decline: did_decline,

    identityToken: root.identityToken(),
    identityType: root.identityType(),

    effective_date: product_cov.effective_date_resolution(),
    enrollerSelects: root.show_enroller_select_date(),
    effectiveDateSettings: root.effective_date_settings,

    send_summary_email: root.should_email_summary_sheet(),
    summaryEmail: root.get_summary_email(),

    employee: root.employee().serialize_data(),
    spouse: root.spouse().serialize_data(),

    is_spouse_address_same_as_employee: root.is_spouse_address_same_as_employee(),
    is_spouse_email_same_as_employee: root.is_spouse_email_same_as_employee(),

    employee_owner: product_cov.policy_owner(),
    employee_other_owner_name: product_cov.other_owner_name(),
    employee_other_owner_ssn: product_cov.other_owner_ssn(),
    spouse_owner: product_cov.spouse_policy_owner(),
    spouse_other_owner_name: product_cov.spouse_other_owner_name(),
    spouse_other_owner_ssn: product_cov.spouse_other_owner_ssn(),

    employee_beneficiary: product_cov.employee_beneficiary_type(),
    spouse_beneficiary: product_cov.spouse_beneficiary_type(),
    employee_contingent_beneficiary_type: product_cov.employee_contingent_beneficiary_type(),
    employee_contingent_beneficiary: product_cov.employee_contingent_beneficiary().serialize(),

    employee_beneficiary_name: product_cov.employee_other_beneficiary().name(),
    employee_beneficiary_relationship: product_cov.employee_other_beneficiary().relationship(),
    employee_beneficiary_ssn: product_cov.employee_other_beneficiary().ssn(),
    employee_beneficiary_dob: product_cov.employee_other_beneficiary().date_of_birth(),

    spouse_beneficiary_name: product_cov.spouse_other_beneficiary().name(),
    spouse_beneficiary_relationship: product_cov.spouse_other_beneficiary().relationship(),
    spouse_beneficiary_ssn: product_cov.spouse_other_beneficiary().ssn(),
    spouse_beneficiary_dob: product_cov.spouse_other_beneficiary().date_of_birth(),
    spouse_contingent_beneficiary_type: product_cov.spouse_contingent_beneficiary_type(),
    spouse_contingent_beneficiary: product_cov.spouse_contingent_beneficiary().serialize()
  };

  if (root.requires_bank_info()) {
    wizard_results.bank_info = bank_info;
    wizard_results.backup_bank_info = backup_bank_info;
  } else {
    wizard_results.bank_info = null;
    wizard_results.backup_bank_info = null;
  }

  wizard_results.children = [];
  _.each(product_cov.applicant_list.get_valid_children(), function (child) {
    wizard_results['children'].push(child.serialize_data());
  });

  if (did_decline) {
    // Return just the basic data if declining.
    return wizard_results;
  }

  wizard_results = $.extend({}, wizard_results, {
    existing_insurance: root.existing_insurance(),
    replacing_insurance: root.replacing_insurance(),

    is_employee_actively_at_work: root.is_employee_actively_at_work(),
    has_spouse_been_treated_6_months: product_cov.has_spouse_been_treated_6_months(),
    has_spouse_been_disabled_6_months: product_cov.has_spouse_been_disabled_6_months()
  });

  if (health_questions) {
    wizard_results.employee_soh_questions = health_questions.serialize_answers_for_applicant(vm.employee());
    wizard_results.spouse_soh_questions = health_questions.serialize_answers_for_applicant(vm.spouse());

  }

  if (!root.should_include_spouse_in_table()) {
    wizard_results.employee_beneficiary = "other";
  }

  // Coverages
  var emp_coverage = product_cov.__get_coverage_for_applicant(root.employee());
  if (emp_coverage.coverage_option().is_valid()) {
    wizard_results['employee_coverage'] = emp_coverage.coverage_option().serialize_data();
  } else {
    wizard_results['employee_coverage'] = ""
  }
  var sp_benefit = product_cov.__get_coverage_for_applicant(root.spouse());
  if (sp_benefit.coverage_option().is_valid()) {
    wizard_results['spouse_coverage'] = sp_benefit.coverage_option().serialize_data();
  } else {
    wizard_results['spouse_coverage'] = ""
  }

  // Children and child coverages
  wizard_results['child_coverages'] = [];
  wizard_results['children_soh_questions'] = [];
  _.each(product_cov.applicant_list.get_valid_children(), function (child) {
    var coverage;
    if (product_cov.product.is_children_coverage_grouped()) {
      coverage = product_cov.__get_coverage_for_applicant(product_cov.applicant_list.get_children_group());
    } else {
      coverage = product_cov.__get_coverage_for_applicant(child);
    }

    wizard_results['child_coverages'].push(coverage.coverage_option().serialize_data());

    if (health_questions) {
      var soh_questions = health_questions.serialize_answers_for_applicant(child);
      wizard_results['children_soh_questions'].push(soh_questions);
    }
  });

  // Replacement form
  wizard_results.replacement_read_aloud = root.replacement_read_aloud();
  wizard_results.replacement_is_terminating = root.replacement_is_terminating();
  wizard_results.replacement_using_funds = root.replacement_using_funds();
  wizard_results.replacement_policies = _.invoke(root.replacement_policies(), "serialize");

  // Rider data
  wizard_results.rider_data = product_cov.selected_riders.serialize_data();


  // Alternative bug fix for missing address data on particular agent's computers.
  //   Grab the employee address data directly without using the binding library.
  wizard_results.address_alternate = {
    street1: $("#eeStreet1").val(),
    street2: $("#eeStreet2").val(),
    city: $("#eeCity").val(),
    state: $("#eeState").val(),
    zip: $("#eeZip").val()
  };

  // Add Signing Ceremony data
  //wizard_results.should_do_signing_ceremony = root.should_do_signing_ceremony();
  wizard_results.applicant_signed = root.applicant_signed();
  wizard_results.agent_signed = root.agent_signed();

  return wizard_results;
}


function submit_decline() {
  var root = window.vm;

  // Don't allow duplicate submissions.
  window.vm.is_submitting(true);
  window.vm.submission_error("");

  var results = _.map(root.product_coverage_viewmodels(), function (product_cov) {
    return build_wizard_results_for_product_coverage(product_cov);
  });

  // Send to server
  ajax_post("/submit-wizard-data", {"wizard_results": results}, function (resp) {
    if (resp.error) {
      bootbox.dialog({
        message: "There was a problem submitting the form (" + resp.error + ").  Please contact the enrollment system administrator.",
        buttons: {
          "success": {
            "label": "OK",
            "className": "btn-sm btn-primary"
          }
        }
      });
    } else {
      // redirect
      location = resp.redirect_url;
    }
  }, handle_remote_error, true);

}
