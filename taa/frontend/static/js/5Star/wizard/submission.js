function submit_application() {
  // Massage the agent_data resubmitted so we don't resend all product info.

  var results = _.map(window.vm.coverage_vm.selected_product_coverages(), function(product_cov) {
    return build_wizard_results_for_product_coverage(product_cov);
  });

  var please_wait_dialogue = bootbox.dialog({
    //just showing action in the interim while getting routed to the Docusign page... the DS page should redirect probably before there's time to read this
    message: "Generating application form for signature, please wait...",
    buttons: {
      "success": {
        "label": "Close",
        "className": "btn-sm btn-primary"
      }
    }
  });

  _send_wizard_results(results);
}

function _send_wizard_results(wizard_results) {

  // Send to server
  ajax_post("/submit-wizard-data", {"wizard_results": wizard_results}, function (resp) {
    if (resp.error) {
      bootbox.dialog({
        message: "There was a problem generating the application form (" + resp.error + ").  Please contact the enrollment system administrator.",
        buttons: {
          "success": {
            "label": "OK",
            "className": "btn-sm btn-primary"
          }
        }
      });
    } else {
      // Docusign redirect
      location = resp.redirect
    }

  }, function (req) {
    handle_error_and_retry(req, wizard_results);
  }, true);

}

function handle_error_and_retry(req, wizard_results) {
  handle_remote_error(req, function retry_callback(success) {
    // We allow re-authentication if they have timed out; if successful, submit again immediately.
    if (success) {
      _send_wizard_results(wizard_results);
    }
  });
}

function build_wizard_results_for_product_coverage(product_cov) {
  var root = window.vm;

  var health_questions = vm.get_product_health_questions(product_cov);

  var wizard_results = {
    //agent_data: root.options,
    case_id: root.enrollment_case.id,
    enrollCity:  root.enrollCity(),
    enrollState:  root.enrollState(),
    payment_mode: root.coverage_vm.payment_mode().frequency,
    payment_mode_text: root.coverage_vm.payment_mode().label,

    // TODO Need health questions
    // health_questions: $.map(window.root.health_questions(), function(q) {return q.question}),

    method: (root.is_in_person_application()) ? 'in_person': 'self_enroll_email',
    did_decline: product_cov.did_decline(),

    identityToken: root.identityToken(),
    identityType: root.identityType(),

    employee: root.employee().serialize_data(),

    employee_soh_questions: health_questions.serialize_answers_for_applicant(vm.employee()),

    spouse: root.spouse().serialize_data(),
    spouse_soh_questions: health_questions.serialize_answers_for_applicant(vm.spouse()),

    is_spouse_address_same_as_employee: root.is_spouse_address_same_as_employee(),
    is_spouse_email_same_as_employee: root.is_spouse_email_same_as_employee(),

    existing_insurance:  root.existing_insurance(),
    replacing_insurance:  root.replacing_insurance(),

    is_employee_actively_at_work: root.is_employee_actively_at_work(),
    has_spouse_been_treated_6_months: product_cov.has_spouse_been_treated_6_months(),
    has_spouse_been_disabled_6_months: product_cov.has_spouse_been_disabled_6_months(),

    employee_owner:  product_cov.policy_owner(),
    employee_other_owner_name:  product_cov.other_owner_name(),
    employee_other_owner_ssn:  product_cov.other_owner_ssn(),
    spouse_owner:  product_cov.spouse_policy_owner(),
    spouse_other_owner_name:  product_cov.spouse_other_owner_name(),
    spouse_other_owner_ssn:  product_cov.spouse_other_owner_ssn(),

    employee_beneficiary:  product_cov.employee_beneficiary_type(),
    spouse_beneficiary:  product_cov.spouse_beneficiary_type(),
    employee_contingent_beneficiary_type: product_cov.employee_contingent_beneficiary_type(),
    employee_contingent_beneficiary: product_cov.employee_contingent_beneficiary().serialize(),

    employee_beneficiary_name:  product_cov.employee_other_beneficiary().name(),
    employee_beneficiary_relationship:  product_cov.employee_other_beneficiary().relationship(),
    employee_beneficiary_ssn:  product_cov.employee_other_beneficiary().ssn(),
    employee_beneficiary_dob:  product_cov.employee_other_beneficiary().date_of_birth(),

    spouse_beneficiary_name:  product_cov.spouse_other_beneficiary().name(),
    spouse_beneficiary_relationship:  product_cov.spouse_other_beneficiary().relationship(),
    spouse_beneficiary_ssn:  product_cov.spouse_other_beneficiary().ssn(),
    spouse_beneficiary_dob:  product_cov.spouse_other_beneficiary().date_of_birth(),
    spouse_contingent_beneficiary_type: product_cov.spouse_contingent_beneficiary_type(),
    spouse_contingent_beneficiary: product_cov.spouse_contingent_beneficiary().serialize()
  };


  if (!root.should_include_spouse_in_table()) {
    wizard_results.employee_beneficiary = "other";
  }

  // Coverages
  var emp_coverage = product_cov.get_coverage_for_applicant(root.employee());
  if (emp_coverage.coverage_option().is_valid()) {
    wizard_results['employee_coverage'] = emp_coverage.coverage_option().serialize_data();
  } else {
    wizard_results['employee_coverage'] = ""
  }
  var sp_benefit = product_cov.get_coverage_for_applicant(root.spouse());
  if (sp_benefit.coverage_option().is_valid()) {
    wizard_results['spouse_coverage'] = sp_benefit.coverage_option().serialize_data();
  } else {
    wizard_results['spouse_coverage'] = ""
  }

  // Children and child coverages
  wizard_results['children'] = [];
  wizard_results['child_coverages'] = [];
  wizard_results['children_soh_questions'] = [];
  _.each(product_cov.get_covered_children(), function(child) {
    wizard_results['children'].push(child.serialize_data());
    var coverage = product_cov.get_coverage_for_applicant(child);
    wizard_results['child_coverages'].push(coverage.coverage_option().serialize_data());

    var soh_questions = health_questions.serialize_answers_for_applicant(child);
    wizard_results['children_soh_questions'].push(soh_questions);
  });

  wizard_results['product_id'] = product_cov.product.product_data.id;

  // Replacement form
  wizard_results.replacement_read_aloud = root.replacement_read_aloud();
  wizard_results.replacement_is_terminating = root.replacement_is_terminating();
  wizard_results.replacement_using_funds = root.replacement_using_funds();
  wizard_results.replacement_policies = _.invoke(root.replacement_policies(), "serialize");

  // TODO: Add this back in
  wizard_results.rider_data = product_cov.selected_riders.serialize_data();

  return wizard_results;
}


//
//function submit_decline() {
//  var root = window.vm;
//
//  // Pull out all the data we need for docusign
//  var wizard_results = {
//    agent_data: root.options,
//    enrollCity:  root.enrollCity(),
//    enrollState:  root.enrollState,
//    product_type: root.insurance_product.product_type,
//    method: (ui.is_in_person_application()) ? 'in_person': 'self_enroll_email',
//    did_decline: ui.did_decline(),
//    employee: root.employee().serialize_data(),
//    spouse: root.spouse().serialize_data()
//  };
//
//  // Children
//  wizard_results['children'] = [];
//  $.each(root.get_valid_children(), function() {
//    var child = this;
//    wizard_results['children'].push(this.serialize_data());
//  });
//
//  wizard_results['product_data'] = ui.insurance_product.product_data;
//
//  // Send to server
//  ajax_post("/submit-wizard-data", {"wizard_results": wizard_results}, function (resp) {
//    if (resp.error) {
//      bootbox.dialog({
//        message: "There was a problem submitting the form (" + resp.error + ").  Please contact the enrollment system administrator.",
//        buttons: {
//          "success": {
//            "label": "OK",
//            "className": "btn-sm btn-primary"
//          }
//        }
//      });
//    } else {
//      // Docusign redirect
//      location = resp.redirect;
//    }
//  }, handle_remote_error, true);
//
//}