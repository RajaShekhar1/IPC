function submit_application() {
  // Massage the agent_data resubmitted so we don't resend all product info.
  var agent_data =  window.ui.defaults;
  delete agent_data['products'];

  // Pull out all the data we need for docusign
  var wizard_results = {
    health_questions: $.map(window.ui.health_questions(), function(q) {return q.question}),
    agent_data: agent_data,
    enrollCity:  window.ui.enrollCity(),
    enrollState:  window.ui.enrollState,
    product_type: window.ui.insurance_product.product_type,
    payment_mode: window.ui.payment_mode(),
    payment_mode_text: window.ui.payment_mode_text_lower(),

    method: (ui.is_in_person_application()) ? 'in_person': 'self_enroll_email',
    did_decline: ui.did_decline(),

    identityToken: window.ui.identityToken(),
    identityType: window.ui.identityType(),

    employee: window.ui.employee().serialize_data(),
    spouse: window.ui.spouse().serialize_data(),

    is_spouse_address_same_as_employee: ui.is_spouse_address_same_as_employee(),
    is_spouse_email_same_as_employee: ui.is_spouse_email_same_as_employee(),

    existing_insurance:  window.ui.existing_insurance(),
    replacing_insurance:  window.ui.replacing_insurance(),
    is_employee_actively_at_work: ui.is_employee_actively_at_work(),
    has_spouse_been_treated_6_months: ui.has_spouse_been_treated_6_months(),
    has_spouse_been_disabled_6_months: ui.has_spouse_been_disabled_6_months(),

    employee_owner:  window.ui.policy_owner(),
    employee_other_owner_name:  window.ui.other_owner_name(),
    employee_other_owner_ssn:  window.ui.other_owner_ssn(),
    spouse_owner:  window.ui.spouse_policy_owner(),
    spouse_other_owner_name:  window.ui.spouse_other_owner_name(),
    spouse_other_owner_ssn:  window.ui.spouse_other_owner_ssn(),

    employee_beneficiary:  window.ui.employee_beneficiary_type(),
    spouse_beneficiary:  window.ui.spouse_beneficiary_type(),
    employee_contingent_beneficiary_type: window.ui.employee_contingent_beneficiary_type(),
    employee_contingent_beneficiary: window.ui.employee_contingent_beneficiary().serialize(),

    employee_beneficiary_name:  window.ui.employee_other_beneficiary().name(),
    employee_beneficiary_relationship:  window.ui.employee_other_beneficiary().relationship(),
    employee_beneficiary_ssn:  window.ui.employee_other_beneficiary().ssn(),
    employee_beneficiary_dob:  window.ui.employee_other_beneficiary().date_of_birth(),

    spouse_beneficiary_name:  window.ui.spouse_other_beneficiary().name(),
    spouse_beneficiary_relationship:  window.ui.spouse_other_beneficiary().relationship(),
    spouse_beneficiary_ssn:  window.ui.spouse_other_beneficiary().ssn(),
    spouse_beneficiary_dob:  window.ui.spouse_other_beneficiary().date_of_birth(),
    spouse_contingent_beneficiary_type: window.ui.spouse_contingent_beneficiary_type(),
    spouse_contingent_beneficiary: window.ui.spouse_contingent_beneficiary().serialize()
  };


  if (!window.ui.should_include_spouse_in_table()) {
    wizard_results.employee_beneficiary = "other";
  }

  // Children
  wizard_results['children'] = [];
  wizard_results['child_coverages'] = [];
  $.each(window.ui.get_valid_children(), function() {
    var child = this;
    wizard_results['children'].push(this.serialize_data());
    var coverage = window.ui.selected_plan().children_recommendation().recommended_benefit;
    wizard_results['child_coverages'].push(coverage.serialize_data());
  });

  // Coverages
  var emp_benefit = window.ui.selected_plan().employee_recommendation().recommended_benefit;
  if (emp_benefit.is_valid()) {
    wizard_results['employee_coverage'] = emp_benefit.serialize_data();
  } else {
    wizard_results['employee_coverage'] = ""
  }
  var sp_benefit = window.ui.selected_plan().spouse_recommendation().recommended_benefit;
  if (sp_benefit.is_valid()) {
    wizard_results['spouse_coverage'] = sp_benefit.serialize_data();
  } else {
    wizard_results['spouse_coverage'] = ""
  }

  wizard_results['product_data'] = ui.insurance_product.product_data;
  // But again, no need to retransmit all this data.
  delete wizard_results['product_data']['replacement_paragraphs'];

  // Replacement form
  wizard_results.replacement_read_aloud = ui.replacement_read_aloud();
  wizard_results.replacement_is_terminating = ui.replacement_is_terminating();
  wizard_results.replacement_using_funds = ui.replacement_using_funds();
  wizard_results.replacement_policies = _.invoke(ui.replacement_policies(), "serialize");

  wizard_results.rider_data = window.ui.selected_riders.serialize_data();

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

  }, handle_remote_error, true);

  bootbox.dialog({
    //just showing action in the interim while getting routed to the Docusign page... the DS page should redirect probably before there's time to read this
    message: "Generating application form for signature, please wait...",
    buttons: {
      "success": {
        "label": "Close",
        "className": "btn-sm btn-primary"
      }
    }
  });
}

function submit_decline() {
  // Pull out all the data we need for docusign
  var wizard_results = {
    agent_data: window.ui.defaults,
    enrollCity:  window.ui.enrollCity(),
    enrollState:  window.ui.enrollState,
    product_type: window.ui.insurance_product.product_type,
    method: (ui.is_in_person_application()) ? 'in_person': 'self_enroll_email',
    did_decline: ui.did_decline(),
    employee: window.ui.employee().serialize_data(),
    spouse: window.ui.spouse().serialize_data()
  };

  // Children
  wizard_results['children'] = [];
  $.each(window.ui.get_valid_children(), function() {
    var child = this;
    wizard_results['children'].push(this.serialize_data());
  });

  wizard_results['product_data'] = ui.insurance_product.product_data;

  // Send to server
  ajax_post("/submit-wizard-data", {"wizard_results": wizard_results}, function (resp) {
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
      // Docusign redirect
      location = resp.redirect;
    }
  }, handle_remote_error, true);

}