function init_validation(ui) {

  $('[data-rel=tooltip]').tooltip();

  var validation_debug = false;
  $('#enrollment-wizard').ace_wizard().on('actionclicked.fu.wizard', function (e, info) {
    if (validation_debug) {
      return true;
    }

    var is_valid;

    if (info.step === 1) {
      // Clear step2 health question error when we attempt to validate step 1
      $("#health_questions_error").html("");

      // Check for "Decline all coverage" and bail out of the wizard if it is checked.
      if (ui.did_decline_all_products()) {
        submit_decline();
        e.preventDefault();
        return;
      }

      // trigger jquery validation
      is_valid = ui.validator.form();

      if (!ui.is_coverage_selection_visible()) {
        ui.step_one_validation_error('Please click \"Show Coverage Options\"');
        e.preventDefault();
        return;
      } else {
        ui.step_one_validation_error(null);
      }

      function validate_coverage_amount(applicant_coverage) {
        var product_id = applicant_coverage.product.product_data.id;
        var product = applicant_coverage.product;
        var applicant = applicant_coverage.applicant;

        var existing_coverage_amount = applicant.get_existing_coverage_amount_for_product(product_id);
        if (!existing_coverage_amount) {
          // Short-circuit, there is no existing coverage to check so we are OK
          return true;
        }

        var selected_option = applicant_coverage.get_selected_coverage();
        var applied_coverage_amount = (selected_option.is_valid())? selected_option.face_value : 0;
        var max_coverage_amount = applicant_coverage.product.get_maximum_coverage_amount(applicant_coverage.applicant);

        var name = applicant.name();
        if (applied_coverage_amount > 0 && existing_coverage_amount && (max_coverage_amount < existing_coverage_amount + applied_coverage_amount)) {

          var additional_allowed_coverage = max_coverage_amount - existing_coverage_amount;
          // Make sure that we don't show a negative number here
          additional_allowed_coverage = _.max([0, additional_allowed_coverage]);
          var msg = ("Due to one or more previous applications for " + product.product_data.name + " this enrollment period for " +
          format_face_value(existing_coverage_amount) +
          " coverage, " + name + " can apply for a maximum of " +
          format_face_value(additional_allowed_coverage) + " additional coverage.");

          alert(msg);
          return false;
        }

        return true;
      }

      // Check each applicant for selecting too much product.
      _.each(ui.coverage_vm.selected_product_coverages(), function (product_coverage) {
        _.each(product_coverage.applicant_coverage_selections(), function (applicant_coverage) {
          var can_apply = validate_coverage_amount(applicant_coverage);
          if (!can_apply) {
            is_valid = false;
            // Break out of loop.
            return false;
          }
        });
        if (!is_valid) {
          // Break out of loop.
          return false;
        }
      });

      if (!ui.is_coverage_selection_visible()) {
        ui.show_coverage_options_visibility_error();
        e.preventDefault();
        return false;
      }

      if (ui.requires_actively_at_work() && ui.is_employee_actively_at_work() === null) {
        e.preventDefault();
        return false;
      }

      if (is_valid) {
        // Scroll to top of page when moving to step 2.
        $(document.body).scrollTop(0);
      } else {
        // Don't allow moving to the next page
        e.preventDefault();
      }
    }
    if (info.step === 2 && info.direction === 'next') {
      var validator = ui.validators.step2;

      // trigger jquery validation
      is_valid = validator.form();

      // validate replacement form
      if (ui.did_select_any_fpp_product() &&
        (ui.should_show_replacement_form())) {
        is_valid = $('#questions-form').valid() && is_valid;
      }

      if (ui.did_select_any_fpp_product() &&
        (ui.replacing_insurance() === null || ui.existing_insurance() === null)) {
        // These always need to be answered
        is_valid = false;
      }

      if (ui.did_select_any_fpp_product() && ui.is_KY_OR_KS() && ui.replacing_insurance()) {
        // Must stop here for these states, no replacements.
        is_valid = false;
      }

      if (ui.did_select_any_fpp_product() && ui.is_self_enroll()
        && (ui.replacement_is_terminating() || ui.replacement_using_funds())) {
        // can't continue as self-enroll with either of these as a yes.
        is_valid = false;
      }

      // validate questions
      is_valid = health_question_buttons.are_health_questions_valid() && is_valid;

      if (!is_valid) {
        $("#health_questions_error").html("Please answer all questions for all applicants.  Invalid responses may prevent you from continuing this online application; if so, please see your agent or enrollment professional.");
        e.preventDefault();
        return;
      } else {
        $("#health_questions_error").html("");
      }
    }
    if (info.step === 3 && info.direction === 'next') {
      is_valid = $('#step3-form').valid();
      if (!is_valid) {
        e.preventDefault();
        return;
      }
    }
    if (info.step === 4 && info.direction === 'next') {
      if (!$('#step4-form').valid()) {
        e.preventDefault();
        return;
      }
    }
    if (info.step === 5 && info.direction === 'next') {
      // Set the beneficiary dob to itself to force the dob validation to run if it is empty and has yet to trigger
      _.forEach(ui.coverage_vm.product_coverage_viewmodels(), function (product_coverage_vm) {
        product_coverage_vm.employee_other_beneficiary().date_of_birth.valueHasMutated();
        if (product_coverage_vm.employee_contingent_beneficiary_type() === 'other') {
          product_coverage_vm.employee_contingent_beneficiary().date_of_birth.valueHasMutated();
        }
      });

      var has_beneficiary_errors = _.any(ui.coverage_vm.product_coverage_viewmodels(), function has_beneficiary_errors(product_coverage_vm) {
        if (product_coverage_vm.employee_contingent_beneficiary_type() === 'other') {
          return !!product_coverage_vm.employee_other_beneficiary().date_of_birth_validation_error() || !!product_coverage_vm.employee_contingent_beneficiary().date_of_birth_validation_error();
        }
        return !!product_coverage_vm.employee_other_beneficiary().date_of_birth_validation_error();
      });

      if (!$('#step5-form').valid() || has_beneficiary_errors) {
        e.preventDefault();
        return;
      }

      if (ui.has_contingent_beneficiary_error()) {
        e.preventDefault();
        return;
      }

    }
    if (info.step == 6 && info.direction == 'next') {
      is_valid = true;

      if (ui.requires_bank_info()) {
        is_valid = is_valid && $('#bank-draft-form').valid();
      }
      is_valid = is_valid && $('#step6-form').valid();
      if (!is_valid) {
        e.preventDefault();
      }
    }

  }).on('finished.fu.wizard', function (e) {
    var is_valid = true;

    if (ui.requires_bank_info()) {
      is_valid = is_valid && $('#bank-draft-form').valid();
    }
    is_valid = is_valid && $('#step6-form').valid();
    if (!is_valid) {
      e.preventDefault();
      return;
    }

    // jQuery validator rule should be handling this, but it's not, so force a popup here
    if (ui.should_confirm_disclosure_notice()
      && !ui.disclaimer_notice_confirmed()
    ) {
      bootbox.dialog({
        message: "Please confirm that you have received the disclosure notice.",
        buttons: {
          "danger": {
            "label": "OK",
            "className": "btn-warning"
          }
        }
      });
      return false;
    }

    if (ui.should_confirm_payroll_deduction()
      && !ui.payroll_deductions_confirmed()) {
      bootbox.dialog({
        message: "Please confirm that you agree to payroll deductions by your employer.",
        buttons: {
          "danger": {
            "label": "OK",
            "className": "btn-warning"
          }
        }
      });
      return false;
    }

    if (!ui.can_submit_wizard()) {
      e.preventDefault();
      vm.submission_error("Please wait, the submission is still processing.");
      return false;
    }

    submit_application(ui);

  }).on('stepclick.fu.wizard', function (e) {
    return true; //return false;//prevent clicking on steps
  });

  //documentation : http://docs.jquery.com/Plugins/Validation/validate
  $.mask.definitions['~'] = '[+-]';
  $('#phone').mask('(999) 999-9999');
  $('#eeDOB').mask('99/99/1999');
  $('.input-mask-date').mask('99/99/9999');
  $('.input-mask-ssn').mask('999-99-9999');
  $('.input-mask-zip').mask('99999');

  jQuery.validator.addMethod("phone", function (value, element) {
    return this.optional(element) || /^\(\d{3}\) \d{3}\-\d{4}( x\d{1,6})?$/.test(value);
  }, "Enter a valid phone number.");

  var step_2_validator = $('#questions-form').validate({
    errorElement: 'div',
    errorClass: 'help-block',
    focusInvalid: false,
    rules: {
      replacement_read_aloud: {
        required: {
          depends: function () {
            return ui.is_replacement_form_required();
          }
        }
      },
      replacement_is_terminating: {
        required: {
          depends: function () {
            return ui.is_replacement_form_required();
          }
        }
      },
      replacement_using_funds: {
        required: {
          depends: function () {
            return ui.is_replacement_form_required();
          }
        }
      }

    },

    highlight: wizard_validate_highlight,
    success: wizard_validate_success,
    errorPlacement: wizard_error_placement
  });

  $.validator.addClassRules("replacement-question", {
    required: {
      depends: function () {
        return ui.is_replacement_form_required();
      }
    }
  });
  $.validator.addClassRules("replacement-details-input", {
    required: {
      depends: function () {
        return ui.should_show_replacement_details_form();
      }
    }
  });
  //
  //$.validator.addMethod("isEmployeeOtherOwnerRequired", function(val, element, params) {
  //  // Required if we have set the owner to someone other than employee.
  //  var product_coverage = ko.dataFor(element);
  //  return (product_coverage.policy_owner() === "other");
  //}, "This field is required.");

  function isEmployeeOtherOwnerRequired(element) {
    var product_coverage = ko.dataFor(element);
    return product_coverage.policy_owner() === "other";
  }

  function isSpouseOtherOwnerRequired(element) {
    var product_coverage = ko.dataFor(element);
    return product_coverage.spouse_policy_owner() === "other";
  }

  $.validator.addClassRules("ee-other-owner-name", {
    required: {
      depends: function (element) {
        return isEmployeeOtherOwnerRequired(element);
      }
    }
  });
  $.validator.addClassRules("ee-other-owner-ssn", {
    required: {
      depends: function (element) {
        return isEmployeeOtherOwnerRequired(element);
      }
    }
  });

  $.validator.addClassRules("sp-other-owner-name", {
    required: {
      depends: function (element) {
        return isSpouseOtherOwnerRequired(element);
      }
    }
  });
  $.validator.addClassRules("sp-other-owner-ssn", {
    required: {
      depends: function (element) {
        return isSpouseOtherOwnerRequired(element);
      }
    }
  });
  $.validator.addMethod('city-state-zip-required', function () {
    var result = !!ui.employee().city() && !!ui.employee().state() && !!ui.employee().zip();
    return result;
  }, "required");

  $('#step3-form').validate({
    errorElement: 'div',
    errorClass: 'help-block',
    focusInvalid: false,
    rules: {
      email: {
        email: true, required: {
          depends: function () {
            return ui.is_self_enroll();
          }
        }
      },
      eeFName2: {required: true},
      eeLName2: {required: true},
      eeGender: {required: true},
      eessn: {required: true},
      eeStreet1: {required: true},
      eeCity: {'city-state-zip-required': true},
      eeState: {'city-state-zip-required': true},
      eeZip: {'city-state-zip-required': true},
      eeOwner: {required: true}
    },

    messages: {
      email: {
        email: "Please provide a valid email."
      },
      eeFName2: "required",
      eeLName2: "required",
      eeGender: "Please choose gender",
      eessn: "required",
      eeStreet1: "required",
      eeCity: "required",
      eeState: "required",
      eeZip: "required",
      eeOwner: "Please confirm policy owner"
    },

    highlight: wizard_validate_highlight,
    success: wizard_validate_success,
    errorPlacement: wizard_error_placement
  });

  $('#step4-form').validate({
    errorElement: 'div',
    errorClass: 'help-block',
    focusInvalid: false,
    rules: {
      spFName2: {required: true},
      spLName2: {required: true},
      spGender: {required: true},
      spssn: {
        required: {
          depends: function (element) {
            return ($("#spOwner-self").is(':checked'))
          }
        }
      },
      spOwner: {required: true}
    },

    messages: {
      spFName2: "required",
      spLName2: "required",
      spGender: "Please choose gender",
      spssn: "Spouse SSN required",//"Spouse SSN required if owner of policy",
      spOwner: "Please confirm policy owner"
    },

    highlight: wizard_validate_highlight,
    success: wizard_validate_success,
    errorPlacement: wizard_error_placement
  });

  // Beneficiary rules

  function is_other_beneficiary_detail_required(element, applicant_type, beneficiary_type) {
    var product_coverage = ko.dataFor(element);
    if (beneficiary_type === "contingent" && !product_coverage.should_show_contingent_beneficiary()) {
      // Not required for this product.
      return false;
    }
    if (applicant_type === "spouse" && !ui.coverage_vm.did_select_spouse_coverage()) {
      return false;
    }

    if (applicant_type === "employee") {
      if (beneficiary_type === "primary") {
        return product_coverage.employee_beneficiary_type() === "other" || !ui.coverage_vm.did_select_spouse_coverage();
      } else {
        return product_coverage.employee_contingent_beneficiary_type() === "other";
      }
    } else if (applicant_type === 'spouse') {
      if (beneficiary_type === "primary") {
        return product_coverage.spouse_beneficiary_type() === "other";
      } else {
        return product_coverage.spouse_contingent_beneficiary_type() === "other";
      }
    }
  }

  var beneficiary_depends_rules = {
    "ee-bene-name": function (el) {
      return is_other_beneficiary_detail_required(el, 'employee', 'primary')
    },
    "ee-bene-rel": function (el) {
      return is_other_beneficiary_detail_required(el, 'employee', 'primary')
    },

    "ee-cont-bene-name": function (el) {
      return is_other_beneficiary_detail_required(el, 'employee', 'contingent')
    },

    "ee-cont-bene-rel": function (el) {
      return is_other_beneficiary_detail_required(el, 'employee', 'contingent')
    },

    "sp-bene-name": function (el) {
      return is_other_beneficiary_detail_required(el, 'spouse', 'primary')
    },
    "sp-bene-rel": function (el) {
      return is_other_beneficiary_detail_required(el, 'spouse', 'primary')
    },

    "sp-cont-bene-name": function (el) {
      return is_other_beneficiary_detail_required(el, 'spouse', 'contingent')
    },

    "sp-cont-bene-rel": function (el) {
      return is_other_beneficiary_detail_required(el, 'spouse', 'contingent')
    }
  };
  for (className in beneficiary_depends_rules) {
    var depends_func = beneficiary_depends_rules[className];
    $.validator.addClassRules(className, {required: {depends: depends_func}})
  }

  $('#step5-form').validate({
    errorElement: 'div',
    errorClass: 'help-block',
    focusInvalid: false,
    rules: {},

    highlight: wizard_validate_highlight,
    success: wizard_validate_success,
    errorPlacement: wizard_error_placement
  });

  $('#step6-form').validate({
    errorElement: 'div',
    errorClass: 'help-block',
    focusInvalid: false,
    rules: {
      confirmDisclaimer: {required: true},
      enrollCity: {required: true},
      tokenType: {required: true},
      ConfirmationToken: {
        required: function () {
          return window.vm.should_use_date_of_hire_for_identity();
        }
      },
      hireDate: {required: true}
    },

    messages: {
      confirmDisclaimer: "please acknowledge that you have received the notice",
      enrollCity: "required",
      tokenType: "required",
      ConfirmationToken: "Valid date required"
    },

    highlight: wizard_validate_highlight,
    success: wizard_validate_success,
    errorPlacement: wizard_error_placement
  });

  //region Bank Draft Info Validation
  function bank_draft_error_placement(error, element) {
    var error_id = error.attr('id');
    var container_id = error_id + '-container';
    var old_element = $('#' + container_id);
    if (old_element.length > 0) {
      old_element.remove();
    }
    if (element.is(':checkbox') || element.is(':radio')) {
      var controls = element.closest('div[class*="col-"]');
      if (controls.find(':checkbox,:radio').length > 1) {
        controls.append(error);
      } else {
        error.insertAfter(element.nextAll('.lbl:eq(0)').eq(0));
      }
    } else if ((element.is('#bank-state') || element.is('#bank-city') || element.is('#bank-zip')) && $('#bank-city-state-zip-error').length === 0) {
      $('#bank-city-state-zip-error').remove();
      var bank_city_state_zip_error = $('<div class="col-xs-12 col-sm-9 col-sm-offset-3" id="bank-city-state-zip-error"></div>');
      bank_city_state_zip_error.insertAfter(element.parent()).append(error);
    } else {
      var error_element = $('<div class="col-xs-12 col-sm-9 col-sm-offset-3" id="' + container_id + '"></div>');
      error_element.insertAfter(element.parent()).append(error);
    }
  }

  $('#bank-draft-form').validate({
    errorElement: 'div',
    errorClass: 'help-block',
    focusInvalid: false,
    highlight: wizard_validate_highlight,
    success: wizard_validate_success,
    errorPlacement: bank_draft_error_placement,
    rules: {
      'bank-account-holder-name': {
        required: {
          depends: ui.requires_bank_info
        }
      },
      'bank-account-type': {
        required: {
          depends: ui.requires_bank_info
        }
      },
      'bank-account-number': {
        required: {
          depends: ui.requires_bank_info
        },
        digits: {
          depends: ui.requires_bank_info
        }
      },
      'bank-routing-number': {
        required: {
          depends: ui.requires_bank_info
        },
        digits: {
          depends: ui.requires_bank_info
        }
      },
      'bank-name': {
        required: {
          depends: ui.requires_bank_info
        }
      },
      'bank-street-one': {
        required: {
          depends: ui.requires_bank_info
        }
      },
      'bank-city-state-zip': {
        required: {
          depends: ui.requires_bank_info
        }
      }
    }
  });
  //endregion

}

function wizard_validate_highlight(e) {
  $(e).closest('.form-group').removeClass('has-info').addClass('has-error');
}

function wizard_validate_success(e) {
  $(e).closest('.form-group').removeClass('has-error').addClass('has-info');
  $(e).remove();
}

function wizard_error_placement(error, element) {
  if (element.is(':checkbox') || element.is(':radio')) {
    var controls = element.closest('div[class*="col-"]');
    if (controls.find(':checkbox,:radio').length > 1) {
      controls.append(error);
    } else {
      error.insertAfter(element.nextAll('.lbl:eq(0)').eq(0));
    }
  } else if (element.is('.select2')) {
    error.insertAfter(element.siblings('[class*="select2-container"]:eq(0)'));
  } else if (element.is('.chosen-select')) {
    error.insertAfter(element.siblings('[class*="chosen-container"]:eq(0)'));
  } else if (element.is('#eeStreet1')) {
    $('#street-error-anchor').remove();
    var street_error_element = $('<div class="col-xs-12 col-sm-9 col-sm-offset-3" id="street-error-anchor"></div>');
    street_error_element.insertAfter(element.parent().parent()).append(error);
  } else if (element.is('#eeCity') || element.is('#eeState') || element.is('#eeZip')) {
    $('#city-state-zip-error-anchor').remove();
    var city_state_zip_element = $('<div class="col-xs-12 col-sm-9 col-sm-offset-3" id="city-state-zip-error-anchor"></div>');
    city_state_zip_element.insertAfter(element.parent()).append(error);
  } else if (element.closest('.form-group').length > 0) {
    error.appendTo(element.closest('.form-group'));
  } else if ((element.is('#bank-state') || element.is('#bank-city') || element.is('#bank-zip')) && $('#bank-city-state-zip-error').length === 0) {
    var bank_city_state_zip_error = $('<div class="col-xs-12 col-sm-9 col-sm-offset-3" id="bank-city-state-zip-error"></div>');
    bank_city_state_zip_error.insertAfter(element.parent()).append(error);
  } else {
    error.insertAfter(element);
  }
  //else error.insertAfter(element.parent());
}

function step_two_error_placement(error, element) {
  'use strict';
  if ((element.is('[name="height_feet_0"]') || element.is('[name="height_inches_0"]') || element.is('[name="weight_0"]')) &&
    $('#weight_0-error').length === 0 && $('#height_feet_0-error').length === 0 && $('#height_inches_0-error').length === 0) {
    error.appendTo($('#employee-height-weight'));
  } else if ((element.is('[name="height_feet_1"]') || element.is('[name="height_inches_1"]') || element.is('[name="weight_1"]')) &&
    $('#weight_1-error').length === 0 && $('#height_feet_1-error').length === 0 && $('#height_inches_1-error').length === 0) {
    error.appendTo($('#spouse-height-weight'));
  } else if (element.is('[name="tobacco-0"]') || element.is('[name="tobacco-1"]') || element.is('[name="gender-0"]') || element.is('[name="gender-1"]')) {
    error.appendTo(element.parent().parent());
  }
}