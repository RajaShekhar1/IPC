
function init_validation(ui) {

  $('[data-rel=tooltip]').tooltip();

  var validation_debug = false;
  $('#enrollment-wizard').ace_wizard().on('actionclicked.fu.wizard', function (e, info) {
    if (validation_debug) {
      return true;
    }

    if (info.step === 1) {

      // Clear step2 health question error when we attempt to validate step 1
      $("#health_questions_error").html("");

      // Check for "Decline all coverage" and bail out of the wizard if it is checked
      // TODO: This is only if we are doing single product.
      //if (ui.did_decline()) {
      //  submit_decline();
      //  e.preventDefault();
      //  return;
      //}

      // trigger jquery validation
      var is_valid = ui.validator.form();

      if (!ui.is_coverage_selection_valid()) {

        ui.show_no_selection_error();
        e.preventDefault();
        return;
      }

      // TODO: Re-implement check for adding on too much coverage.

      //var current_product_id = ui.insurance_product.product_data.id;
      //var plan = ui.selected_plan();

      function validate_coverage_amount(applicant, applicant_type, selected_coverage) {
        var existing_coverage_amount = applicant.get_existing_coverage_amount_for_product(current_product_id);
        if (!existing_coverage_amount) {
          // Short-circuit, there is no existing coverage to check so we are OK
          return true;
        }

        var applied_coverage_amount = (selected_coverage.is_valid())? selected_coverage.face_value: 0;
        var max_coverage_amount = ui.insurance_product.get_maximum_coverage_amount(applicant_type);

        var name = applicant.name();
        if (existing_coverage_amount && (max_coverage_amount < existing_coverage_amount+applied_coverage_amount)) {

          var additional_allowed_coverage = max_coverage_amount - existing_coverage_amount;
          // Make sure that we don't show a negative number here
          additional_allowed_coverage = _.max([0, additional_allowed_coverage]);
          var msg = ("Due to one or more previous applications this enrollment period for "+
          format_face_value(existing_coverage_amount)+
          " coverage, "+name+" can apply for a maximum of "+
          format_face_value(additional_allowed_coverage)+" additional coverage.");

          alert(msg);
          return false;
        }

        return true;
      }

      // TODO: Re-enable for multiproduct.
      // Check each applicant for selecting too much product.
      //_.each(plan.get_covered_applicants_with_type(), function(val) {
      //  var applicant_type = {"Employee":"employee", "Spouse": "spouse", "Child": "children"}[val.type];
      //  var can_apply = validate_coverage_amount(val.applicant, applicant_type, val.coverage);
      //  if (!can_apply) {
      //    is_valid = false;
      //  }
      //});

      if (!is_valid) {
        e.preventDefault();
      }
    }
    if (info.step == 2 && info.direction == 'next') {
      var is_valid = true;

      return true;

      // validate replacement form
      if (ui.did_select_any_fpp_product() &&
          (ui.should_show_replacement_form())) {
        is_valid &= $('#questions-form').valid();
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
      is_valid &=  are_health_questions_valid();

      if (!is_valid) {
        $("#health_questions_error").html("Please answer all questions for all applicants.  Invalid responses may prevent you from continuing this online application; if so, please see your agent or enrollment professional.");
        e.preventDefault();
        return;
      } else {
        $("#health_questions_error").html("");
      }
    }
    if (info.step == 3 && info.direction == 'next') {
      if (!$('#step3-form').valid()) {
        e.preventDefault();
        return;
      }
    }
    if (info.step == 4 && info.direction == 'next') {
      if (!$('#step4-form').valid()) {
        e.preventDefault();
        return;
      }
    }
    if (info.step == 5 && info.direction == 'next') {
      if (!$('#step5-form').valid()) {
        e.preventDefault();
        return;
      }

      if (ui.has_contingent_beneficiary_error()) {
        e.preventDefault();
        return;
      }

    }
    if (info.step == 6 && info.direction == 'next') {
      if (!$('#step6-form').valid()) {
        e.preventDefault();
        return;
      }
    }

  }).on('finished.fu.wizard', function (e) {

    if (!$('#step6-form').valid()) return false;

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

    submit_application();

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
          depends: function() {
            return ui.is_replacement_form_required();
          }
        }
      },
      replacement_is_terminating: {
        required: {
          depends: function() {
            return ui.is_replacement_form_required();
          }
        }
      },
      replacement_using_funds: {
        required: {
          depends: function() {
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
      depends: function() {
        return ui.is_replacement_form_required();
      }
    }
  });
  $.validator.addClassRules("replacement-details-input", {
    required: {
      depends: function() {
        return ui.should_show_replacement_details_form();
      }
    }
  });

  $('#step3-form').validate({
    errorElement: 'div',
    errorClass: 'help-block',
    focusInvalid: false,
    rules: {
      email: {email: true, required: {
        depends: function() {return ui.is_self_enroll();}
      }},
      eeFName2: {required: true},
      eeLName2: {required: true},
      eeGender: {required: true},
      eessn: {required: true},
      eeStreet1: {required: true},
      eeCity: {required: true},
      eeState: {required: true},
      eeZip: {required: true},
      eeOwner: {required: true},
      eeOtherOwnerName: {
        required: {
          depends: function (element) {
            return ($("#eeOwner-other").is(':checked'))
          }
        }
      },
      eeOtherOwnerSSN: {
        required: {
          depends: function (element) {
            return ($("#eeOwner-other").is(':checked'))
          }
        }
      }
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
      eeOwner: "Please confirm policy owner",
      eeOtherOwnerName: "required",
      eeOtherOwnerSSN: "required"
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
      spOwner: {required: true},
      spOtherOwnerName: {
        required: {
          depends: function (element) {
            return ($("#spOwner-other").is(':checked'))
          }
        }
      },
      spOtherOwnerSSN: {
        required: {
          depends: function (element) {
            return ($("#spOwner-other").is(':checked'))
          }
        }
      }
    },

    messages: {
      spFName2: "required",
      spLName2: "required",
      spGender: "Please choose gender",
      spssn: "Spouse SSN required",//"Spouse SSN required if owner of policy",
      spOwner: "Please confirm policy owner",
      spOtherOwnerName: "required",
      spOtherOwnerSSN: "required"
    },

    highlight: wizard_validate_highlight,
    success: wizard_validate_success,
    errorPlacement: wizard_error_placement
  });

  $('#step5-form').validate({
    errorElement: 'div',
    errorClass: 'help-block',
    focusInvalid: false,
    rules: {
      eeBeneOtherName: {
        required: {
          depends: ui.is_employee_beneficiary_info_required
        }
      },
      eeBeneOtherRelation: {
        required: {
          depends: ui.is_employee_beneficiary_info_required
        }
      },

      // TODO: Re-enable using Knockout per-product
      //eeContBeneOtherName: {
      //  required: {
      //    depends: function(element) {
      //      return (
      //          ui.should_show_contingent_beneficiary() &&
      //          ui.employee_contingent_beneficiary_type() === "other"
      //      )
      //    }
      //  }
      //},
      //eeContBeneOtherRelation: {
      //  required: {
      //    depends: function(element) {
      //      return (
      //          ui.should_show_contingent_beneficiary() &&
      //          ui.employee_contingent_beneficiary_type() === "other"
      //      )
      //    }
      //  }
      //},

      //spBeneOtherName: {
      //  required: {
      //    depends: function(element) {
      //      return (ui.did_select_spouse_coverage() && $("#spBeneOther").is(':checked'))
      //    }
      //  }
      //},
      //spBeneOtherRelation: {
      //  required: {
      //    depends: function(element) {
      //      return (ui.did_select_spouse_coverage() && $("#spBeneOther").is(':checked'))
      //    }
      //  }
      //},
      //spContBeneOtherName: {
      //  required: {
      //    depends: function(element) {
      //      return (
      //          ui.should_show_contingent_beneficiary() &&
      //          ui.did_select_spouse_coverage() &&
      //          ui.spouse_contingent_beneficiary_type() === "other"
      //      );
      //    }
      //  }
      //},

      //spContBeneOtherRelation: {
      //  required: {
      //    depends: function(element) {
      //      return (
      //          ui.should_show_contingent_beneficiary() &&
      //          ui.did_select_spouse_coverage() &&
      //          ui.spouse_contingent_beneficiary_type() === "other"
      //      );
      //    }
      //  }
      //}
    },

    messages: {
      eeBeneOtherName: "required",
      eeBeneOtherRelation: "required",
      spBeneOtherName: "required",
      eeContBeneOtherName: "required",
      eeContBeneOtherRelation: "required",
      spBeneOtherRelation: "required",
      spContBeneOtherName: "required",
      spContBeneOtherRelation: "required"
    },

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
        required: function() {
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
    if (controls.find(':checkbox,:radio').length > 1) controls.append(error);
    else error.insertAfter(element.nextAll('.lbl:eq(0)').eq(0));
  }
  else if (element.is('.select2')) {
    error.insertAfter(element.siblings('[class*="select2-container"]:eq(0)'));
  }
  else if (element.is('.chosen-select')) {
    error.insertAfter(element.siblings('[class*="chosen-container"]:eq(0)'));
  } else if (element.closest('.form-group').length > 0) {
    error.appendTo(element.closest('.form-group'));
  }
  else error.insertAfter(element);
  //else error.insertAfter(element.parent());
}

