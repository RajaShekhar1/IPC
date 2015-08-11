var wizard_viewmodel = (function() {
// Root model of the Benefits wizard User Interface
function WizardUI(defaults) {
  var self = this;

  self.defaults = defaults;
  self.case_id = defaults.case_id;
  self.insurance_product = build_product(self, defaults.products);

  // Confirmation checkboxes for step 6
  self.disclaimer_notice_confirmed = ko.observable(false);
  self.payroll_deductions_confirmed = ko.observable(false);

  // End of step 1, option to decline coverage for all products and skip to the end
  self.did_decline = ko.observable(false);

  // Type of enrollment
  self.is_in_person_application = ko.observable('is_in_person' in defaults && defaults.is_in_person);
  self.is_self_enroll = ko.pureComputed(function() {return !self.is_in_person_application()});

  self.identityToken = ko.observable("");
  self.identityType = ko.observable("");

  self.enrollCity = ko.observable(defaults.enroll_city || "");
  self.enrollState = defaults.state;
  self.was_state_provided = ("state" in defaults && defaults.state !== null && defaults.state != "XX");

  self.company_name = ko.observable(defaults.company_name || "(Unknown Company)");

  // Step 2 data
  self.existing_insurance = ko.observable(null);
  self.replacing_insurance = ko.observable(null);

  self.NAIC_AND_MI = ['AK', 'AL', 'AR', 'AZ', 'CO', 'IA', 'LA', 'MD', 'ME', 'MS', 'MT',
    'NC', 'NE', 'NH', 'NJ', 'NM', 'OH', 'OR', 'RI', 'SC', 'TX', 'UT', 'VA', 'VT',
    'WI', 'WV', 'MI'];

  self.is_NAIC_OR_MI = function() {
    return self.insurance_product.is_fpp_product() && _.contains(self.NAIC_AND_MI, self.enrollState);
  };

  self.is_KY_OR_KS = function() {
    return self.enrollState == "KY" || self.enrollState == "KS";
  };

  self.is_CT_DC_ND_VI = function() {
    return _.contains(['CT', 'DC', 'ND', 'VI'], self.enrollState);
  };

  self.is_non_NAIC_other = function() {
    return !(self.is_NAIC_OR_MI() || self.is_KY_OR_KS() || self.is_CT_DC_ND_VI());
  };

  self.get_has_existing_question_highlight = function() {
    if (self.is_self_enroll()) {
      return 'flag';
    }

    return "checkmark";
  };

  self.get_replacing_question_highlight = function() {
    if (self.is_KY_OR_KS()) {
      return "stop";
    }
    if (self.is_CT_DC_ND_VI()) {
      return "checkmark";
    }

    return 'flag';
  };

  self.warning_modal_title = ko.observable("");
  self.warning_modal_body = ko.observable("");
  self.show_warning_modal = function(title, body) {
    self.warning_modal_title(title);
    self.warning_modal_body(body);
    $("#warning_modal").modal('show');
  };

  var default_warning_body = 'STOP: A "yes" response to this question disqualifies you from completing this application in your state.';

  self.select_has_existing_insurance = function() {
    self.existing_insurance(true);
  };

  self.select_replacing_insurance = function() {
    self.replacing_insurance(true);

    if (!self.insurance_product.is_fpp_product() || self.is_KY_OR_KS()) {
      self.show_warning_modal("Replacement Notice", default_warning_body);
    }
  };

  self.should_show_NAIC_replacement_notice = ko.computed(function() {
    // Show special notice if we are in this category and we say 'No' to the replacement question.
    return (self.is_NAIC_OR_MI() && self.replacing_insurance() === false);
  });

  self.should_show_replacement_form = ko.computed(function() {
    if (!self.insurance_product.is_fpp_product()) {
      return false;
    }

    if (self.is_KY_OR_KS() || self.is_CT_DC_ND_VI())
      return false;

    // Self-enroll is the only way that existing insurance does anything when yes.
    if (self.is_self_enroll() && self.existing_insurance())
      return true;

    if (self.should_show_NAIC_replacement_notice())
      return false;

    return self.replacing_insurance();
  });

  self.replacement_read_aloud = ko.observable(false);
  self.replacement_is_terminating = ko.observable(null);
  self.replacement_using_funds = ko.observable(null);

  // Watch for self-enroll situation that stops the user from continuing.
  var self_enroll_stop_message = "This response requires you to speak with your insurance representative directly, and prevents you from continuing enrollment at this time. You may cancel this enrollment session and contact your representative.";
  self.replacement_is_terminating.subscribe(function() {
    if (self.is_self_enroll() && self.replacement_is_terminating()) {
      self.show_warning_modal("Replacement Notice", self_enroll_stop_message);
    }
  });
  self.replacement_using_funds.subscribe(function() {
    if (self.is_self_enroll() && self.replacement_using_funds()) {
      self.show_warning_modal("Replacement Notice", self_enroll_stop_message);
    }
  });

  // Policy details form
  self.should_show_replacement_details_form = ko.pureComputed(function() {
    if (self.is_self_enroll()) {
      // Not allowed to do this for self-enroll.
      return false;
    }
    return self.replacement_using_funds() || self.replacement_is_terminating();
  });

  self.get_replacement_paragraphs = ko.computed(function() {
    var paragraph_map = self.insurance_product.get_replacement_paragraphs();
    var paragraphs = paragraph_map[self.enrollState];
    if (!paragraphs) {
      return [];
    }
    return paragraphs;
  });

  self.replacement_policies = ko.observableArray([new ReplacementPolicy()]);

  self.add_replacement_policy = function() {
    self.replacement_policies.push(new ReplacementPolicy());
  };

  self.remove_replacement_policy = function(policy) {
    self.replacement_policies.remove(policy);
  };

  self.is_replacement_form_required = ko.computed(function() {
    return (
        self.insurance_product.is_fpp_product() &&
        (self.existing_insurance() || self.replacing_insurance())
    );
  });

  self.policy_owner = ko.observable("self");
  self.other_owner_name = ko.observable("");
  self.other_owner_ssn = ko.observable("");

  self.spouse_policy_owner = ko.observable("employee");
  self.spouse_other_owner_name = ko.observable("");
  self.spouse_other_owner_ssn = ko.observable("");


  // Group the selected options for this product into a 'BenefitsPackage' which has
  //  the individual choices for employee, spouse, and children
  self.selected_plan = ko.observable(new NullBenefitsPackage(self));

  // SOH Questions (depends on product and selected plan)
  var questions = process_spouse_question_data(self, defaults.spouse_questions, self.insurance_product.product_data);
  var soh_questions = process_health_question_data(self, defaults.health_questions, self.insurance_product.product_data);
  $.merge(questions, soh_questions);
  self.health_questions = ko.observableArray(questions);


  // Which, if any, of the good, better, best recommended plans was chosen (even if customized)
  self.selected_recommendation = ko.observable(null);

  // Employee
  self.employee = ko.observable(new InsuredApplicant(
      InsuredApplicant.EmployeeType,
      self.defaults.employee_data || {},
      self.selected_plan,
      self.health_questions
  ));

  // Extended questions for step 1
  self.should_show_gender = function() {return self.insurance_product.requires_gender();};
  self.should_show_height = function() {return self.insurance_product.requires_height();};
  self.should_show_weight = function() {return self.insurance_product.requires_weight();};
  self.should_show_smoker = function() {return self.insurance_product.requires_is_smoker();};
  self.should_show_extended_questions = function() {
    return (
        self.should_show_gender() ||
        self.should_show_height() ||
        self.should_show_weight() ||
        self.should_show_smoker()
    );
  };

  // Spouse info
  var spouse_data = self.defaults.spouse_data || {};
  // default the last name to employee's last name if not present
  if (!spouse_data.last) {
    spouse_data.last = self.employee().last();
  }

  self.spouse = ko.observable(new InsuredApplicant(
      InsuredApplicant.SpouseType,
      spouse_data,
      self.selected_plan,
      self.health_questions
  ));

  var is_initially_showing_spouse = (
      self.spouse().first() && self.spouse().first() !== "" &&
      self.spouse().last() && self.spouse().last() !== "" &&
      self.spouse().birthdate() && self.spouse().birthdate() !== undefined
  );
  // Corresponds to the 'Married' checkbox
  self.should_show_spouse = ko.observable(is_initially_showing_spouse);

  self.is_spouse_valid = ko.computed(function() {
    return self.insurance_product.is_valid_spouse(self.spouse()) ;
  });

  self.should_include_spouse_in_table = ko.computed(function() {
    return self.should_show_spouse() && self.spouse().is_valid() && self.is_spouse_valid();
  });
  self.show_spouse_name = ko.computed(function() {
    return (self.should_include_spouse_in_table()) ? self.spouse().name() : "";
  });

  // Spouse address
  self.is_spouse_address_same_as_employee = ko.observable(!self.spouse().address1() || (
          self.employee().address1() === self.spouse().address1() &&
          self.employee().address2() === self.spouse().address2() &&
          self.employee().city() === self.spouse().city() &&
          self.employee().state() === self.spouse().state() &&
          self.employee().zip() === self.spouse().zip()
      ));
  self.is_spouse_email_same_as_employee = ko.observable((!self.spouse().email() || self.employee().email() === self.spouse().email()));

  // Beneficiaries
  self.employee_beneficiary_type = ko.observable("spouse");
  self.employee_other_beneficiary = ko.observable(new beneficiary.Beneficiary());
  self.employee_contingent_beneficiary_type = ko.observable("none");
  self.employee_contingent_beneficiary = ko.observable(new beneficiary.Beneficiary());

  self.spouse_beneficiary_type = ko.observable("spouse");
  self.spouse_other_beneficiary = ko.observable(new beneficiary.Beneficiary());
  self.spouse_contingent_beneficiary_type = ko.observable("none");
  self.spouse_contingent_beneficiary = ko.observable(new beneficiary.Beneficiary());

  self.is_employee_beneficiary_info_required = function () {
    return !self.did_select_spouse_coverage() || $("#eeBeneOther").is(':checked')
  };
  //self.is_employee_contingent_info_required = function() {
  //    return
  //};

  // Children
  self.should_include_children = ko.observable(
      self.defaults.children_data.length > 0
  );
  if (self.defaults.children_data.length == 0) {
    self.children = ko.observableArray([
      // Start with two blank child entries
      new InsuredApplicant(InsuredApplicant.ChildType, {last: self.employee().last() || ""}, self.selected_plan, self.health_questions),
      new InsuredApplicant(InsuredApplicant.ChildType, {last: self.employee().last() || ""}, self.selected_plan, self.health_questions)
    ]);
  } else {
    self.children = ko.observableArray($.map(self.defaults.children_data, function(child_data) {
      // if child's last name is blank, default it to the employee's last name
      if (!child_data.last) {
        child_data.last = self.employee().last();
      }
      return new InsuredApplicant(InsuredApplicant.ChildType, child_data, self.selected_plan, self.health_questions);
    }));
  }

  self.get_valid_children = ko.computed(function() {
    var children = [];
    $.each(self.children(), function() {
      if (this.is_valid() && self.insurance_product.is_valid_child(this)) {
        children.push(this);
      }
    });
    return children;
  });

  self.show_children_names = ko.computed(function() {
    if (self.should_include_children()) {
      // TODO: Put this in a better spot
      $('.input-mask-ssn').mask('999-99-9999');

      var valid_children = self.get_valid_children();
      var firstnames = $.map(valid_children, function(child) {
        return child.first();
      });
      var names = firstnames.join(", ");
      if (valid_children.length > 1) {
        names += " (Each)";
      }
      return names;
    } else {
      return "";
    }
  });


  self.add_child = function() {
    var child_insured_applicant = new InsuredApplicant(InsuredApplicant.ChildType, {last: self.employee().last() || ""}, self.selected_plan, self.health_questions);
    self.children.push(child_insured_applicant);

    // Re-apply date mask to children (also ssn for step 4)
    $('.input-mask-date').mask('99/99/9999');
    $('.input-mask-ssn').mask('999-99-9999');

  };

  self.remove_child = function(child) {
    self.children.remove(child);

    // Uncheck include children if last child
    if (self.children().length == 0) {
      self.should_include_children(false);
    }
  };

  self.rendered_child = function(element) {
    $(element).hide().slideDown(400);
  };
  self.removing_child = function(element) {
    $(element).slideUp(function() {$(element).remove();});
  };
  self.has_valid_children = ko.computed(function() {
    return  self.get_valid_children().length > 0;
  });

  self.are_children_ages_valid = ko.computed(function() {
    var all_valid = true;
    $.each(self.children(), function() {
      if (!self.insurance_product.is_valid_child(this)) {
        all_valid = false;
        return false;
      }
    });
    return all_valid;
  });

  self.should_include_children_in_table = ko.computed(function() {
    return self.should_include_children() && self.has_valid_children() && self.are_children_ages_valid();
  });

  // Store the actual benefits for children here, rather
  //  than on each child since the benefits are the same
  self.child_benefits = ko.observable(new InsuredApplicant(InsuredApplicant.ChildType, {}, self.selected_plan, self.health_questions));
  self.get_children_options = ko.computed(function() {
    if (!self.should_include_children_in_table()) {
      return [];
    }

    // return the benefit options of the any child
    return self.insurance_product.get_coverage_options_for_applicant('children')();
  });

  // Payment mode
  self.payment_mode = ko.observable(defaults.payment_mode);
  if(defaults.payment_mode_choices == null) {
    self.payment_mode_choices = ko.observable(null);
  } else {
    self.payment_mode_choices = ko.observable(defaults.payment_mode_choices);
  }

  self.payment_mode_text = ko.pureComputed(function() {
    try {
      return _.find(self.payment_mode_choices(), function(x) { return x.mode == self.payment_mode() }).name;
    } catch(e) {
      return '(NA)'
    }
  });

  self.payment_mode_text_lower = ko.pureComputed(function() {
    try {
      return _.find(self.payment_mode_choices(), function(x) { return x.mode == self.payment_mode() }).name.toLowerCase();
    } catch(e) {
      return '(NA)'
    }
  });

  self.is_show_rates_clicked = ko.observable(false);

  self.is_rate_table_loading = ko.observable(false);

  self.is_employee_info_valid = ko.computed(function() {
    return self.insurance_product.is_valid_employee(self.employee()) ;
  });

  self.is_payment_mode_valid = ko.computed(function() {
    return self.payment_mode() != undefined;
  });


  self.can_display_rates_table = ko.computed(function() {

    // All employee info
    var valid = self.employee().is_valid();
    valid &= self.is_employee_info_valid();
    valid &= self.is_payment_mode_valid();

    // Trigger jquery validation manually
    if (self.is_show_rates_clicked()) {
      self.validator.form();
    }

    return valid;
  });


  self.is_recommended_table_visible = ko.computed(function() {
    return (self.is_show_rates_clicked() && self.can_display_rates_table());
  });
  self.show_recommendations_table = function() {
    // Trigger validation manually
    if (!self.validator.form()) {
      return;
    }

    if (!self.can_display_rates_table()) {
      return;
    }

    self.refresh_rate_table();

    self.is_show_rates_clicked(true);

  };

  self.update_rate_table = function() {
    // Reset some validation errors
    $.each(limit_error_lookup, function(k, v) {
      limit_error_lookup[k](null);
    });
    //self.validator.resetForm();

    if (!self.is_recommended_table_visible()) {
      self.is_rate_table_loading(false);
      return;
    }
    self.refresh_rate_table();
  };

  self.refresh_rate_table = function() {
    self.is_rate_table_loading(true);

    var product_id = self.insurance_product.product_data.id;
    ajax_post(
        "/products/"+product_id+"/rates",
        self.build_rate_parameters(),
        self.show_updated_rates,
        self.handle_update_rates_error,
        true
    );
  };

  self.select_recommended_benefit = function(recommendations) {

    self.selected_recommendation(recommendations);

    // Reset custom options
    self.employee().selected_custom_option(null);
    self.spouse().selected_custom_option(null);
    self.child_benefits().selected_custom_option(null);

    // Use the recommended options for the selected plan
    var new_plan = self.get_new_plan_from_recommendations(recommendations);
    self.selected_plan(new_plan);
  };

  self.get_new_plan_from_recommendations = function(recommendations){
    var benefits_package = new BenefitsPackage(self, recommendations.name());
    benefits_package.employee_recommendation(recommendations.employee_recommendation());
    benefits_package.spouse_recommendation(recommendations.spouse_recommendation());
    benefits_package.children_recommendation(recommendations.children_recommendation());
    return benefits_package;
  };

  self.build_rate_parameters = function() {

    return {
      //product_type: self.insurance_product.product_type,
      employee: self.employee().serialize_data(),
      spouse: self.should_include_spouse_in_table()? self.spouse().serialize_data() : null,
      num_children: self.children().length,
      payment_mode: self.payment_mode()
    };
  };


  // When a custom setting is selected
  self.apply_selected_customization = function() {
    var new_plan;
    if (self.selected_recommendation()) {
      // Apply this first
      new_plan = self.get_new_plan_from_recommendations(self.selected_recommendation());
    } else {
      new_plan = new BenefitsPackage(self, "Custom");
    }

    var benefit;
    if (self.employee().is_valid()) {
      benefit = self.employee().selected_custom_option();
      if (benefit) {
        new_plan.name("Custom");
        new_plan.employee_recommendation(new Recommendation(benefit));
      }
    }

    if (self.should_include_spouse_in_table()) {
      benefit = self.spouse().selected_custom_option();
      if (benefit) {
        new_plan.name("Custom");
        new_plan.spouse_recommendation(new Recommendation(benefit));
      }
    }

    if (self.should_include_children_in_table()) {
      benefit = self.child_benefits().selected_custom_option();
      if (benefit) {
        new_plan.name("Custom");
        new_plan.children_recommendation(new Recommendation(benefit));
      }
    }

    // set the new plan
    self.selected_plan(new_plan);
  };

  // Update rates when certain values change

  self.should_rates_update = ko.computed(function() {
    // trigger all the following as dependencies
    var watch_data_values = [
      self.employee().birthdate,
      self.employee().is_smoker,
      self.employee().height,
      self.employee().weight,
      self.spouse().birthdate,
      self.spouse().is_smoker,
      self.spouse().weight,
      self.spouse().height,
      self.children,
      self.payment_mode,
      //self.should_include_spouse_in_table,
      self.should_show_spouse,
      self.should_include_children_in_table
    ];
    _.each(watch_data_values, function(observable) {
      // Link this observable as a dependency
      observable()
    });

  }).extend({
        rateLimit: {timeout: 500, method: "notifyWhenChangesStop"},
        notify: 'always'
      }
  );

  self.should_rates_update.subscribe(self.update_rate_table);

  self.should_show_spouse.subscribe(function(val) {
    if (self.should_show_spouse()) {
      // TODO: Cannot remember why this is necessary.
      setTimeout(function() {
        // Force validation
        self.validator.form();
      }, 0);
    }
  });

  self.riders = ko.observable({
    emp: [],
    sp: []
  });

  self.case_riders = ko.observableArray();

  self.selected_riders = ko.observable({
    emp: ko.observableArray([]),
    sp: ko.observableArray([])
  });

  self.enrollment_riders = ko.observableArray();

  self.get_selected_riders = ko.computed(function(person) {
    console.log(person);
    return self.selected_riders().emp()
  });

  function get_rider_by_code(code) {
    for(var i = 0; i < self.enrollment_riders.length; i++) {
      if(code == self.enrollment_riders[i].code) {
        return self.enrollment_riders[i];
      }
    }
  }

  self.toggle_selected_riders = function(rider_code, prefix) {
    rider = get_rider_by_code(rider_code);
    selected_riders = self.selected_riders()[prefix];
    if(selected_riders.indexOf(rider) == -1) {
      selected_riders.push(rider);
    } else {
      selected_riders.splice(selected_riders.indexOf(rider), 1);
    }
  }

  self.show_updated_rates = function(resp) {
    var data = resp.data;
    self.insurance_product.parse_benefit_options('employee', self.employee(), data.employee_rates);
    self.insurance_product.parse_benefit_options('spouse', self.spouse(), data.spouse_rates);

    // Reset child rates
    self.insurance_product.parse_benefit_options('children', self.child_benefits(), data.children_rates);

    if (data.recommendations) {
      self.recommendations.good.set_recommendations(data.recommendations['good']);
      self.recommendations.better.set_recommendations(data.recommendations['better']);
      self.recommendations.best.set_recommendations(data.recommendations['best']);
    }

    // Update selection with new data
    if (self.selected_plan().is_valid()) {
      self.apply_selected_customization();
    }

    // Done loading rates
    self.is_rate_table_loading(false);
  };

  self.handle_update_rates_error = function(resp) {
    if (resp.status === 400 && resp.responseJSON && resp.responseJSON.errors) {
      $.each(resp.responseJSON.errors, function() {
        var field_name = this.field;
        var message = this.error;
        // set error and show with validator
        limit_error_lookup[field_name](true);
        self.validator.form();
      });
    } else {
      handle_remote_error();
    }
  };


  self.lose_input_focus = function(data, event) {
    var element = $(event.target);
    element.blur();
  };

  // accessors for selected plan
  self.did_select_employee_coverage = ko.computed(function() {
    var rec = self.selected_plan().employee_recommendation();
    return (rec.is_valid() && rec.recommended_benefit.is_valid());
  });
  self.did_select_spouse_coverage = ko.computed(function() {
    var rec = self.selected_plan().spouse_recommendation();
    return (rec.is_valid() && rec.recommended_benefit.is_valid());
  });
  self.did_select_children_coverage = ko.computed(function() {
    var rec = self.selected_plan().children_recommendation();
    return (rec.is_valid() && rec.recommended_benefit.is_valid());
  });



  self.has_contingent_beneficiary_error = ko.computed(function() {
    var employee_beneficiary_error = (
        self.employee_contingent_beneficiary_type() === 'spouse' &&
        self.employee_beneficiary_type() === 'spouse'
    );
    var spouse_beneficiary_error = (
        self.did_select_spouse_coverage() &&
        self.insurance_product.should_show_contingent_beneficiary() &&
        self.spouse_contingent_beneficiary_type() === 'spouse' &&
        self.spouse_beneficiary_type() === 'spouse'
    );
    return (
        self.insurance_product.should_show_contingent_beneficiary()
        && (
            employee_beneficiary_error
            || spouse_beneficiary_error
        )
    );
  });



  self.show_health_modal = function() {
    $("#health_modal").modal('show');
  };

  self.recommendations = {
    good: new BenefitsPackage(self, 'Good'),
    better: new BenefitsPackage(self, 'Better'),
    best: new BenefitsPackage(self, 'Best')
  };


  // jquery form validator
  $.validator.addMethod("minAge", function(val, element, params) {
    var age = age_for_date(val);
    return (age !== "" && age >= params);
  }, "Must be at least {0} years old for this product");

  $.validator.addMethod("maxAge", function(val, element, params) {
    var age = age_for_date(val);
    return (age !== "" && age <= params);
  }, "Must be no more than {0} years old for this product");


  // Height and Weight limits for Group CI

  var limit_error_lookup = {
    employee_height:self.employee().height_error,
    employee_weight:self.employee().weight_error,
    spouse_height:self.spouse().height_error,
    spouse_weight:self.spouse().weight_error
  };

  $.validator.addMethod("empHeightLimit", function(val, el, params) {
    return self.employee().height_error() == null;
  }, "The height or weight entered is outside the limits for this product.");
  $.validator.addMethod("empWeightLimit", function(val, el, params) {
    return self.employee().weight_error() == null;
  }, "The height or weight entered is outside the limits for this product.");
  $.validator.addMethod("spHeightLimit", function(val, el, params) {
    return self.spouse().height_error() == null;
  }, "The height or weight entered is outside the limits for this product.");
  $.validator.addMethod("spWeightLimit", function(val, el, params) {
    return self.spouse().weight_error() == null;
  }, "The height or weight entered is outside the limits for this product.");

  function any_valid_spouse_field() {
    //return self.should_include_spouse_in_table(); //self.should_show_spouse();
    return self.spouse().any_valid_field();
  }
  self.validator = $("#step1-form").validate({
    highlight: wizard_validate_highlight,
    success: wizard_validate_success,
    errorPlacement: wizard_error_placement,
    errorElement: 'div',
    errorClass: 'help-block',
    //focusInvalid: false,
    rules: {
      eeBenefitFName: "required",
      eeBenefitLName: "required",
      eeBenefitDOB: {
        required: true,
        date: true,
        minAge: self.insurance_product.min_emp_age(),
        maxAge: self.insurance_product.max_emp_age()
      },
      'tobacco-0': {
        required: true
      },
      'gender-0': "required",
      spFName: {
        required: {
          depends: any_valid_spouse_field
        }
      },
      spLName: {
        required: { depends: any_valid_spouse_field }
      },
      spDOB: {
        required: {depends: any_valid_spouse_field},
        date: {depends: any_valid_spouse_field},
        minAge: {
          param: self.insurance_product.min_sp_age(),
          depends: any_valid_spouse_field
        },
        maxAge: {
          param: self.insurance_product.max_sp_age(),
          depends: any_valid_spouse_field
        }
      },
      'tobacco-1': {required: {depends: any_valid_spouse_field }},
      'gender-1': {required: {depends: any_valid_spouse_field }},
      'weight_0': {
        required: true,
        empWeightLimit: true
      },
      'weight_1': {
        required: { depends: any_valid_spouse_field },
        spWeightLimit: true
      },
      'height_feet_0': {
        required: true,
        empHeightLimit: true
      },
      'height_inches_0': {
        required: true,
        empHeightLimit: true
      },
      'height_feet_1': {
        required: { depends: any_valid_spouse_field },
        spHeightLimit: true
      },
      'height_inches_1': {
        required: { depends: any_valid_spouse_field },
        spHeightLimit: true
      },
      paymentMode: {
        required: true
      },
      debug: true
    },
    groups: {
      emp_height: "height_feet_0 height_inches_0",
      sp_height: "height_feet_1 height_inches_1"
    }
  });


  function is_child_name_required(element) {
    return true;
  }
  function is_child_field_required(element) {
    if ($(element).attr("id") === "child-first-0" ||
        $(element).attr("id") === "child-last-0" ||
        $(element).attr("id") === "child-dob-0"
    ) {
      // Treat the first child as always required if
      // the children checkbox is checked
      return self.should_include_children();
    }

    var child = ko.dataFor(element);
    if (!child) {
      return false;
    }

    return child.any_valid_field();
  }

  $.validator.addClassRules("child_birthdate",
      {
        required: {
          depends: is_child_field_required
        },
        date: {
          depends: is_child_field_required
        },
        minAge: {
          param: self.insurance_product.min_child_age(),
          depends: is_child_field_required
        },
        maxAge: {
          param: self.insurance_product.max_child_age(),
          depends: is_child_field_required
        }
      }
  );

  $.validator.addClassRules("child_first",  {
        required: {
          depends: is_child_name_required
        }
      }
  );
  $.validator.addClassRules("child_last",  {
        required: {
          depends: is_child_name_required
        }
      }
  );



  self.is_form_valid = ko.computed(function() {

    return (
        self.selected_plan().is_valid() &&
        self.selected_plan().has_at_least_one_benefit_selected()
    );
  });

  self.attempted_advance_step = ko.observable(false);
  self.is_selection_error_visible = ko.computed(function() {
    return self.attempted_advance_step() && !self.is_form_valid();
  });
  self.show_no_selection_error = function() {
    self.attempted_advance_step(true);
  };

  // Statement of Health questions
  // A computed observable list of applicants who have selected coverage
  self.get_covered_applicants = ko.computed(function() {
    if (!self.selected_plan()) {
      return [];
    } else {
      return self.selected_plan().get_all_covered_people();
    }
  });

  self.get_all_applicants = ko.computed(function() {
    var people = [self.employee()];
    if (self.should_include_spouse_in_table()) {
      people.push(self.spouse());
    }
    _.each(self.get_valid_children(), function(child) {
      people.push(child);
    });

    return people;
  });

  self.do_any_health_questions_need_answering = ko.computed(function() {
    if (self.health_questions().length == 0) {
      return false;
    }
    return _.any(self.health_questions(), function(question) {
      return question.does_any_applicant_need_to_answer();
    })
  }, null, {deferEvaluation: true});


  self.should_show_other_insurance_questions = ko.computed(function() {
    return (
        self.insurance_product.product_type != "Group CI"
    );
  });

  // Decline info box
  self.did_decline.subscribe(function(val) {
    if (val) {
      bootbox.dialog({
        message: "Please note that even after declining enrollment, you may still change your decision and elect to enroll at a later time, provided that your employer's enrollment period is still active.",
        buttons: {
          "success": {
            "label": "Close",
            "className": "btn-sm btn-primary"
          }
        }
      });
    }
  });

  self.can_decline = ko.computed(function() {
    // Can not decline if any applicant has applied for coverage already

    return (!_.any(self.get_all_applicants(), function(applicant) {
      return applicant.get_existing_coverage_amount_for_product(self.insurance_product.product_data.id);
    }));

  });

  // New FPP form questions
  self.is_employee_actively_at_work = ko.observable(null);
  self.has_spouse_been_treated_6_months = ko.pureComputed(function() {
    return _find_answer_for_spouse_question_with_label("Spouse Treated 6 Months");
  });
  self.has_spouse_been_disabled_6_months = ko.pureComputed(function() {
    return _find_answer_for_spouse_question_with_label("Spouse Disabled 6 Months");
  });

  function _find_answer_for_spouse_question_with_label(lbl) {
    if (!self.did_select_spouse_coverage()) {
      return null;
    }

    var spouse_question = _.find(self.health_questions(), function(q) {
      return q.get_question_label() === lbl;
    });
    if (!spouse_question) {
      return null;
    }

    if (spouse_question.can_spouse_skip_due_to_GI()) {
      return "GI";
    }

    // Find the answer given
    var spouse_answer = _.find(self.spouse().health_questions(), function(answer) {
      return answer.question.label === spouse_question.get_question_label();
    });
    if (!spouse_answer) {
      return null;
    }

    return spouse_answer.answer();
  }

  self.exit_application = function() {
    bootbox.dialog({
      message: "Are you sure you want to exit? All data on this application will be discarded.",
      buttons: {
        default: {
          label: "Cancel",
          className: "btn-default",
          callback: function () {

          }
        },
        danger: {
          label: "Exit application and discard data",
          className: "btn-danger",
          callback: function () {
            window.location.href = "/enrollment-case/"+self.case_id+"#enrollment";
          }
        }
      }
    });
  };
}
  return {WizardUI: WizardUI}
})();