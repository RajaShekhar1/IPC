var wizard_viewmodel = (function() {

  ko.bindingHandlers.wizard = {
    init: function(element, val){
      $(element).ace_wizard();
    }
  };

  function CoverageVM(available_products, applicant_list) {
    this.available_products = available_products;
    this.applicants = applicant_list;

    this.set_product_rates = function(product_rates) {
      // Find the right product, call set_rates
      var product_coverage = _.find(this.product_coverage_viewmodels(), function(p) {
        return p.product.product_data.id === product_rates.product_id
      }, this);
      if (product_coverage) {
        product_coverage.set_product_rates(product_rates);
      }
    };

    this.product_coverage_viewmodels = ko.observableArray(_.map(available_products, function(p) {
      return new ProductCoverageViewModel(p, applicant_list);
    }));

    // Which product coverage is being displayed right now?
    this.current_product = ko.observable(this.product_coverage_viewmodels()[0]);

    this.has_multiple_products = ko.pureComputed(function() {
      return this.product_coverage_viewmodels().length > 1;
    }, this);

    this.previous_product = ko.pureComputed(function() {
      var index = this.product_coverage_viewmodels().indexOf(this.current_product());
      return val_or_null(this.product_coverage_viewmodels(), index - 1);
    }, this);

    this.next_product = ko.pureComputed(function() {
      var index = this.product_coverage_viewmodels().indexOf(this.current_product());
      return val_or_null(this.product_coverage_viewmodels(), index + 1);
    }, this);

    function val_or_null(array, index) {
      if (index >= 0 && index < index.length) {
        return array[index];
      }
      return null;
    }
  }

  function ProductRatesVM() {
    this.rates = ko.observable();
  }
  ProductRatesVM.prototype = {

  };

  function ProductCoverageViewModel(product, applicant_list) {
    var self = this;

    self.product = product;
    self.applicant_list = applicant_list;
    self.available_recommendations = ko.observableArray(null);
    self.selected_recommendation = ko.observable(new NullRecommendation({}));
    self.rates = new ProductRatesVM();

    // Cache the selections for applicants, instances should be instantiated just once.
    self._applicant_coverage_viewmodels = {};

    self.applicant_coverage_selections = ko.pureComputed(this._get_applicant_coverage_selections, this);

  }

  ProductCoverageViewModel.prototype = {
    format_product_name: function() {
      return this.product.product_data.name;
    },

    set_product_rates: function(rates) {
      if (rates.recommendations) {
        // TODO: do we try to keep the previously selected recommendation?
        this.selected_recommendation(new NullRecommendation({}));
        this.available_recommendations(rates.recommendations);
      }
    },

    get_coverage_selection_template: function() {
      // in the future, products may not all use the recommendation system for coverage selection.
      return 'recommendation_coverage_selection';
    },

    select_recommended_coverage: function(recommendation) {
      // Record the selected recommendation
      this.selected_recommendation(recommendation);

      // Apply the recommendation to each applicant
      _.each(this.applicant_coverage_selections(), function(applicant_coverage) {
        applicant_coverage.select_recommended_coverage(recommendation);
      }, this);
    },

    _get_applicant_coverage_selections: function() {
      //
      // Return every valid ApplicantGroup that can get coverage for this product.
      var selections = [];

      if (this.applicant_list.has_valid_employee()) {
        selections.push(this.get_coverage_for_applicant(this.applicant_list.get_employee()));
      }
      if (this.applicant_list.has_valid_spouse()) {
        selections.push(this.get_coverage_for_applicant(this.applicant_list.get_spouse()));
      }
      if (this.applicant_list.has_valid_children()) {
        // TODO: insert check to see if product groups children for coverage, otherwise add each child separately.
        selections.push(this.get_coverage_for_applicant(this.applicant_list.get_children_group()));
      }

      return selections;
    },

    get_coverage_for_applicant: function(applicant) {
      // special case for group of children; if a child applicant is passed, use the group coverage
      if (applicant.type == wizard_applicant.Applicant.ChildType) {
        applicant = this.applicant_list.get_children_group();
      }

      if (applicant._id in this._applicant_coverage_viewmodels) {
        return this._applicant_coverage_viewmodels[applicant._id];
      } else {
        var new_selection_instance = new ApplicantCoverageSelectionVM(applicant, this.product);
        this._applicant_coverage_viewmodels[applicant._id] = new_selection_instance;
        return new_selection_instance;
      }
    },

    get_applicant_recommendations: function(recommendation) {
      return _.map(this.applicant_coverage_selections(), function(applicant_coverage) {
        return new ApplicantRecommendationVM(applicant_coverage.applicant, recommendation, this.rates);
      }, this);
    }
  };

  function ApplicantRecommendationVM(applicant, recommendation, rates) {
    var self = this;
    self.applicant = applicant;
    self.recommendation = recommendation;
    self.rates = rates;

    self.get_coverage_recommendation = function() {
      if (self.recommendation === null) {
        return new NullBenefitOption();
      } else {
        var rec_cov = self.recommendation.coverages[applicant.type];
        if (!rec_cov) {
          return new NullBenefitOption();
        } else {
          // Lookup in rates
          return self.rates.get_option_for_coverage(rec_cov);
        }
      }
    };

    self.is_valid = function() {
      return self.get_coverage_recommendation().is_valid();
    };
  }

  function ApplicantCoverageSelectionVM(applicant, product) {
    this.applicant = applicant;
    this.product = product;

    this.coverage_option = ko.observable(new NullBenefitOption());
    this.riders = ko.observableArray();
    this.beneficiaries = ko.observableArray([]);
  }
  ApplicantCoverageSelectionVM.prototype = {
    select_recommended_coverage: function(recommendation) {
      var coverage_option = recommendation[this.applicant.type];
      if (coverage_option) {
        this.coverage_option(coverage_option);
      }
    },

    select_custom_coverage: function(option) {
      this.coverage_option(option);
    },

    has_selected_coverage: function(){
      return this.coverage_option().is_valid();
    },

    get_selected_coverage: function() {
      return this.coverage_option();
    },

    format_name: function() {
      return this.applicant.name();
    }
  };

  function WizardVM(options) {
    var self = this;
    self.enrollment_case = options.case_data;
    self.products = wizard_products.build_products(self, options.products);

    self.is_in_person_application = (options.enrollment_type === "inperson");

    self.is_rate_table_loading = ko.observable(false);
    self.is_show_rates_clicked = ko.observable(false);

    init_applicants();

    init_jquery_validator();



    self.coverage_vm = new CoverageVM(self.products, self.applicant_list);
    self.product_coverage_viewmodels = self.coverage_vm.product_coverage_viewmodels;

    // Step 1 ViewModel observables
    var should_show_spouse_initially = self.spouse().any_valid_field();
    self.should_show_spouse = ko.observable(should_show_spouse_initially);

    var should_show_children_initially = false;
    self.should_include_children = ko.observable(should_show_children_initially);

    self.add_child = function() {
      var child = wizard_applicant.create_applicant(
          {
            last: self.employee().last(),
            type: wizard_applicant.Applicant.ChildType
          }
      );
      self.applicant_list.add_applicant(child);
    };

    self.remove_child = function(child) {
      self.applicant_list.remove_applicant(child);

      // Uncheck the show children switch if this is the last child.
      if (self.applicant_list.get_valid_children().length === 0) {
        self.should_include_children(false);
      }
    };

    // callbacks for animation on children list changes
    self.rendered_child = function(element) {
      $(element).hide().slideDown(400);
    };
    self.removing_child = function(element) {
      $(element).slideUp(function() {$(element).remove();});
    };

    self.is_recommended_table_visible = ko.computed(function() {
      return (self.is_show_rates_clicked() && self.can_display_rates_table());
    });

    self.update_rate_table = function() {
      // Reset some validation errors
      //$.each(limit_error_lookup, function(k, v) {
      //  limit_error_lookup[k](null);
      //});

      //self.validator.resetForm();
      if (!self.is_rate_table_loading()) {
        self.is_rate_table_loading(true);
      }
      self.is_show_rates_clicked(true);

      if (!self.is_recommended_table_visible()) {
        self.is_rate_table_loading(false);
        return;
      }
      self.refresh_rate_table();
    };

    self.refresh_rate_table = function() {
      self.is_rate_table_loading(true);

      product_rates_service.get_product_rates(
          self.products,
          self.payment_mode(),
          self.applicant_list,
          self.show_updated_rates,
          self.handle_update_rates_error
      );
    };

    self.show_updated_rates = function() {
      for (var i = 0; i < arguments.length; i++) {
        // each product sends a separate rates request. We pull it out here.
        var product_rate_response = arguments[i][0];
        var product_rates = product_rate_response.data;
        self.coverage_vm.set_product_rates(product_rates)
       }
      //_.each(arguments, function(product_rate_response) {
      //  if (!product_rate_response.hasOwnProperty('data')) {
      //    // break
      //    return false;
      //  }
      //
      //  ;
      //  console.log(product_rates);
      //});

      //self.insurance_product.parse_benefit_options('employee', self.employee(), data.employee_rates);
      //self.insurance_product.parse_benefit_options('spouse', self.spouse(), data.spouse_rates);

      // Reset child rates
      //self.insurance_product.parse_benefit_options('children', self.child_benefits(), data.children_rates);

      //if (data.recommendations) {
      //  self.recommendations.good.set_recommendations(data.recommendations['good']);
      //  self.recommendations.better.set_recommendations(data.recommendations['better']);
      //  self.recommendations.best.set_recommendations(data.recommendations['best']);
      //}

      // Update selection with new data
      //if (self.selected_plan().is_valid()) {
      //  self.apply_selected_customization();
      //}

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



    self.should_include_spouse_in_table = ko.computed(function() {
      return self.should_show_spouse() && self.spouse().is_valid() ;
          // TODO: See if this needs to be here again
          //&& self.is_spouse_valid_for_product();
    });
    self.show_spouse_name = ko.computed(function() {
      return (self.should_include_spouse_in_table()) ? self.spouse().name() : "";
    });
    self.should_include_children_in_table = ko.computed(function() {
      return (self.should_include_children()
          && self.applicant_list.has_valid_children()
          && self.are_children_ages_valid()
      );
    });

    // Payment mode
    self.payment_modes = _.map(options.payment_modes || [], function(pm){
      return payment_mode.create_payment_mode(pm);
    });
    self.payment_mode = ko.observable(
        payment_mode.select_initial_payment_mode(options.case_data, self.payment_modes)
    );
    self.can_change_payment_mode = function() {
      return payment_mode.can_change_payment_mode(options.case_data);
    };
    self.is_payment_mode_valid = ko.pureComputed(function(){
      return (
          (!self.can_change_payment_mode())
          || (self.can_change_payment_mode() && self.payment_mode() !== null)
      );
    });

    // Recommendations table visibility
    self.has_show_rates_been_clicked = ko.observable(false);
    self.can_display_rates_table = ko.computed(function() {
      return (self.applicant_list.has_valid_employee()
              // TODO: Reimplement
              // && product validation
            && self.is_payment_mode_valid()
      );
    });

    // User indicates he is ready to show the coverage selection options.
    self.show_coverage_selection_table = function() {

      if (!self.can_display_rates_table()) {
        return;
      }

      var valid_form = true;

      // Trigger the jQuery validator. This test allows jasmine tests to work without DOM node present for form.
      if (self.validator) {
        valid_form = self.validator.form();
      }

      if (valid_form) {
        self.has_show_rates_been_clicked(true);
      }

      self.update_rate_table();
    };

    self.is_coverage_selection_visible = ko.pureComputed(function() {
      return (self.has_show_rates_been_clicked() && self.can_display_rates_table());
    });

    self.should_display_show_rates_button = ko.pureComputed(function() {
      return !self.is_coverage_selection_visible();
    });

    self.should_show_critical_illness_styling = function() {
      // insurance_product.has_critical_illness_coverages
      return false;
    };

    // Extended questions for step 1
    self.should_show_extended_questions = function() {
      return (
          any_product('requires_gender') ||
          any_product('requires_height') ||
          any_product('requires_weight') ||
          any_product('requires_is_smoker')
      );
    };
    var any_product = function(method_name) {
      return _.any(_.invoke(self.products, 'requires_gender'));
    };

    // validation helpers
    function min_emp_age() {
      return _.min(_.invoke(self.products, 'min_emp_age'));
    }
    function min_sp_age() {
      return _.min(_.invoke(self.products, 'min_sp_age'));
    }
    function min_ch_age() {
      return _.min(_.invoke(self.products, 'min_child_age'));
    }
    function max_emp_age() {
      return _.max(_.invoke(self.products, 'max_emp_age'));
    }
    function max_sp_age() {
      return _.max(_.invoke(self.products, 'max_sp_age'));
    }
    function max_ch_age() {
      return _.max(_.invoke(self.products, 'max_child_age'));
    }

    // Can the applicant globally decline coverage?
    self.can_decline = ko.computed(function() {
      // TODO: Reimplement
      /*
      // Can not decline if any applicant has applied for coverage already
      return (!_.any(self.applicant_list.applicants(), function(applicant) {
        return applicant.get_existing_coverage_amount_for_product(self.insurance_product.product_data.id);
      }));
      */
      return true;
    });

    // Globally decline coverage
    self.did_decline = ko.observable(false);

    // Did they select coverage?
    self.is_selection_error_visible = ko.observable(false);

    self.exit_application = function() {
      // TODO: Reimplement
    };


    // STEP 2 stuff
    self.warning_modal_title = ko.observable("");
    self.warning_modal_body = ko.observable("");
    self.show_warning_modal = function(title, body) {
      self.warning_modal_title(title);
      self.warning_modal_body(body);
      $("#warning_modal").modal('show');
    };


    function init_applicants() {
      var initial_applicant_list = _.map(options.applicants || [], function (applicant_data) {
        return wizard_applicant.create_applicant(applicant_data);
      });
      self.applicant_list = new wizard_applicant.ApplicantList(initial_applicant_list);

      // Applicant shortcuts
      self.employee = function() {
        return self.applicant_list.get_employee();
      };

      self.spouse = function() {
        return self.applicant_list.get_spouse();
      };

      self.children = ko.computed(function() {
        return self.applicant_list.get_children();
      });


      // Add up to two blank children to the form to start if fewer than two are provided.
      var num_initial_children = self.children().length;
      var last_name = self.employee().last() || "";
      if (num_initial_children < 2) {
        var num_blank_children_forms = (2 - num_initial_children);
        for (var i = 0; i < num_blank_children_forms; i += 1) {
          var ch = wizard_applicant.create_applicant({last: last_name, type: wizard_applicant.Applicant.ChildType});
          self.applicant_list.add_applicant(ch);
        }
      }

    }

    function init_jquery_validator() {
      // jquery form validator
      $.validator.addMethod("minAge", function(val, element, params) {
        var age = wizard_applicant.age_for_date(val);
        return (age !== "" && age >= params);
      }, "Must be at least {0} years old for this product");

      $.validator.addMethod("maxAge", function(val, element, params) {
        var age = wizard_applicant.age_for_date(val);
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
            minAge: min_emp_age(),
            maxAge: max_emp_age()
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
              param: min_sp_age(),
              depends: any_valid_spouse_field
            },
            maxAge: {
              param: max_sp_age(),
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
          }
          /*
          paymentMode: {
            required: true
          },

          debug: true*/

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
              param: min_ch_age(),
              depends: is_child_field_required
            },
            maxAge: {
              param: max_ch_age(),
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
    }

    // in validation.js, wizard validation
    init_validation(self);

  }


  return {WizardVM: WizardVM}
})();