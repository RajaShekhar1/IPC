var wizard_viewmodel = (function () {

  // Simple data-binding to initialize the UX Wizard widget.
  ko.bindingHandlers.wizard = {
    init: function (element, val_accessor) {
      var val = val_accessor();
      val($(element).ace_wizard());

      $(element).wizard('selectedItem', {step: 1});
    }
  };

  // Main Step 1 view model. Handles the coverage selection of each applicant for every
  //  product that is being offered.
  function CoverageVM(available_products, applicant_list, case_data, all_payment_modes,
                      should_include_spouse, should_include_children, root) {
    this.products = available_products;
    this.applicants = applicant_list;

    this.should_include_spouse = should_include_spouse;
    this.should_include_children = should_include_children;
    this.root = root;

    this.multiproduct_wizard_widget = ko.observable(null);

    this.applicants_in_table = ko.computed(function () {
      var applicants_in_table = [];
      if (this.applicants.has_valid_employee()) {
        applicants_in_table.push(this.applicants.get_employee());
      }
      if (this.applicants.has_valid_spouse() && this.should_include_spouse()) {
        applicants_in_table.push(this.applicants.get_spouse());
      }
      if (this.applicants.has_valid_children() && this.should_include_children()) {
        _.each(this.applicants.get_children(), function (c) {
          applicants_in_table.push(c);
        });
      }
      return applicants_in_table;
    }, this);

    // Payment mode
    this.payment_modes = _.map(all_payment_modes || [], function (pm) {
      return payment_mode_module.create_payment_mode(pm);
    });
    this.payment_mode = ko.observable(
      payment_mode_module.select_initial_payment_mode(case_data, this.payment_modes)
    );
    this.can_change_payment_mode = ko.computed(function () {
      return payment_mode_module.can_change_payment_mode(case_data);
    });
    this.is_payment_mode_valid = ko.pureComputed(this._is_payment_mode_valid, this);

    this.product_coverage_viewmodels = ko.observableArray(_.map(this.products, function (p) {
      return new ProductCoverageViewModel(
        p,
        case_data,
        applicant_list,
        this.payment_mode,
        this.should_include_spouse,
        this.should_include_children,
        this.root);
    }, this));

    // Which product coverage is being displayed right now?
    this.current_product = ko.observable(this.product_coverage_viewmodels()[0]);

    this.select_product = function (product_coverage) {
      this.current_product(product_coverage);
    }.bind(this);

    this.has_multiple_products = ko.pureComputed(function () {
      return this.product_coverage_viewmodels().length > 1;
    }, this);

    this.previous_product = ko.pureComputed(function () {
      var index = this.product_coverage_viewmodels().indexOf(this.current_product());
      return val_or_null(this.product_coverage_viewmodels(), index - 1);
    }, this);

    this.next_product = ko.pureComputed(function () {
      var index = this.product_coverage_viewmodels().indexOf(this.current_product());
      return val_or_null(this.product_coverage_viewmodels(), index + 1);
    }, this);

    function val_or_null(array, index) {
      if (index >= 0 && index < array.length) {
        return array[index];
      }
      return null;
    }

    this.coverage_summary_vm = new CoverageSummaryVM(this.product_coverage_viewmodels, this.applicants);

    var self = this;
    this.selected_product_coverages = ko.pureComputed(function () {
      return _.select(self.product_coverage_viewmodels(), function (prod_cov) {
        return prod_cov.did_select_coverage();
      });
    }, this);

    self.did_select_employee_coverage = ko.pureComputed(function () {
      return _.any(self.selected_product_coverages(), function (cov) {
        return cov.did_select_employee_coverage();
      });
    });

    self.did_select_spouse_coverage = ko.pureComputed(function () {
      return _.any(self.selected_product_coverages(), function (cov) {
        return cov.did_select_spouse_coverage();
      });
    });

    self.did_select_children_coverage = ko.pureComputed(function () {
      return _.any(self.selected_product_coverages(), function (cov) {
        return cov.did_select_children_coverage();
      });
    });

    self.get_covered_children = ko.pureComputed(function () {
      // FIXME: Should this return ApplicantCoverages instead?
      var children = [];
      _.each(self.selected_product_coverages(), function (cov) {
        _.each(cov.get_covered_children(), function (child) {
          if (_.includes(children, child)) {
            // return means continue here.
            return;
          }
          children.push(child);
        });
      });
      return children;
    });
  }

  CoverageVM.prototype = {
    _is_payment_mode_valid: function () {
      return (
        (!this.can_change_payment_mode())
        || (this.can_change_payment_mode() && this.payment_mode() != null)
      );
    },
    get_applicant_coverage_option_for_product: function (applicant, product) {
      if (applicant.type == wizard_applicant.Applicant.SpouseType && !this.should_include_spouse()) {
        return new NullCoverageOption();
      }
      if (applicant.type == wizard_applicant.Applicant.ChildType && !this.should_include_children()) {
        return new NullCoverageOption();
      }
      var applicant_coverage_selection = this.get_applicant_coverage_for_product(applicant, product);
      return applicant_coverage_selection.get_selected_coverage();
    },
    get_applicant_coverage_for_product: function (applicant, product) {
      var product_coverage = this.get_product_coverage(product);
      return product_coverage.get_coverage_for_applicant(applicant);
    },
    format_total_premium_for_product: function (product) {
      var product_coverage = this.get_product_coverage(product);
      var total = product_coverage.get_total_premium();
      return format_premium_value(total);
    },

    get_enabled_riders_for_product: function (product) {
      return this.get_product_coverage(product).enabled_riders;
    },

    format_total_premium_for_applicant: function (applicant) {
      var applicant_total = 0.0;
      _.each(this.product_coverage_viewmodels(), function (pcov) {
        if (pcov.did_decline()) {
          // continue
          return true;
        }
        var matching_applicant_coverage = _.find(pcov.valid_applicant_coverage_selections(), function (app_cov) {
          return app_cov.applicant === applicant;
        });
        if (!matching_applicant_coverage) {
          return format_premium_value(0.0);
        }

        applicant_total += matching_applicant_coverage.get_total_premium();
      });
      return format_premium_value(applicant_total);
    },

    format_grand_total_premium: function () {
      var grand_total = 0.0;
      _.each(this.product_coverage_viewmodels(), function (pcov) {
        if (pcov.did_decline()) {
          // continue
          return true;
        }
        grand_total += pcov.get_total_premium();
      });
      return format_premium_value(grand_total);
    },

    get_product_coverage: function (product) {
      return _.find(this.product_coverage_viewmodels(), function (pcov) {
        return pcov.product.product_data.id === product.product_data.id;
      });
    },

    go_to_next_product: function () {
      if (this.next_product()) {
        this.current_product(this.next_product());
      }
    },
    go_to_previous_product: function () {
      if (this.previous_product()) {
        this.current_product(this.previous_product());
      }

    }
  };


  function ProductCoverageViewModel(product, case_data, applicant_list, payment_mode,
                                    should_include_spouse, should_include_children, root) {

    // ProductCoverageViewModel keeps track of the coverage selections for the applicants for a single product.

    var self = this;

    self.product = product;
    self.case_data = case_data;
    self.payment_mode = payment_mode;
    self.applicant_list = applicant_list;
    self.should_include_spouse = should_include_spouse;
    self.should_include_children = should_include_children;
    self.root = root;

    self.did_decline = ko.observable(false);
    self.available_recommendations = product_rates_service.get_product_recommendations(self.product, payment_mode);
    self.selected_recommendation = ko.observable(null);

    // Cache the selections for applicants, instances should be instantiated just once.
    self._applicant_coverage_viewmodels = {};

    // What options do the applicants have to choose from?
    self.applicant_coverage_selections = ko.computed(this._get_applicant_coverage_selections, this);

    self.get_coverage_options_for_applicant = function (applicant) {
      return _.find(self.applicant_coverage_selections(), function (cov) {
        return cov.applicant.type === applicant.type;
      }).get_coverage_options();
    };

    self.has_completed_selection = ko.computed(this._has_completed_selection, this);

    // Has any applicant selected something? (even if 'no benefit')
    self.did_select_coverage = ko.pureComputed(function () {
      return self.has_completed_selection() && !self.did_decline();
    }, this);

    // Whether or not any valid benefit has been selected. (excludes "no benefit" selection)
    self.did_select_valid_coverage = ko.pureComputed(function () {
      return self.did_select_coverage() && (
          self.did_select_employee_coverage() ||
          self.did_select_spouse_coverage() ||
          self.did_select_children_coverage()
        );
    });

    self.did_select_employee_coverage = ko.pureComputed(function () {
      return _.any(self.valid_applicant_coverage_selections(), function (cov) {
        return cov.applicant.type == wizard_applicant.Applicant.EmployeeType && cov.coverage_option().is_valid();
      });
    });

    self.did_select_spouse_coverage = ko.pureComputed(function () {
      return _.any(self.valid_applicant_coverage_selections(), function (cov) {
        return cov.applicant.type == wizard_applicant.Applicant.SpouseType && cov.coverage_option().is_valid();
      });
    });

    self.did_select_children_coverage = ko.pureComputed(function () {
      return _.any(self.valid_applicant_coverage_selections(), function (cov) {
        return cov.applicant.type == wizard_applicant.Applicant.ChildType && cov.coverage_option().is_valid();
      });
    });

    self.get_covered_children = ko.computed(function () {
      return self._get_covered_children();
    });


    // Riders
    // Enabled riders for this case
    function is_rider_enabled_for_case(rider, case_data) {
      // See if there is a product-setting that enables this rider for this case.
      return _.any(case_data.product_settings.riders, function (r) {
        return (
          self.product.product_data.id === r.product_id &&
          rider.code === r.rider_code &&
          r.is_selected
        );
      });
    }

    self.enabled_riders = _.select(product.product_data.riders, function (r) {
      return is_rider_enabled_for_case(r, case_data);
    });

    // Currently shown rider (for modal pop-up explanations).
    self.shown_rider = ko.observable(null);

    // Build the rider view models.
    self.rider_options = [];
    _.each([self.applicant_list.get_employee(), self.applicant_list.get_spouse()], function (applicant) {
      _.each(self.enabled_riders, function (rider) {
        var vm = new ApplicantRiderOptionVM(self, rider, applicant);
        self.rider_options.push(vm);
      });
    });

    self.visible_rider_options = ko.computed(function () {
      return _.select(self.rider_options, function (option) {
        return option.is_visible()
      });
    });

    // Currently selected riders. Only Employee and spouse can have riders.
    self.selected_riders = ko.computed(function () {
      var selected_emp_riders = _.select(self.visible_rider_options(), function (option) {
        return option.applicant === self.applicant_list.get_employee() && option.is_selected();
      });

      var selected_sp_riders = _.select(self.visible_rider_options(), function (option) {
        return option.applicant === self.applicant_list.get_spouse() && option.is_selected();
      });

      return {
        emp: _.pluck(selected_emp_riders, 'rider'),
        sp: _.pluck(selected_sp_riders, 'rider')
      };
    });

    self.selected_riders.serialize_data = ko.computed(function () {
      return {
        emp: self.selected_riders().emp,
        sp: self.selected_riders().sp
      };
    });

    self.get_selected_riders = function (person) {
      if (person.applicant_type === "employee") {
        return self.selected_riders().emp;
      } else if (person.applicant_type === "spouse") {
        return self.selected_riders().sp;
      }
      return [];
    };

    // Policy owners
    self.policy_owner = ko.observable("self");
    self.other_owner_name = ko.observable("");
    self.other_owner_ssn = ko.observable("");

    self.spouse_policy_owner = ko.observable("employee");
    self.spouse_other_owner_name = ko.observable("");
    self.spouse_other_owner_ssn = ko.observable("");

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


    self.should_show_contingent_beneficiary = function () {
      return self.product.should_show_contingent_beneficiary();
    };

    self.has_contingent_beneficiary_error = ko.computed(function () {
      var employee_beneficiary_error = (
        self.employee_contingent_beneficiary_type() === 'spouse' &&
        self.employee_beneficiary_type() === 'spouse'
      );
      var spouse_beneficiary_error = (
        self.did_select_spouse_coverage() &&
        self.should_show_contingent_beneficiary() &&
        self.spouse_contingent_beneficiary_type() === 'spouse' &&
        self.spouse_beneficiary_type() === 'spouse'
      );
      return (
        self.should_show_contingent_beneficiary()
        && (
          employee_beneficiary_error
          || spouse_beneficiary_error
        )
      );
    });

    // Shortcut methods for the "Spouse-only" questions for FPP products.
    self.has_spouse_been_treated_6_months = ko.pureComputed(function () {
      return _find_answer_for_spouse_question_with_label("Spouse Treated 6 Months");
    });
    self.has_spouse_been_disabled_6_months = ko.pureComputed(function () {
      return _find_answer_for_spouse_question_with_label("Spouse Disabled 6 Months");
    });

    function _find_answer_for_spouse_question_with_label(lbl) {
      if (!self.did_select_spouse_coverage()) {
        return null;
      }
      var product_questions = self.root.get_product_health_questions(self);
      var spouse_question = _.find(product_questions.health_questions(), function (q) {
        return q.get_question_label() === lbl;
      });
      if (!spouse_question) {
        return null;
      }

      if (spouse_question.can_spouse_skip_due_to_GI()) {
        return "GI";
      }

      // Find the answer given
      var spouse_answer = product_questions.get_applicant_answer_for_question(self.root.spouse(), spouse_question);
      return spouse_answer.value();
    }

  }

  ProductCoverageViewModel.prototype = {
    format_product_name: function () {
      return this.product.product_data.name;
    },

    get_coverage_selection_template: function () {
      // in the future, products may not all use the recommendation system for coverage selection.
      if (this.product.does_use_recommended_coverage_table()) {
        return 'recommendation_coverage_selection';
      } else {
        // Simple coverage category selection.
        return 'category_coverage_selection';
      }

    },

    select_recommended_coverage: function (recommendation_set) {
      // Record the selected recommendation
      this.selected_recommendation(recommendation_set);

      // Apply the recommendation to each applicant
      _.each(this.valid_applicant_coverage_selections(), function (applicant_coverage) {
        applicant_coverage.select_recommended_coverage(recommendation_set);
      }, this);
    },

    _has_completed_selection: function () {
      // We want to know if we can advance to the next step, so all products have selections or were declined.
      if (this.did_decline()) {
        return true;
      }

      return _.all(this.valid_applicant_coverage_selections(), function (applicant_coverage) {
        return applicant_coverage.did_select_option();
      }, this);
    },

    _get_applicant_coverage_selections: function () {
      //
      // Return every valid ApplicantGroup that can get coverage for this product.
      var selections = [];

      if (this.applicant_list.has_valid_employee()) {
        selections.push(this.__get_coverage_for_applicant(this.applicant_list.get_employee()));
      }
      if (this.applicant_list.has_valid_spouse() && this.should_include_spouse()) {
        selections.push(this.__get_coverage_for_applicant(this.applicant_list.get_spouse()));
      }
      if (this.applicant_list.has_valid_children() && this.should_include_children()) {
        // TODO: insert check to see if this product groups children for coverage, otherwise add each child separately.
        selections.push(this.__get_coverage_for_applicant(this.applicant_list.get_children_group()));
      }

      return selections;
    },

    valid_applicant_coverage_selections: function () {
      return _.filter(this.applicant_coverage_selections(), function (acov) {
        var applicant = acov.applicant;
        return (
          (applicant.type === wizard_applicant.Applicant.EmployeeType && this.applicant_list.has_valid_employee()) ||
          (applicant.type === wizard_applicant.Applicant.SpouseType && (this.applicant_list.has_valid_spouse() && this.should_include_spouse())) ||
          (applicant.type === wizard_applicant.Applicant.ChildType && (this.applicant_list.has_valid_children() && this.should_include_children()))
        )
      }, this);
    },

    get_coverage_for_applicant: function (applicant) {
      return this.__get_coverage_for_applicant(applicant);
      //return _.find(this.valid_applicant_coverage_selections(), function(acov) {
      //  return acov.applicant === applicant;
      //})
    },

    // This raw method should not be used outside this class.
    __get_coverage_for_applicant: function (applicant) {
      // special case for group of children; if a child applicant is passed, use the group coverage
      if (applicant.type == wizard_applicant.Applicant.ChildType) {
        applicant = this.applicant_list.get_children_group();
      }

      var applicant_coverage;
      if (applicant._id in this._applicant_coverage_viewmodels) {
        applicant_coverage = this._applicant_coverage_viewmodels[applicant._id];
      } else {
        var new_selection_instance = new ApplicantCoverageSelectionVM(applicant, this);
        this._applicant_coverage_viewmodels[applicant._id] = new_selection_instance;
        applicant_coverage = new_selection_instance;
      }

      return applicant_coverage;
    },

    _get_covered_children: function () {
      var coverage = this.get_coverage_for_applicant(this.applicant_list.get_children_group());
      if (coverage.coverage_option().is_valid()) {
        // Return only valid children with coverage.
        return _.filter(this.applicant_list.get_children(), function (child) {
          return child.is_valid();
        });
      } else {
        return [];
      }
    },

    get_total_premium: function () {
      var total = 0.0;
      if (this.did_decline()) {
        return total;
      }
      _.each(this.applicant_coverage_selections(), function (applicant_coverage) {
        total += applicant_coverage.get_total_premium();
      }, this);
      return total;
    }
  };


  function ApplicantCoverageSelectionVM(applicant, product_coverage) {
    this.applicant = applicant;
    this.product_coverage = product_coverage;
    this.product = product_coverage.product;

    this.customized_coverage_option = ko.observable(null);
    this.recommended_coverage_option = ko.observable(null);

    this.coverage_option = ko.computed(function () {
      // Check to see if the applicant is currently valid.
      if (!this.applicant.is_valid()) {
        return new NullCoverageOption();
      } else if (this.applicant.type === wizard_applicant.Applicant.SpouseType && !this.product_coverage.root.should_include_spouse_in_table()) {
        return new NullCoverageOption();
      } else if (this.applicant.type === wizard_applicant.Applicant.ChildType && !this.product_coverage.root.should_include_children_in_table()) {
        return new NullCoverageOption();
      }

      var custom_option = this.customized_coverage_option();
      var recommended_option = this.recommended_coverage_option();

      if (custom_option) {
        return custom_option;
      } else if (recommended_option) {
        return recommended_option;
      } else {
        return new NullCoverageOption();
      }
    }, this);

    this.did_select_option = ko.pureComputed(function () {
      // Just see if a selection was made, even if the selection is NullCoverageOption.
      return (this.customized_coverage_option() !== null &&
          // The selection dropdown sets this to undefined directly
        this.customized_coverage_option() !== undefined) ||
        this.recommended_coverage_option() !== null;
    }, this);

    this.riders = ko.observableArray();
    this.beneficiaries = ko.observableArray([]);

    this.get_coverage_options = ko.pureComputed(this._get_coverage_options, this);
  }

  ApplicantCoverageSelectionVM.prototype = {
    select_recommended_coverage: function(recommendation_set) {
      var coverage_option = recommendation_set.get_recommended_applicant_coverage(this.applicant.type);
      if (coverage_option) {
        this.recommended_coverage_option(coverage_option);
        // Reset the custom option,  if any.
        this.customized_coverage_option(null);
      }
    },

    select_custom_coverage: function (option) {
      this.customized_coverage_option(option);
    },

    has_selected_coverage: function () {
      return this.coverage_option().is_valid();
    },

    get_selected_coverage: function () {
      return this.coverage_option();
    },

    get_cumulative_coverage_amount: function () {
      // Add together previous coverage application amounts and the current application amount.
      var previous_coverage_amount = this.applicant.get_existing_coverage_amount_for_product(this.product.product_data.id);
      var current_coverage_amount = (this.has_selected_coverage() ? this.coverage_option().face_value : 0);
      return previous_coverage_amount + current_coverage_amount;
    },

    format_name: function () {
      return this.applicant.name();
    },

    _get_coverage_options: function () {
      var options = [new NullCoverageOption()];
      options = $.merge(options,
        product_rates_service.get_product_coverage_options_for_applicant(this.product, this.applicant)()
      );
      return options;
    },

    format_recommendation_coverage: function (recommendation_set) {
      var coverage = recommendation_set.get_recommended_applicant_coverage(this.applicant.type);
      return coverage.format_face_value();
    },
    format_recommendation_premium: function (recommendation_set) {
      var recommendation = recommendation_set.get_applicant_recommendation(this.applicant.type);
      return recommendation.format_total_premium();
    },

    format_selected_coverage: function () {
      return this.get_selected_coverage().format_face_value();
    },
    format_selected_premium: function () {
      if (this.applicant.type == wizard_applicant.Applicant.ChildType &&
        this.product_coverage.product.is_fpp_product() &&
        this.has_selected_coverage()) {
        // Do the formatting here for single product enroll fpp children multiplier ... ugh.
        var premium = this.get_selected_coverage().get_total_premium() * window.vm.coverage_vm.applicants.get_valid_children().length;
        return format_premium_value(premium);
      }
      return this.get_selected_coverage().format_premium_option();
    },

    get_total_premium: function () {
      // If this is an FPP product, and the applicant is a children group, we multiply the selected premium by the number of children.
      if (this.applicant.type === wizard_applicant.Applicant.ChildType && this.product.is_fpp_product()) {
        return this.coverage_option().premium * this.applicant.applicants().length;
      }

      return this.coverage_option().premium;
    },

    get_previous_coverage_amount: function () {
      return this.applicant.get_existing_coverage_amount_for_product(this.product.product_data.id);
    }

  };


  function CoverageSummaryVM(product_coverages, applicant_list) {
    this.product_coverages = product_coverages;
    this.applicant_list = applicant_list;

    this.product_names = ko.pureComputed(function () {
      return _.map(this.product_coverages(), function (pcov) {
        return pcov.product.format_product_name();
      });
    }, this);

    this.valid_applicants = ko.pureComputed(function () {
      return this.applicant_list.get_valid_applicants_for_coverage();

    }, this);

    this.all_coverages = ko.computed(function () {
      var _coverages = [];
      _.each(this.product_coverages(), function (pcov) {
        _.each(pcov.valid_applicant_coverage_selections(), function (app_cov) {
          _coverages.push(app_cov);
        });
      });
      return _coverages;
    }, this);

  }


  // ViewModel for a rider option on the wizard.
  function ApplicantRiderOptionVM(root, rider, applicant) {
    var self = this;
    self.MAX_COVERAGE = 150000;

    self.root = root;
    self.rider = rider;
    self.applicant = applicant;

    self._user_selection = ko.observable(false);

    self.can_user_select = ko.computed(function () {
      // Right now, only AIR is selectable.
      return self.rider.code === 'AIR';
    });

    self.is_visible = ko.computed(function () {
      return self.root.get_coverage_for_applicant(self.applicant).has_selected_coverage();
    });

    self.is_selected = ko.computed({
      read: function () {
        if (self.can_user_select()) {
          return self._user_selection();
        } else {
          // If the rider is showing, it should be selected since it is a group-level rider.
          return self.is_visible();
        }
      },
      write: function (val) {
        self._user_selection(val);
      }
    });


    self.format_rider_name = function () {
      return self.rider.user_facing_name;
    };

    self.format_applicant_name = function () {
      return self.applicant.name();
    };

    self.has_rider_modal = function () {
      return (self.rider.code === "AIR" || self.rider.code === "QOL3" || self.rider.code === "QOL4" || self.rider.code === "WP");
    };

    self.show_rider_info = function () {
      self.root.shown_rider(self);
      switch (self.rider.code) {
        case "AIR":
          $("#modal-auto-increase-rider").modal('show');
          break;
        case "WP":
          $("#modal-wop-rider").modal('show');
          break;
        case "QOL3":
          $("#modal-qol3-rider").modal('show');
          break;
        case "QOL4":
          $("#modal-qol4-rider").modal('show');
          break;
      }
    };

    self.get_policy_years = function () {
      var policy_years = [];

      for (var year = 2; year <= 6; year++) {
        if (self.get_age_for_policy_year(year) <= 70
          && self.get_total_coverage_for_year(year) < self.MAX_COVERAGE) {
          policy_years.push(year);
        }
      }
      return policy_years;
    };

    self.get_age_for_policy_year = function (n) {
      return self.applicant.get_age() + n - 1;
    };

    self.format_coverage_for_year = function (n) {
      var coverage = self.get_coverage_for_year(n);
      return format_face_value(coverage);
    };

    self.format_total_coverage_for_year = function (n) {
      var coverage = self.get_total_coverage_for_year(n);
      return format_face_value(coverage);
    };

    self.format_coverage_for_policy = function () {
      var coverage = 0;
      var years = self.get_policy_years();
      for (i = 0; i < years.length; i++) {
        coverage += self.get_coverage_for_year(years[i]);
      }
      return format_face_value(coverage);
    };

    self.get_coverage_for_year = function (n) {
      var age = self.get_age_for_policy_year(n);
      return self.get_coverage_for_applicant_age(age);
    };

    self.get_total_coverage_for_year = function (n) {
      var additional_coverage = self.get_coverage_for_year(n);
      if (n == 2) {
        var selected_coverage = parseInt(self.root.get_coverage_for_applicant(self.applicant).coverage_option().face_value);
        return selected_coverage + additional_coverage;
      }

      var total_coverage_last_year = self.get_total_coverage_for_year(n - 1);
      return additional_coverage + total_coverage_last_year;
    };

    self.get_coverage_for_applicant_age = function (n) {
      var _coverage_values = {
        18: '15339',
        19: '15339',
        20: '15339',
        21: '15339',
        22: '15339',
        23: '15339',
        24: '15339',
        25: '15339',
        26: '15249',
        27: '14986',
        28: '14566',
        29: '13978',
        30: '13299',
        31: '12621',
        32: '11954',
        33: '11280',
        34: '10612',
        35: '9962',
        36: '9336',
        37: '8739',
        38: '8176',
        39: '7636',
        40: '7123',
        41: '6624',
        42: '6154',
        43: '5727',
        44: '5339',
        45: '4986',
        46: '4668',
        47: '4381',
        48: '4117',
        49: '3869',
        50: '3629',
        51: '3390',
        52: '3152',
        53: '2920',
        54: '2698',
        55: '2495',
        56: '2311',
        57: '2147',
        58: '2002',
        59: '1871',
        60: '1751',
        61: '1641',
        62: '1540',
        63: '1445',
        64: '1353',
        65: '1264',
        66: '1174',
        67: '1085',
        68: '999',
        69: '917',
        70: '839'
      };

      return parseInt(_coverage_values[n]);
    };

    self.format_premium = function () {
      if (self.rider.code === "AIR") {
        return "No initial premium charge";
      } else {
        // Right now, all other types of riders have premiums included in table above.
        return "Included";
      }
    };

  }


  function WizardVM(options) {
    var self = this;
    self.options = options;
    self.enrollment_case = options.case_data;
    self.products = wizard_products.build_products(self, options.products);

    self.is_rate_table_loading = product_rates_service.is_loading_rates;
    self.is_show_rates_clicked = ko.observable(false);

    init_applicants();

    init_jquery_validator();

    self.set_wizard_step = function () {
      var wizard = $('#enrollment-wizard').data('fu.wizard');
      wizard.currentStep = 1;
      wizard.setState();
    };

    // Step 1 ViewModel observables
    self.should_include_spouse_in_table = ko.computed(function () {
      return self.should_show_spouse() && self.spouse().is_valid();
    });

    self.should_include_children_in_table = ko.computed(function () {
      return (self.should_include_children()
        && self.applicant_list.has_valid_children()
      );
    });

    // Main step1 viewmodel - handles selecting coverage of products.
    self.coverage_vm = new CoverageVM(self.products, self.applicant_list, options.case_data, options.payment_modes,
      self.should_show_spouse, self.should_include_children, self);
    self.product_coverage_viewmodels = self.coverage_vm.product_coverage_viewmodels;

    // Add/remove children
    self.add_child = function () {
      var child = wizard_applicant.create_applicant(
        {
          last: self.employee().last(),
          type: wizard_applicant.Applicant.ChildType
        }
      );
      self.applicant_list.add_applicant(child);
    };
    self.remove_child = function (child) {
      self.applicant_list.remove_applicant(child);

      // Uncheck the show children switch if this is the last child.
      if (self.applicant_list.get_children().length === 0) {
        self.should_include_children(false);
      }
    };

    // callbacks for animation on children list changes
    self.rendered_child = function (element) {
      $(element).hide().slideDown(400);
    };
    self.removing_child = function (element) {
      $(element).slideUp(function () {
        $(element).remove();
      });
    };

    self.is_recommended_table_visible = ko.computed(function () {
      return (self.is_show_rates_clicked() && self.can_display_rates_table());
    });

    self.is_applicant_editor_visible = ko.pureComputed(function () {
      return !self.is_recommended_table_visible();
    });

    self.update_rate_table = function () {

      if (self.validator) {
        self.validator.resetForm();
      }

      self.is_show_rates_clicked(true);

      if (!self.is_recommended_table_visible()) {
        return;
      }
      self.refresh_rate_table();
    };

    self.refresh_rate_table = function () {

      product_rates_service.update_product_rates(
        self.products,
        self.coverage_vm.payment_mode(),
        self.applicant_list,
        self.handle_update_rates_error,
        self.enrollment_case.situs_state,
        self.coverage_vm
      );
    };

    self.handle_update_rates_error = function (resp) {
      if (resp.status === 400 && resp.responseJSON && resp.responseJSON.errors) {
        $.each(resp.responseJSON.errors, function () {
          var field_name = this.field;
          var message = this.error;
          // set error and show with validator
          self.limit_error_lookup[field_name](true);
          self.validator.form();
        });
      } else {
        handle_remote_error(resp, function () {
        });
      }
    };

    self.should_allow_grandchildren = ko.computed(function () {
      return !!self.products && self.products.length === 1 && _.some(self.products, function (product) {
          return product.is_fpp_product();
        });
    });

    self.show_spouse_name = ko.computed(function () {
      return (self.should_include_spouse_in_table()) ? self.spouse().name() : "";
    });


    // Recommendations table visibility
    self.has_show_rates_been_clicked = ko.observable(false);

    self.can_display_rates_table = ko.computed(function () {
      // TODO: Reimplement
      // && product validation
      var is_employee_valid = self.applicant_list.has_valid_employee();
      var is_payment_valid = self.coverage_vm.is_payment_mode_valid();
      var has_limit_errors = self.any_limit_error();
      return is_employee_valid && is_payment_valid && !has_limit_errors;
    });

    // User indicates he is ready to show the coverage selection options.
    self.show_coverage_selection_table = function () {

      // Reset some validation errors
      $.each(self.limit_error_lookup, function (k, v) {
        self.limit_error_lookup[k](null);
      });

      var valid_form = true;

      if (!self.can_display_rates_table()) {
        valid_form = false;
      }

      // Trigger the jQuery validator. This test allows jasmine tests to work without DOM node present for form.
      valid_form = self.validator.form() && valid_form;

      if (valid_form) {
        self.has_show_rates_been_clicked(true);
        self.update_rate_table();
      }

    };

    self.show_applicant_editor = function () {
      // Hide the rates table
      self.has_show_rates_been_clicked(false);
      self.is_show_rates_clicked(false);
    };

    self.is_coverage_selection_visible = ko.pureComputed(function () {
      return (self.has_show_rates_been_clicked() && self.can_display_rates_table());
    });

    self.should_display_show_rates_button = ko.pureComputed(function () {
      return !self.is_coverage_selection_visible();
    });

    self.should_show_critical_illness_styling = function () {
      // TODO: Move this out of main wizard.
      // insurance_product.has_critical_illness_coverages
      return false;
    };

    // Extended questions for step 1
    self.should_show_extended_questions = function () {
      return (
        any_product('requires_gender') ||
        any_product('requires_height') ||
        any_product('requires_weight') ||
        any_product('requires_is_smoker')
      );
    };

    self.should_show_gender = function () {
      return any_product('requires_gender');
    };
    self.should_show_height = function () {
      return any_product('requires_height');
    };
    self.should_show_weight = function () {
      return any_product('requires_weight');
    };
    self.should_show_smoker = function () {
      return any_product('requires_is_smoker');
    };

    var any_product = function (method_name) {
      return _.any(_.invoke(self.products, method_name));
    };
    var all_products = function (method_name) {
      return _.all(_.invoke(self.products, method_name));
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
    self.can_decline = ko.computed(function () {

      // Can not decline if any applicant has applied for coverage already
      return (!_.any(self.product_coverage_viewmodels(), function (product_coverage) {
        return _.any(self.applicant_list.applicants(), function (applicant) {
          // Does the applicant have existing coverage for this product?
          return _.any(applicant.existing_coverages, function (existing_coverage) {
            return existing_coverage.product_id === product_coverage.product.id
              && existing_coverage.coverage_status === 'enrolled';
          });
        });
      }));

    });

    // Can we submit step 6
    self.is_submitting = ko.observable(false);
    self.submission_error = ko.observable("");

    self.can_submit_wizard = ko.pureComputed(function () {
      return !self.is_submitting();
    });

    // Globally decline coverage.
    self.did_decline = ko.observable(false);

    self.did_decline_all_products = ko.pureComputed(function () {
      return self.did_decline() || _.all(self.product_coverage_viewmodels(), function (pcov) {
          return pcov.did_decline();
        });
    });

    // Decline info box
    self.did_decline.subscribe(function (val) {
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

    self.exit_application = function () {
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
              window.location.href = "/enrollment-case/" + self.options.case_data.id + "#enrollment";
            }
          }
        }
      });
    };

    // Did they select coverage?
    self.attempted_advance_step = ko.observable(false);
    self.is_selection_error_visible = ko.computed(function () {
      return self.attempted_advance_step() && !self.is_coverage_selection_valid();
    });
    self.show_no_selection_error = function () {
      self.attempted_advance_step(true);
    };

    // Just for step 1 ...
    self.is_coverage_selection_valid = function () {
      return _.all(self.coverage_vm.product_coverage_viewmodels(), function (prod_cov) {
        return prod_cov.did_select_valid_coverage() || prod_cov.did_decline();
      });
    };

    // STEP 2 data and methods.
    self.existing_insurance = ko.observable(null);
    self.replacing_insurance = ko.observable(null);
    self.is_employee_actively_at_work = ko.observable(null);

    // Todo - Move to health_questions.js, and cache the ProductHealthQuestions objects if necessary.
    self.selected_product_health_questions = ko.computed(function () {
      return _.map(self.coverage_vm.selected_product_coverages(), function (product_cov) {
        return new health_questions.ProductHealthQuestions(
          product_cov,
          options.spouse_questions,
          options.health_questions
        )
      });
    });

    self.get_product_health_questions = function (product_coverage) {
      return _.find(self.selected_product_health_questions(), function (phq) {
        return phq.product_coverage === product_coverage;
      });
    };

    self.should_show_other_insurance_questions = ko.pureComputed(function () {
      // If any selected product requires this
      return _.any(self.coverage_vm.selected_product_coverages(), function (product_cov) {
        return product_cov.product.should_show_other_insurance_questions();
      });
    });

    self.get_replacement_paragraphs = function () {
      var paragraph_map = {};
      _.each(self.coverage_vm.selected_product_coverages(), function (prod_cov) {
        var paragraphs = prod_cov.product.get_replacement_paragraphs();
        // TODO: This overwrites the state -> paragraph mapping if there are multiple products with
        //   replacement paragraphs.
        $.extend(paragraph_map, paragraphs);
      });
      var paragraphs = paragraph_map[self.enrollment_case.situs_state];
      if (!paragraphs) {
        return [];
      }
      return paragraphs;
    };

    self.did_select_any_fpp_product = ko.pureComputed(function () {
      return _.find(self.coverage_vm.selected_product_coverages(), function (ps) {
        return ps.product.is_fpp_product();
      });
    });

    self.NAIC_AND_MI = ['AK', 'AL', 'AR', 'AZ', 'CO', 'IA', 'LA', 'MD', 'ME', 'MS', 'MT',
      'NC', 'NE', 'NH', 'NJ', 'NM', 'OH', 'OR', 'RI', 'SC', 'TX', 'UT', 'VA', 'VT',
      'WI', 'WV', 'MI'];

    self.is_NAIC_OR_MI = function () {
      return self.did_select_any_fpp_product() && _.contains(self.NAIC_AND_MI, self.enrollment_case.situs_state);
    };

    self.is_KY_OR_KS = function () {
      return self.enrollment_case.situs_state == "KY" || self.enrollment_case.situs_state == "KS";
    };

    self.is_CT_DC_ND_VI = function () {
      return _.contains(['CT', 'DC', 'ND', 'VI'], self.enrollment_case.situs_state);
    };

    self.is_non_NAIC_other = function () {
      return !(self.is_NAIC_OR_MI() || self.is_KY_OR_KS() || self.is_CT_DC_ND_VI());
    };

    self.is_in_person_application = function () {
      return 'is_in_person' in options && options.is_in_person
    };
    self.is_self_enroll = function () {
      return !self.is_in_person_application()
    };
    self.get_has_existing_question_highlight = function () {
      if (self.is_self_enroll()) {
        return 'flag';
      }

      return "checkmark";
    };
    self.get_replacing_question_highlight = function () {
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
    self.show_warning_modal = function (title, body) {
      self.warning_modal_title(title);
      self.warning_modal_body(body);
      $("#warning_modal").modal('show');
    };
    var default_warning_body = 'STOP: A "yes" response to this question disqualifies you from completing this application in your state.';

    self.select_has_existing_insurance = function () {
      self.existing_insurance(true);
    };

    self.select_replacing_insurance = function () {
      self.replacing_insurance(true);

      if (!self.did_select_any_fpp_product() || self.is_KY_OR_KS()) {
        self.show_warning_modal("Replacement Notice", default_warning_body);
      }
    };
    self.should_show_NAIC_replacement_notice = ko.computed(function () {
      // Show special notice if we are in this category and we say 'No' to the replacement question.
      return (self.is_NAIC_OR_MI() && self.replacing_insurance() === false);
    });


    self.should_show_replacement_form = ko.computed(function () {
      if (!self.did_select_any_fpp_product()) {
        return false;
      }

      if (self.is_KY_OR_KS() || self.is_CT_DC_ND_VI()) {
        return false;
      }

      // Self-enroll is the only way that existing insurance does anything when yes.
      if (self.is_self_enroll() && self.existing_insurance()) {
        return true;
      }

      if (self.should_show_NAIC_replacement_notice()) {
        return false;
      }

      return self.replacing_insurance();
    });

    self.replacement_read_aloud = ko.observable(false);
    self.replacement_is_terminating = ko.observable(null);
    self.replacement_using_funds = ko.observable(null);

    // Watch for self-enroll situation that stops the user from continuing.
    var self_enroll_stop_message = "This response requires you to speak with your insurance representative directly, and prevents you from continuing enrollment at this time. You may cancel this enrollment session and contact your representative.";
    self.replacement_is_terminating.subscribe(function () {
      if (self.is_self_enroll() && self.replacement_is_terminating()) {
        self.show_warning_modal("Replacement Notice", self_enroll_stop_message);
      }
    });
    self.replacement_using_funds.subscribe(function () {
      if (self.is_self_enroll() && self.replacement_using_funds()) {
        self.show_warning_modal("Replacement Notice", self_enroll_stop_message);
      }
    });

    // Policy details form
    self.should_show_replacement_details_form = ko.pureComputed(function () {
      if (self.is_self_enroll()) {
        // Not allowed to do this for self-enroll.
        return false;
      }
      return self.replacement_using_funds() || self.replacement_is_terminating();
    });

    self.replacement_policies = ko.observableArray([new ReplacementPolicy()]);

    self.add_replacement_policy = function () {
      self.replacement_policies.push(new ReplacementPolicy());
    };

    self.remove_replacement_policy = function (policy) {
      self.replacement_policies.remove(policy);
    };

    self.is_replacement_form_required = ko.computed(function () {
      return (
        self.did_select_any_fpp_product() &&
        (self.existing_insurance() || self.replacing_insurance())
      );
    });

    // STEP 3 Data - Other Employee Info
    self.company_name = ko.observable(self.enrollment_case.company_name);


    // Step 4 Data - Other Dependant info
    // Spouse address
    self.is_spouse_address_same_as_employee = ko.observable(!self.spouse().address1() || (
        self.employee().address1() === self.spouse().address1() &&
        self.employee().address2() === self.spouse().address2() &&
        self.employee().city() === self.spouse().city() &&
        self.employee().state() === self.spouse().state() &&
        self.employee().zip() === self.spouse().zip()
      ));
    self.is_spouse_email_same_as_employee = ko.observable((!self.spouse().email() || self.employee().email() === self.spouse().email()));


    // Step 5 - Beneficiaries
    self.should_show_step_5 = function () {
      return _.any(self.coverage_vm.selected_product_coverages(), function (pcov) {
        return pcov.product.should_show_step_5();
      })
    };
    self.should_show_contingent_beneficiary = function () {
      return _.any(_.invoke(self.products, "should_show_contingent_beneficiary"));
    };
    self.has_contingent_beneficiary_error = ko.computed(function () {
      return _.any(self.coverage_vm.selected_product_coverages(), function (prod_cov) {
        return prod_cov.has_contingent_beneficiary_error();
      });
    });


    // Step 6 Data
    self.disclaimer_notice_confirmed = ko.observable(false);
    self.payroll_deductions_confirmed = ko.observable(false);
    self.identityToken = ko.observable("");
    self.identityType = ko.observable("");
    self.enrollState = ko.observable(self.enrollment_case.situs_state);
    self.enrollCity = ko.observable(self.enrollment_case.situs_city);
    self.should_confirm_disclosure_notice = ko.pureComputed(function () {
      return any_selected_product('should_confirm_disclosure_notice');
    });
    self.should_confirm_payroll_deduction = function () {
      return any_selected_product('should_confirm_payroll_deduction');
    };
    self.should_use_date_of_hire_for_identity = function () {
      // TODO: double check this.
      return true;
    };

    function any_selected_product(method) {
      var selected_products = _.pluck(self.coverage_vm.selected_product_coverages(), 'product');
      return _.any(_.invoke(selected_products, method));
    }

    function init_applicants() {
      var initial_applicant_list = _.map(options.applicants || [], function (applicant_data) {
        return wizard_applicant.create_applicant(applicant_data);
      });

      self.should_show_spouse = ko.observable(false);
      self.should_include_children = ko.observable(false);

      self.applicant_list = new wizard_applicant.ApplicantList(initial_applicant_list,
        self.should_show_spouse, self.should_include_children);

      // Applicant shortcuts
      self.employee = function () {
        return self.applicant_list.get_employee();
      };

      self.spouse = function () {
        return self.applicant_list.get_spouse();
      };

      self.children = ko.computed(function () {
        return self.applicant_list.get_children();
      });


      var should_show_spouse_initially = self.spouse().any_valid_field();
      self.should_show_spouse(should_show_spouse_initially);

      var should_show_children_initially = self.applicant_list.has_valid_children();
      self.should_include_children(should_show_children_initially);

      // Add up to two blank children to the form to start if none are provided.
      var num_initial_children = self.children().length;
      var last_name = self.employee().last() || "";
      if (num_initial_children == 0) {
        var num_blank_children_forms = (2 - num_initial_children);
        for (var i = 0; i < num_blank_children_forms; i += 1) {
          var ch = wizard_applicant.create_applicant({last: last_name, type: wizard_applicant.Applicant.ChildType});
          self.applicant_list.add_applicant(ch);
        }
      }

    }

    function init_jquery_validator() {
      // jquery form validator
      $.validator.addMethod("minAge", function (val, element, params) {
        var age = wizard_applicant.age_for_date(val);
        return (age !== "" && age >= params);
      }, "Must be at least {0} years old for this product");

      $.validator.addMethod("maxAge", function (val, element, params) {
        var age = wizard_applicant.age_for_date(val);
        return (age !== "" && age <= params);
      }, "Must be no more than {0} years old for this product");


      // Height and Weight limits for Group CI
      self.limit_error_lookup = {
        employee_height: self.employee().height_error,
        employee_weight: self.employee().weight_error,
        spouse_height: self.spouse().height_error,
        spouse_weight: self.spouse().weight_error
      };

      self.any_limit_error = ko.pureComputed(function () {
        return self.employee().height_error() ||
          self.employee().weight_error() ||
          self.spouse().height_error() ||
          self.spouse().weight_error();
      });

      $.validator.addMethod("empHeightLimit", function (val, el, params) {
        return self.employee().height_error() == null;
      }, "The height or weight entered is outside the limits for this product.");
      $.validator.addMethod("empWeightLimit", function (val, el, params) {
        return self.employee().weight_error() == null;
      }, "The height or weight entered is outside the limits for this product.");
      $.validator.addMethod("spHeightLimit", function (val, el, params) {
        return self.spouse().height_error() == null;
      }, "The height or weight entered is outside the limits for this product.");
      $.validator.addMethod("spWeightLimit", function (val, el, params) {
        return self.spouse().weight_error() == null;
      }, "The height or weight entered is outside the limits for this product.");

      $.validator.addMethod("isValidPaymentMode", function (value, element) {
        // Use this custom method because our payment mode selector options have objects as values which
        //  confuses jquery validate.
        return self.coverage_vm.is_payment_mode_valid();
      }, "You must select a payment mode.");

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
            required: {depends: any_valid_spouse_field}
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
          'tobacco-1': {required: {depends: any_valid_spouse_field}},
          'gender-1': {required: {depends: any_valid_spouse_field}},
          'weight_0': {
            required: true,
            empWeightLimit: true
          },
          'weight_1': {
            required: {depends: any_valid_spouse_field},
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
            required: {depends: any_valid_spouse_field},
            spHeightLimit: true
          },
          'height_inches_1': {
            required: {depends: any_valid_spouse_field},
            spHeightLimit: true
          },

          paymentMode: {
            isValidPaymentMode: {depends: self.can_change_payment_mode}
          }

          /*debug: true*/

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
        // Treat the first child as always required if the children checkbox is checked
        if ($(element).attr("id") === "child-first-0" ||
          $(element).attr("id") === "child-last-0" ||
          $(element).attr("id") === "child-dob-0"
        ) {
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

      $.validator.addClassRules("child_first", {
          required: {
            depends: is_child_name_required
          }
        }
      );
      $.validator.addClassRules("child_last", {
          required: {
            depends: is_child_name_required
          }
        }
      );
    }

    // in validation.js, wizard validation
    init_validation(self);

  }


// TODO:  Will need to expose this function to the other modules, integrate Barrett's reauth mechanism below
  function handle_remote_error(request, retry_callback) {
    if (request.status == 401) {
      if (ui.account_href != null) {
        prompt_login(retry_callback);
      } else {
        // The user wasn't logged in, so just restart our session
        login_reauth(null, null, retry_callback);
      }
    }
    else {
      alert("Sorry, an error occurred communicating with the server.");
    }
  }

  function prompt_login(callback) {
    bootbox.confirm({
      message: "Please type your password to login again: <input id='password' class='form-control' placeholder='Password' type='password'/>",
      title: "Session Timed Out, Please Re-Login:",
      buttons: {
        "cancel": {"label": "Cancel"},
        "confirm": {
          "label": "Login",
          "className": "width-35 pull-right btn btn-primary"
        }
      },
      callback: function (result) {
        if (result == true) {
          login_reauth(ui.account_href, $('#password').val(), callback);
        }
      }
    });
  }

  function login_reauth(account_href, password, callback) {
    var post_data = {
      "account_href": account_href,
      "password": password,
      "success_message": "You were re-authenticated successfully.  Please continue with the application.",
      "session_data": {
        "is_self_enroll": ui.is_self_enroll(),
        "active_case_id": ui.case_id,
        "enrolling_census_record_id": ui.record_id
      }
    };

    ajax_post('/reauth', post_data, function (reauth_response) {
      bootbox.alert(reauth_response.message);
      if (callback) {
        callback(true);
      }
    }, function () {
      bootbox.alert("There was a problem reauthenticating. Please try again.");
      if (callback) {
        callback(false);
      }
    }, true);
  }

  function ajax_post(url, data, on_success, on_error, is_json) {
    var options = {
      data: data,
      error: on_error,
      success: on_success,
      // expected return data type
      dataType: "json",
      method: "POST"
    };
    if (is_json === true) {
      options.contentType = "application/json; charset=utf-8";
      options.processData = false;
      options.data = JSON.stringify(data);
    }
    $.ajax(url, options);
  }


  return {WizardVM: WizardVM};
})
();