
// IE8 and below polyfill for object.create
if (typeof Object.create != 'function') {
    (function () {
        var F = function () {};
        Object.create = function (o) {
            if (arguments.length > 1) {
              throw Error('Second argument not supported');
            }
            if (o === null) {
              throw Error('Cannot set a null [[Prototype]]');
            }
            if (typeof o != 'object') {
              throw new TypeError('Argument must be an object');
            }
            F.prototype = o;
            return new F();
        };
    })();
}

function init_rate_form(data, rider_data) {
    data.riders = rider_data;

    var ui = new WizardUI(data);

    // Allow other JS functions to access the ui object
    window.ui = ui;

    ko.applyBindings(ui);
}

function build_product(root, products) {

    if (products.length == 0) {
        alert("Error: No products to enroll.");
        return null;
    }

    // use the first product until multi-product
    var product_data = products[0];

    var base_type = product_data.base_product_type;
    var is_fpp_gov = product_data.is_fpp_gov;
    var base_product;
    if (base_type == "FPPTI") {
        base_product = new FPPTIProduct(product_data);
    } else if (base_type == "FPPCI") {
        base_product = new FPPCIProduct(product_data);
    } else if (base_type == "Group CI") {
        base_product = new GroupCIProduct(root, product_data);
    } else if (is_fpp_gov) {
        base_product = new FPPGovProduct(product_data, base_type);
    } else {
        // default product?
        alert("Invalid product type '"+base_type+"'");
        base_product = new FPPTIProduct(product_data);
    }

    // Check if this is a Guaranteed Issue product
    if (product_data.is_guaranteed_issue) {
        return new GIProductDecorator(base_product, product_data);
    } else {
        return base_product;
    }
}

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
    self.employee_other_beneficiary = ko.observable(new Beneficiary());
    self.employee_contingent_beneficiary_type = ko.observable("none");
    self.employee_contingent_beneficiary = ko.observable(new Beneficiary());

    self.spouse_beneficiary_type = ko.observable("spouse");
    self.spouse_other_beneficiary = ko.observable(new Beneficiary());
    self.spouse_contingent_beneficiary_type = ko.observable("none");
    self.spouse_contingent_beneficiary = ko.observable(new Beneficiary());

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


    // Available riders not tied to applicant.
    self.all_riders = ko.observableArray(defaults.riders);

	// Currently shown rider
    self.shown_rider = ko.observable(null);

    // ViewModel for a rider option on the wizard.
    function ApplicantRiderOptionVM(root, rider, applicant) {
        var self = this;
        self.root = root;
        self.rider = rider;
        self.applicant = applicant;

        self.is_selected = ko.observable(false);

        self.is_visible = ko.computed(function() {
            return self.applicant.has_selected_valid_coverage();
        });

        self.format_rider_name = function() {
            return self.rider.name;
        };

        self.format_applicant_name = function() {
            return self.applicant.name();
        };

        self.has_rider_modal = function() {
            return (self.rider.code === "AIR");
        };

        self.show_rider_info = function() {
			self.root.shown_rider(self);
			$("#modal-auto-increase-rider").modal('show');
        };

		self.get_policy_years = function() {
			var policy_years = [];
			for (year = 2; year <= 6; year++) {
				if (self.get_age_for_policy_year(year) < 70) {
					policy_years.push(year);
				}
			}
			return policy_years;
		};

		self.get_age_for_policy_year = function(n) {
			return self.applicant.get_age() + n - 1;
		}

		self.format_coverage_for_year = function(n) {
			var coverage = self.get_coverage_for_year(n);
			return format_face_value(coverage);
		}

		self.format_total_coverage_for_year = function(n) {
			var coverage = self.get_total_coverage_for_year(n);
			return format_face_value(coverage);
		}

		self.format_coverage_for_policy = function() {
			var coverage = 0;
            var years = self.get_policy_years();
			for (i = 0; i < years.length; i++) {
				coverage += self.get_coverage_for_year(years[i]);
			}
			return format_face_value(coverage);
		}

		self.get_coverage_for_year = function(n) {
			var age = self.get_age_for_policy_year(n);
			return self.get_coverage_for_applicant_age(age);
		}

		self.get_total_coverage_for_year = function(n) {
			var additional_coverage = self.get_coverage_for_year(n);
			if (n == 2) {
				var selected_coverage = self.applicant.selected_coverage().face_value;
				return selected_coverage + additional_coverage;
			}

			var total_coverage_last_year = self.get_total_coverage_for_year(n - 1);
			return additional_coverage + total_coverage_last_year;
		}

		self.get_coverage_for_applicant_age = function(n) {
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
		}

        self.format_premium = function() {
            if (self.rider.code === "AIR") {
                return "No initial premium charge";
            } else {
                // TODO: lookup rider rate in table.
                return "$TODO";
            }
        };

    }

    // Build the rider viewmodels
    self.rider_options = [];
    _.each([self.employee(), self.spouse()], function(applicant) {
        _.each(self.all_riders(), function(rider) {
            var vm = new ApplicantRiderOptionVM(self, rider, applicant);
            self.rider_options.push(vm);
        });
    });

    self.visible_rider_options = ko.computed(function() {
        return _.select(self.rider_options, function(option) {return option.is_visible()});
    });

    // Currently selected riders. Only Employee and spouse can have riders.
    self.selected_riders = ko.computed(function() {
        var selected_emp_riders = _.select(self.visible_rider_options(), function(option) {
           return option.applicant === self.employee() && option.is_selected();
        });

        var selected_sp_riders = _.select(self.visible_rider_options(), function(option) {
           return option.applicant === self.spouse() && option.is_selected() ;
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

    self.get_selected_riders = function(person) {
      if (person.applicant_type === "employee") {
        return self.selected_riders().emp;
      } else if (person.applicant_type === "spouse") {
        return self.selected_riders().sp;
      } 
      return [];
    };


    // Rider rates are filled in when the rate lookup is performed for product rates lookup.
    // Note: not used as of Oct 1st since group riders are not enabled yet.
    self.rider_rates = ko.observable({
      emp: [],
      sp: []
    });


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

        self.rider_rates({
          emp: data.emp_rider_rates,
          sp: data.sp_rider_rates
        });

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
    self.did_person_select_coverage = function(person) {
      if(person.applicant_type) {
       person = person.applicant_type;
      }
      if(person=="employee") {
        return self.did_select_employee_coverage();
      }
      if(person=="spouse") {
        return self.did_select_spouse_coverage();
      }
      return false;
    }



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


function process_health_question_data(root, health_question_data_by_product, product_data) {
    // Build up the master list of health questions for the product
    var questions = [];

    _.each(health_question_data_by_product, function (product_health_questions, product_id) {
        if (product_id == product_data.id) {
            var question_factory;
            if (product_data.is_guaranteed_issue) {
                question_factory = function (question_data) {
                    return new GIHealthQuestion(root.insurance_product, question_data, root.selected_plan,
                        product_data.gi_criteria,
                        product_data.statement_of_health_bypass_type, product_data.bypassed_soh_questions);
                };
            } else if (root.insurance_product.product_type == "Group CI") {
                question_factory = function (question_data) {
                    return new GIHealthQuestion(root.insurance_product, question_data, root.selected_plan,

                        [
                            // Employees must answer if >= 10000 coverage
                            {
                                guarantee_issue_amount: 10000,
                                applicant_type: 'Employee',
                                age_max: null,
                                age_min: null,
                                weight_min: null,
                                weight_max: null,
                                height_min: null,
                                height_max: null
                            },
                            // Spouse always has to answer, so don't put criteria in for spouse.
                            {
                                // Children never have to answer, make the max GI amount bigger to get this effect.
                                guarantee_issue_amount: 25000,
                                applicant_type: 'Child',
                                age_max: null,
                                age_min: null,
                                weight_min: null,
                                weight_max: null,
                                height_min: null,
                                height_max: null
                            }
                        ],
                        // Skip over these questions (all but first two )
                        "selected",
                        [
                            {question_type_label: "5yr Heart"},
                            {question_type_label: "5yr Hypertension / Cholesterol"},
                            {question_type_label: "5yr Lung / Colon"},
                            {question_type_label: "5yr Skin Cancer"},
                            {question_type_label: "5yr HPV/HSV"},
                            {question_type_label: "Abnormal Results"},
                            {question_type_label: "Ever been rejected"}
                        ]
                    );
                }
            } else {
                question_factory = function (question_data) {
                    return new StandardHealthQuestion(question_data, root.selected_plan);
                }
            }

            questions = _.map(product_health_questions, question_factory);
        }
    });
    return questions;
}

function process_spouse_question_data(root, question_data_by_product, product_data) {
    // Build up the master list of questions for the product
    var questions = [];

    _.each(question_data_by_product, function (product_questions, product_id) {
        if (product_id == product_data.id) {
            var question_factory;
            if (product_data.is_guaranteed_issue) {
                question_factory = function (question_data) {
                    return new GIHealthQuestion(root.insurance_product, question_data, root.selected_plan,
                        product_data.gi_criteria,
                        product_data.statement_of_health_bypass_type, product_data.bypassed_soh_questions);
                };
            } else {
                question_factory = function (question_data) {
                    return new StandardHealthQuestion(question_data, root.selected_plan);
                }
            }

            questions = _.map(product_questions, question_factory);
        }
    });
    return questions;
}


// Model for different insurance products
// Product is abstract base class
function Product() {
}
Product.prototype = {

    // Override if necessary

    min_emp_age: function() {return 18},
    max_emp_age: function() {return 70},

    min_sp_age: function() {return 18},
    max_sp_age: function() {return 70},

    min_child_age: function() {return 0},
    max_child_age: function() {return 23},

    is_valid_employee: function(employee) {
        // Only age matters for most products
        var age = employee.get_age();
        return (age >= this.min_emp_age() && age <= this.max_emp_age());
    },

    is_valid_spouse: function(spouse) {
        var age = spouse.get_age();
        return (age >= this.min_sp_age() && age <= this.max_sp_age());
    },

    is_valid_child: function(child) {
        var age = child.get_age();
        return (age >= this.min_child_age() && age <= this.max_child_age());
    },

    // Allow the details of the benefit's face value, display to be based on the product
    get_new_benefit_option: function(options) {
        return new BenefitOption(options);
    },

    requires_gender: function() {return false;},
    requires_height: function() {return false;},
    requires_weight: function() {return false;},
    requires_is_smoker: function() {return false;},

    // SOH questions
    has_critical_illness_coverages: function() {return false;},

    all_coverage_options: {
        employee: ko.observableArray([]),
        spouse: ko.observableArray([]),
        children: ko.observableArray([])
    },

    find_recommended_coverage_benefit: function(applicant_type, desired_face_value) {
        var benefit = new NullBenefitOption({});
        $.each(this.all_coverage_options[applicant_type](), function() {
            if (this.face_value == desired_face_value) {
                benefit = this;
                return false;
            }
        });
        return benefit;
    },


    get_coverage_options_for_applicant: function(applicant_type) {
        // returns an observable
        return this.all_coverage_options[applicant_type];
    },

    get_maximum_coverage_amount: function(applicant_type) {
        var options = this.get_coverage_options_for_applicant(applicant_type)();
        if (options.length > 0) {
            return _.max(_.pluck(options, "face_value"));
        }
        return 0;
    },

    parse_benefit_options: function(applicant_type, applicant, rates) {
        var self = this;
        var all_options = [new NullBenefitOption()];

        if (rates.bypremium) {
            var by_premium_options = $.map(rates.bypremium, function(rate) {
                return self.get_new_benefit_option({
                    is_by_face: false,
                    face_value: rate.coverage,
                    premium: rate.premium
                });
            });
            // Extends an array with another array
            $.merge(all_options, by_premium_options);
        }

        if (rates.byface) {
            var by_face_options = $.map(rates.byface, function(rate) {
                return self.get_new_benefit_option({
                    is_by_face: true,
                    face_value: rate.coverage,
                    premium: rate.premium
                });
            });
            // Extends an array with another array
            $.merge(all_options, by_face_options);
        }

        self.all_coverage_options[applicant_type](all_options);
    },

    should_show_step_5: function() {
        return true;
    },

    should_use_date_of_hire_for_identity: function() {
        // Right now all FPP products will use this.
        return true;
    },

    is_fpp_product: function() {
        // Returns true if this product falls into the class of Family Protection Plan products
        return true;
    },

    should_confirm_disclosure_notice: function() {
        // Accelerated benefit disclosure notice checkbox is for FPP plans.
        return this.is_fpp_product();
    },

    should_confirm_payroll_deduction: function() {
        // Payroll deduction agree checkbox on new FPP form.
        return this.is_fpp_product();
    },

    should_show_contingent_beneficiary: function() {
        // Just new FPP form for now
        return this.is_fpp_product();
    },

    get_replacement_paragraphs: function() {
        return [];
    }

};

function FPPTIProduct(product_data) {
    this.product_type = "FPPTI";
    this.product_data = product_data;
}
// Inherit from product
FPPTIProduct.prototype = Object.create(Product.prototype);

FPPTIProduct.prototype.get_replacement_paragraphs = function() {
    return this.product_data.replacement_paragraphs;
};

function FPPCIProduct(product_data) {
    this.product_type = "FPPCI";
    this.product_data = product_data;
}
// Inherit from product
FPPCIProduct.prototype = Object.create(Product.prototype);
FPPCIProduct.prototype.get_new_benefit_option = function(options) {
    return new CIBenefitOption(new BenefitOption(options));
};
FPPCIProduct.prototype.has_critical_illness_coverages = function() {
    return true;
};
FPPCIProduct.prototype.get_replacement_paragraphs = function() {
    return this.product_data.replacement_paragraphs;
};


function GroupCIProduct(root, product_data) {
    var self = this;
    self.root = root;
    self.product_type = "Group CI";
    self.product_data = product_data;

    // Set up the coverage options for Group CI; they can change as options are selected
    self.employee_current_benefit_subscription = null;

    self.spouse_options_for_demographics = ko.observableArray([]);
    self.all_spouse_rate_options = ko.observableArray([]);
    self.all_posible_children_options = ko.observableArray([]);

    self.all_coverage_options = {
        employee: ko.observableArray([]),
        spouse: ko.observableArray([]),
        children: ko.observableArray([])
    };

    self.update_spouse_coverage_options = function() {
        var emp_benefit = self.root.selected_plan().employee_recommendation().recommended_benefit;

        // Triggered whenever the employee's selected coverage changes
        var valid_options = [];

        // If the employee has answered yes to any question, we have no limits
        var anyYesQuestions = self.root.employee().has_answered_any_question_yes();

        var null_spouse_option = self.spouse_options_for_demographics()[0];

        // Limit to 50% of employee's current selection, or 25k max
        if (!anyYesQuestions && (!emp_benefit || !emp_benefit.is_valid())) {
            // Empty list
            // push the NullBenefitOption
            valid_options.push(null_spouse_option);
        } else if (anyYesQuestions || emp_benefit.face_value >= 50000) {
            // add all the options
            $.merge(valid_options, self.spouse_options_for_demographics());
        } else {
            // Cap at 25k or half the employee rate
            var limit = emp_benefit.face_value / 2.0;

            $.each(self.spouse_options_for_demographics(), function() {
                var rate = this;

                if (rate.face_value <= limit) {
                    valid_options.push(rate);
                } else {
                    if (valid_options.length > 0 &&
                        valid_options[valid_options.length-1].face_value < limit) {
                        // Append the exact limit to the rate options
                        var all_options_by_rate = _.indexBy(self.all_spouse_rate_options(), "face_value");
                        if (all_options_by_rate[limit]) {
                            valid_options.push(all_options_by_rate[limit]);
                        }
                    }
                    // Break out of the loop, we've hit the limit
                    return false;
                }
            });
        }

        self.all_coverage_options.spouse(valid_options);

        // Need to make sure the actual coverage set for the spouse is not outside the custom options
        var current_spouse_coverage = self.root.selected_plan().spouse_recommendation().recommended_benefit;
        if (!_.find(valid_options, function(opt) {return opt.face_value == current_spouse_coverage.face_value;})) {
            self.root.selected_plan().spouse_recommendation(new NullRecommendation(null_spouse_option));
        }

        // Update the children options too
        // Triggered whenever the employee's selected coverage changes
        var valid_children_options = [];
        var null_child_option = self.all_posible_children_options()[0];

        // Cannot select coverage if the employee has not selected coverage
        if (!anyYesQuestions && (!emp_benefit || !emp_benefit.is_valid())) {
            // push the NullBenefitOption
            valid_children_options.push(null_child_option);
        } else if (anyYesQuestions || emp_benefit.face_value >= 10000) {
            // add all the options
            $.merge(valid_children_options, self.all_posible_children_options());
        } else {
            // need to exclude children when emp coverage is less than 10000, do nothing.
            // push the NullBenefitOption
            valid_children_options.push(null_child_option);
        }

        self.all_coverage_options.children(valid_children_options);

        // Need to make sure the actual coverage set for the spouse is not outside the custom options
        var current_child_coverage = self.root.selected_plan().children_recommendation().recommended_benefit;
        if (!_.find(valid_children_options, function(opt) {return opt.face_value == current_child_coverage.face_value;})) {
            self.root.selected_plan().children_recommendation(new NullRecommendation(null_child_option));
        }

    };

    self.update_children_coverage_options = function() {
        var emp_benefit = self.root.selected_plan().employee_recommendation().recommended_benefit;


    };

    // overrides the default impl.
    self.get_coverage_options_for_applicant = function(applicant_type) {
        // returns an observable
        // use 'this' here to make sure we reference the correct object when
        //  decorating with GI
        return this.all_coverage_options[applicant_type];
    };

    function convert_rate_to_benefit_option(rate) {
        return self.get_new_benefit_option({
            is_by_face: true,
            face_value: rate.coverage,
            premium: rate.premium
        });
    }

    // overrides the default impl.
    self.parse_benefit_options = function(applicant_type, applicant, rates) {
        // need to limit spouse benefit options to half the employee's currently selected option.
        var self = this;

        if (applicant_type == "employee") {
            // Make sure we have a reference to the employee's currently selected option

            if (self.employee_current_benefit_subscription === null) {
                // Should only happen once, the first time rates are called
                self.employee_current_benefit_subscription = applicant.selected_plan.subscribe(
                    self.update_spouse_coverage_options
                );

                // Also subscribe to changes to the yes/no questions for employee
                var subscribe_to_question_answers = function(questions) {
                    // Subscribe to answers to questions
                    _.each(questions, function(soh_answer) {
                        soh_answer.answer.subscribe(self.update_spouse_coverage_options);
                    });
                };
                applicant.health_questions.subscribe(subscribe_to_question_answers);
                // also invoke it now
                subscribe_to_question_answers(applicant.health_questions());

            }


            // $5,000 to $100,000
            var valid_options = [new NullBenefitOption()];
            var rate_choices = [];
            $.each(rates.byface, function() {
                var rate = this;
                if (rate.coverage % 5000 == 0) {
                    rate_choices.push(rate);
                }
            });
            $.merge(valid_options, $.map(rate_choices, convert_rate_to_benefit_option));
            self.all_coverage_options[applicant_type](valid_options);
        }
        if (applicant_type == "spouse" && rates.byface !== undefined) {
            // $5,000 increments up to 50% of employee's current selection, 25k max
            // First, just get rates up to 25k or age limit
            //  the employee selection limit is handled in the computed function above
            var demographic_spouse_rates = [];
            var all_spouse_rates = [];
            $.each(rates.byface, function() {
                var rate = this;
                if (rate.coverage % 5000 == 0 && rate.coverage <= 25000) {
                    demographic_spouse_rates.push(rate);
                }

                // Collect all valid half-way points too in a different list
                if (rate.coverage <= 25000) {
                    all_spouse_rates.push(rate);
                }
            });
            self.spouse_options_for_demographics($.merge(
                [new NullBenefitOption()],
                $.map(demographic_spouse_rates, convert_rate_to_benefit_option)
            ));
            self.all_spouse_rate_options($.merge(
                [new NullBenefitOption()],
                $.map(all_spouse_rates, convert_rate_to_benefit_option)
            ));
            var sp_options = [new NullBenefitOption()];
            $.merge(sp_options, $.map(demographic_spouse_rates, convert_rate_to_benefit_option));
            self.all_coverage_options.spouse(sp_options);
        }
        if (applicant_type == "children" && rates.byface !== undefined) {
            var ch_options = [new NullBenefitOption()];
            $.merge(ch_options, $.map(rates.byface, convert_rate_to_benefit_option));
            self.all_posible_children_options(ch_options);
            self.all_coverage_options.children(ch_options);
        }
    };


}
GroupCIProduct.prototype = Object.create(Product.prototype);
//GroupCIProduct.prototype.get_new_benefit_option = function(options) {
//    return new CIBenefitOption(new BenefitOption(options));
//};
GroupCIProduct.prototype.is_valid_employee = function(employee) {
    // Need to validate age and is_smoker is valid
    var age = employee.get_age();
    var valid = (age >= this.min_emp_age() && age <= this.max_emp_age());
    valid &= employee.is_smoker() != null;
    valid &= employee.has_valid_weight();
    valid &= employee.has_valid_height();
    valid &= employee.has_valid_gender();

    return valid;
};

GroupCIProduct.prototype.is_valid_spouse = function(spouse) {
    var age = spouse.get_age();
    var valid = (age >= this.min_sp_age() && age <= this.max_sp_age());
    valid &= spouse.is_smoker() != null;
    valid &= spouse.has_valid_weight();
    valid &= spouse.has_valid_height();
    valid &= spouse.has_valid_gender();
    return valid;
};

GroupCIProduct.prototype.requires_gender = function() {return true;};
GroupCIProduct.prototype.requires_height = function() {return true;};
GroupCIProduct.prototype.requires_weight = function() {return true;};
GroupCIProduct.prototype.requires_is_smoker = function() {return true;};
GroupCIProduct.prototype.has_critical_illness_coverages = function() {
    return true;
};
GroupCIProduct.prototype.should_show_step_5 = function() {return false;};

GroupCIProduct.prototype.should_use_date_of_hire_for_identity = function() {
    // Not an FPP product, use normal identity options.
    return false;
};
GroupCIProduct.prototype.is_fpp_product = function() {
    // Returns true if this product falls into the class of Family Protection Plan products
    return false;
};


// FPP Gov
function FPPGovProduct(product_data, product_type) {
    this.product_type = product_type || "FPP-Gov";
    this.product_data = product_data;
}
FPPGovProduct.prototype = Object.create(Product.prototype);
FPPGovProduct.prototype.requires_gender = function() {return false;};
FPPGovProduct.prototype.requires_height = function() {return false;};
FPPGovProduct.prototype.requires_weight = function() {return false;};
FPPGovProduct.prototype.requires_is_smoker = function() {return false;};
FPPGovProduct.prototype.has_critical_illness_coverages = function() {
    return false;
};
FPPGovProduct.prototype.get_replacement_paragraphs = function() {
    return this.product_data.replacement_paragraphs;
};


// Guaranteed Issue Product decorator
//  Wraps a base product type
function GIProductDecorator(product, product_data) {
    var self = this;

    self.product = product;
    self.product_data = product_data;
    self.product_type = product.product_type;

    self.all_coverage_options = {
        employee: ko.observableArray([]),
        spouse: ko.observableArray([]),
        children: ko.observableArray([])
    };

    // Defined all of product's methods on self and delegate to self.product by default
    _.each(_.methods(product), function(method) {
        self[method] = function() {
            return self.product[method].apply(self, arguments);
        }
    });

}



// Step 2 Question Types

var GlobalSOHQuestion = function(question_text) {
    var self = this;
    self.question_text = question_text;
};
GlobalSOHQuestion.prototype.get_question_text = function() {
    return this.question_text;
};


var NonHealthQuestion = function(question_text) {
    var self = this;
    self.question_text = question_text;
};
NonHealthQuestion.prototype.get_question_text = function() {
    return this.question_text;
};


var StandardHealthQuestion = function(question, selected_plan) {
    // A viewmodel that keeps track of which applicants need to answer which health questions
    var self = this;

    // should be an observableArray of InsuredApplicant objects
    self.selected_plan = selected_plan;

    // Simple object with .question_text and .label
    self.question = question;

    self.does_employee_need_to_answer = ko.computed(function() {
        if (self.question.is_spouse_only) {
            return false;
        }
        return self.selected_plan().did_select_employee_coverage();
    });
    self.does_spouse_need_to_answer = ko.computed(function() {
        return self.selected_plan().did_select_spouse_coverage();
    });

    self.does_child_need_to_answer = function(child) {
        if (self.question.is_spouse_only) {
            return false;
        }
        return self.selected_plan().did_select_children_coverage();
    };

    self.can_employee_skip_due_to_GI = function() {return false};
    self.can_spouse_skip_due_to_GI = function() {return false};

    self.show_yes_dialogue_employee = function() {return self.show_yes_dialogue(); };
    self.show_yes_dialogue_spouse = function() {return self.show_yes_dialogue(); };
    self.show_yes_dialogue_child = function(child) {return self.show_yes_dialogue(); };


    self.show_yes_dialogue = function() {
        if (self.does_yes_stop_app()) {
            handle_question_yes();
        } else {
            // do nothing
        }
    };

    self.does_any_applicant_need_to_answer = ko.computed(function() {

        return _.any(self.selected_plan().get_covered_applicants_with_type(), function(data) {
            return self.does_applicant_need_to_answer(data.type, data.applicant);
        });
    });
};
StandardHealthQuestion.prototype.get_question_text = function() {
    return this.question.question_text;
};
StandardHealthQuestion.prototype.get_question_label = function() {
    return this.question.label;
};

StandardHealthQuestion.prototype.does_applicant_need_to_answer = function(applicant_type, applicant) {
    var self = this;
    if (applicant_type == "Employee") {
        return self.does_employee_need_to_answer();
    } else if (applicant_type == "Spouse") {
        return self.does_spouse_need_to_answer();
    } else if (applicant_type == "Child") {
        return self.does_child_need_to_answer(applicant);
    }
    console.error("Got unknown applicant type '"+applicant_type+"'");
    return true;
};
StandardHealthQuestion.prototype.get_yes_highlight = function() {
    return (this.does_yes_stop_app()) ? 'stop' : 'checkmark';
};
StandardHealthQuestion.prototype.does_yes_stop_app = function() {
    return !this.question.is_ignored;
};


var GIHealthQuestion = function(product, question, selected_plan, applicant_criteria, skip_mode, skipped_questions) {
    var self = this;
    self.product = product;
    self.selected_plan = selected_plan;
    self.question = question;
    self.applicant_criteria = applicant_criteria;
    self.skip_mode = skip_mode;
    self.skipped_questions = skipped_questions;


    self.does_employee_need_to_answer = ko.computed(function() {
        if (!self.selected_plan()) return false;
        if (!self.selected_plan().did_select_employee_coverage()) {
            return false;
        }
        if (self.can_employee_skip_due_to_GI()) {
            return false;
        }
        if (self.question.is_spouse_only) {
            return false;
        }

        return true;
    });



    self.does_spouse_need_to_answer = ko.computed(function() {
        if (!self.selected_plan()) return false;
        if (!self.selected_plan().did_select_spouse_coverage()) {
            return false;
        }
        if (self.can_spouse_skip_due_to_GI()) {
            return false;
        }

        return true;
    });


    self.does_child_need_to_answer = function(child_applicant) {
        if (!self.selected_plan()) return false;
        if (!self.selected_plan().did_select_children_coverage()) {
            return false;
        }

        if (self.has_child_met_GI_criteria(child_applicant) && self.should_skip_if_GI_criteria_met()) {
            return false;
        }

        if (self.question.is_spouse_only) {
            return false;
        }

        return true;
    };

    self.get_met_gi_criteria_for_employee = ko.computed(function() {
        if (!self.selected_plan().did_select_employee_coverage()) {
            return undefined;
        }

        var coverage = self.selected_plan().employee_recommendation().recommended_benefit;
        var applicant = self.selected_plan().root.employee();
        var employee_criteria = self.get_criteria("Employee");
        return _.find(employee_criteria, function(criterion) {
            return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
        });
    });

    self.has_employee_met_GI_criteria = ko.computed(function() {
        return self.get_met_gi_criteria_for_employee() !== undefined;
    });


    self.get_met_gi_criteria_for_spouse = ko.computed(function() {
        if (!self.selected_plan().did_select_spouse_coverage()) {
            return undefined;
        }
        var coverage = self.selected_plan().spouse_recommendation().recommended_benefit;
        var applicant = self.selected_plan().root.spouse();
        var criteria = self.get_criteria("Spouse");
        return _.find(criteria, function(criterion) {
            return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
        });
    });

    self.has_spouse_met_GI_criteria = ko.computed(function() {
        return self.get_met_gi_criteria_for_spouse() !== undefined;
    });

    self.can_employee_skip_due_to_GI = ko.computed(function() {
        return self.has_employee_met_GI_criteria() && self.should_skip_if_GI_criteria_met();
    });

    self.can_spouse_skip_due_to_GI = ko.computed(function() {
        return self.has_spouse_met_GI_criteria() && self.should_skip_if_GI_criteria_met();
    });

    self.get_met_gi_criteria_for_child = function(child_applicant) {
        if (!self.selected_plan().did_select_children_coverage()) {
            return undefined;
        }
        var criteria = self.get_criteria("Child");
        var coverage = self.selected_plan().children_recommendation().recommended_benefit;
        return _.find(criteria, function(criterion) {
            return self.does_applicant_meet_GI_criteria(child_applicant, coverage, criterion);
        });
    };


    self.has_child_met_GI_criteria = function(child_applicant) {
        return self.get_met_gi_criteria_for_child(child_applicant) !== undefined;
    };

    self.does_any_applicant_need_to_answer = ko.computed(function() {
        return _.any(self.selected_plan().get_covered_applicants_with_type(), function(data) {
            return self.does_applicant_need_to_answer(data.type, data.applicant);
        });
    });

    self.show_yes_dialogue_employee = function() {
          self.show_yes_dialogue('Employee', self.selected_plan().root.employee());
    };

    self.show_yes_dialogue_spouse = function() {
          self.show_yes_dialogue('Spouse', self.selected_plan().root.spouse());
    };

    self.show_yes_dialogue_child = function(child) {
        self.show_yes_dialogue('Children', child);
    };

    self.get_best_GI_criteria = function(applicant, coverage, criteria) {
        // sort by gi amount descending then pick the one, if any, that is strictly less than
        //  the selected coverage. Returns undefined if there is no qualifying option.
        var descendingCriteria = _.sortBy(criteria, function(c) {return -c.guarantee_issue_amount;});
        return _.find(descendingCriteria, function(criterion) {
            return (
                self.does_applicant_meet_demographic_GI_criteria(applicant, criterion) &&
                criterion.guarantee_issue_amount < coverage.face_value
            );
        });
    };

    self.show_yes_dialogue = function(applicant_type, applicant) {
        // If we get here, we know the applicant doesn't completely qualify for GI
        //   with the coverage selected, and has answered 'Yes' to a question.
        //   We need to determine if the applicant can bypass this question by
        //   reducing coverage.

        // If this is a required question, we show the normal 'you must answer no' dialogue
        if (!self.should_skip_if_GI_criteria_met()) {
            handle_question_yes();
            return;
        }

        // See if there is a GI condition with a lower coverage amount.
        var criteria = self.get_criteria(applicant_type);
        var coverage, reduced_gi_criterion, applicant_coverage_options, coverage_applicant;
        // Does the applicant meet any of the demographic GI criteria?
        if (applicant_type == "Employee") {
            coverage = self.selected_plan().employee_recommendation().recommended_benefit;
            reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
            applicant_coverage_options = self.product.get_coverage_options_for_applicant('employee');
            coverage_applicant = applicant;
        } else if (applicant_type == "Spouse") {
            coverage = self.selected_plan().spouse_recommendation().recommended_benefit;
            reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
            applicant_coverage_options = self.product.get_coverage_options_for_applicant('spouse');
            coverage_applicant = applicant;
        } else if (applicant_type == "Children") {
            coverage = self.selected_plan().children_recommendation().recommended_benefit;
            reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
            applicant_coverage_options = self.product.get_coverage_options_for_applicant('children');
            // The 'coverage applicant' is a fake applicant representing all child coverage
            coverage_applicant = self.selected_plan().root.child_benefits();
        }

        // If no option was found, present the normal yes dialogue.
        if (!reduced_gi_criterion) {
            handle_question_yes();
            return;
        }

        // Otherwise, show a special dialogue that gives a reduce benefit option for continuing

        var face_amount = coverage.format_face_value();
        var gi_amount = reduced_gi_criterion.guarantee_issue_amount;
        var formatted_gi_amount = format_face_value(gi_amount);

        var button_options = {
            reduce: {label: "Reduce the coverage", className: 'btn-success', callback: function() {
                //
                var max_option = _.max(
                        _.filter(applicant_coverage_options(), function(o) {
                            return o.face_value <= gi_amount && o.face_value > 0
                        }),
                        function(o) {return o.face_value}
                );
                coverage_applicant.selected_custom_option(max_option);
                self.selected_plan().root.apply_selected_customization();

            }},
            remove: {label: "Remove this applicant", className: 'btn-danger', callback: function() {
                // If child, we remove only the selected child, not all child coverage
                if (applicant_type == "Children") {
                    self.selected_plan().root.children.remove(applicant);

                } else {
                    var null_option = _.find(applicant_coverage_options(), function(o) {
                        return o.face_value == 0
                    });

                    coverage_applicant.selected_custom_option(null_option);
                    self.selected_plan().root.apply_selected_customization();
                }
            }},
            ignore: {label: "Ignore and Continue", className: 'btn-default', callback: function() {
                // Nothing to do in this case
            }}
        };

        bootbox.dialog({
            message: 'A "yes" response to this question prohibits this person from obtaining the selected '+face_amount+' of coverage. You may proceed, however, by reducing your coverage to the guaranteed coverage amount of '+formatted_gi_amount+'.'+
                     '<br><br>Alternatively, you may remove this individual from the coverage selection altogether (in Step 1) before proceeding with the rest of the application.',
            buttons: button_options
        });
    };


    self.get_criteria = function(applicant_type) {
        if (applicant_type == "Children") {
            applicant_type = "Child";
        }
        return _.filter(self.applicant_criteria, function(c) {return c.applicant_type == applicant_type;});
    };

    self.does_applicant_meet_GI_criteria = function(applicant, coverage, criterion) {
        return (
            self.does_applicant_meet_coverage_GI_criteria(applicant, coverage, criterion) &&
            self.does_applicant_meet_demographic_GI_criteria(applicant, criterion)
        );
    };

    self.does_applicant_meet_coverage_GI_criteria = function(applicant, coverage, criterion) {
        return coverage.face_value <= criterion.guarantee_issue_amount;
    };

    self.does_applicant_meet_demographic_GI_criteria = function(applicant, criterion) {
        // Checks all the criteria _except_ for the coverage so we know if the applicant
        //  can reduce his coverage to meet this criterion

        var does_meet = true;

        if (criterion.age_max !== null) {
            does_meet &= applicant.get_age() <= criterion.age_max;
        }
        if (criterion.age_min !== null) {
            does_meet &= applicant.get_age() >= criterion.age_min;
        }
        if (criterion.height_max !== null) {
            does_meet &= applicant.height() !== null && applicant.height() <= criterion.height_max
        }
        if (criterion.height_min !== null) {
            does_meet &= applicant.height() !== null && applicant.height() >= criterion.height_min;
        }
        if (criterion.weight_max !== null) {
            does_meet &= applicant.weight() !== null && applicant.weight() <= criterion.weight_max
        }
        if (criterion.weight_min !== null) {
            does_meet &= applicant.weight() !== null && applicant.weight() >= criterion.weight_min;
        }

        return does_meet;
    };
};
GIHealthQuestion.prototype = Object.create(StandardHealthQuestion.prototype);

GIHealthQuestion.prototype.should_skip_if_GI_criteria_met = function() {
    var self = this;
    if (self.skip_mode == "all") {
        return true;
    } else {
        return _.any(self.skipped_questions, function(q) {
            return self.get_question_label() == q.question_type_label
        });
    }
};
GIHealthQuestion.prototype.does_yes_stop_app = function() {
    var self = this;

    // If GI, clicking YES always stops (but will show the reduce/remove dialogue if optional).
    return true;
};

function HealthQuestionAnswer(question, button_group, question_object) {
    var self = this;

    self.question = question;

    // This has all the additional attributes for the question
    self.question_object = question_object;

    self.button_group = ko.observable(null);


    self.answer = ko.computed(function() {
        if (self.button_group() == null) {
            return null;
        } else {
            return self.button_group().get_val();
        }
    });

    self.serialize = function() {
        // question: text,
        // answer: [Yes|No|GI] (GI means it was skipped due to GI)
        var answer;
        if (self.button_group()) {
            answer = (self.button_group().is_required()) ? self.button_group().get_val() : "GI";
        } else {
            answer = null;
        }

        return {
            question: self.question.question_text,
            label: self.question.label,
            is_spouse_only: self.question.is_spouse_only || false,
            answer: answer
        }
    };
}

// ViewModel for Beneficiaries
function Beneficiary(options) {
    var self = this;

    var defaults = {
        name: "",
        relationship: "",
        ssn: "",
        date_of_birth: "",
        is_nonperson_entity: false
    };
    options = $.merge({}, defaults, options);

    self.name = ko.observable(options.name);
    self.relationship = ko.observable(options.relationship);
    self.ssn = ko.observable(options.ssn);
    self.date_of_birth = ko.observable(options.date_of_birth);
    self.is_nonperson_entity = ko.observable(options.is_nonperson_entity);

    self.serialize = function() {
        return {
            name: self.name(),
            relationship: self.relationship(),
            ssn: self.ssn(),
            date_of_birth: self.date_of_birth(),
            is_nonperson_entity: self.is_nonperson_entity()
        };
    };
}

// Main ViewModel for all applicants
var _applicant_count = 0;
function InsuredApplicant(applicant_type, options, selected_plan, product_health_questions) {
    var self = this;

    var defaults = {
        first: "",
        last: "",
        email: "",
        phone: "",
        birthdate: null,
        ssn: "",
        gender: null,
        height: null,
        weight: null,
        is_smoker: null,
        street_address: "",
        street_address2: "",
        city: "",
        state: "",
        zip: "",
        existing_coverages: []
    };
    var applicant_data = $.extend({}, defaults, options);

    self.applicant_type = applicant_type;
    self.selected_plan = selected_plan;

    // a basic internal id we can use in loops to distinguish applicants and get
    //  unique names, attributes, etc. for validation and lookup
    self._id = _applicant_count++;

    self.first = ko.observable(applicant_data.first);
    self.last = ko.observable(applicant_data.last);
    self.email = ko.observable(applicant_data.email);
    self.phone = ko.observable(applicant_data.phone);
    self.birthdate = ko.observable(applicant_data.birthdate);
    self.ssn = ko.observable(applicant_data.ssn);
    self.gender = ko.observable(applicant_data.gender);

    // Extended questions
    self.height = ko.observable(parseFloat(applicant_data.height) ? parseFloat(applicant_data.height) : null);
    self.weight = ko.observable(applicant_data.weight);
    self.is_smoker = ko.observable(applicant_data.is_smoker);

    self.height_error = ko.observable(null);
    self.weight_error = ko.observable(null);

    self.address1 = ko.observable(applicant_data.street_address);
    self.address2 = ko.observable(applicant_data.street_address2);
    self.city = ko.observable(applicant_data.city);
    self.state = ko.observable(applicant_data.state);
    self.zip = ko.observable(applicant_data.zip);

    // From previous application(s)
    self.existing_coverages = applicant_data.existing_coverages || [];

    self.health_questions = ko.computed(function() {

        var questions = [];

        // We need to depend on the root's health_questions for the current product
        _.each(product_health_questions(), function(hq) {
            // Not answered ("null") by default
            var q = new HealthQuestionAnswer(hq.question, null, hq);
            questions.push(q);
        });

        return questions;
    });


    self.has_answered_any_question_yes = ko.pureComputed(function() {
        return _.any(self.health_questions(), function(soh_answer) {
            return soh_answer.answer() == "Yes";
        });
    });


    self.has_valid_gender = ko.pureComputed(function() {
        return self.gender() !== null;
    });
    self.has_valid_height = ko.computed(function() {
        return self.height() != null && self.height() > 0 && self.height_error() == null;
    });
    self.has_valid_weight = ko.computed(function() {
        return self.weight() != null && self.weight_error() == null;
    });

    self.is_valid = ko.computed(function() {
        return (
            $.trim(self.first()) != "" &&
            $.trim(self.last()) != "" &&
            $.trim(self.birthdate()) != ""
        );
    });

    self.any_valid_field = ko.computed(function() {
        return (
            $.trim(self.first()) != "" ||
            $.trim(self.last()) != "" ||
            $.trim(self.birthdate()) != ""
            );
    });

    self.name = ko.computed(function() {
        // Only show if valid
        if (self.is_valid()) {
            return self.first();
        } else {
            return "";
        }
    });

    self.get_age = ko.computed(function() {
        return age_for_date(self.birthdate());
    });


    self.benefit_options = {
        by_coverage: ko.observableArray([]),
        by_premium: ko.observableArray([])
    };
    self.all_options = ko.computed(function() {
        var options = [new NullBenefitOption()];
        // Extends an array with another array
        $.merge(options, self.benefit_options.by_premium());
        $.merge(options, self.benefit_options.by_coverage());
        return options;
    });


    self.selected_custom_option = ko.observable(new NullBenefitOption());

    self.selected_coverage = ko.computed(function() {
        if (self.applicant_type == InsuredApplicant.EmployeeType) {
            return self.selected_plan().employee_recommendation().recommended_benefit;
        } else if (self.applicant_type == InsuredApplicant.SpouseType) {
            return self.selected_plan().spouse_recommendation().recommended_benefit;
        } else if (self.applicant_type == InsuredApplicant.ChildType) {
            return self.selected_plan().children_recommendation().recommended_benefit;
        }
        throw "Bad applicant type for applicant: "+self.applicant_type;
    });

    self.has_selected_valid_coverage = ko.computed(function() {
        return self.selected_coverage().is_valid();
    });
    
    self.display_selected_coverage = ko.computed(function() {
        return self.selected_coverage().format_face_value();
    });
    self.display_premium = ko.computed(function() {
        return self.selected_coverage().format_premium();
    });

    self.display_riders = function(rider_code, person) {
      person = person.applicant_type;
      if(!person) {
        return;
      }
      var rider_amount = 0;
      if(person==="employee") {
        rider_amount = window.ui.rider_rates()['emp'][rider_code];
      } else if(person==="spouse") {
        rider_amount = window.ui.rider_rates()['sp'][rider_code];
      } 
      return "$"+rider_amount.toFixed(2);
    };

    self.get_existing_coverage_amount_for_product = function(product_id) {
        return parseFloat(self.get_existing_coverage_amount_by_product()[product_id]);
    };

    self.get_existing_coverage_amount_by_product = function() {
        return _.reduce(self.existing_coverages, function(by_product_id, coverage) {
            if (!(coverage.product.id in by_product_id)) {
                by_product_id[coverage.product.id] = 0.0;
            }
            by_product_id[coverage.product.id] += parseFloat(coverage.coverage_face_value);
            return by_product_id;
        }, {});
    };

    self.serialize_data = function() {
        var data = {};

        data.first = self.first();
        data.last = self.last();
        data.email = self.email();
        data.age = self.get_age();
        data.weight = self.weight();
        data.height = self.height();
        data.is_smoker = self.is_smoker();
        data.birthdate = self.birthdate();
        data.ssn = self.ssn();
        data.gender = self.gender();
        data.phone = self.phone();
        data.address1 = self.address1();
        data.address2 = self.address2();
        data.city = self.city();
        data.state = self.state();
        data.zip = self.zip();

        // Serialize the SOH questions
        data.soh_questions = [];
        _.each(self.health_questions(), function(soh_answer) {
            if (soh_answer.button_group()
                    && soh_answer.question_object.question.is_spouse_only) {
                // We don't want to serialize these questions right now since we want only the health questions.
                //   (and these are sent separately right now using special observables has_spouse_been_[treated|disabled]_6_months...)
                // In the future, may add an attribute like 'non-health-question' to strip these out.
                // skip (continue)
                return true;
            }
            data.soh_questions.push(soh_answer.serialize());
        });

        return data;
    }
}
InsuredApplicant.EmployeeType = "employee";
InsuredApplicant.SpouseType = "spouse";
InsuredApplicant.ChildType = "children";


function age_for_date(date) {
    var bd = parse_date(date, "MM/DD/YYYY");
    if (bd.isValid()) {
        if (bd.isAfter(now())) {
            // Avoid returning -0 for future dates less than one
            return -1;
        } else {
            // Valid age
            return now().diff(bd, "years");
        }
    } else {
        // Invalid age
        return "";
    }
}

function BenefitOption(options) {
    var self = this;

    self.is_by_face = options.is_by_face;
    self.premium = options.premium;
    self.face_value = options.face_value;

    self.format_premium = function() {
        return format_premium_value(self.premium);
    };

    self.format_premium_option = function() {
        return self.format_premium() + " " + ui.payment_mode_text_lower();
    };
    self.format_face_value = function() {
        return format_face_value(self.face_value);
    };
    self.format_face_option = function() {
        return self.format_face_value() + " face amount";
    };
    self.format_for_dropdown = function() {
        if (self.is_by_face) {
            return self.format_face_option();
        } else {
            return self.format_premium_option();
        }
    };
    self.is_valid = function() {
        return true;
    };

    self.serialize_data = function() {
        return {
            premium: self.premium,
            face_value: self.face_value
        }
    }
}
BenefitOption.display_benefit_option = function(item) {
    return item.format_for_dropdown();
};

function CIBenefitOption(wrapped_option) {
    var self = this;
    self.is_by_face = wrapped_option.is_by_face;
    self.premium = wrapped_option.premium;
    self.face_value = wrapped_option.face_value;
    self.format_premium = wrapped_option.format_premium;
    self.format_premium_option = wrapped_option.format_premium_option;
    self.format_face_value = function() {
        var face_value_formatted = format_face_value(self.face_value);
        var ci_value = Math.round(self.face_value * .3);
        var ci_value_formatted = format_face_value(ci_value);

        return face_value_formatted + "<br><small>("+ci_value_formatted+" CI)</small>";
    };
    self.format_for_dropdown = wrapped_option.format_for_dropdown;
    self.is_valid = wrapped_option.is_valid;
    self.serialize_data = wrapped_option.serialize_data;
}


function NullBenefitOption() {
    var self = this;

    self.is_by_face = true;
    self.premium = 0;
    self.face_value = 0;

    self.is_valid = function() {
        return false;
    };

    self.format_premium_option = function() {
        return "";
    };
    self.format_premium = function() {

    };

    self.format_face_value = function() {
        return "- no benefit -";
    };
    self.format_for_dropdown = function() {
        return "- no benefit -";
    };
    self.serialize_data = function() {
        return {}
    }
}


function BenefitsPackage(root, name) {
    var self = this;
    self.root = root;
    self.name = ko.observable(name);

    self.employee_recommendation = ko.observable(new NullRecommendation());
    self.spouse_recommendation = ko.observable(new NullRecommendation());
    self.children_recommendation = ko.observable(new NullRecommendation());

    self.set_recommendations = function(recommendations) {

        if (root.employee().is_valid()) {
            self.employee_recommendation(self.build_recommendation(self.root.employee(), recommendations['employee']));
        } else {
            self.employee_recommendation(new NullRecommendation());
        }

        if (root.should_include_spouse_in_table()) {
            self.spouse_recommendation(self.build_recommendation(self.root.spouse(), recommendations['spouse']));
        } else {
            self.spouse_recommendation(new NullRecommendation());
        }

        if (root.should_include_children_in_table()) {
            self.children_recommendation(self.build_recommendation(self.root.child_benefits(), recommendations['children']));
        } else {
            self.children_recommendation(new NullRecommendation());
        }
    };

    self.build_recommendation = function(applicant, recommended_val) {
        var benefit = self.get_recommended_benefit(applicant, recommended_val);
        return new Recommendation(benefit);
    };

    self.get_recommended_benefit = function(applicant, recommended_val) {
        if (recommended_val == null || recommended_val == "") {
            return new NullBenefitOption();
        } else {
            var applicant_type;
            if (applicant == root.employee()) {
                applicant_type = "employee";
            } else if (applicant == root.spouse()) {
                applicant_type = "spouse";
            } else {
                applicant_type = "children";
            }

            return root.insurance_product.find_recommended_coverage_benefit(applicant_type, recommended_val);
        }
    };

    self.get_package_benefits = function() {
        var benefits = [
            self.employee_recommendation().recommended_benefit,
            self.spouse_recommendation().recommended_benefit
        ];
        // For each child, push our child benefit recommendation
        if (self.children_recommendation().recommended_benefit) {
            for (var i = 0; i < self.root.get_valid_children().length; i++) {
                benefits.push(self.children_recommendation().recommended_benefit);
            }
        }
        return benefits;
    };


    self.get_total_premium = ko.computed(function() {
        var benefits = self.get_package_benefits();

        // Sum the benefit premiums
        var total = 0.0;
        $.each(benefits, function() {
            if (this.premium != null) {
                total += this.premium;
            }
        });
        return total;
    });

    self.formatted_total_premium = ko.computed(function() {
        if (self.get_total_premium() > 0.0) {
            return format_premium_value(self.get_total_premium());
        } else {
            return "";
        }
    });


    self.is_valid = function() {return true};

    self.did_select_employee_coverage = ko.computed(function() {
        return self.employee_recommendation().recommended_benefit.is_valid();
    });

    self.did_select_spouse_coverage = ko.computed(function() {
        return self.spouse_recommendation().recommended_benefit.is_valid();
    });

    self.did_select_children_coverage = ko.computed(function() {
        return self.children_recommendation().recommended_benefit.is_valid();
    });

    self.did_person_select_coverage = function(person) {
      if(person.applicant_type) {
       person = person.applicant_type;
      }
      if(person=="employee") {
        return self.did_select_employee_coverage();
      }
      if(person=="spouse") {
        return self.did_select_spouse_coverage();
      }
      return false;
    };

    self.get_total_riders = ko.computed(function() {
        total_rider_amount = 0;
        if(window.ui) {
          var people = ['employee', 'spouse'];
          var people_short = ['emp', 'sp']
          for(var j=0; j<people.length; j++) {
            if(!self.did_person_select_coverage(people[j])) {
              continue;
            }
            var riders = window.ui.get_selected_riders(people[j]);
            for(var i=0; i<riders.length; i++) {
              total_rider_amount += window.ui.rider_rates()[people_short[j]][riders[i].code];
            }
          }
        }
        return total_rider_amount;
    });

    self.formatted_total_premium_with_riders = ko.computed(function() {
        if (self.get_total_premium() > 0.0) {
            var riders = 0;
            if(self.get_total_riders() > 0.0) {
              riders = self.get_total_riders();
            }
            var premium = self.get_total_premium();
            return format_premium_value(premium+riders);
        } else {
            return "";
        }
    });
    
    self.get_all_people = ko.computed(function() {
        var employee = root.employee();
        var people = [employee];
        if (root.should_include_spouse_in_table()) {
            var spouse = root.spouse();
            people.push(spouse);
        }

        if (root.should_include_children_in_table()) {
            $.each(root.get_valid_children(), function () {
                var child = this;
                people.push(child);
            });
        }
        return people;
    });

    self.get_all_covered_people = ko.computed(function() {
        var people = [];

        if (root.did_select_employee_coverage()) {
            var employee = root.employee();
            people.push(employee);
        }

        if (root.did_select_spouse_coverage()) {
            var spouse = root.spouse();
            people.push(spouse);
        }

        if (root.did_select_children_coverage()) {
            $.each(root.get_valid_children(), function () {
                var child = this;
                people.push(child);
            });
        }
        return people;
    });

    self.get_all_people_labels = ko.computed(function() {
        var labels = [root.employee().name()];
        if (root.should_include_spouse_in_table()) {
            labels.push(root.spouse().name());
        }
        if (root.should_include_children_in_table()) {
            $.each(root.get_valid_children(), function () {
                var child = this;
                labels.push(child.name());
            });
        }
        return labels;
    });

    self.get_all_covered_people_labels = ko.computed(function() {
        var labels = [];
        if (root.did_select_employee_coverage()) {
            labels.push(root.employee().name());
        }
        if (root.did_select_spouse_coverage()) {
            labels.push(root.spouse().name());
        }
        if (root.did_select_children_coverage()) {
            $.each(root.get_valid_children(), function () {
                var child = this;
                labels.push(child.name());
            });
        }
        return labels;
    });

    self.get_covered_applicants_with_type = ko.computed(function() {
        var applicants = [];
        if (root.did_select_employee_coverage()) {

            applicants.push({applicant: root.employee(), type: "Employee", coverage: self.employee_recommendation().recommended_benefit});
        }
        if (root.did_select_spouse_coverage()) {
            applicants.push({applicant: root.spouse(), type: "Spouse", coverage: self.spouse_recommendation().recommended_benefit});
        }
        if (root.did_select_children_coverage()) {
            $.each(root.get_valid_children(), function () {
                var child = this;
                applicants.push({applicant: child, type: "Child", coverage: self.children_recommendation().recommended_benefit});
            });
        }
        return applicants;
    });

    self.get_covered_children = ko.computed(function() {
        return _.map(_.filter(self.get_covered_applicants_with_type(), function(d) {
                        return d.type == "Child";
                    }), function(d) { return d.applicant;});
    });

    self.get_people_with_labels = ko.computed(function() {
        var people = self.get_all_people();
        var labels = self.get_all_people_labels();

        var out = [];
        for (var i = 0; i < people.length; i++) {
            out.push({person: people[i], label: labels[i]});
        }
        return out;
    });

    self.has_at_least_one_benefit_selected = function() {
        var any_benefits = false;
        $.each(self.get_package_benefits(), function() {
            if (this.is_valid()) {
                any_benefits = true;
                return false;
            }
        });
        return any_benefits;
    };

}
function NullBenefitsPackage(root) {
    var self = this;

    self.root = root;
    self.employee_recommendation = ko.observable(new NullRecommendation());
    self.spouse_recommendation = ko.observable(new NullRecommendation());
    self.children_recommendation = ko.observable(new NullRecommendation());
    self.get_total_premium = function() { return null; };
    self.formatted_total_premium = function() { return ""; };
    self.formatted_total_premium_with_riders = function() { return ""; };
    self.is_valid = function () {return false};
    self.get_all_people = function() {return [];};
    self.get_all_covered_people = function() {return [];};
    self.get_covered_applicants_with_type = function() {return [];};
    self.get_all_people_labels = function() {return [];};
    self.get_all_covered_people_labels = function() {return [];};
    self.get_people_with_labels = function() {return [];};
    self.has_at_least_one_benefit_selected = function() {return false;};

    self.did_select_employee_coverage = function() { return false;};
    self.did_select_spouse_coverage = function() { return false;};
    self.did_select_children_coverage = function() { return false; };
    self.did_person_select_coverage = function(person) { return false;};
    self.get_covered_children = function() { return [];}};



function Recommendation(recommended_benefit) {
    var self = this;
    self.recommended_benefit = recommended_benefit;

    self.is_valid = function() {
        return true;
    };

    self.format_premium_option = function() {
        return self.recommended_benefit.format_premium_option()
    };
    self.format_face_value = function() {
        return self.recommended_benefit.format_face_value();
    };
}

function NullRecommendation(option) {
    var self = this;
    self.recommended_benefit = option || new NullBenefitOption();
    self.is_valid = function() {return false;};

}

function QuestionButton(element, val, highlight_func, unhighlight_func) {
    var self = this;

    self.elements = [$(element)];
    self.val = val;

    self.highlight = function() {
        $.each(self.elements, function() {
            highlight_func(this);
        });
    };
    self.unhighlight = function() {
        $.each(self.elements, function() {
            unhighlight_func(this);
        });
    };
}
function QuestionButtonGroup(question, is_required) {
    var self = this;
    self.question = question;
    self.buttons = [];
    self.selected_btn = ko.observable(null);
    self.is_required = is_required;

    self.get_val = ko.computed(function() {
        if (self.selected_btn() == null) {
            return false;
        } else {
            return self.selected_btn().val;
        }
    });


    self.add_button = function(element, val, high_func, unhigh_func) {
        var btn = null;
        $.each(self.buttons, function() {
            if (this.val == val) {
                btn = this;
            }
        });
        if (btn) {
            btn.elements.push(element);
        } else {
            self.buttons.push(new QuestionButton(element, val, high_func, unhigh_func));
        }
    };
    self.click_button = function(val) {
        var btn = null;
        $.each(self.buttons, function() {
            if (this.val == val) {
                btn = this;
            }
            this.unhighlight();
        });

        btn.highlight();
        self.selected_btn(btn);
    };
}
var questions = [];
var general_questions_by_id = {};
ko.bindingHandlers.flagBtn = {
    init: function(element, value_accessor) {

        var val = ko.unwrap(value_accessor());

        // for testing purpose, mark each element with a CSS class and yes or no value
        $(element).addClass("flagBtn").addClass("val_"+val.val);

        var btn_group;
        if (val.applicant) {
            var applicant = val.applicant;
            var question_text = val.question.get_question_text();
            var applicant_health_answer = _.find(applicant.health_questions(), function(soh_answer) {
                return soh_answer.question.question_text == question_text;
            });

            btn_group = applicant_health_answer.button_group();
            if (!btn_group) {
                btn_group = new QuestionButtonGroup(val.question, val.is_required);
                applicant_health_answer.button_group(btn_group);
            }
            if (applicant_health_answer.answer() == val.val) {
                btn_group.click_button(val.val);
            }
        } else {
            var group_lookup = general_questions_by_id;
            if (val.question.get_question_text() in group_lookup) {
                btn_group = group_lookup[val.question.get_question_text()];
            } else {
                btn_group = new QuestionButtonGroup(val.question, val.is_required);
                group_lookup[val.question.get_question_text()] = btn_group;
            }

            if (val.is_selected === true) {
                btn_group.click_button(val.val);
            }
        }

        btn_group.add_button(element, val.val, function(el) {
            var highlight_type;
            if (typeof val.highlight === "function") {
                highlight_type = val.highlight();
            } else {
                highlight_type = val.highlight;
            }
            if (highlight_type === "flag") {
                $(el
                ).prepend('<i class="icon glyphicon glyphicon-flag"></i>'
                ).addClass("btn btn-warning"
                );
            } else if (highlight_type === "checkmark") {
                $(el
                ).prepend('<i class="icon glyphicon glyphicon-ok"></i>'
                ).addClass("btn-success"
                );
            } else if (highlight_type === "stop") {
                $(el
                ).prepend('<i class="icon glyphicon glyphicon-remove"></i>'
                ).addClass("btn-danger"
                );
            }

        }, function(el) {
            $(el).removeClass("btn-success btn-warning btn-danger"
            ).addClass("btn-default"
            );
            $(el).find(".glyphicon").remove();
        });

        $(element).on("click", function() {
            btn_group.click_button(val.val);
            if (val.onclick) {
                val.onclick();
            }
        });

        // Initial value, if they are revisiting this page
        if (btn_group.selected_btn && btn_group.selected_btn.val == val.val) {
            btn_group.click_button(val.val);
        }

    },
    update: function(element, value_accessor) {

    }
};

function handle_existing_insurance_modal() {
    if (ui.insurance_product.is_fpp_product()) {

    } else {
        $("#modal_text_existing_warning_title").show();
        $("#modal_text_replacement_warning_title").hide();
        $("#modal_text_soh_warning_title").hide();

        $("#modal_text_existing_warning").show();
        //$("#modal_text_existing_warning_remote").hide();
        $("#modal_text_replacement_warning").hide();
        $("#modal_text_soh_warning").hide();

        $("#health_modal").modal('show');
        $("#existing_warning_text").show();

    }

    window.ui.existing_insurance(true);
}

function reset_existing_insurance_warning() {
    window.ui.existing_insurance(false);
    $("#existing_warning_text_remote").hide();
    $("#existing_warning_text").hide();
}

function handle_replacement_insurance_modal() {
    if (ui.insurance_product.is_fpp_product()) {

    } else {
        $("#modal_text_existing_warning_title").hide();
        $("#modal_text_replacement_warning_title").show();
        $("#modal_text_soh_warning_title").hide();

        $("#modal_text_existing_warning").hide();
        $("#modal_text_existing_warning_remote").hide();
        $("#modal_text_replacement_warning").show();
        $("#modal_text_soh_warning").hide();

        $("#health_modal").modal('show');
        $("#replacement_warning_text").show();
    }

    window.ui.replacing_insurance(true);

}
function reset_replacement_insurance_warning() {
    $("#replacement_warning_text").hide();
    window.ui.replacing_insurance(false);
}


function handle_question_yes() {
    $("#modal_text_existing_warning_title").hide();
    $("#modal_text_replacement_warning_title").hide();
    $("#modal_text_soh_warning_title").show();

    $("#modal_text_existing_warning").hide();
    $("#modal_text_existing_warning_remote").hide();
    $("#modal_text_replacement_warning").hide();
    $("#modal_text_soh_warning").show();

    $("#health_modal").modal('show');
}

function are_health_questions_valid() {
    // Will need much better code here in general
    //  should be able to highlight buttons that were missed or something

    // this one can be yes or no
    if (ui.should_show_other_insurance_questions() &&
        ui.is_in_person_application() &&
        general_questions_by_id['existing_insurance'].get_val() === null) {
        //el = $(general_questions_by_id['existing_insurance'].buttons[0].elements[0]);
        return false;
    }

    if (ui.should_show_other_insurance_questions()
        && (
            !ui.insurance_product.is_fpp_product()
            && general_questions_by_id['replace_insurance'].get_val() != "No"
            ) ||
            (
             ui.insurance_product.is_fpp_product()
             && general_questions_by_id['replace_insurance'].get_val() === null
            )
        ) {
        //el = $(general_questions_by_id['existing_insurance'].buttons[0].elements[0]);
        return false;
    }

    // fpp form
    if (ui.insurance_product.is_fpp_product()) {
        if (ui.is_employee_actively_at_work() === null) {
            return false;
        }
    }


    var valid = true;

    $.each(window.ui.selected_plan().get_all_covered_people(), function() {
        var covered_person = this;
        $.each(covered_person.health_questions(), function() {
            if (this.button_group()
                    && this.button_group().is_required()
                    && (
                        // If a no-op question, but still required, must select yes or no.
                        (this.question.is_ignored
                         && this.button_group().get_val() === null
                        )
                        // If this is required, must be no.
                        || (!this.question.is_ignored
                            && this.button_group().get_val() !== "No"
                        )
                    )) {
                valid = false;
                // break
                return false;
            }
        });
        if (!valid) {
            // break
            return false;
        }
    });

    return valid;
}


function format_face_value(val) {
    if (val == null) {
        return "(NA)";
    }
    return "$"+numberWithCommas(val);
}

function format_premium_value(val) {
    if (val == null) {
        return "(NA)";
    }
    return "$"+numberWithCommas(val.toFixed(2));
}



function handle_remote_error() {
    alert("Sorry, an error occurred communicating with the server.");
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



function init_validation() {
    $(document).on('change', 'input:radio[id^="eeOwner-"]', function () {
        var other = $('#eeOtherOwner');
        var inp = $('#eeOtherOwnerName').get(0);
        var inpss = $('#eeOtherOwnerSSN').get(0);

        if (other.hasClass('hide')) {
            other.fadeIn("medium");
            other.removeClass('hide');
            inp.removeAttribute('disabled');
            inp.placeholder = "Full Name";
            inpss.removeAttribute('disabled');
        } else {
            $('#eeOtherOwner').addClass('hide');
            inp.setAttribute('disabled', 'disabled');
            inp.placeholder = "employee is policy owner";
            inp.value = "";
            inpss.setAttribute('disabled', 'disabled');
            inpss.value = "";
        }
    });

    $(document).on('change', 'input:radio[id^="spOwner-"]', function () {
        var other = $('#spOtherOwner');
        var inp = $('#spOtherOwnerName').get(0);
        var inpss = $('#spOtherOwnerSSN').get(0);

        if (other.hasClass('hide') && $('#spOwner-other').prop('checked')) {
            other.fadeIn("medium");
            other.removeClass('hide');
            inp.removeAttribute('disabled');
            inp.placeholder = "Full Name";
            inpss.removeAttribute('disabled');
        } else {
            $('#spOtherOwner').addClass('hide');
            inp.setAttribute('disabled', 'disabled');
            inp.placeholder = "employee/spouse is policy owner";
            inp.value = "";
            inpss.setAttribute('disabled', 'disabled');
            inpss.value = "";
        }
    });



    $('[data-rel=tooltip]').tooltip();

    var validation_debug = false;
    $('#fuelux-wizard').ace_wizard().on('change', function (e, info) {
        if (validation_debug) {
            return true;
        }

        if (info.step == 1) {

            // Clear step2 health question error when we attempt to validate step 1
            $("#health_questions_error").html("");

            // Check for "Decline all coverage" and bail out of the wizard if it is checked
            if (ui.did_decline()) {
                submit_decline();
                return false;
            }

            // trigger jquery validation
            var is_valid = window.ui.validator.form();

            if (!window.ui.is_form_valid()) {

                window.ui.show_no_selection_error();
                return false;
            }

            var current_product_id = ui.insurance_product.product_data.id;
            var plan = ui.selected_plan();

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


            // Check for adding on too much coverage
            _.each(plan.get_covered_applicants_with_type(), function(val) {
                var applicant_type = {"Employee":"employee", "Spouse": "spouse", "Child": "children"}[val.type];
                var can_apply = validate_coverage_amount(val.applicant, applicant_type, val.coverage);
                if (!can_apply) {
                    is_valid = false;
                }
            });


            return is_valid;
        }
        if (info.step == 2 && info.direction == 'next') {
            var is_valid = true;

            // validate replacement form
            if (ui.insurance_product.is_fpp_product() &&
                    (ui.should_show_replacement_form())) {
                is_valid &= $('#questions-form').valid();
            }

            if (ui.insurance_product.is_fpp_product() &&
                (ui.replacing_insurance() === null || ui.existing_insurance() === null)) {
                // These always need to be answered
                is_valid = false;
            }

            if (ui.insurance_product.is_fpp_product() && ui.is_KY_OR_KS() && ui.replacing_insurance()) {
                // Must stop here for these states, no replacements.
                is_valid = false;
            }
            if (ui.insurance_product.is_fpp_product() && ui.is_self_enroll()
                && (ui.replacement_is_terminating() || ui.replacement_using_funds())) {
                // can't continue as self-enroll with either of these as a yes.
                is_valid = false;
            }

            // validate questions
            is_valid &=  are_health_questions_valid();
            if (!is_valid) {
                $("#health_questions_error").html("Please answer all questions for all applicants.  Invalid responses may prevent you from continuing this online application; if so, please see your agent or enrollment professional.");
                return false;
            } else {
                $("#health_questions_error").html("");
                return true;
            }
        }
        if (info.step == 3 && info.direction == 'next') {
            if (!$('#step3-form').valid()) return false;
        }
        if (info.step == 4 && info.direction == 'next') {
            if (!$('#step4-form').valid()) return false;
        }
        if (info.step == 5 && info.direction == 'next') {
	        var skip_for_now = false;
	        if (skip_for_now) {
                return true;
            }
	        if (!$('#step5-form').valid()) {
                return false;
            }

            if (window.ui.has_contingent_beneficiary_error()) {
                return false;
            }

        }
        if (info.step == 6 && info.direction == 'next') {
            if (!$('#step6-form').valid()) return false;
        }

        return true;

    }).on('finished', function (e) {

	if (!$('#step6-form').valid()) return false;

        // jQuery validator rule should be handling this, but it's not, so force a popup here
        if (window.ui.insurance_product.should_confirm_disclosure_notice()
             && !window.ui.disclaimer_notice_confirmed()
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

        if (window.ui.insurance_product.should_confirm_payroll_deduction()
             && !window.ui.payroll_deductions_confirmed()) {
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

    }).on('stepclick', function (e) {
        return true; //return false;//prevent clicking on steps
    });

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
            /*eeBeneOtherSSN: {
                required: {
                    depends: function(e) {
                        return (
                            ui.is_employee_beneficiary_info_required() &&
                            !ui.employee_other_beneficiary().is_nonperson_entity()
                        );
                    }
                }
            },
            eeBeneOtherDOB: {
                required: {
                    depends: function(e) {
                        return (
                            ui.is_employee_beneficiary_info_required() &&
                            !ui.employee_other_beneficiary().is_nonperson_entity()
                        );
                    }
                }
            },
            */
            eeContBeneOtherName: {
                required: {
                    depends: function(element) {
                        return (
                            ui.insurance_product.should_show_contingent_beneficiary() &&
                            window.ui.employee_contingent_beneficiary_type() === "other"
                        )
                    }
                }
            },
            eeContBeneOtherRelation: {
                required: {
                    depends: function(element) {
                        return (
                            ui.insurance_product.should_show_contingent_beneficiary() &&
                            window.ui.employee_contingent_beneficiary_type() === "other"
                        )
                    }
                }
            },
            /*
            eeContBeneOtherSSN: {
                required: {
                    depends: function(e) {
                        return (
                            ui.insurance_product.should_show_contingent_beneficiary() &&
                            window.ui.employee_contingent_beneficiary_type() === "other" &&
                            !ui.employee_contingent_beneficiary().is_nonperson_entity()
                        );
                    }
                }
            },
            eeContBeneOtherDOB: {
                required: {
                    depends: function(e) {
                        return (
                            ui.insurance_product.should_show_contingent_beneficiary() &&
                            window.ui.employee_contingent_beneficiary_type() === "other" &&
                            !ui.employee_contingent_beneficiary().is_nonperson_entity()
                        );
                    }
                }
            },
            */

            spBeneOtherName: {
                required: {
                    depends: function(element) {
                        return (window.ui.did_select_spouse_coverage() && $("#spBeneOther").is(':checked'))
                    }
                }
            },
            spBeneOtherRelation: {
                required: {
                    depends: function(element) {
                        return (window.ui.did_select_spouse_coverage() && $("#spBeneOther").is(':checked'))
                    }
                }
            },
            /*
            spBeneOtherSSN: {
                required: {
                    depends: function(e) {
                        return (
                            window.ui.did_select_spouse_coverage() &&
                            $("#spBeneOther").is(':checked') &&
                            !ui.spouse_other_beneficiary().is_nonperson_entity()
                        );
                    }
                }
            },
            spBeneOtherDOB: {
                required: {
                    depends: function(e) {
                        return (
                            window.ui.did_select_spouse_coverage() &&
                            $("#spBeneOther").is(':checked') &&
                            !ui.spouse_other_beneficiary().is_nonperson_entity()
                        );
                    }
                }
            },
            */
            spContBeneOtherName: {
                required: {
                    depends: function(element) {
                        return (
                            ui.insurance_product.should_show_contingent_beneficiary() &&
                            ui.did_select_spouse_coverage() &&
                            ui.spouse_contingent_beneficiary_type() === "other"
                        );
                    }
                }
            },

            spContBeneOtherRelation: {
                required: {
                    depends: function(element) {
                        return (
                            ui.insurance_product.should_show_contingent_beneficiary() &&
                            ui.did_select_spouse_coverage() &&
                            ui.spouse_contingent_beneficiary_type() === "other"
                        );
                    }
                }
            },
            /*
            spContBeneOtherSSN: {
                required: {
                    depends: function(e) {
                        return (
                            ui.insurance_product.should_show_contingent_beneficiary() &&
                            ui.did_select_spouse_coverage() &&
                            ui.spouse_contingent_beneficiary_type() === "other" &&
                            !ui.spouse_contingent_beneficiary().is_nonperson_entity()
                        );
                    }
                }
            },
            spContBeneOtherDOB: {
                required: {
                    depends: function(e) {
                        return (
                            ui.insurance_product.should_show_contingent_beneficiary() &&
                            ui.did_select_spouse_coverage() &&
                            ui.spouse_contingent_beneficiary_type() === "other" &&
                            !ui.spouse_contingent_beneficiary().is_nonperson_entity()
                        );
                    }
                }
            }
            */
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
	    /* required: function(element) {
		    if (ui.is_in_person_application()) {
			return true;
		    } else {
			return false;
		    }
		}
	    },
	    */
	    ConfirmationToken: {required: true}
        },

        messages: {
            confirmDisclaimer: "please acknowledge that you have received the notice",
	    enrollCity: "required",
	    tokenType: "required",
            ConfirmationToken: "required"
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


// Simple container for replacement policy details
function ReplacementPolicy() {
    var self = this;

    self.name = ko.observable('');
    self.policy_number = ko.observable('');
    self.insured = ko.observable('');
    self.replaced_or_financing = ko.observable(null);
    self.replacement_reason = ko.observable("");

    self.serialize = function() {
        return {
            name: self.name(),
            policy_number: self.policy_number(),
            insured: self.insured(),
            replaced_or_financing: self.replaced_or_financing(),
            replacement_reason: self.replacement_reason()
        };
    };
}

// This code was adding group riders to the auto-selected riders. Will need this functionality when we add them back in.
//
//function init_riders(riders) {
//  for(var i = 0; i<riders.length; i++) {
//    var rider = riders[i];
//    if (!rider.enrollment_level) {
//      window.ui.selected_riders["emp"](riders[i]);
//      window.ui.selected_riders["sp"](riders[i]);
//    }
//  }
//}

var cycleStringify = function(obj) {
  seen = [];
  return JSON.stringify(obj, function(key, val) {
    if (val != null && typeof val == "object") {
      if (seen.indexOf(val) >= 0) {
        return;
      }
      seen.push(val);
    }
    return val;
  });
}
