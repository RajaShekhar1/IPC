


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
              throw TypeError('Argument must be an object');
            }
            F.prototype = o;
            return new F();
        };
    })();
}

function init_rate_form(data) {
    
    
    var product = build_product(data.products);
    var ui = new WizardUI(product, data);
    
    ui.build_health_questions(data.health_questions);
    
    // Allow other JS functions to access the ui object
    window.ui = ui;
    
    ko.applyBindings(ui);
    
}

function build_product(products) {
    
    if (products.length == 0) {
        alert("Error: No products to enroll.");
        return null;
    }
    
    // use the first product until multi-product 
    var product_data = products[0];
    
    var base_type = product_data.base_product_type;
    var base_product;
    if (base_type == "FPPTI") {
        base_product = new FPPTIProduct(product_data); 
    } else if (base_type == "FPPCI") {
        base_product = new FPPCIProduct(product_data);
    } else if (base_type == "Group CI") {
        base_product = new GroupCIProduct(product_data);
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
function WizardUI(product, defaults) {
    var self = this;
    
    self.defaults = defaults;
    self.insurance_product = product;
    self.health_questions = ko.observableArray([]);
    
    self.disclaimer_notice_confirmed = ko.observable(false);
    
    
    // Data used on steps 2-5
    self.is_in_person_application = ko.observable('is_in_person' in defaults && defaults.is_in_person);
    self.identityToken = ko.observable("");
    self.identityType = ko.observable("");
    
    self.enrollCity = ko.observable(defaults.enroll_city || "");
    self.enrollState = defaults.state;
    self.was_state_provided = ("state" in defaults && defaults.state !== null && defaults.state != "XX");
    
    self.company_name = ko.observable(defaults.company_name || "(Unknown Company)");
    
    self.existing_insurance = "no";
    self.replacing_insurance = "no";

    self.policy_owner = ko.observable("self");
    self.other_owner_name = ko.observable("");
    self.other_owner_ssn = ko.observable("");
    
    self.spouse_policy_owner = ko.observable("employee");
    self.spouse_other_owner_name = ko.observable("");
    self.spouse_other_owner_ssn = ko.observable("");
    
    
    self.employee_beneficiary = ko.observable("spouse");
    self.spouse_beneficiary = ko.observable("employee");
    self.employee_beneficiary_name = ko.observable("");
    self.employee_beneficiary_relationship = ko.observable("");
    self.employee_beneficiary_ssn = ko.observable("");
    self.employee_beneficiary_dob = ko.observable("");
    
    self.spouse_beneficiary_name = ko.observable("");
    self.spouse_beneficiary_relationship = ko.observable("");
    self.spouse_beneficiary_ssn = ko.observable("");
    self.spouse_beneficiary_dob = ko.observable("");
    
    // Employee 
    self.employee = ko.observable(new InsuredApplicant(self.defaults.employee_data || {}));
    
    // Extended questions for step 1
    self.should_show_gender = function() {return product.requires_gender();};
    self.should_show_height = function() {return product.requires_height();};
    self.should_show_weight = function() {return product.requires_weight();};
    self.should_show_smoker = function() {return product.requires_is_smoker();};
    self.should_show_extended_questions = function() {
        return (
            self.should_show_gender() ||
            self.should_show_height() || 
            self.should_show_weight() ||
            self.should_show_smoker()
        );
    };
    
    // Spouse info
    var spouse_data = self.defaults.spouse_data || {last: self.defaults.employee_last} || {};
    
    self.should_show_spouse = ko.observable((
            spouse_data.first !== undefined && 
            spouse_data.last !== undefined && 
            spouse_data.birthdate !== undefined
    ));
    
    self.spouse = ko.observable(new InsuredApplicant(spouse_data));
    
    self.is_spouse_age_valid = ko.computed(function() {
        return self.insurance_product.is_valid_spouse_age(self.spouse().get_age()) ;
    });
    
    self.should_include_spouse_in_table = ko.computed(function() {
        return self.should_show_spouse() && self.spouse().is_valid() && self.is_spouse_age_valid();
    });
    self.show_spouse_name = ko.computed(function() {
        return (self.should_include_spouse_in_table()) ? self.spouse().name() : "";
    });
    
    // Children
    self.should_include_children = ko.observable(
        self.defaults.children_data.length > 0
    );
    if (self.defaults.children_data.length == 0) {
        self.children = ko.observableArray([
            // Start with two blank child entries
            new InsuredApplicant({last: self.defaults.employee_last || ""}),
            new InsuredApplicant({last: self.defaults.employee_last || ""})
        ]);
    } else {
        self.children = ko.observableArray($.map(self.defaults.children_data, function(child_data) {
            return new InsuredApplicant(child_data);
        }));
    }
    
    self.get_valid_children = ko.computed(function() {
        var children = [];
        $.each(self.children(), function() {
            if (this.is_valid() && self.insurance_product.is_valid_child_age(this.get_age())) {
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
        var child_insured_applicant = new InsuredApplicant({last: self.defaults.employee_last || ""});
        self.children.push(child_insured_applicant);
	// hide Add button if now at max children
	if (self.children().length > 3) {
	    $("#addChildBtn").hide();
	}
        // Re-apply jquery date masks
        $('.input-mask-date').mask('99/99/9999')
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
            if (!self.insurance_product.is_valid_child_age(this.get_age())) {
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
    self.child_benefits = ko.observable(new InsuredApplicant({}));
    self.get_children_options = ko.computed(function() {
        if (!self.should_include_children_in_table()) {
            return [];
        }
        
        // return the benefit options of the any child
        return self.child_benefits().all_options();
    });
    
    // Recommended and selected benefits
    self.is_show_rates_clicked = ko.observable(false);
    
    self.is_employee_age_valid = ko.computed(function() {
        return self.insurance_product.is_valid_employee_age(self.employee().get_age()) ;
    });
    
    
    self.can_display_rates_table = ko.computed(function() {
        // TODO: some of the age constants or other requirements will be determined by the product
        
        // All employee info
        var valid = self.employee().is_valid();
        valid &= self.is_employee_age_valid();
        
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
        self.is_show_rates_clicked(true);
        self.refresh_rate_table();
    };
    
    self.update_rate_table = function() {
        if (!self.is_recommended_table_visible()) {
            return;
        }
        self.refresh_rate_table();
    };
    
    self.refresh_rate_table = function() {
        ajax_post(
            "/get_rates", 
            self.build_rate_parameters(), 
            self.show_updated_rates, 
            handle_remote_error,
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
            product_type: self.insurance_product.product_type,
            employee: self.employee().serialize_data(),
            spouse: self.should_include_spouse_in_table()? self.spouse().serialize_data() : null,
            num_children: self.children().length
        };
    };
    
    self.selected_plan = ko.observable(new NullBenefitsPackage());
    self.selected_recommendation = ko.observable(null);
    
    // When a custom setting is selected
    self.apply_selected_customization = function() {
        var new_plan;
        if (self.selected_recommendation()) {
            // Apply this first
            new_plan = self.get_new_plan_from_recommendations(self.selected_recommendation());
        } else {
            new_plan = new BenefitsPackage(self, "Custom");
        }
        
        if (self.employee().is_valid()) {
            var benefit = self.employee().selected_custom_option();
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
    var watch_data_values = [
        self.employee().birthdate,
        self.spouse().birthdate,
        self.children,
        self.should_include_spouse_in_table,
        self.should_include_children_in_table
    ];
    $.each(watch_data_values, function() {
        this.subscribe(self.update_rate_table);
    });
    
    
    self.show_updated_rates = function(data) {
        self.parse_benefit_options(self.employee(), data.employee_rates);
        self.parse_benefit_options(self.spouse(), data.spouse_rates);
        // Reset child rates
        self.child_benefits(new InsuredApplicant({}));
        self.parse_benefit_options(self.child_benefits(), data.children_rates);
        
        if (data.recommendations) {
            self.recommendations.good.set_recommendations(data.recommendations['good']);
            self.recommendations.better.set_recommendations(data.recommendations['better']);
            self.recommendations.best.set_recommendations(data.recommendations['best']);
        }
        
        // Update selection with new data
        if (self.selected_plan().is_valid()) {
            self.apply_selected_customization();
        }
    };
    
    self.parse_benefit_options = function(applicant, rates) {
        if (rates.weekly_byface) {
            applicant.benefit_options.by_coverage($.map(rates.weekly_byface, function(rate) {
                return self.insurance_product.get_new_benefit_option({
                    is_by_face: true,
                    face_value: rate.coverage,
                    weekly_premium: rate.premium
                });
            }));
        }
        
        if (rates.weekly_bypremium) {
            applicant.benefit_options.by_premium($.map(rates.weekly_bypremium, function(rate) {
                return self.insurance_product.get_new_benefit_option({
                    is_by_face: false,
                    face_value: rate.coverage,
                    weekly_premium: rate.premium
                });
            }));
        }
    };
    
    self.formatted_monthly_premium = ko.computed(function() {
        return self.selected_plan().formatted_monthly_premium();
    });
    
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
    
    function any_valid_spouse_field() {
        return self.should_show_spouse();
        //return self.spouse().any_valid_field();
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
            debug: true
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
    
    self.do_any_health_questions_need_answering = ko.computed(function() {
        if (self.health_questions().length == 0) {
            return false;
        } 
        return _.any(self.health_questions(), function(question) {
            return question.does_any_applicant_need_to_answer();
        })
    });
    
    // Should be invoked once at startup
    self.build_health_questions = function(health_question_data_by_product) {
        var product_data = self.insurance_product.product_data;
        _.each(health_question_data_by_product, function(product_health_questions, product_id) {
            if (product_id == product_data.id) {
                var question_factory;
                if (product_data.is_guaranteed_issue) {
                    question_factory = function(question_data) {
                         return new GIHealthQuestion(question_data, self.selected_plan,
                             product_data.gi_criteria,
                             product_data.statement_of_health_bypass_type, product_data.bypassed_soh_questions);
                    }
                } else {
                    question_factory = function(question_data) {
                        return new StandardHealthQuestion(question_data, self.selected_plan);
                    }
                }
                self.health_questions(_.map(product_health_questions, question_factory));
            } 
        });
        
    };
    
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
    
    is_valid_employee_age: function(age) {
        return (age >= this.min_emp_age() && age <= this.max_emp_age());
    }, 
    
    is_valid_spouse_age: function(age) {
        return (age >= this.min_sp_age() && age <= this.max_sp_age());
    },
    
    is_valid_child_age: function(age) {
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
    has_critical_illness_coverages: function() {return false;}
    
};

function FPPTIProduct(product_data) {
    this.product_type = "FPPTI";
    this.product_data = product_data;
}
// Inherit from product
FPPTIProduct.prototype = Object.create(Product.prototype);

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

function GroupCIProduct(product_data) {
    this.product_type = "Group CI";
    this.product_data = product_data;
}
GroupCIProduct.prototype = Object.create(Product.prototype);
GroupCIProduct.prototype.get_new_benefit_option = function(options) {
    return new CIBenefitOption(new BenefitOption(options));
};
GroupCIProduct.prototype.requires_gender = function() {return true;};
GroupCIProduct.prototype.requires_height = function() {return true;};
GroupCIProduct.prototype.requires_weight = function() {return true;};
GroupCIProduct.prototype.requires_is_smoker = function() {return true;};
GroupCIProduct.prototype.has_critical_illness_coverages = function() {
    return true;
};


// Guaranteed Issue Product decorator
//  Wraps a base product type
function GIProductDecorator(product, product_data) {
    var self = this;
    
    self.product = product;
    self.product_data = product_data;
    self.product_type = product.product_type;
    
    
    // Delegate to self.product by default for methods inherited from Product
    _.each(_.methods(Product.prototype), function(method) {
        self[method] = function() {
            return self.product[method].apply(self, arguments);   
        }
    });
    
    // Overrides
    /*
    self.get_health_questions = function(selected_plan) {
        
        // If we meet the criteria, 
        if (selected_plan.employee_recommendation().recommended_benefit.face_value >= 50000) {
            if (product_data.statement_of_health_bypass_type == 'all') {
                return [];
            } else {
                return self.product.get_health_questions(selected_plan);
            }
        } else {
            return self.product.get_health_questions(selected_plan);
        }
    };
    
    self.does_one_applicant_meet_criteria = function(selected_plan) {
        $.each(product_data.gi_criteria, function() {
            var criteria = this;
            if (self.does_applicant_meet_criteria(applicant, criteria)) {
                
            }
            
        });
    };
    
    self.does_applicant_meet_criteria = function(applicant, criteria) {
        
    };
    */
}


var GlobalSOHQuestion = function(question_text) {
    var self = this;
    self.question_text = question_text;
};
GlobalSOHQuestion.prototype.get_question_text = function() {
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
        return self.selected_plan().did_select_employee_coverage();
    });
    self.does_spouse_need_to_answer = ko.computed(function() {
        return self.selected_plan().did_select_spouse_coverage();
    });
    self.do_children_need_to_answer = ko.computed(function() {
        return self.selected_plan().did_select_children_coverage();
    });
    
    self.show_yes_dialogue_employee = function() {return self.show_yes_dialogue(); };
    self.show_yes_dialogue_spouse = function() {return self.show_yes_dialogue(); };
    self.show_yes_dialogue_children = function() {return self.show_yes_dialogue(); };
    
    
    self.show_yes_dialogue = function() {
        handle_question_yes();  
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
    return true;
};


var GIHealthQuestion = function(question, selected_plan, applicant_criteria, skip_mode, skipped_questions) {
    var self = this;
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
        if (self.has_employee_met_GI_criteria() && self.should_skip_if_GI_criteria_met()) {
            return false;
        }
        
        return true;
    });
    
    self.does_spouse_need_to_answer = ko.computed(function() {
        if (!self.selected_plan()) return false;
        if (!self.selected_plan().did_select_spouse_coverage()) {
            return false;
        } 
        if (self.has_spouse_met_GI_criteria() && self.should_skip_if_GI_criteria_met()) {
            return false;
        }
        
        return true;
    });
    
    self.do_children_need_to_answer = ko.computed(function() {
        if (!self.selected_plan()) return false;
        if (!self.selected_plan().did_select_children_coverage()) {
            return false;
        } 
        if (self.has_child_met_GI_criteria() && self.should_skip_if_GI_criteria_met()) {
            return false;
        }
        
        return true;
    });
    
    
    self.has_employee_met_GI_criteria = ko.computed(function() {
        if (!self.selected_plan().did_select_employee_coverage()) {
            return false;
        } 
        
        var coverage = self.selected_plan().employee_recommendation().recommended_benefit;
        var applicant = self.selected_plan().root.employee();
        var employee_criteria = self.get_criteria("Employee");
        var result = _.any(employee_criteria, function(criterion) {
            return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
        });
        return result;
    });
    
    self.has_spouse_met_GI_criteria = ko.computed(function() {
        if (!self.selected_plan().did_select_spouse_coverage()) {
            return false;
        }
        var coverage = self.selected_plan().spouse_recommendation().recommended_benefit;
        var applicant = self.selected_plan().root.spouse();
        var criteria = self.get_criteria("Spouse");
        var result = _.any(criteria, function(criterion) {
            return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
        });
        return result;
    });
    
    self.has_child_met_GI_criteria = ko.computed(function() {
        if (!self.selected_plan().did_select_children_coverage()) {
            return false;
        }
        var coverage = self.selected_plan().children_recommendation().recommended_benefit;
        var children = self.selected_plan().root.children();
        var criteria = self.get_criteria("Child");
        var result = _.all(children, function(child_applicant) {
            return _.any(criteria, function(criterion) {
                return self.does_applicant_meet_GI_criteria(child_applicant, coverage, criterion);
            });
        });
        
        return result;
    });
    
    self.does_any_applicant_need_to_answer = ko.computed(function() {
        return _.any(self.selected_plan().get_covered_applicants_with_type(), function(data) {
            return self.does_applicant_need_to_answer(data.type, data.applicant);
        });
    });
    
    self.show_yes_dialogue_employee = function() {
          self.show_yes_dialogue('Employee');
    };
    
    self.show_yes_dialogue_spouse = function() {
          self.show_yes_dialogue('Spouse');
    };
    
    self.show_yes_dialogue_children = function() {
        self.show_yes_dialogue('Children');
    };
    
    self.show_yes_dialogue = function(applicant_type) {
        // If the applicant meets the GI, we show the normal 'you must answer no' dialogue since this must be a required question
        if (!self.should_skip_if_GI_criteria_met() || (
            (applicant_type == "Employee" && self.has_employee_met_GI_criteria()) ||
            (applicant_type == "Spouse" && self.has_spouse_met_GI_criteria()) ||
            (applicant_type == "Children" && self.has_child_met_GI_criteria())
            )) {
            
            handle_question_yes();
            return;
        }
        
        // Otherwise, show a special dialogue that gives a few options for continuing 
        var coverage, criteria, applicant;
        if (applicant_type == 'Employee') {
            applicant = self.selected_plan().root.employee();
            coverage = self.selected_plan().employee_recommendation().recommended_benefit;
            criteria = self.get_criteria("Employee");
        } else if (applicant_type == 'Spouse') {
            applicant = self.selected_plan().root.spouse();
            coverage = self.selected_plan().spouse_recommendation().recommended_benefit;
            criteria = self.get_criteria("Spouse");
        } else {
            applicant = self.selected_plan().root.child_benefits();
            coverage = self.selected_plan().children_recommendation().recommended_benefit;
            criteria = self.get_criteria("Child");
        }
        
        
        var face_amount = coverage.format_face_value();
        var gi_amount = _.max(_.map(criteria, function(c) {return c.guarantee_issue_amount}));
        var formatted_gi_amount = format_face_value(gi_amount);
        
        var button_options = {
            reduce: {label: "Reduce the coverage", className: 'btn-success', callback: function() {
                // 
                var max_option = _.max(
                        _.filter(applicant.all_options(), function(o) {
                            return o.face_value <= gi_amount && o.face_value > 0
                        }), 
                        function(o) {return o.face_value}
                );
                applicant.selected_custom_option(max_option);
                self.selected_plan().root.apply_selected_customization();
                
            }},
            remove: {label: "Remove this applicant", className: 'btn-danger', callback: function() {
                var null_option = _.find(applicant.all_options(), function(o) {
                    return o.face_value == 0
                });
                applicant.selected_custom_option(null_option);
                self.selected_plan().root.apply_selected_customization();
            }},
            ignore: {label: "Ignore and Continue", className: 'btn-default', callback: function() {
                // Nothing to do in this case
            }}
        };
        
        bootbox.dialog({
            //title: "SPECIAL GI YES WINDOW: "+applicant_type, 
            message: 'A "yes" response to this question prohibits this person from obtaining the selected '+face_amount+' of coverage. You may proceed, however, by reducing your coverage to the guaranteed coverage amount of '+formatted_gi_amount+'.'+ 
                     '<br><br>Alternatively, you may remove this individual from the coverage selection altogether (in Step 1) before proceeding with the rest of the application.',
            buttons: button_options
        });  
    };
    
    
    self.get_criteria = function(applicant_type) {
        return _.filter(self.applicant_criteria, function(c) {return c.applicant_type == applicant_type;});
    };
    
    self.does_applicant_meet_GI_criteria = function(applicant, coverage, criterion) {
        var does_meet = true;
        
        does_meet &= coverage.face_value <= criterion.guarantee_issue_amount;
        
        if (criterion.age_max !== null) {
            does_meet &= applicant.get_age() < criterion.age_max;         
        }
        if (criterion.age_min !== null) {
            does_meet &= applicant.get_age() > criterion.age_min;
        }
        if (criterion.height_max !== null) {
            does_meet &= applicant.height() !== null && applicant.height() < criterion.height_max
        }
        if (criterion.height_min !== null) {
            does_meet &= applicant.height() !== null && applicant.height() > criterion.height_min;
        }
        if (criterion.weight_max !== null) {
            does_meet &= applicant.weight() !== null && applicant.weight() < criterion.weight_max
        }
        if (criterion.weight_min !== null) {
            does_meet &= applicant.weight() !== null && applicant.weight() > criterion.weight_min;
        }
        
        return does_meet;
    };
};
GIHealthQuestion.prototype = Object.create(StandardHealthQuestion.prototype);

GIHealthQuestion.prototype.does_applicant_need_to_answer = function(applicant_type, applicant) {
    var self = this;
    if (applicant_type == "Employee") {
        return self.does_employee_need_to_answer();
    } else if (applicant_type == "Spouse") {
        return self.does_spouse_need_to_answer();
    } else if (applicant_type == "Child") {
        return self.do_children_need_to_answer();
    }
    console.error("Got unknown applicant type '"+applicant_type+"'");
    return true;
};

GIHealthQuestion.prototype.should_skip_if_GI_criteria_met = function() {
    var self = this;
    if (self.skip_mode == "all") {
        return true;
    } else {
        return _.find(self.skipped_questions, function(q) {
            return self.get_question_label() == q.question_type_label
        });
    }
};



// Main ViewModel for all applicants
var _applicant_count = 0;
function InsuredApplicant(options) {
    var self = this;
    
    // a basic internal id we can use in loops to distinguish applicants 
    self._id = _applicant_count++;
    
    self.first = ko.observable(options.first || "");
    self.last = ko.observable(options.last || "");
    self.email = ko.observable(options.email || "");
    self.phone = ko.observable(options.phone || "");
    self.birthdate = ko.observable(options.birthdate || null);
    self.ssn = ko.observable(options.ssn || "");
    self.gender = ko.observable(options.gender || "");
    
    // Extended questions
    self.height = ko.observable(options.height ? options.height : null);
    self.weight = ko.observable(options.weight || null);
    self.is_smoker = ko.observable(options.is_smoker || null);
    
    self.address1 = ko.observable(options.street_address || "");
    self.address2 = ko.observable(options.street_address2 || "");
    self.city = ko.observable(options.city || "");
    self.state = ko.observable(options.state || "");
    self.zip = ko.observable(options.zip || "");
    
    self.health_questions = {};
    
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
    
    self.find_recommended_coverage_benefit = function(desired_face_value) {
        var benefit = new NullBenefitOption({});
        $.each(self.benefit_options.by_coverage(), function() {
            if (this.face_value == desired_face_value) {
                benefit = this;
                return false;
            } 
        });
        return benefit;
    };
    
    self.update_selected_option = function(data, event) {
        var selected_option = $(event.target).find(":selected")[0];
        var benefit = selected_option['data-item'];
        
        if (benefit) {
            self.selected_custom_option(benefit);
        }
    };
    
    // We need to attach the benefit to each option element for later use 
    //  this is required due to the optionsValue binding
    self.attach_benefit = function(option, item) {
        option['data-item'] = item;
    };
    
    self.selected_custom_option = ko.observable(new NullBenefitOption());
    
    self.selected_coverage = ko.observable(new NullBenefitOption());
    self.display_selected_coverage = ko.computed(function() {
        return self.selected_coverage().format_face_value();
    });
    self.display_weekly_premium = ko.computed(function() {
        return self.selected_coverage().format_weekly_premium();
    });
    self.display_monthly_premium = ko.computed(function() {
        return format_premium_value(
            get_monthly_premium_from_weekly(self.selected_coverage().weekly_premium)
        );
    });
    
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
        data.soh_questions = {};
        $.each(self.health_questions, function(question_text, button_group) {
            data.soh_questions[question_text] = button_group.serialize();
        });
        
        return data;
    }
}

function age_for_date(date) {
    var bd = moment(date, "MM/DD/YYYY");
    if (bd.isValid()) {
        if (bd.isAfter(moment())) {
            // Avoid returning -0 for future dates less than one
            return -1;
        } else {
            // Valid age
            return moment().diff(bd, "years");
        }
    } else {
        // Invalid age
        return "";
    }
}

function BenefitOption(options) {
    var self = this;
    
    self.is_by_face = options.is_by_face;
    self.weekly_premium = options.weekly_premium;
    self.face_value = options.face_value;
    
    self.format_weekly_premium = function() {
        return format_premium_value(self.weekly_premium);
    };
    
    self.format_weekly_premium_option = function() {
        return self.format_weekly_premium() + " weekly";
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
            return self.format_weekly_premium_option();
        }
    };
    self.is_valid = function() {
        return true;
    };
    
    self.serialize_data = function() {
        return {
            weekly_premium: self.weekly_premium,
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
    self.weekly_premium = wrapped_option.weekly_premium;
    self.face_value = wrapped_option.face_value;
    self.format_weekly_premium = wrapped_option.format_weekly_premium;
    self.format_weekly_premium_option = wrapped_option.format_weekly_premium_option;
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
    self.weekly_premium = 0;
    self.face_value = 0;
    
    self.is_valid = function() {
        return false;
    };
    
    self.format_weekly_premium_option = function() {
        return "";
    };
    self.format_weekly_premium = function() {
        
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
            return applicant.find_recommended_coverage_benefit(recommended_val);
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
    
    self.get_total_weekly_premium = ko.computed(function() {
        var benefits = self.get_package_benefits();
        
        // Sum the benefit weekly premiums
        var total = 0.0;
        $.each(benefits, function() {
            if (this.weekly_premium != null) {
                total += this.weekly_premium;
            }
        });
        return total;
    });
    
    self.formatted_total_weekly_premium = ko.computed(function() {
        if (self.get_total_weekly_premium() > 0.0) {
            return format_premium_value(self.get_total_weekly_premium());
        } else {
            return "";
        }
    });
    
    self.get_monthly_premium = function(weekly_premium) {
        return get_monthly_premium_from_weekly(weekly_premium);
    };
    self.get_total_monthly_premium = ko.computed(function() {
        var benefits = self.get_package_benefits();
        var total = 0.0;
        $.each(benefits, function() {
            if (this.weekly_premium != null) {
                total += self.get_monthly_premium(this.weekly_premium);    
            } 
        });
        return total;
    });
    self.formatted_monthly_premium = ko.computed(function() {
        return format_premium_value(self.get_total_monthly_premium());   
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
    
    
    self.get_all_people = ko.computed(function() {
        var employee = root.employee();
        // Make sure selected coverage is set on each of the person objects
        employee.selected_coverage(self.employee_recommendation().recommended_benefit);
        
        var people = [employee];
        
        if (root.should_include_spouse_in_table()) {
            var spouse = root.spouse();
            spouse.selected_coverage(self.spouse_recommendation().recommended_benefit);
            people.push(spouse);
        }
        
        if (root.should_include_children_in_table()) {
            $.each(root.get_valid_children(), function () {
                var child = this;
                child.selected_coverage(self.children_recommendation().recommended_benefit);
                people.push(child);
            });
        }
        return people;
    });
    
    self.get_all_covered_people = ko.computed(function() {
        var people = [];
        
        if (root.did_select_employee_coverage()) {
            var employee = root.employee();
            employee.selected_coverage(self.employee_recommendation().recommended_benefit);
            people.push(employee);
        }
        
        if (root.did_select_spouse_coverage()) {
            var spouse = root.spouse();
            spouse.selected_coverage(self.spouse_recommendation().recommended_benefit);
            people.push(spouse);
        }
        
        if (root.did_select_children_coverage()) {
            $.each(root.get_valid_children(), function () {
                var child = this;
                child.selected_coverage(self.children_recommendation().recommended_benefit);
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
            applicants.push({applicant: root.employee(), type: "Employee"});
        }
        if (root.did_select_spouse_coverage()) {
            applicants.push({applicant: root.spouse(), type: "Spouse"});
        }
        if (root.did_select_children_coverage()) {
            $.each(root.get_valid_children(), function () {
                var child = this;
                applicants.push({applicant: child, type: "Child"});
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
function NullBenefitsPackage() {
    var self = this;
    
    self.employee_recommendation = ko.observable(new NullRecommendation());
    self.spouse_recommendation = ko.observable(new NullRecommendation());
    self.children_recommendation = ko.observable(new NullRecommendation());
    self.get_total_weekly_premium = function() { return null;};
    self.formatted_total_weekly_premium = function() { return "";};
    self.get_total_monthly_premium = function(){return 0;};
    self.formatted_monthly_premium = function() { return "";};
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
    
    
}

function Recommendation(recommended_benefit) {
    var self = this;
    self.recommended_benefit = recommended_benefit;
    
    self.is_valid = function() {
        return true;
    };
    
    self.format_weekly_premium_option = function() {
        return self.recommended_benefit.format_weekly_premium_option()
    };
    self.format_face_value = function() {
        return self.recommended_benefit.format_face_value();
    };
}
function ChildrenRecommendation(recommended_benefit) {
    var self = this;
    self.recommended_benefit = recommended_benefit;
    
    self.is_valid = function() {
        return true;
    };
    self.format_weekly_premium_option = function() {
        return self.recommended_benefit.format_weekly_premium_option() + " (each)";
    };
    self.format_face_value = function() {
        return self.recommended_benefit.format_face_value();
    };
}

function NullRecommendation() {
    var self = this;
    self.recommended_benefit = new NullBenefitOption();
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
    self.selected_btn = null;
    self.is_required = is_required;
    
    self.get_val = function() {
        if (self.selected_btn == null) {
            return false;
        } else {
            return self.selected_btn.val;
        }
    };
    
    self.serialize = function() {
        // question: text,
        // answer: [Yes|No|GI] (GI means it was skipped due to GI)
        var answer = (self.is_required()) ? self.get_val() : "GI";
        return {
            question: self.question.question.question_text,
            answer: answer
        }
    };
    
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
        self.selected_btn = btn;
    };
}
var questions = [];
var general_questions_by_id = {};
ko.bindingHandlers.flagBtn = {
    init: function(element, value_accessor) {
        var val = ko.unwrap(value_accessor());
        
        var btn_group, group_lookup;
        if (val.applicant) {
            group_lookup = val.applicant.health_questions;
        } else {
            group_lookup = general_questions_by_id;
        }
        if (val.question.get_question_text() in group_lookup) {
            btn_group = group_lookup[val.question.get_question_text()];
        } else {
            btn_group = new QuestionButtonGroup(val.question, val.is_required);
            group_lookup[val.question.get_question_text()] = btn_group;
        }
        
        btn_group.add_button(element, val.val, function(el) {
            if (val.highlight == "flag") {
                $(el
                ).prepend('<i class="icon glyphicon glyphicon-flag"></i>'
                ).addClass("btn btn-warning"
                );
            } else if (val.highlight == "checkmark") {
                $(el
                ).prepend('<i class="icon glyphicon glyphicon-ok"></i>'
                ).addClass("btn-success"
                );
            } else if (val.highlight == "stop") {
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
    $("#modal_text_existing_warning_title").show();
    $("#modal_text_replacement_warning_title").hide();
    $("#modal_text_soh_warning_title").hide();

    $("#modal_text_existing_warning").show();
    $("#modal_text_existing_warning_remote").hide();
    $("#modal_text_replacement_warning").hide();
    $("#modal_text_soh_warning").hide();
 
    $("#health_modal").modal('show');
    $("#existing_warning_text").show();
    
    window.ui.existing_insurance = "yes";

}
function handle_existing_insurance_modal_remote() {
    $("#modal_text_existing_warning_title").show();
    $("#modal_text_replacement_warning_title").hide();
    $("#modal_text_soh_warning_title").hide();

    $("#modal_text_existing_warning").hide();
    $("#modal_text_existing_warning_remote").show();
    $("#modal_text_replacement_warning").hide();
    $("#modal_text_soh_warning").hide();
 
    $("#health_modal").modal('show');
    $("#existing_warning_text_remote").show();
} 

function reset_existing_insurance_warning() {
    window.ui.existing_insurance = "no";
    $("#existing_warning_text_remote").hide();
    $("#existing_warning_text").hide();
}

function handle_replacement_insurance_modal() {
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
function reset_replacement_insurance_warning() {
    $("#replacement_warning_text").hide();
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
    
    var el;
    // this one can be yes or no
    if (ui.is_in_person_application() && general_questions_by_id['existing_insurance'].get_val() === null) {
        //el = $(general_questions_by_id['existing_insurance'].buttons[0].elements[0]);
        return false;
    }
    if (!ui.is_in_person_application() && general_questions_by_id['existing_insurance_remote'].get_val() != "No") {
        //el = $(general_questions_by_id['existing_insurance'].buttons[0].elements[0]);
        return false;
    }
    if (general_questions_by_id['replace_insurance'].get_val() != "No") {
        //el = $(general_questions_by_id['existing_insurance'].buttons[0].elements[0]);
        return false;
    }
    var valid = true;
    
    $.each(window.ui.selected_plan().get_all_covered_people(), function() {
        var covered_person = this;
        $.each(covered_person.health_questions, function() {
            if (this.is_required() && this.get_val() != "No") {
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


function get_monthly_premium_from_weekly(weekly_premium) {
    return Math.round((weekly_premium*100 * 52) / 12)/100.0;
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
            
            // trigger jquery validation
            var is_valid = window.ui.validator.form();
                
            if (!window.ui.is_form_valid()) {
                
                window.ui.show_no_selection_error();
                return false;
            } 
            return is_valid;
        }
        if (info.step == 2 && info.direction == 'next') {
            
            // validate questions
            var is_valid =  are_health_questions_valid();
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
	    if (skip_for_now) return true;
	    if (!$('#step5-form').valid()) return false;
        }
        if (info.step == 6 && info.direction == 'next') {
            if (!$('#step6-form').valid()) return false;
        }
        
        return true;
        
    }).on('finished', function (e) {
        
	if (!$('#step6-form').valid()) return false;

	//jQuery validator rule should be handling this, but it's not, so force a popup here
	if (!$("#confirmDisclaimer").is(':checked')) {
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
    
    // Pull out all the data we need for docusign 
    var wizard_results = {
        health_questions: $.map(window.ui.health_questions(), function(q) {return q.question}),
        agent_data: window.ui.defaults,
        enrollCity:  window.ui.enrollCity(),
        enrollState:  window.ui.enrollState,
        product_type: window.ui.insurance_product.product_type,
        
        method: (ui.is_in_person_application()) ? 'in_person': 'self_enroll_email',
        
        identityToken: window.ui.identityToken(),
        identityType: window.ui.identityType(),
            
        employee: window.ui.employee().serialize_data(),
        spouse: window.ui.spouse().serialize_data(),
        
        existing_insurance:  window.ui.existing_insurance,
        replacing_insurance:  window.ui.replacing_insurance,
        
        employee_owner:  window.ui.policy_owner(),
        employee_other_owner_name:  window.ui.other_owner_name(),
        employee_other_owner_ssn:  window.ui.other_owner_ssn(),
        spouse_owner:  window.ui.spouse_policy_owner(),
        spouse_other_owner_name:  window.ui.spouse_other_owner_name(),
        spouse_other_owner_ssn:  window.ui.spouse_other_owner_ssn(),
    
        employee_beneficiary:  window.ui.employee_beneficiary(),
        spouse_beneficiary:  window.ui.spouse_beneficiary(),
        employee_beneficiary_name:  window.ui.employee_beneficiary_name(),
        employee_beneficiary_relationship:  window.ui.employee_beneficiary_relationship(),
        employee_beneficiary_ssn:  window.ui.employee_beneficiary_ssn(),
        employee_beneficiary_dob:  window.ui.employee_beneficiary_dob(),
        
        spouse_beneficiary_name:  window.ui.spouse_beneficiary_name(),
        spouse_beneficiary_relationship:  window.ui.spouse_beneficiary_relationship(),
        spouse_beneficiary_ssn:  window.ui.spouse_beneficiary_ssn(),
        spouse_beneficiary_dob:  window.ui.spouse_beneficiary_dob()
    };

	if (!window.ui.should_include_spouse_in_table()) {
	    wizard_results['employee_beneficiary'] = "other";
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
        
        // Benefits
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
        
        // Send to 'listener' for debugging
        /*
	  ajax_post("http://requestb.in/1l091cx1", {"wizard_results": wizard_results}, function(resp) {
            alert("Just sent to requestb.in");            
        }, handle_remote_error, true);
        */

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
        
    }).on('stepclick', function (e) {
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

    $('#step3-form').validate({
        errorElement: 'div',
        errorClass: 'help-block',
        focusInvalid: false,
        rules: {
            email: {email: true},
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
            spssn: "spouse SSN required if owner of policy",
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
                    depends: function(element) {
                        return (!window.ui.did_select_spouse_coverage() || $("#eeBeneOther").is(':checked'))
                    }   
                }
            },
            eeBeneOtherRelation: {
                required: {
                    depends: function(element) {
                        return (!window.ui.did_select_spouse_coverage() || $("#eeBeneOther").is(':checked'))
                    }
                }
            },
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
            }
        },

        messages: {
            eeBeneOtherName: "required",
	    eeBeneOtherRelation: "required",
            spBeneOtherName: "required",
	    spBeneOtherRelation: "required"
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
    } 
    else error.insertAfter(element);
    //else error.insertAfter(element.parent());
}

