

function init_rate_form(data) {
    
    var product;
    if (data.product_id == "FPPTI") {
        product = new FPPTIProduct();
    } else {
        // default product?
        product = new FPPTIProduct();
    }
    
    var ui = new WizardUI(new FPPTIProduct(), data);
    
    // Allow other JS functions to access the ui object
    window.ui = ui;
    
    ko.applyBindings(ui);
}

// Root model of the Benefits wizard User Interface
function WizardUI(product, defaults) {
    var self = this;
    
    self.defaults = defaults;
    self.insurance_product = product;
    
    // Data used on steps 2-5
    self.is_in_person_application = ko.observable(('is_in_person' in defaults)?defaults.is_in_person : true);
    self.was_state_provided = ("state" in defaults);
    self.state = ko.observable(defaults.state || "");
    self.company_name = ko.observable(defaults.company_name || "(Unknown Company)");
    
    self.policy_owner = ko.observable("self");
    self.other_owner_name = ko.observable("");
    self.other_owner_ssn = ko.observable("");
    
    
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
    self.employee = ko.observable(new Beneficiary({
        first: self.defaults.employee_first || "",
        last: self.defaults.employee_last || "",
        email: self.defaults.employee_email || ""
    }));
    
    var MS_MARRIED = 'Married',
        MS_UNMARRIED = 'Unmarried';
    self.marital_statuses = [MS_MARRIED, MS_UNMARRIED];
    self.employee_marital_status = ko.observable(null);
    self.should_show_spouse = ko.computed(function() {
       return self.employee_marital_status() == MS_MARRIED; 
    });
    
    // Spouse info
    self.spouse = ko.observable(new Beneficiary({}));
    self.show_spouse_name = ko.computed(function() {
        return (self.spouse().is_valid()) ? self.spouse().name() : "";
    });
    self.should_include_spouse_in_table = ko.computed(function() {
        return self.should_show_spouse() && self.spouse().is_valid();
    });
    
    // Children
    self.should_include_children = ko.observable(false);
    self.children = ko.observableArray([
        // Start with two blank child entries
        new Beneficiary({}),
        new Beneficiary({})
    ]);
    
    self.show_children_names = ko.computed(function() {
        if (self.should_include_children()) {
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
    self.get_valid_children = ko.computed(function() {
        var children = [];
        $.each(self.children(), function() {
            if (this.is_valid()) {
                children.push(this);
            } 
        });
        return children;
    });
    
    self.add_child = function() {
        var child_beneficiary = new Beneficiary({});
        self.children.push(child_beneficiary);
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
    self.should_include_children_in_table = ko.computed(function() {
        return self.should_include_children() && self.has_valid_children();  
    });
    
    // Store the actual benefits for children here, rather 
    //  than on each child since the benefits are the same
    self.child_benefits = ko.observable(new Beneficiary({}));
    self.get_children_options = ko.computed(function() {
        if (!self.should_include_children_in_table()) {
            return [];
        }
        
        // return the benefit options of the any child
        return self.child_benefits().all_options();
    });
    
    // Recommended and selected benefits
    self.is_recommended_table_visible = ko.observable(false);
    self.can_display_rates_table = ko.computed(function() {
        // TODO: validation
        return true;
    });
    self.show_recommendations_table = function() {
        if (!self.can_display_rates_table()) {
            return;
        }
        self.is_recommended_table_visible(true);
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
            handle_remote_error
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
            employee_first: self.employee().first(),
            employee_last: self.employee().last(),
            employee_birthdate: self.employee().birthdate(),
            spouse_first: self.spouse().first(),
            spouse_last: self.spouse().last(),
            spouse_birthdate: self.spouse().birthdate(),
            num_children: self.children().length
        };
    };
    
    self.recommendations = {
        good: new BenefitsPackage(self, 'Good'),
        better: new BenefitsPackage(self, 'Better'),
        best: new BenefitsPackage(self, 'Best')
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
        self.child_benefits(new Beneficiary({}));
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
        
        // Or, force reselection of plan
        //self.selected_plan(new NullBenefitsPackage());
    };
    
    self.parse_benefit_options = function(beneficiary, rates) {
        if (rates.weekly_byface) {
            beneficiary.benefit_options.by_coverage($.map(rates.weekly_byface, function(rate) {
                return new BenefitOption({
                    is_by_face: true,
                    face_value: rate.coverage,
                    weekly_premium: rate.premium
                });
            }));
        }
        
        if (rates.weekly_bypremium) {
            beneficiary.benefit_options.by_premium($.map(rates.weekly_bypremium, function(rate) {
                return new BenefitOption({
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
    
    self.is_form_valid = function() {
        return (self.selected_plan().is_valid()); 
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
}


function FPPTIProduct() {
    var self = this;
    
    self.product_type = "FPPTI";
    
    self.is_valid_employee_age = function(age) {
        return (age >= 18 && age <= 70);
    };
    
    self.is_valid_spouse_age = function(age) {
        return (age >= 18 && age <= 70);
    };
    
    self.is_valid_child_age = function(age) {
        return (age >= 0 && age <= 23);
    }
}

function Beneficiary(options) {
    var self = this;
    
    self.first = ko.observable(options.first || "");
    self.last = ko.observable(options.last || "");
    self.email = ko.observable(options.email || "");
    self.birthdate = ko.observable(options.birthdate || null);
    self.ssn = ko.observable(options.ssn || "");
    self.gender = ko.observable(options.gender || "");
    
    self.is_valid = ko.computed(function() {
        return (
            $.trim(self.first()) != "" &&
            $.trim(self.last()) != "" &&
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
        var bd = moment(self.birthdate(), "MM/DD/YYYY");
        if (bd.isValid()) {
            return moment().diff(bd, "years");  
        } else {
            return "";
        }
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
        var benefit = new BenefitOption({});
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
        data.ssn = self.ssn();
        data.gender = self.gender();
        
        return data;
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
    
    self.build_recommendation = function(beneficiary, recommended_val) {
        var benefit = self.get_recommended_benefit(beneficiary, recommended_val);
        return new Recommendation(benefit);
    };
    
    self.get_recommended_benefit = function(beneficiary, recommended_val) {
        if (recommended_val == null || recommended_val == "") {
            return new NullBenefitOption();
        } else {
            return beneficiary.find_recommended_coverage_benefit(recommended_val);
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
    
    self.get_all_people = ko.computed(function() {
        var employee = root.employee();
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
    self.get_all_people = function() {return [];}
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
    self.format_weekly_premium_option = function() {
        return self.recommended_benefit.format_weekly_premium_option() + " (each)";
    };
    self.is_valid = function() {
        return true;
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


ko.bindingHandlers.slideDownIf = {
    init: function(element, value_accessor) {
        var val = ko.unwrap(value_accessor());
        $(element).toggle(val);
    },
    update: function(element, value_accessor) {
        // value should be a boolean
        var val = ko.unwrap(value_accessor());
        if (val) {
            $(element).slideDown(400);
        } else {
            $(element).slideUp(400);
        }
    }
};

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

// http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function handle_remote_error() {
    alert("Sorry, an error occurred communicating with the server.");    
}

function ajax_post(url, data, on_success, on_error) {
    $.ajax(url, {
        data: data,
        error: on_error,
        success: on_success,
        // expected return data type
        dataType: "json",
        method: "POST"
    });
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

        if (other.hasClass('hide')) {
            other.fadeIn("medium");
            other.removeClass('hide');
            inp.removeAttribute('disabled');
            inp.placeholder = "Full Name";
            inpss.removeAttribute('disabled');
        } else {
            $('#spOtherOwner').addClass('hide');
            inp.setAttribute('disabled', 'disabled');
            inp.placeholder = "spouse is policy owner";
            inp.value = "";
            inpss.setAttribute('disabled', 'disabled');
            inpss.value = "";
        }
    });
    
    
    
    $('[data-rel=tooltip]').tooltip();

    var validation_debug = true;
    $('#fuelux-wizard').ace_wizard().on('change', function (e, info) {
        if (validation_debug) {
            return true;
        }
        
        if (info.step == 1) {
            if (!window.ui.is_form_valid()) {
                return false;
            } 
        }
        if (info.step == 2) {
            if (!$('#step2-form').valid()) return false;
        }
        
        return true;
        
    }).on('finished', function (e) {
        
        // Pull out all the data we need for docusign 
        var wizard_results = {
            agent_data: window.ui.defaults,
            
            employee: window.ui.employee().serialize_data(),
            spouse: window.ui.spouse().serialize_data(),
            
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
        }
        var sp_benefit = window.ui.selected_plan().spouse_recommendation().recommended_benefit;
        if (sp_benefit.is_valid()) {
            wizard_results['spouse_coverage'] = sp_benefit.serialize_data();
        }
        
        
        // Send to server
        ajax_post("/submit-wizard-data", {"wizard_results": wizard_results}, function(resp) {
            if (resp.error) {
                alert("There was a problem: "+resp.error);
            } else {
                // Docusign redirect
                // location = resp.redirect 
            }
            
        }, handle_remote_error);
        
        bootbox.dialog({
            message: "Thank you! Your information was successfully saved!",
            buttons: {
                "success": {
                    "label": "OK",
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
    
    /*
    $('#step1-form').validate({
        errorElement: 'div',
        errorClass: 'help-block',
        rules: {
            eeBenefitDOB: {
                required: true
            }
        }
    });
    */
    
    $('#step2-form').validate({
        errorElement: 'div',
        errorClass: 'help-block',
        focusInvalid: false,
        rules: {
            email: {
                required: true,
                email: true
            },
            eeFName: {
                required: true
            },
            eeLName: {
                required: true
            },
            phone: {
                required: true,
                phone: 'required'
            },
            comment: {
                required: true
            },
            state: {
                required: true
            },
            gender: 'required',
            agree: 'required'
        },

        messages: {
            email: {
                required: "Please provide a valid email.",
                email: "Please provide a valid email."
            },
            subscription: "Please choose at least one option",
            gender: "Please choose gender",
            agree: "Please confirm your agreement"
        },
        
        highlight: function (e) {
            $(e).closest('.form-group').removeClass('has-info').addClass('has-error');
        },

        success: function (e) {
            $(e).closest('.form-group').removeClass('has-error').addClass('has-info');
            $(e).remove();
        },
        
        errorPlacement: function (error, element) {
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
            else error.insertAfter(element.parent());
        },

        submitHandler: function (form) {
        },
        
        invalidHandler: function (event, validator) { 
            // display error alert on form submit   
            $('.alert-danger', $('.login-form')).show();
        }
        
    });
	
}



