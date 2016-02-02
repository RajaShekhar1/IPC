var wizard_products = (function () {

// Factory function for product data passed to wizard. Instantiates the correct product classes for each
//  serialized product.
function build_products(root, products) {
  if (products.length == 0) {
    alert("Error: No products to enroll.");
    return null;
  }

  var product_objects =  _.map(products, function (product) {
    return build_product(root, product);
  });

  // Filter out products that don't show up on the wizard.
  return _.filter(product_objects, function(product) {
    return product !== null;
  });
}

function build_product(root, product_data) {

  var base_type = product_data.base_product_type;
  var base_product;
  if (base_type === "FPPTI") {
    base_product = new FPPTIProduct(product_data);
  } else if (base_type === "FPPCI") {
    base_product = new FPPCIProduct(product_data);
  } else if (base_type === "Group CI") {
    base_product = new GroupCIProduct(root, product_data);
  } else if (base_type === "FPP-Gov") {
    base_product = new FPPGovProduct(product_data);
  } else if (base_type === "ACC" || base_type === "HI") {
    return null;
  } else {
    // default product?
    alert("Invalid product type '" + base_type + "'");
    base_product = new FPPTIProduct(product_data);
  }

  // Check if this is a Guaranteed Issue product
  //if (product_data.is_guaranteed_issue) {
  //  return new GIProductDecorator(base_product, product_data);
  //} else {
    return base_product;
  //}
}


// Model for different insurance products
// Product is abstract base class
function Product() {
}
Product.prototype = {

  // Override if necessary

  min_emp_age: function () {
    return 18
  },
  max_emp_age: function () {
    return 70
  },

  min_sp_age: function () {
    return 18
  },
  max_sp_age: function () {
    return 70
  },

  min_child_age: function () {
    return 0
  },
  max_child_age: function () {
    return 23
  },

  is_valid_employee: function (employee) {
    // Only age matters for most products
    var age = employee.get_age();
    return (age >= this.min_emp_age() && age <= this.max_emp_age());
  },

  is_valid_spouse: function (spouse) {
    var age = spouse.get_age();
    return (age >= this.min_sp_age() && age <= this.max_sp_age());
  },

  is_valid_child: function (child) {
    var age = child.get_age();
    return (age >= this.min_child_age() && age <= this.max_child_age());
  },

  // Allow the details of the benefit's face value, display to be based on the product
  //get_new_benefit_option: function (options) {
  //  return new BenefitOption(options);
  //},

  create_coverage_option: function(options) {
    // Some products (CI for now) will override this.
    return new CoverageOption(options);
  },

  requires_gender: function () {
    return false;
  },
  requires_height: function () {
    return false;
  },
  requires_weight: function () {
    return false;
  },
  requires_is_smoker: function () {
    return false;
  },

  // SOH questions
  has_critical_illness_coverages: function () {
    return false;
  },

  get_maximum_coverage_amount: function (applicant) {

    var options = product_rates_service.get_product_coverage_options_for_applicant(this, applicant)();
    if (options.length > 0) {
      return _.max(_.pluck(options, "face_value"));
    }
    return 0;
  },

  does_override_rate_options: function() {
    return false;
  },

  should_show_other_insurance_questions: function() {
    // should we show replacement questions
    // right now it is anything that isn't group CI, which is all FPP products.
    return this.is_fpp_product();
  },

  should_show_step_5: function () {
    return true;
  },

  should_use_date_of_hire_for_identity: function () {
    // Right now all FPP products will use this.
    return true;
  },

  is_fpp_product: function () {
    // Returns true if this product falls into the class of Family Protection Plan products
    return true;
  },

  should_confirm_disclosure_notice: function () {
    // Accelerated benefit disclosure notice checkbox is for FPP plans.
    return this.is_fpp_product();
  },

  should_confirm_payroll_deduction: function () {
    // Payroll deduction agree checkbox on new FPP form.
    return this.is_fpp_product();
  },

  should_show_contingent_beneficiary: function () {
    // Just new FPP form for now
    return this.is_fpp_product();
  },

  get_replacement_paragraphs: function () {
    return [];
  },

  does_use_recommended_coverage_table: function() {
    // Most products use this format for coverage selection.
    return true;
  }

};

function ApplicantSelectionProduct(product_data) {
  this.product_type = product_data.base_product_type;
  this.product_data = product_data;
}
ApplicantSelectionProduct.prototype = {
  does_use_recommended_coverage_table: function() {
    // Don't use recommended coverage selection for these types of products.
    return false;
  }
};



function FPPTIProduct(product_data) {
  this.product_type = "FPPTI";
  this.product_data = product_data;
}
// Inherit from product
FPPTIProduct.prototype = Object.create(Product.prototype);

FPPTIProduct.prototype.get_replacement_paragraphs = function () {
  return this.product_data.replacement_paragraphs;
};

function FPPCIProduct(product_data) {
  this.product_type = "FPPCI";
  this.product_data = product_data;
}
// Inherit from product
FPPCIProduct.prototype = Object.create(Product.prototype);
FPPCIProduct.prototype.create_coverage_option = function (options) {
  return new CICoverageOption(new CoverageOption(options));
};
FPPCIProduct.prototype.has_critical_illness_coverages = function () {
  return true;
};
FPPCIProduct.prototype.get_replacement_paragraphs = function () {
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

  self.does_override_rate_options = function(applicant_type) {
    // Group CI has to filter out some of the options returned by the rates module.
    return (
      applicant_type === wizard_applicant.Applicant.EmployeeType ||
      applicant_type === wizard_applicant.Applicant.SpouseType
    );
  };



  self.filter_coverage_options_for_applicant_type = function(all_options, applicant_type) {
    if (applicant_type === wizard_applicant.Applicant.EmployeeType) {
      return self.filter_employee_coverage_options(all_options);
    } else if (applicant_type === wizard_applicant.Applicant.SpouseType) {
      return self.filter_spouse_coverage_options(all_options);
    } else {
      return self.filter_children_coverage_options(all_options);
    }
  };

  self.filter_employee_coverage_options = function(all_options) {
    // Basic options for employee, every 5k up to 100k.
    return self.filter_base_rate_options(all_options);
  };

  function get_employee_total_cumulative_coverage() {
    // FIXME: access to global window vm in this function.

    var emp_coverage_option = window.vm.coverage_vm.get_applicant_coverage_option_for_product(window.vm.employee(), self);
    var emp_existing_coverage_amount = window.vm.employee().get_existing_coverage_amount_for_product(self);
    var emp_current_coverage_amount = (emp_coverage_option.is_valid() ? emp_coverage_option.face_value : 0);
    return emp_existing_coverage_amount + emp_current_coverage_amount;
  }

  function has_employee_selected_coverage() {
    // FIXME: access to global window vm in this function.

    var applicant_coverage = window.vm.coverage_vm.get_applicant_coverage_for_product(window.vm.employee(), self);
    return applicant_coverage.has_selected_coverage();
  }

  function has_employee_answered_required_question_yes() {
    // TODO: Move this check to validate step?
    //self.root.employee().has_answered_any_question_yes();
    return false;
  }

  self.filter_spouse_coverage_options = function(all_options) {
    // Filter spouse options based on employee selection.

    // FIXME: access to global window vm in this function.

    // Get the 5,000 to 100,000 coverage options to start with.
    var base_options = self.filter_base_rate_options(all_options);

    // Limit to 50% of employee's current selection, or 25k max.
    // BUT, If the employee has answered yes to any required question, we should not limit the dependent coverage options.
    if (!has_employee_selected_coverage() || has_employee_answered_required_question_yes()) {
      // All base options up to and including 25k.
      return _.filter(base_options, function(o) {return o.face_value <= 25000;});
    } else {
      // Cap at 25k or half the employee total applied coverage.
      var limit = _.min([get_employee_total_cumulative_coverage() / 2.0, 25000]);
      var valid_options = _.filter(base_options, function(o) {return o.face_value <= limit});
      self.append_halfway_coverage_option_if_needed(valid_options, limit, all_options);
      return valid_options;
    }
  };

  self.append_halfway_coverage_option_if_needed = function(valid_options, max_coverage_limit, all_options) {
    // Add an additional option if possible that is exactly 1/2 the employee selected coverage
    //  if there isn't one already.

    if (valid_options.length && valid_options[valid_options.length - 1].face_value < max_coverage_limit) {
      var half_opt = _.find(all_options, function (o) {
        return o.face_value === max_coverage_limit
      });
      if (half_opt) {
        valid_options.push(half_opt);
      }
    }
  };

  self.filter_children_coverage_options = function(all_options) {
    // Similar to spouse filtering above, but limited to 10k and no in-between option appended.
    
    // Get the 5,000 to 100,000 coverage options to start with.
    var base_options = self.filter_base_rate_options(all_options);

    // Limit to 50% of employee's current selection, or 10K max.
    // BUT, If the employee has answered yes to any required question, we should not limit the dependent coverage options.
    if (!has_employee_selected_coverage() || has_employee_answered_required_question_yes()) {
      // All base options up to and including 10K.
      return _.filter(base_options, function(o) {return o.face_value <= 10000;});
    } else {
      // Cap at 10K or half the employee total applied coverage. Do not add in-between option like for spouse.
      var limit = _.min([get_employee_total_cumulative_coverage() / 2.0, 10000]);
      return _.filter(base_options, function(o) {return o.face_value <= limit});
    }
  };

  self.filter_base_rate_options = function(all_options) {
    // Usually just present $5,000 to $100,000 in $5000 increments.
    return _.filter(all_options, function(o) {return o.face_value % 5000 === 0;});
  };


  //
  //self.update_spouse_coverage_options = function () {
  //  var emp_benefit = self.root.selected_plan().employee_recommendation().recommended_benefit;
  //
  //  // Triggered whenever the employee's selected coverage changes
  //  var valid_options = [];
  //
  //  // If the employee has answered yes to any question, we have no limits
  //  var anyYesQuestions = self.root.employee().has_answered_any_question_yes();
  //
  //  var null_spouse_option = self.spouse_options_for_demographics()[0];
  //
  //  // Limit to 50% of employee's current selection, or 25k max
  //  if (!anyYesQuestions && (!emp_benefit || !emp_benefit.is_valid())) {
  //    // Empty list
  //    // push the NullBenefitOption
  //    valid_options.push(null_spouse_option);
  //  } else if (anyYesQuestions || emp_benefit.face_value >= 50000) {
  //    // add all the options
  //    $.merge(valid_options, self.spouse_options_for_demographics());
  //  } else {
  //    // Cap at 25k or half the employee rate
  //    var limit = emp_benefit.face_value / 2.0;
  //
  //    $.each(self.spouse_options_for_demographics(), function () {
  //      var rate = this;
  //
  //      if (rate.face_value <= limit) {
  //        valid_options.push(rate);
  //      } else {
  //        if (valid_options.length > 0 &&
  //            valid_options[valid_options.length - 1].face_value < limit) {
  //          // Append the exact limit to the rate options
  //          var all_options_by_rate = _.indexBy(self.all_spouse_rate_options(), "face_value");
  //          if (all_options_by_rate[limit]) {
  //            valid_options.push(all_options_by_rate[limit]);
  //          }
  //        }
  //        // Break out of the loop, we've hit the limit
  //        return false;
  //      }
  //    });
  //  }
  //
  //  self.all_coverage_options.spouse(valid_options);
  //
  //  // Need to make sure the actual coverage set for the spouse is not outside the custom options
  //  var current_spouse_coverage = self.root.selected_plan().spouse_recommendation().recommended_benefit;
  //  if (!_.find(valid_options, function (opt) {
  //        return opt.face_value == current_spouse_coverage.face_value;
  //      })) {
  //    self.root.selected_plan().spouse_recommendation(new NullRecommendation(null_spouse_option));
  //  }
  //
  //  // Update the children options too
  //  // Triggered whenever the employee's selected coverage changes
  //  var valid_children_options = [];
  //  var null_child_option = self.all_posible_children_options()[0];
  //
  //  // Cannot select coverage if the employee has not selected coverage
  //  if (!anyYesQuestions && (!emp_benefit || !emp_benefit.is_valid())) {
  //    // push the NullBenefitOption
  //    valid_children_options.push(null_child_option);
  //  } else if (anyYesQuestions || emp_benefit.face_value >= 10000) {
  //    // add all the options
  //    $.merge(valid_children_options, self.all_posible_children_options());
  //  } else {
  //    // need to exclude children when emp coverage is less than 10000, do nothing.
  //    // push the NullBenefitOption
  //    valid_children_options.push(null_child_option);
  //  }
  //
  //  self.all_coverage_options.children(valid_children_options);
  //
  //  // Need to make sure the actual coverage set for the spouse is not outside the custom options
  //  var current_child_coverage = self.root.selected_plan().children_recommendation().recommended_benefit;
  //  if (!_.find(valid_children_options, function (opt) {
  //        return opt.face_value == current_child_coverage.face_value;
  //      })) {
  //    self.root.selected_plan().children_recommendation(new NullRecommendation(null_child_option));
  //  }
  //
  //};
  //
  //self.update_children_coverage_options = function () {
  //  var emp_benefit = self.root.selected_plan().employee_recommendation().recommended_benefit;
  //
  //
  //};


  // Overrides the default implementation for Group CI behavior.
  //self.parse_benefit_options = function (applicant_type, applicant, rates) {
  //  // need to limit spouse benefit options to half the employee's currently selected option.
  //  var self = this;
  //
  //  if (applicant_type == "employee") {
  //    // Make sure we have a reference to the employee's currently selected option
  //
  //    if (self.employee_current_benefit_subscription === null) {
  //      // Should only happen once, the first time rates are called
  //      self.employee_current_benefit_subscription = applicant.selected_plan.subscribe(
  //          self.update_spouse_coverage_options
  //      );
  //
  //      // Also subscribe to changes to the yes/no questions for employee
  //      var subscribe_to_question_answers = function (questions) {
  //        // Subscribe to answers to questions
  //        _.each(questions, function (soh_answer) {
  //          soh_answer.answer.subscribe(self.update_spouse_coverage_options);
  //        });
  //      };
  //      applicant.health_questions.subscribe(subscribe_to_question_answers);
  //      // also invoke it now
  //      subscribe_to_question_answers(applicant.health_questions());
  //
  //    }
  //
  //
  //    // $5,000 to $100,000
  //    var valid_options = [new NullCoverageOption()];
  //    var rate_choices = [];
  //    $.each(rates.byface, function () {
  //      var rate = this;
  //      if (rate.coverage % 5000 == 0) {
  //        rate_choices.push(rate);
  //      }
  //    });
  //    $.merge(valid_options, $.map(rate_choices, convert_rate_to_benefit_option));
  //    self.all_coverage_options[applicant_type](valid_options);
  //  }
  //  if (applicant_type == "spouse" && rates.byface !== undefined) {
  //    // $5,000 increments up to 50% of employee's current selection, 25k max
  //    // First, just get rates up to 25k or age limit
  //    //  the employee selection limit is handled in the computed function above
  //    var demographic_spouse_rates = [];
  //    var all_spouse_rates = [];
  //    $.each(rates.byface, function () {
  //      var rate = this;
  //      if (rate.coverage % 5000 == 0 && rate.coverage <= 25000) {
  //        demographic_spouse_rates.push(rate);
  //      }
  //
  //      // Collect all valid half-way points too in a different list
  //      if (rate.coverage <= 25000) {
  //        all_spouse_rates.push(rate);
  //      }
  //    });
  //    self.spouse_options_for_demographics($.merge(
  //        [new NullCoverageOption()],
  //        $.map(demographic_spouse_rates, convert_rate_to_benefit_option)
  //    ));
  //    self.all_spouse_rate_options($.merge(
  //        [new NullCoverageOption()],
  //        $.map(all_spouse_rates, convert_rate_to_benefit_option)
  //    ));
  //    var sp_options = [new NullCoverageOption()];
  //    $.merge(sp_options, $.map(demographic_spouse_rates, convert_rate_to_benefit_option));
  //    self.all_coverage_options.spouse(sp_options);
  //  }
  //  if (applicant_type == "children" && rates.byface !== undefined) {
  //    var ch_options = [new NullCoverageOption()];
  //    $.merge(ch_options, $.map(rates.byface, convert_rate_to_benefit_option));
  //    self.all_posible_children_options(ch_options);
  //    self.all_coverage_options.children(ch_options);
  //  }
  //};


}
GroupCIProduct.prototype = Object.create(Product.prototype);

GroupCIProduct.prototype.is_valid_employee = function (employee) {
  // Need to validate age and is_smoker is valid
  var age = employee.get_age();
  var valid = (age >= this.min_emp_age() && age <= this.max_emp_age());
  valid &= employee.is_smoker() != null;
  valid &= employee.has_valid_weight();
  valid &= employee.has_valid_height();
  valid &= employee.has_valid_gender();

  return valid;
};

GroupCIProduct.prototype.is_valid_spouse = function (spouse) {
  var age = spouse.get_age();
  var valid = (age >= this.min_sp_age() && age <= this.max_sp_age());
  valid &= spouse.is_smoker() != null;
  valid &= spouse.has_valid_weight();
  valid &= spouse.has_valid_height();
  valid &= spouse.has_valid_gender();
  return valid;
};

GroupCIProduct.prototype.requires_gender = function () {
  return true;
};
GroupCIProduct.prototype.requires_height = function () {
  return true;
};
GroupCIProduct.prototype.requires_weight = function () {
  return true;
};
GroupCIProduct.prototype.requires_is_smoker = function () {
  return true;
};
GroupCIProduct.prototype.has_critical_illness_coverages = function () {
  return true;
};
GroupCIProduct.prototype.should_show_step_5 = function () {
  return false;
};

GroupCIProduct.prototype.should_use_date_of_hire_for_identity = function () {
  // Not an FPP product, use normal identity options.
  return false;
};
GroupCIProduct.prototype.is_fpp_product = function () {
  // Returns true if this product falls into the class of Family Protection Plan products
  return false;
};


// FPP Gov
function FPPGovProduct(product_data) {
  this.product_type = "FPP-Gov";
  this.product_data = product_data;
}
FPPGovProduct.prototype = Object.create(Product.prototype);
FPPGovProduct.prototype.requires_gender = function () {
  return false;
};
FPPGovProduct.prototype.requires_height = function () {
  return false;
};
FPPGovProduct.prototype.requires_weight = function () {
  return false;
};
FPPGovProduct.prototype.requires_is_smoker = function () {
  return false;
};
FPPGovProduct.prototype.has_critical_illness_coverages = function () {
  return false;
};
FPPGovProduct.prototype.get_replacement_paragraphs = function () {
  return this.product_data.replacement_paragraphs;
};
//
//
//// Guaranteed Issue Product decorator
////  Wraps a base product type
//function GIProductDecorator(product, product_data) {
//  var self = this;
//
//  self.product = product;
//  self.product_data = product_data;
//  self.product_type = product.product_type;
//
//  // Defined all of product's methods on self and delegate to self.product by default
//  _.each(_.methods(product), function (method) {
//    self[method] = function () {
//      return self.product[method].apply(self, arguments);
//    }
//  });
//
//}
//
  return {
    build_products: build_products
  };
})();



