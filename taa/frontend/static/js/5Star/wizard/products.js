var wizard_products = (function () {

// Factory function for product data passed to wizard. Instantiates the correct product classes for each
//  serialized product.
function build_products(root, products) {
  if (products.length == 0) {
    alert("Error: No products to enroll.");
    return null;
  }

  return _.map(products, function (product) {
    return build_product(root, product);
  })
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
  } else {
    // default product?
    alert("Invalid product type '" + base_type + "'");
    base_product = new FPPTIProduct(product_data);
  }

  // Check if this is a Guaranteed Issue product
  if (product_data.is_guaranteed_issue) {
    return new GIProductDecorator(base_product, product_data);
  } else {
    return base_product;
  }
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

  all_coverage_options: {
    employee: ko.observableArray([]),
    spouse: ko.observableArray([]),
    children: ko.observableArray([])
  },

  find_recommended_coverage_benefit: function (applicant_type, desired_face_value) {
    var benefit = new NullCoverageOption({});
    $.each(this.all_coverage_options[applicant_type](), function () {
      if (this.face_value == desired_face_value) {
        benefit = this;
        return false;
      }
    });
    return benefit;
  },


  get_coverage_options_for_applicant: function (applicant_type) {
    // returns an observable
    return this.all_coverage_options[applicant_type];
  },

  get_maximum_coverage_amount: function (applicant_type) {
    var options = this.get_coverage_options_for_applicant(applicant_type)();
    if (options.length > 0) {
      return _.max(_.pluck(options, "face_value"));
    }
    return 0;
  },

  parse_benefit_options: function (applicant_type, applicant, rates) {
    var self = this;
    var all_options = [new NullCoverageOption()];

    if (rates.bypremium) {
      var by_premium_options = $.map(rates.bypremium, function (rate) {
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
      var by_face_options = $.map(rates.byface, function (rate) {
        return self.get_new_benefit_option({
          is_by_face: true,
          face_value: rate.coverage,
          premium: rate.premium
        });
      });
      // Extends an array with another array
      $.merge(all_options, by_face_options);
    }

    return all_options;
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

  self.update_spouse_coverage_options = function () {
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

      $.each(self.spouse_options_for_demographics(), function () {
        var rate = this;

        if (rate.face_value <= limit) {
          valid_options.push(rate);
        } else {
          if (valid_options.length > 0 &&
              valid_options[valid_options.length - 1].face_value < limit) {
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
    if (!_.find(valid_options, function (opt) {
          return opt.face_value == current_spouse_coverage.face_value;
        })) {
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
    if (!_.find(valid_children_options, function (opt) {
          return opt.face_value == current_child_coverage.face_value;
        })) {
      self.root.selected_plan().children_recommendation(new NullRecommendation(null_child_option));
    }

  };

  self.update_children_coverage_options = function () {
    var emp_benefit = self.root.selected_plan().employee_recommendation().recommended_benefit;


  };

  // overrides the default impl.
  self.get_coverage_options_for_applicant = function (applicant_type) {
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

  // Overrides the default implementation for Group CI behavior.
  self.parse_benefit_options = function (applicant_type, applicant, rates) {
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
        var subscribe_to_question_answers = function (questions) {
          // Subscribe to answers to questions
          _.each(questions, function (soh_answer) {
            soh_answer.answer.subscribe(self.update_spouse_coverage_options);
          });
        };
        applicant.health_questions.subscribe(subscribe_to_question_answers);
        // also invoke it now
        subscribe_to_question_answers(applicant.health_questions());

      }


      // $5,000 to $100,000
      var valid_options = [new NullCoverageOption()];
      var rate_choices = [];
      $.each(rates.byface, function () {
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
      $.each(rates.byface, function () {
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
          [new NullCoverageOption()],
          $.map(demographic_spouse_rates, convert_rate_to_benefit_option)
      ));
      self.all_spouse_rate_options($.merge(
          [new NullCoverageOption()],
          $.map(all_spouse_rates, convert_rate_to_benefit_option)
      ));
      var sp_options = [new NullCoverageOption()];
      $.merge(sp_options, $.map(demographic_spouse_rates, convert_rate_to_benefit_option));
      self.all_coverage_options.spouse(sp_options);
    }
    if (applicant_type == "children" && rates.byface !== undefined) {
      var ch_options = [new NullCoverageOption()];
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
  _.each(_.methods(product), function (method) {
    self[method] = function () {
      return self.product[method].apply(self, arguments);
    }
  });

}

  return {
    build_products: build_products
  };
})();



