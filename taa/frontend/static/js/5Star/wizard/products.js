var wizard_products = (function () {

// Factory function for product data passed to wizard. Instantiates the correct product classes for each
//  serialized product.
  function build_products(root, products) {
    'use strict';
    if (products.length === 0) {
      alert("Error: No products to enroll.");
      return null;
    }

    var product_objects = _.map(products, function (product) {
      return build_product(root, product);
    });

    // Filter out products that don't show up on the wizard.
    return _.filter(product_objects, function (product) {
      return product !== null;
    });
  }

  function build_product(root, product_data) {

    var base_type = product_data.base_product_type;
    var base_product;
    if (base_type === "FPPTI" || base_type === "FPPTIW" || base_type == "FPPTIB" || base_type == "FPPTIY") {
      base_product = new FPPTIProduct(product_data);
    } else if (base_type === "FPPCI") {
      base_product = new FPPCIProduct(product_data);
    } else if (base_type === "Group CI") {
      base_product = new GroupCIProduct(root, product_data);
    } else if (base_type === "FPP-Gov") {
      base_product = new FPPGovProduct(product_data);
    } else if (base_type === "ACC") {
      base_product = new ACCProduct(product_data);
    } else if (base_type === "ACAOF") {
      base_product = new ACAOFProduct(product_data);
    } else if (base_type === 'HI') {
      base_product = new HIProduct(product_data);
    } else if (base_type === 'HIA01') {
      base_product = new HIA01Product(product_data);
    } else if (base_type === 'HIB01') {
      base_product = new HIB01Product(product_data);
    } else if (base_type === 'HIAOBG') {
      base_product = new HIAOBGProduct(product_data);
    } else if (base_type === 'Static Benefit') {
      base_product = new MembershipProduct(product_data);
    } else {
      // default product?
      alert("Invalid product type '" + base_type + "'");
      base_product = new FPPTIProduct(product_data);
    }

    return base_product;
  }

  //region Coverage Type
  var CoverageType = {
    get Normal() { return 'normal'; },
    get Simple() { return 'simple'; },
    get Forced() { return 'forced'; }
  };
  //endregion

  //region Product
// Model for different insurance products
// Product is abstract base class
  function Product() {
    Object.defineProperty(this, 'has_brochure', {value: true, configurable: true});
    Object.defineProperty(this, 'coverage_type', {value: CoverageType.Normal, configurable: true});
    Object.defineProperty(this, 'is_simple_coverage', {get: function () { return this.coverage_type === CoverageType.Simple; }});
    Object.defineProperty(this, 'is_normal_coverage', {get: function () { return this.coverage_type === CoverageType.Normal; }});
    Object.defineProperty(this, 'is_forced_coverage', {get: function () { return this.coverage_type === CoverageType.Forced; }});
    //Object.defineProperty(this, 'can_decline', { get: function () { return this.product_data.base_product_type !== 'Static Benefit'; } });
  }

  Product.prototype = {
    // Age max limits are subject to case-specific overrides. Also, some product subclasses will change the default as well.

    min_emp_age: function () {
      return 18;
    },
    max_emp_age: function () {
      var default_max_emp_age = 70;

      // Check for case-specific override for employee.
      if (this.product_data.coverage_limits && this.product_data.coverage_limits.max_age && this.product_data.coverage_limits.max_age.is_enabled) {
        var max_emp_age = parseInt(this.product_data.coverage_limits.max_age.max_employee_age);
        if (max_emp_age && !isNaN(max_emp_age) && max_emp_age < default_max_emp_age) {
          return max_emp_age;
        }
      }

      return default_max_emp_age;
    },

    min_sp_age: function () {
      return 18;
    },
    max_sp_age: function () {
      var default_max_sp_age = 70;

      // Check for case-specific override for spouse.
      if (this.product_data.coverage_limits && this.product_data.coverage_limits.max_age && this.product_data.coverage_limits.max_age.is_enabled) {
        var max_spouse_age = parseInt(this.product_data.coverage_limits.max_age.max_spouse_age);
        if (max_spouse_age && !isNaN(max_spouse_age) && max_spouse_age < default_max_sp_age) {
          return max_spouse_age;
        }
      }

      return default_max_sp_age;
    },

    min_child_age: function () {
      return 0;
    },
    max_child_age: function () {
      return 23;
    },

    is_valid_applicant: function(applicant) {
      var self = this;

      if (applicant.type === wizard_applicant.Applicant.EmployeeType) {
        return this.is_valid_employee(applicant);
      } else if (applicant.type === wizard_applicant.Applicant.SpouseType) {
        return this.is_valid_spouse(applicant);
      } else if (applicant.type === wizard_applicant.Applicant.ChildType && applicant.is_group()) {
        return _.any(applicant.applicants(), function(a) { return self.is_valid_child(a);});
      } else {
        return this.is_valid_child(applicant);
      }
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

    is_children_coverage_grouped: function() {
      // Some products require
      return false;
    },

    // Allow the details of the benefit's face value, display to be based on the product
    //get_new_benefit_option: function (options) {
    //  return new BenefitOption(options);
    //},

    create_coverage_option: function (options) {
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

    // HI/ACC Product functions
    has_simple_coverage: function () {
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

    does_override_rate_options: function () {
      return false;
    },

    should_show_other_insurance_questions: function () {
      // should we show replacement questions
      // right now it is anything that isn't group CI, which is all FPP products.
      return this.is_fpp_product();
    },

    should_show_step_two: function () {
      return true;
    },

    should_show_step_four: function () {
      return true;
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

    does_use_recommended_coverage_table: function () {
      // Most products use this format for coverage selection.
      return true;
    },

    get_occupations: function () {
      return [];
    },

    requires_occupation: function () {
      return false;
    },

    get_coverage_tiers: function () {
      return [];
    },

    requires_additional_information: function () {
      return this.requires_gender() || this.requires_height() ||
        this.requires_weight() || this.requires_is_smoker();
    },

    can_decline: function () {
      return this.product_type !== 'Static Benefit';
    }

  };
  //endregion

  //region Applicant Selection Product
  function ApplicantSelectionProduct(product_data) {
    Product.call(this);
    this.product_type = product_data.base_product_type;
    this.product_data = product_data;
  }

  ApplicantSelectionProduct.prototype = {
    does_use_recommended_coverage_table: function () {
      // Don't use recommended coverage selection for these types of products.
      return false;
    }
  };
  //endregion

  //region FPPTI Product
  function FPPTIProduct(product_data) {
    Product.call(this);
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
  //endregion

  //region Group CI Product
  function GroupCIProduct(root, product_data) {
    Product.call(this);
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

    self.does_override_rate_options = function (applicant_type) {
      // Group CI has to filter out some of the options returned by the rates module.
      return (
        applicant_type === wizard_applicant.Applicant.EmployeeType ||
        applicant_type === wizard_applicant.Applicant.SpouseType
      );
    };

    self.filter_coverage_options_for_applicant_type = function (all_options, applicant_type) {

      if (applicant_type === wizard_applicant.Applicant.EmployeeType) {
        return self.filter_employee_coverage_options(all_options);
      } else if (applicant_type === wizard_applicant.Applicant.SpouseType) {
        return self.filter_spouse_coverage_options(all_options);
      } else {
        return self.filter_children_coverage_options(all_options);
      }
    };

    self.filter_employee_coverage_options = function (all_options) {
      // Basic options for employee, every 5k up to 100k.
      var basic_options = self.filter_base_rate_options(all_options);

      // If employee is older than 65, and the only option is 2500, use that.
      if (all_options.length == 1) {
        return all_options;
      }
      return basic_options;
    };

    function get_employee_total_cumulative_coverage() {
      // Add together existing coverage applications and the current application.

      var emp_coverage_option = window.vm.coverage_vm.get_applicant_coverage_option_for_product(window.vm.employee(), self);
      var emp_existing_coverage_amount = window.vm.employee().get_existing_coverage_amount_for_product(self);
      var emp_current_coverage_amount = (emp_coverage_option.is_valid()? emp_coverage_option.face_value : 0);
      return emp_existing_coverage_amount + emp_current_coverage_amount;
    }

    function has_employee_selected_coverage() {

      var applicant_coverage = window.vm.coverage_vm.get_applicant_coverage_for_product(window.vm.employee(), self);
      return applicant_coverage.has_selected_coverage();
    }

    function has_employee_answered_required_question_yes() {
      // TODO: Move this check to validate step?
      // Right now (March 2016), this is not important to check according to Bill.
      //self.root.employee().has_answered_any_question_yes();
      return false;
    }

    self.filter_spouse_coverage_options = function (all_options) {
      // Filter spouse options based on employee selection.

      // Get the 5,000 to 100,000 coverage options to start with.
      var base_options = self.filter_spouse_base_rate_options(all_options);

      // Limit to 50% of employee's current selection, or 25k max.
      // BUT, If the employee has answered yes to any required question, we should not limit the dependent coverage options.
      if (!has_employee_selected_coverage() || has_employee_answered_required_question_yes()) {
        // All base options up to and including 25k.
        return _.filter(base_options, function (o) {
          return o.face_value <= 25000;
        });
      } else {
        // Cap at 25k or half the employee total applied coverage.
        var limit = _.min([get_employee_total_cumulative_coverage() / 2.0, 25000]);
        var valid_options = _.filter(base_options, function (o) {
          return o.face_value <= limit
        });
        self.append_halfway_coverage_option_if_needed(valid_options, limit, all_options);
        return valid_options;
      }
    };

    self.append_halfway_coverage_option_if_needed = function (valid_options, max_coverage_limit, all_options) {
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

    self.filter_children_coverage_options = function (all_options) {
      // Similar to spouse filtering above, but limited to 10k and no in-between option appended.

      // Get the 5,000 to 100,000 coverage options to start with.
      var base_options = self.filter_children_base_rate_options(all_options);

      // Limit to 50% of employee's current selection, or 10K max.
      // BUT, If the employee has answered yes to any required question, we should not limit the dependent coverage options.
      if (!has_employee_selected_coverage() || has_employee_answered_required_question_yes()) {
        // All base options up to and including 10K.
        return _.filter(base_options, function (o) {
          return o.face_value <= 10000;
        });
      } else {
        // Cap at 10K or half the employee total applied coverage. Do not add in-between option like for spouse.
        var limit = _.min([get_employee_total_cumulative_coverage() / 2.0, 10000]);
        return _.filter(base_options, function (o) {
          return o.face_value <= limit
        });
      }
    };

    self.filter_base_rate_options = function (all_options) {
      // Usually just present $5,000 to $100,000 in $5000 increments.
      return _.filter(all_options, function (o) {
        return o.face_value % 5000 === 0;
      });
    };

    self.filter_spouse_base_rate_options = function(all_options) {
      // Spouse options include increments of 2500.
      return _.filter(all_options, function (o) {
        return o.face_value % 2500 === 0;
      });
    };

    self.filter_children_base_rate_options = function(all_options) {
      // Spouse options include increments of 2500.
      return _.filter(all_options, function (o) {
        return o.face_value % 2500 === 0;
      });
    };


  }

  GroupCIProduct.prototype = Object.create(Product.prototype);

  GroupCIProduct.prototype.is_valid_employee = function (employee) {
    // Need to validate age and is_smoker is valid
    var age = employee.get_age();
    var valid = (age >= this.min_emp_age() && age <= this.max_emp_age());
    // valid &= employee.is_smoker() != null;
    // valid &= employee.has_valid_weight();
    // valid &= employee.has_valid_height();
    // valid &= employee.has_valid_gender();

    return valid;
  };

  GroupCIProduct.prototype.is_valid_spouse = function (spouse) {
    var age = spouse.get_age();
    var valid = (age >= this.min_sp_age() && age <= this.max_sp_age());
    // valid &= spouse.is_smoker() != null;
    // valid &= spouse.has_valid_weight();
    // valid &= spouse.has_valid_height();
    // valid &= spouse.has_valid_gender();
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
  //endregion

  GroupCIProduct.prototype.max_child_age = function () {
    return 26;
  };
  GroupCIProduct.prototype.is_children_coverage_grouped = function() {
    return true;
  };

  GroupCIProduct.max_emp_age = function () {
    // If there are limits on the case that further restrict the age, we want those to override; otherwise use 70.
    var base_max_age = Product.prototype.max_emp_age.call(this);
    return Math.min(70, base_max_age);
  };
  
  function FPPGovProduct(product_data) {
    Product.call(this);
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
  //endregion

  //region HI Product
  function HIProduct(product_data) {
    Product.call(this);
    Object.defineProperty(this, 'coverage_type', {value: CoverageType.Simple, configurable: true});
    this.product_type = "HI";
    this.product_data = product_data;
  }

  HIProduct.prototype = Object.create(Product.prototype);

  HIProduct.prototype.requires_gender = function () {
    return false;
  };

  HIProduct.prototype.requires_height = function () {
    return false;
  };

  HIProduct.prototype.requires_weight = function () {
    return false;
  };

  HIProduct.prototype.requires_is_smoker = function () {
    return false;
  };

  HIProduct.prototype.has_critical_illness_coverages = function () {
    return false;
  };

  HIProduct.prototype.has_critical_illness_coverages = function () {
    return false;
  };

  HIProduct.prototype.has_simple_coverage = function () {
    return true;
  };

  HIProduct.prototype.requires_occupation = function () {
    return true;
  };

  HIProduct.prototype.get_coverage_tiers = function (applicant_types) {
    var hi_coverage_tiers = ['EE', 'ES', 'EC', 'EF'];
    if (Array.isArray(applicant_types)) {
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.SpouseType })) {
        _.remove(hi_coverage_tiers, function (tier) { return tier === 'ES' || tier === 'EF'; })
      }
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.ChildType; })) {
        _.remove(hi_coverage_tiers, function (tier) { return tier === 'EC' || tier === 'EF'; })
      }
    }
    return hi_coverage_tiers;
  };

  HIProduct.prototype.create_coverage_option = function (options) {
    return new SimpleCoverageOption(options);
  };

  HIProduct.prototype.is_fpp_product = function () {
    return false;
  };

  HIProduct.prototype.should_show_step_two = function () {
    return false;
  };

  HIProduct.prototype.should_show_step_four = function () {
    return false;
  };

  HIProduct.prototype.should_show_step_5 = function () {
    return false;
  };
  HIProduct.prototype.max_child_age = function () {
    return 26;
  };
  //endregion
    
  //region HIB01 Product
  function HIB01Product(product_data) {
    Product.call(this);
    Object.defineProperty(this, 'coverage_type', {value: CoverageType.Simple, configurable: true});
    this.product_type = "HIB01";
    this.product_data = product_data;
  }

  HIB01Product.prototype = Object.create(Product.prototype);

  HIB01Product.prototype.requires_gender = function () {
    return false;
  };

  HIB01Product.prototype.requires_height = function () {
    return false;
  };

  HIB01Product.prototype.requires_weight = function () {
    return false;
  };

  HIB01Product.prototype.requires_is_smoker = function () {
    return false;
  };

  HIB01Product.prototype.has_critical_illness_coverages = function () {
    return false;
  };

  HIB01Product.prototype.has_critical_illness_coverages = function () {
    return false;
  };

  HIB01Product.prototype.has_simple_coverage = function () {
    return true;
  };

  HIB01Product.prototype.requires_occupation = function () {
    return true;
  };

  HIB01Product.prototype.get_coverage_tiers = function (applicant_types) {
    var hi_coverage_tiers = ['EE', 'ES', 'EC', 'EF'];
    if (Array.isArray(applicant_types)) {
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.SpouseType })) {
        _.remove(hi_coverage_tiers, function (tier) { return tier === 'ES' || tier === 'EF'; })
      }
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.ChildType; })) {
        _.remove(hi_coverage_tiers, function (tier) { return tier === 'EC' || tier === 'EF'; })
      }
    }
    return hi_coverage_tiers;
  };

  HIB01Product.prototype.create_coverage_option = function (options) {
    return new SimpleCoverageOption(options);
  };

  HIB01Product.prototype.is_fpp_product = function () {
    return false;
  };

  HIB01Product.prototype.should_show_step_two = function () {
    return false;
  };

  HIB01Product.prototype.should_show_step_four = function () {
    return false;
  };

  HIB01Product.prototype.should_show_step_5 = function () {
    return false;
  };
  HIB01Product.prototype.max_child_age = function () {
    return 26;
  };
  //endregion

  //region HIA01 Product
  function HIA01Product(product_data) {
    Product.call(this);
    Object.defineProperty(this, 'coverage_type', {value: CoverageType.Simple, configurable: true});
    this.product_type = "HIA01";
    this.product_data = product_data;
  }

  HIA01Product.prototype = Object.create(Product.prototype);

  HIA01Product.prototype.requires_gender = function () {
    return false;
  };

  HIA01Product.prototype.requires_height = function () {
    return false;
  };

  HIA01Product.prototype.requires_weight = function () {
    return false;
  };

  HIA01Product.prototype.requires_is_smoker = function () {
    return false;
  };

  HIA01Product.prototype.has_critical_illness_coverages = function () {
    return false;
  };

  HIA01Product.prototype.has_critical_illness_coverages = function () {
    return false;
  };

  HIA01Product.prototype.has_simple_coverage = function () {
    return true;
  };

  HIA01Product.prototype.requires_occupation = function () {
    return true;
  };

  HIA01Product.prototype.get_coverage_tiers = function (applicant_types) {
    var hia01_coverage_tiers = ['EE', 'ES', 'EC', 'EF'];
    if (Array.isArray(applicant_types)) {
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.SpouseType })) {
        _.remove(hia01_coverage_tiers, function (tier) { return tier === 'ES' || tier === 'EF'; })
      }
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.ChildType; })) {
        _.remove(hia01_coverage_tiers, function (tier) { return tier === 'EC' || tier === 'EF'; })
      }
    }
    return hia01_coverage_tiers;
  };

  HIA01Product.prototype.create_coverage_option = function (options) {
    return new SimpleCoverageOption(options);
  };

  HIA01Product.prototype.is_fpp_product = function () {
    return false;
  };

  HIA01Product.prototype.should_show_step_two = function () {
    return false;
  };

  HIA01Product.prototype.should_show_step_four = function () {
    return false;
  };

  HIA01Product.prototype.should_show_step_5 = function () {
    return false;
  };
  HIA01Product.prototype.max_child_age = function () {
    return 26;
  };
  //endregion

  //region HIAOBG Product
  function HIAOBGProduct(product_data) {
    Product.call(this);
    Object.defineProperty(this, 'coverage_type', {value: CoverageType.Simple, configurable: true});
    this.product_type = "HIAOBG";
    this.product_data = product_data;
  }

  HIAOBGProduct.prototype = Object.create(Product.prototype);

  HIAOBGProduct.prototype.requires_gender = function () {
    return false;
  };

  HIAOBGProduct.prototype.requires_height = function () {
    return false;
  };

  HIAOBGProduct.prototype.requires_weight = function () {
    return false;
  };

  HIAOBGProduct.prototype.requires_is_smoker = function () {
    return false;
  };

  HIAOBGProduct.prototype.has_critical_illness_coverages = function () {
    return false;
  };

  HIAOBGProduct.prototype.has_critical_illness_coverages = function () {
    return false;
  };

  HIAOBGProduct.prototype.has_simple_coverage = function () {
    return true;
  };

  HIAOBGProduct.prototype.requires_occupation = function () {
    return true;
  };

  HIAOBGProduct.prototype.get_coverage_tiers = function (applicant_types) {
    var hi_coverage_tiers = ['EE', 'ES', 'EC', 'EF'];
    if (Array.isArray(applicant_types)) {
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.SpouseType })) {
        _.remove(hi_coverage_tiers, function (tier) { return tier === 'ES' || tier === 'EF'; })
      }
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.ChildType; })) {
        _.remove(hi_coverage_tiers, function (tier) { return tier === 'EC' || tier === 'EF'; })
      }
    }
    return hi_coverage_tiers;
  };

  HIAOBGProduct.prototype.create_coverage_option = function (options) {
    return new SimpleCoverageOption(options);
  };

  HIAOBGProduct.prototype.is_fpp_product = function () {
    return false;
  };

  HIAOBGProduct.prototype.should_show_step_two = function () {
    return false;
  };

  HIAOBGProduct.prototype.should_show_step_four = function () {
    return false;
  };

  HIAOBGProduct.prototype.should_show_step_5 = function () {
    return false;
  };
  HIAOBGProduct.prototype.max_child_age = function () {
    return 26;
  };
  //endregion

  //region ACAOFProduct
  function ACAOFProduct(product_data) {
    Product.call(this);
    Object.defineProperty(this, 'coverage_type', {value: CoverageType.Simple, configurable: true});
    this.product_type = "ACAOF";
    this.product_data = product_data;
  }

  ACAOFProduct.prototype = Object.create(Product.prototype);

  ACAOFProduct.prototype.has_simple_coverage = function () {
    return true;
  };

  ACAOFProduct.prototype.get_occupations = function () {
    return ['Management', 'Worker', 'Secretary'];
  };

  ACAOFProduct.prototype.requires_occupation = function () {
    return true;
  };

  ACAOFProduct.prototype.get_coverage_tiers = function (applicant_types) {
    var acaof_coverage_tiers = ['EE', 'ES', 'EC', 'EF'];
    if (Array.isArray(applicant_types)) {
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.SpouseType })) {
        _.remove(acaof_coverage_tiers, function (tier) { return tier === 'ES' || tier === 'EF'; })
      }
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.ChildType; })) {
        _.remove(acaof_coverage_tiers, function (tier) { return tier === 'EC' || tier === 'EF'; })
      }
    }
    return acaof_coverage_tiers;
  };

  ACAOFProduct.prototype.create_coverage_option = function (options) {
    return new SimpleCoverageOption(options);
  };

  ACAOFProduct.prototype.is_fpp_product = function () {
    return false;
  };

  ACAOFProduct.prototype.should_show_step_two = function () {
    return false;
  };

  ACAOFProduct.prototype.should_show_step_four = function () {
    return false;
  };
  ACAOFProduct.prototype.max_child_age = function() {
    // If there are limits on the case that further restrict the age, we want those to override; otherwise use 26.
    return 26;
  };
  //endregion
    
  //region ACCProduct
  function ACCProduct(product_data) {
    Product.call(this);
    Object.defineProperty(this, 'coverage_type', {value: CoverageType.Simple, configurable: true});
    this.product_type = "ACC";
    this.product_data = product_data;
  }

  ACCProduct.prototype = Object.create(Product.prototype);

  ACCProduct.prototype.has_simple_coverage = function () {
    return true;
  };

  ACCProduct.prototype.get_occupations = function () {
    return ['Management', 'Worker', 'Secretary'];
  };

  ACCProduct.prototype.requires_occupation = function () {
    return true;
  };

  ACCProduct.prototype.get_coverage_tiers = function (applicant_types) {
    var acc_coverage_tiers = ['EE', 'ES', 'EC', 'EF'];
    if (Array.isArray(applicant_types)) {
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.SpouseType })) {
        _.remove(acc_coverage_tiers, function (tier) { return tier === 'ES' || tier === 'EF'; })
      }
      if (!_.any(applicant_types, function (applicant_type) { return applicant_type === wizard_applicant.Applicant.ChildType; })) {
        _.remove(acc_coverage_tiers, function (tier) { return tier === 'EC' || tier === 'EF'; })
      }
    }
    return acc_coverage_tiers;
  };

  ACCProduct.prototype.create_coverage_option = function (options) {
    return new SimpleCoverageOption(options);
  };

  ACCProduct.prototype.is_fpp_product = function () {
    return false;
  };

  ACCProduct.prototype.should_show_step_two = function () {
    return false;
  };

  ACCProduct.prototype.should_show_step_four = function () {
    return false;
  };
  ACCProduct.prototype.max_child_age = function() {
    // If there are limits on the case that further restrict the age, we want those to override; otherwise use 26.
    return 26;
  };
  //endregion

  //region Membership Product
  function MembershipProduct(product_data) {
    Product.call(this);
    Object.defineProperty(this, 'has_brochure', {value: false, configurable: true});
    Object.defineProperty(this, 'coverage_type', {value: CoverageType.Forced, configurable: true});
    this.product_type = "Static Benefit";
    this.product_data = product_data;
    this.flat_fee = ko.pureComputed(function () {
      return product_data.flat_fee;
    });
  }

  MembershipProduct.prototype = Object.create(Product.prototype);

  MembershipProduct.prototype.should_show_step_two = function () {return false;};
  MembershipProduct.prototype.should_show_step_5 = function () {return false;};
  MembershipProduct.prototype.should_show_other_insurance_questions  = function () {return false;};
  MembershipProduct.prototype.should_show_step_four = function () {return false;};

  MembershipProduct.prototype.create_coverage_option = function (options) {
    return new SimpleCoverageOption(options);
  };

  MembershipProduct.prototype.is_fpp_product = function () {return false;};


  //endregion

  return {
    build_products: build_products
  };
})();
