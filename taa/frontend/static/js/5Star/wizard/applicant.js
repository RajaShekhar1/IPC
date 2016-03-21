var wizard_applicant = (function () {
  var _applicant_count = 0;

  // Main ViewModel for all applicants
  function Applicant(options) {
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

    self.type = applicant_data.type;

    // a basic internal id we can use in loops to distinguish applicants and get
    //  unique names, attributes, etc. for validation and lookup
    self._id = _applicant_count++;

    self.first = ko.observable(applicant_data.first);
    self.last = ko.observable(applicant_data.last);
    self.email = ko.observable(applicant_data.email);
    self.phone = ko.observable(applicant_data.phone);
    self.birthdate = ko.observable(normalize_date_of_birth(applicant_data.birthdate));
    self.ssn = ko.observable(applicant_data.ssn);
    self.gender = ko.observable(applicant_data.gender);

    // Extended questions
    self.height = ko.observable(parseFloat(applicant_data.height) ? parseFloat(applicant_data.height) : null);
    self.weight = ko.observable(applicant_data.weight || null);
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

    self.has_valid_gender = ko.pureComputed(function () {
      return self.gender() !== null;
    });
    self.has_valid_height = ko.computed(function () {
      return self.height() != null && self.height() > 0 && self.height_error() == null;
    });
    self.has_valid_weight = ko.computed(function () {
      return self.weight() != null && self.weight_error() == null;
    });

    self.is_valid = ko.computed(function () {
      return (
          $.trim(self.first()) != "" &&
          $.trim(self.last()) != "" &&
          $.trim(self.birthdate()) != ""
      );
    });

    self.any_valid_field = ko.computed(function () {
      return (
          $.trim(self.first()) !== "" ||
          $.trim(self.last()) !== "" ||
          $.trim(self.birthdate()) !== ""
      );
    });

    self.name = ko.computed(function () {
      // Only show if valid
      if (self.is_valid()) {
        return self.first();
      } else {
        return "";
      }
    });

    self.get_age = ko.computed(function () {
      return age_for_date(self.birthdate());
    });

    self.is_age_in_band = function(min_age, max_age) {
      var is_in_band = true;

      if (min_age !== null) {
        is_in_band = is_in_band && self.get_age() >= min_age;
      }

      if (max_age !== null) {
        is_in_band = is_in_band && self.get_age() <= max_age;
      }

      return is_in_band;
    };

    self.serialize_data = function () {
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

      return data;
    };

    self.get_existing_coverage_amount_for_product = function(product_id) {
      var amount = self.get_existing_coverage_amount_by_product()[product_id];
      if (!amount) {
        return 0;
      }
      return parseFloat(amount);
    };

    self.get_existing_coverage_amount_by_product = function() {
      return _.reduce(self.existing_coverages, function(by_product_id, coverage) {
        if (!(coverage.product.id in by_product_id)) {
          by_product_id[coverage.product.id] = 0.0;
        }
        if (coverage.coverage_status === 'enrolled') {
          by_product_id[coverage.product.id] += parseFloat(coverage.coverage_face_value);
        }
        return by_product_id;
      }, {});
    };


  }

  Applicant.EmployeeType = "employee";
  Applicant.SpouseType = "spouse";
  Applicant.ChildType = "children";


  // An applicant group is used for children; it acts like an applicant for coverage purposes,
  // but groups its applicants together (currently only for children)
  var ApplicantGroup = function(options, applicants) {
    var self = this;
    self.type = options.type;
    // applicants should be an observable list of children.
    self.applicants = applicants;

    self._id = _applicant_count++;
    self.is_valid = ko.computed(function () {
      return _.all(self.applicants(), function(applicant) {
        return (applicant.any_valid_field() && applicant.is_valid()) || !applicant.any_valid_field();
      });
    });

    self.name = ko.pureComputed(function() {
      var non_empty_name_applicants = _.filter(self.applicants(), function(applicant) {
        return $.trim(applicant.name()) !== "";
      });
      return _.invoke(non_empty_name_applicants, "name").join(", ");
    });

    self.is_age_in_band = function(min_age, max_age) {
      return self.all_ages_in_band(min_age, max_age);
    };

    self.all_ages_in_band = function(min_age, max_age) {
      // Inclusive of bounds.

      var is_in_band = true;

      if (min_age !== null) {
        is_in_band = is_in_band && _.all(self.applicants(), function(applicant) {
          return applicant.get_age() >= min_age;
        });
      }

      if (max_age !== null) {
        is_in_band = is_in_band && _.all(self.applicants(), function(applicant) {
          return applicant.get_age() <= max_age;
        });
      }

      return is_in_band;
    };

    self.any_valid_field = function() {
      return _.any(self.applicants(), function(applicant) {return applicant.any_valid_field();});
    };
    self.serialize_data = function() {
      return {
        type: self.type,
        applicants: _.map(self.applicants(), function(applicant) {return applicant.serialize_data();})
      };
    };
    self.get_existing_coverage_amount_for_product = function(product_id) {
      if (self.applicants().length === 0) {
        return 0;
      }
      // For now, just return the coverage for the first child.
      return self.applicants()[0].get_existing_coverage_amount_for_product(product_id);
    };
  };

  var ApplicantList = function(initial_list, should_show_spouse, should_show_children) {
    this.applicants = ko.observableArray(initial_list || []);
    this.children = ko.pureComputed(this.get_children, this);
    this._children_group = null;

    this.should_show_spouse = should_show_spouse;
    this.should_show_children = should_show_children;
    this.get_valid_applicants_for_coverage = ko.pureComputed(this._get_valid_applicants_for_coverage, this);
    this.get_valid_applicants = ko.pureComputed(this._get_valid_applicants, this);

    // Create a default employee and spouse
    if (this.get_employee() === undefined) {
      this.applicants.push(create_applicant({type: Applicant.EmployeeType}));
    }
    if (this.get_spouse() === undefined) {
      this.applicants.push(create_applicant({
        type: Applicant.SpouseType,
        last: this.get_employee().last()
      }));
    }
  };
  ApplicantList.prototype = {
    _get_valid_applicants_for_coverage: function() {
      var applicants = [];
      if (this.has_valid_employee()) {
        applicants.push(this.get_employee());
      }
      if (this.should_show_spouse() && this.has_valid_spouse()) {
        applicants.push(this.get_spouse());
      }
      if (this.should_show_children() && this.has_valid_children()) {
        applicants.push(this.get_children_group());
      }
      return applicants;
    },
    _get_valid_applicants: function() {
      return _.filter(vm.coverage_vm.applicants.applicants(), function(a){
        return a.is_valid();
      });
    },
    get_employee: function() {
      return this._find_applicant_by_type(Applicant.EmployeeType);
    },

    get_spouse: function() {
      return this._find_applicant_by_type(Applicant.SpouseType);
    },

    get_children: function() {
      return _.filter(this.applicants(), function(a) {return a.type === Applicant.ChildType;});
    },

    get_children_group: function() {
      if (this._children_group === null) {
        this._children_group = new ApplicantGroup({type: Applicant.ChildType}, this.children);
      }
      return this._children_group;
    },

    has_valid_employee: function() {
      return this.get_employee() && this.get_employee().is_valid();
    },

    has_valid_spouse: function() {
      return this.get_spouse() && this.get_spouse().is_valid();
    },

    has_valid_children: function() {
      return _.any(this.get_children(), function(c) {return c.is_valid();});
    },

    get_valid_children: function() {
      return _.filter(this.get_children(), function(c) {return c.is_valid();});
    },

    add_applicant: function(applicant) {
      this.applicants.push(applicant);
    },

    remove_applicant: function(applicant) {
      //_.remove(this.applicants(), {'type': applicant.type, 'first': applicant.first, 'last': applicant.last});
      this.applicants.remove(applicant);
    },

    _find_applicant_by_type: function(applicant_type) {
      return _.find(this.applicants(), function(applicant) {
        return applicant.type === applicant_type;
      });
    }
  };

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

  function create_applicant(options) {
    return new Applicant(options);
  }

  return {
    Applicant: Applicant,
    ApplicantGroup: ApplicantGroup,
    ApplicantList: ApplicantList,
    create_applicant: create_applicant,
    age_for_date: age_for_date
  };
})();
