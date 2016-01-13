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
    self.birthdate = ko.observable(normalize_date(applicant_data.birthdate));
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
          $.trim(self.first()) != "" ||
          $.trim(self.last()) != "" ||
          $.trim(self.birthdate()) != ""
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

      // Serialize the SOH questions
      //data.soh_questions = [];
      //_.each(self.health_questions(), function(soh_answer) {
      //  if (soh_answer.button_group()
      //      && soh_answer.question_object.question.is_spouse_only) {
      //    // We don't want to serialize these questions right now since we want only the health questions.
      //    //   (and these are sent separately right now using special observables has_spouse_been_[treated|disabled]_6_months...)
      //    // In the future, may add an attribute like 'non-health-question' to strip these out.
      //    // skip (continue)
      //    return true;
      //  }
      //  data.soh_questions.push(soh_answer.serialize());
      //});

      return data;
    };

    /*
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
     */
    //self.benefit_options = {
    //  by_coverage: ko.observableArray([]),
    //  by_premium: ko.observableArray([])
    //};
    //self.all_options = ko.computed(function() {
    //  var options = [new NullBenefitOption()];
    //  // Extends an array with another array
    //  $.merge(options, self.benefit_options.by_premium());
    //  $.merge(options, self.benefit_options.by_coverage());
    //  return options;
    //});
    //
    //
    //self.selected_custom_option = ko.observable(new NullBenefitOption());
    //
    //self.selected_coverage = ko.computed(function() {
    //  if (self.type == Applicant.EmployeeType) {
    //    return self.selected_plan().employee_recommendation().recommended_benefit;
    //  } else if (self.type == Applicant.SpouseType) {
    //    return self.selected_plan().spouse_recommendation().recommended_benefit;
    //  } else if (self.type == Applicant.ChildType) {
    //    return self.selected_plan().children_recommendation().recommended_benefit;
    //  }
    //  throw "Bad applicant type for applicant: "+self.type;
    //});
    //
    //self.display_selected_coverage = ko.computed(function() {
    //  return self.selected_coverage().format_face_value();
    //});
    //
    //self.display_premium = ko.computed(function() {
    //  return self.selected_coverage().format_premium();
    //});
    //
    //self.display_riders = function(rider_code) {
    //  var person = window.ui.current_person().type;
    //  var rider_amount;
    //  if(person==="employee") {
    //    rider_amount = window.ui.riders()['emp'][rider_code];
    //  } else if(person==="spouse") {
    //    rider_amount = window.ui.riders()['sp'][rider_code];
    //  }
    //  return "$"+rider_amount.toFixed(2);
    //}
    //
    //self.get_existing_coverage_amount_for_product = function(product_id) {
    //  return parseFloat(self.get_existing_coverage_amount_by_product()[product_id]);
    //};
    //
    //self.get_existing_coverage_amount_by_product = function() {
    //  return _.reduce(self.existing_coverages, function(by_product_id, coverage) {
    //    if (!(coverage.product.id in by_product_id)) {
    //      by_product_id[coverage.product.id] = 0.0;
    //    }
    //    by_product_id[coverage.product.id] += parseFloat(coverage.coverage_face_value);
    //    return by_product_id;
    //  }, {});
    //};


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
      return _.all(self.applicants(), function(applicant) {return applicant.is_valid();});
    });

    self.name = ko.pureComputed(function() {
      var non_empty_name_applicants = _.filter(self.applicants(), function(applicant) {
        return $.trim(applicant.name()) !== "";
      });
      return _.invoke(non_empty_name_applicants, "name").join(", ");
    });

    self.any_valid_field = function() {
      return _.any(self.applicants(), function(applicant) {return applicant.any_valid_field();});
    };
    self.serialize_data = function() {
      return {
        type: self.type,
        applicants: _.map(self.applicants(), function(applicant) {return applicant.serialize_data();})
      }
    };
  };

  var ApplicantList = function(initial_list, should_show_spouse, should_show_children) {
    this.applicants = ko.observableArray(initial_list || []);
    this.children = ko.pureComputed(this.get_children, this);
    this._children_group = null;

    this.should_show_spouse = should_show_spouse;
    this.should_show_children = should_show_children;
    this.get_valid_applicants_for_coverage = ko.pureComputed(this._get_valid_applicants_for_coverage, this);

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
