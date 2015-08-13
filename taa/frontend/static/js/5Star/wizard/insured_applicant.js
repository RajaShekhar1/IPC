// Main ViewModel for all applicants
var _applicant_count = 0;
function InsuredApplicant(applicant_type, options, product_selection) {
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

  self.display_selected_coverage = ko.computed(function() {
    return self.selected_coverage().format_face_value();
  });

  self.display_premium = ko.computed(function() {
    return self.selected_coverage().format_premium();
  });

  self.display_riders = ko.computed(function() {
    console.log(self.applicant_type);
    // return self.root.selected_coverage();
    return "$";
  });

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