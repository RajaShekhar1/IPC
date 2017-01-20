var coverage_settings_module = (function() {


  function format_face_value(val) {
    if (val == null) {
      return "(NA)";
    }
    return "$" + numberWithCommas(val);
  }

  function format_premium_value(val) {
    if (val == null) {
      return "(NA)";
    }
    return "$" + numberWithCommas(val.toFixed(2));
  }


  // Settings for max Emp and Spouse ages on a per-case, per product basis.
  function MaximumAgeSettingsVM(params) {
    var self = this;

    var defaults = {
      is_enabled: false,
      max_employee_age: 70,
      max_spouse_age: 70
    };
    var options = (params.settings.max_age) ? params.settings.max_age : defaults;

    self.is_enabled = ko.observable(options.is_enabled !== undefined ? options.is_enabled : false);
    self.max_employee_age = ko.observable(options.max_employee_age || 70);
    self.max_spouse_age = ko.observable(options.max_spouse_age || 70);

    self.serialize = function() {
      return {
        is_enabled: self.is_enabled(),
        max_employee_age: self.max_employee_age(),
        max_spouse_age: self.max_spouse_age()
      };
    }
  }


  // Settings for the maximum coverage and premiums on a per-case, per product basis.
  function MaximumCoverageSettingsVM(params) {
    var self = this;

    var defaults = {
      is_enabled: false,
      applicant_limits: []
    };
    var options = (params.settings.max_coverage) ? params.settings.max_coverage : defaults;

    self.is_enabled = ko.observable(options.is_enabled !== undefined ? options.is_enabled : false);

    // Link to the case payment mode so we can display premium frequencies.
    self.payment_mode = params.payment_mode;

    // Initialize a coverage limit object for each limitation.
    var initial_limits = _.map(options.applicant_limits, function(data) {
      return new ApplicantAgeBandCoverageLimitVM(data, self.payment_mode);
    });
    self.applicant_limits = ko.observableArray(initial_limits);

    self.sorted_applicant_limits = ko.computed(function() {
      var limits = self.applicant_limits();
      limits.sort(function(a, b) {
        if (a.applicant_type() !== b.applicant_type()) {
          // Sort by applicant type first
          var type_num_lookup = {"employee": 1, "spouse": 2, "child": 3};
          var type_num_a = type_num_lookup[a.applicant_type()];
          var type_num_b = type_num_lookup[b.applicant_type()];
          return type_num_a - type_num_b;
        }
        // Sort by age band lower limit.
        return a.min_age() - b.min_age();
      });
      return limits;
    });

    // Used for adding a new item, bound to the 'add restriction' modal.
    self.new_applicant_limit = ko.observable(null);
    self.validation_errors = ko.observableArray();

    self.delete_applicant_limit = function(limit) {
      self.applicant_limits.remove(limit);
    };

    self.show_applicant_limit_form = function () {
      if (self.new_applicant_limit()) {
        // This will hide the input form.
        self.new_applicant_limit(null);
      } else {
        // Bind the input form to a blank coverage limit object, which will display it.
        self.new_applicant_limit(new ApplicantAgeBandCoverageLimitVM({}, self.payment_mode));
      }
    };

    self.add_coverage_limit = function (limit) {

      // Clear errors
      self.validation_errors([]);

      // Validate
      var validation_errors = self.validate_new_limit(limit);
      if (validation_errors.length > 0) {
        self.validation_errors(validation_errors);
        return;
      }

      // Passed validation, display in table.
      self.applicant_limits.push(limit);

      // Clear the form
      self.new_applicant_limit(null);
    };

    self.validate_new_limit = function(limit) {
      var errors = [];

      var min_age = parseInt(limit.min_age());
      var max_age = parseInt(limit.max_age());
      var coverage_limit = parseInt(limit.max_coverage());
      var premium_limit = parseFloat(limit.max_premium());

      if (!limit.display_applicant_type()) {
        add_error(errors, limit.applicant_type, "Applicant type is required.");
      }

      if (!is_valid_age(min_age)) {
        add_error(errors, limit.min_age, "Lower age band limit is required.");
      }

      if (!is_valid_age(max_age)) {
        add_error(errors, limit.max_age, "Upper age band limit is required.");
      }

      if (min_age > max_age) {
        add_error(errors, limit.min_age, "Age band lower limit must be less than or equal to the upper limit.");
      }

      // Check for age band overlap
      var num_overlapping = _.filter(self.applicant_limits(), function(l) {
        return l.applicant_type() === limit.applicant_type() && (
            l.min_age() <= limit.max_age() && l.max_age() >= limit.min_age()
          );
      }).length;

      if (num_overlapping > 0) {
        add_error(errors, limit.min_age, "Age band overlaps an existing coverage limit for this applicant.")
      }

      // Validate coverage and / or premium limits.
      var is_valid_coverage = !isNaN(coverage_limit);
      var is_valid_premium = !isNaN(premium_limit);
      if (!is_valid_coverage && !is_valid_premium) {
        add_error(errors, limit.max_coverage, "You must specify either a max coverage, max premium, or both.");
      }

      return errors;
    };

    function is_valid_age(num) {
      return !isNaN(num) && num >= 0 && num < 120;
    }

    function add_error(err_list, observable, message) {
      err_list.push(new ValidationError(message));
    }

    self.serialize = function () {
      var out = {
        is_enabled: self.is_enabled(),
        applicant_limits: []
      };

      _.each(self.applicant_limits(), function(limit) {
        out.applicant_limits.push(limit.serialize());
      });

      return out;
    }
  }

  function ValidationError(message) {
    this.message = message;
  }

  function parse_int_or_null(val) {
    if (isNaN(parseInt(val))) {
      return null;
    } else {
      return parseInt(val);
    }
  }

  function parse_float_or_null(val) {
    if (isNaN(parseFloat(val))) {
      return null;
    } else {
      return parseFloat(val);
    }
  }



  function ApplicantAgeBandCoverageLimitVM(options, payment_mode) {
    var self = this;

    _.defaults(options, {
      min_age: 0,
      max_age: 70,
      max_coverage: null,
      max_premium: null
    });

    // Used for displaying premiums.
    self.payment_mode = payment_mode;

    self.applicant_type = ko.observable(options.applicant_type);
    self.min_age = ko.observable(parse_int_or_null(options.min_age));
    self.max_age = ko.observable(parse_int_or_null(options.max_age));
    self.max_coverage = ko.observable(parse_int_or_null(options.max_coverage));
    self.max_premium = ko.observable(parse_float_or_null(options.max_premium));

    self.coverage_options = [];
    // TODO: Use the product's maximum coverage here.
    for (var c = 1000; c <= 150000; c += 1000) {
      self.coverage_options.push(c);
    }

    self.display_applicant_type = ko.pureComputed(function() {
      if (self.applicant_type() === "employee") {
        return "Employee";
      } else if (self.applicant_type() === "spouse") {
        return "Spouse";
      } else if (self.applicant_type() === "child") {
        return "Child";
      }
      return "";
    });

    self.display_age_band = ko.pureComputed(function() {
      return "Between " + self.min_age() + " & " + self.max_age();
    });

    self.display_coverage_maximum = ko.pureComputed(function() {
      if (self.max_coverage() !== null) {
        return format_face_value(self.max_coverage());
      }
      return "(N/A)";
    });

    self.display_premium_maximum = ko.pureComputed(function() {
      if (self.max_premium() !== null && !isNaN(parseFloat(self.max_premium()))) {
        return format_premium_value(parseFloat(self.max_premium())) + " " + self.display_premium_frequency();
      }
      return "(N/A)";
    });

    self.display_premium_frequency = ko.pureComputed(function() {
      if (self.payment_mode().mode === -1) {
        // Default to monthly if the payment mode is "leave for applicant to select".
        return "monthly";
      }

      return self.payment_mode().name.toLowerCase();
    });

    self.serialize = function() {
      return {
        applicant_type: self.applicant_type(),
        min_age: self.min_age(),
        max_age: self.max_age(),
        max_coverage: self.max_coverage(),
        max_premium: self.max_premium()
      };
    };

  }



  var max_age_vm_cache = {};
  var max_coverage_vm_cache = {};

  return {
    get_max_age_vm_for_product: function(options) {
      if (!max_age_vm_cache[options.product.id]) {
        max_age_vm_cache[options.product.id] = new MaximumAgeSettingsVM(options);
      }

      return max_age_vm_cache[options.product.id];
    },
    get_max_coverage_vm_for_product: function(options) {
      if (!max_coverage_vm_cache[options.product.id]) {
        max_coverage_vm_cache[options.product.id] = new MaximumCoverageSettingsVM(options);
      }

      return max_coverage_vm_cache[options.product.id];
    }
  }
})();


