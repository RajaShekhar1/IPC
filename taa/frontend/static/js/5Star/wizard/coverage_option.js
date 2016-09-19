function CoverageOption(options) {
  var self = this;

  _.defaults(options, {
    is_by_face: false,
    premium: 0,
    payment_mode: 0,
    face_value: 0,
    applicant_type: wizard_applicant.Applicant.EmployeeType,
    tier: null,
    flat_fee: 0
  });

  self.is_by_face = options.is_by_face;
  self.premium = options.premium;
  // This is an observable
  self.payment_mode = options.payment_mode;
  self.face_value = options.face_value;
  self.applicant_type = options.applicant_type;
  self.tier = options.tier;

  self.get_total_premium = function () {
    return self.premium;
  };

  self.format_premium = function () {
    return format_premium_value(self.premium);
  };

  self.format_premium_option = function () {
    return self.format_premium() + " " + self.payment_mode().display_lowercase();
  };
  self.format_face_value = function () {
    return format_face_value(self.face_value);
  };
  self.format_face_option = function () {
    return self.format_face_value() + " face amount";
  };
  self.format_for_dropdown = function () {
    if (self.is_by_face) {
      return self.format_face_option();
    } else {
      return self.format_premium_option();
    }
  };
  self.is_valid = function () {
    return true;
  };

  self.serialize_data = function () {
    return {
      premium: self.premium,
      face_value: self.face_value
    };
  };
}
CoverageOption.display_benefit_option = function (item) {
  return item.format_for_dropdown();
};

// This should used as a way to view a summary of a group coverage.
function GroupedCoverageVM(applicant_group, applicant_options) {
  var self = this;

  self.applicant_group = applicant_group;
  self.applicant_options = applicant_options;

  self.get_total_premium = function () {
    return self.premium;
  };

  self.format_premium = function () {
    return "GroupedCoverageVM premium ";
    return format_premium_value(self.premium);
  };

  self.format_premium_option = function () {
    return "GroupedCoverageVM premium opt";
    return self.format_premium() + " " + self.payment_mode().display_lowercase();
  };
  self.format_face_value = function () {
    return "GroupedCoverageVM face val";
    return format_face_value(self.face_value);
  };
  self.format_face_option = function () {
    return "GroupedCoverageVM face opt";
    return self.format_face_value() + " face amount";
  };
  self.format_for_dropdown = function () {
    if (self.is_by_face) {
      return self.format_face_option();
    } else {
      return self.format_premium_option();
    }
  };
  self.is_valid = function () {
    return true;
  };

  self.serialize_data = function () {
    return {
      premium: self.premium,
      face_value: self.face_value
    };
  };

}


function CICoverageOption(wrapped_option) {
  var self = this;
  self.is_by_face = wrapped_option.is_by_face;
  self.premium = wrapped_option.premium;
  self.get_total_premium = wrapped_option.get_total_premium;
  self.face_value = wrapped_option.face_value;
  self.format_premium = wrapped_option.format_premium;
  self.format_premium_option = wrapped_option.format_premium_option;
  self.format_face_value = function () {
    var face_value_formatted = format_face_value(self.face_value);
    var ci_value = Math.round(self.face_value * .3);
    var ci_value_formatted = format_face_value(ci_value);

    return face_value_formatted + "<br><small>(" + ci_value_formatted + " CI)</small>";
  };
  self.format_for_dropdown = wrapped_option.format_for_dropdown;
  self.is_valid = wrapped_option.is_valid;
  self.serialize_data = wrapped_option.serialize_data;
  self.payment_mode = wrapped_option.payment_mode;
  self.applicant_type = wrapped_option.applicant_type;
}


function SimpleCoverageOption(options) {
  var self = this;
  self.is_by_face = true;
  self.premium = options.premium;

  // Convert premium to a number if it is a string
  if (typeof self.premium === 'string') {
    self.premium = parseFloat(self.premium);
  }

  // For ACC or HI, one of: EE, ES, EC, EF
  self.coverage_tier = options.coverage_tier;

  // This is an observable
  self.payment_mode = options.payment_mode;
  self.face_value = options.face_value;
  self.applicant_type = options.applicant_type;

  self.get_total_premium = function () {
    return self.premium;
  };

  self.format_premium = function () {
    // Only show employee info for this kind of product.
    if (self.applicant_type != wizard_applicant.Applicant.EmployeeType) {
      return "";
    }
    return format_premium_value(self.premium);
  };

  self.format_premium_option = function () {
    // Only show employee info for this kind of product.
    if (self.applicant_type != wizard_applicant.Applicant.EmployeeType) {
      return "";
    }
    return self.format_premium() + " " + self.payment_mode().display_lowercase();
  };
  self.format_face_value = function () {
    // Only show employee info for this kind of product.
    if (self.applicant_type != wizard_applicant.Applicant.EmployeeType) {
      return "";
    }
    return 'Selected';
  };
  self.format_face_option = function () {
    // Only show employee info for this kind of product.
    if (self.applicant_type != wizard_applicant.Applicant.EmployeeType) {
      return "";
    }
    return self.format_face_value() + " face amount";
  };
  self.format_for_dropdown = function () {
    // Only show employee info for this kind of product.
    if (self.applicant_type != wizard_applicant.Applicant.EmployeeType) {
      return "";
    }
    if (self.is_by_face) {
      return self.format_face_option();
    } else {
      return self.format_premium_option();
    }
  };
  self.is_valid = function () {
    return true;
  };

  self.serialize_data = function () {
    return {
      premium: self.premium,
      coverage_selection: self.coverage_tier
    }
  }
}

function FlatFeeCoverageOption(options) {
  var self = this;
  _.defaults(self, options, {
    is_by_face: false,
    premium: 0,
    payment_mode: payment_mode_module.create_payment_mode_by_frequency(12),
    face_value: 0,
    applicant_type: null,
    tier: null,
    flat_fee: 0
  });

  self.is_applicant_employee = ko.pureComputed(function () {
    return self.applicant_type === wizard_applicant.Applicant.EmployeeType;
  });

  self.premium = self.is_applicant_employee()? parseFloat(self.flat_fee) : 0.0;
  self.flat_fee = self.is_applicant_employee()? parseFloat(self.flat_fee) : 0.0;

  self.is_valid = function () {
    return true;
  };

  self.format_premium = function () {
    if (self.is_applicant_employee()) {
      if (self.payment_mode.frequency === 12) {
        return '$' + parseFloat(self.flat_fee).toFixed(2);
      }
      return '$' + parseFloat(self.flat_fee * 12 / self.payment_mode.frequency).toFixed(2);
    }
    return '$0.00';
  };

  self.format_premium_option = function () {
    if (self.is_applicant_employee()) {
      return self.format_premium() + ' ' + self.payment_mode.label.toLowerCase();
    }
    return '';
  };

  self.format_face_value = function () {
    if (self.is_applicant_employee()) {
      return 'Selected';
    }
    return 'Included';
  };

  self.serialize_data = function () {
    return {
      premium: self.premium,
      flat_fee: self.flat_fee,
      coverage_selection: true
    };
  };
}

function NullCoverageOption() {
  var self = this;

  self.is_by_face = true;
  self.premium = 0;
  self.face_value = 0;
  self.is_removed_from_product = ko.observable(false);

  self.is_valid = function () {
    return false;
  };

  self.format_premium_option = function () {
    return "";
  };
  self.format_premium = function () {

  };

  self.get_total_premium = function () {
    return 0;
  };

  self.format_face_value = function () {
    return "- no benefit -";
  };
  self.format_for_dropdown = function () {
    return "- no benefit -";
  };
  self.serialize_data = function () {
    return {}
  };
  self.payment_mode = function () {
    return null;
  };
}

var null_coverage = new NullCoverageOption();
