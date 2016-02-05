
function CoverageOption(options) {
    var self = this;

    self.is_by_face = options.is_by_face;
    self.premium = options.premium;
    // This is an observable
    self.payment_mode = options.payment_mode;
    self.face_value = options.face_value;
    self.applicant_type = options.applicant_type;

    self.get_total_premium = function() {
      return self.premium;
    };

    self.format_premium = function() {
      return format_premium_value(self.premium);
    };

    self.format_premium_option = function() {
      return self.format_premium() + " " + self.payment_mode().display_lowercase();
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
        return self.format_premium_option();
      }
    };
    self.is_valid = function() {
      return true;
    };

    self.serialize_data = function() {
      return {
        premium: self.premium,
        face_value: self.face_value
      }
    }
  }
  CoverageOption.display_benefit_option = function(item) {
    return item.format_for_dropdown();
  };

  function CICoverageOption(wrapped_option) {
    var self = this;
    self.is_by_face = wrapped_option.is_by_face;
    self.premium = wrapped_option.premium;
    self.get_total_premium = wrapped_option.get_total_premium;
    self.face_value = wrapped_option.face_value;
    self.format_premium = wrapped_option.format_premium;
    self.format_premium_option = wrapped_option.format_premium_option;
    self.format_face_value = function() {
      var face_value_formatted = format_face_value(self.face_value);
      var ci_value = Math.round(self.face_value * .3);
      var ci_value_formatted = format_face_value(ci_value);

      return face_value_formatted + "<br><small>("+ci_value_formatted+" CI)</small>";
    };
    self.format_for_dropdown = wrapped_option.format_for_dropdown;
    self.is_valid = wrapped_option.is_valid;
    self.serialize_data = wrapped_option.serialize_data;
    self.payment_mode = wrapped_option.payment_mode;
    self.applicant_type = wrapped_option.applicant_type;
  }


  function NullCoverageOption() {
    var self = this;

    self.is_by_face = true;
    self.premium = 0;
    self.face_value = 0;

    self.is_valid = function() {
      return false;
    };

    self.format_premium_option = function() {
      return "";
    };
    self.format_premium = function() {

    };

    self.get_total_premium = function() {
      return 0;
    };

    self.format_face_value = function() {
      return "- no benefit -";
    };
    self.format_for_dropdown = function() {
      return "- no benefit -";
    };
    self.serialize_data = function() {
      return {}
    };
    self.payment_mode = function() {
      return null;
    };
  }

