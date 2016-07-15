var CaseEnrollmentPeriod = function CaseEnrollmentPeriod(period, effective_date_settings) {
  var self = this;
  var defaults = {
    effective_date_type: "default",
    period_type: "ongoing",
    case_id: null,
    description: "default"
  };
  var settings = $.extend({}, defaults, period);

  self.period_type = settings.period_type;
  if (typeof effective_date_settings == 'string') {
    self.effective_date_type = effective_date_settings;
  }
  else {
    self.effective_date_type = effective_date_settings.method;
  }
  self.is_both = (self.period_type == "both");
  self.is_ongoing = (self.period_type == "ongoing");
  self.is_open = (self.period_type == "open");
  self.description = settings.description;

  self.case_id = settings.case_id;

  // Used for the missing start / end error
  self.error = ko.observable("");

  function strip_year(d) {
    // Strip off the year of the given date
    var month_day_search = /\d{4}-(\d{2}-\d{2})/;
    var matches = d.match(month_day_search);
    if (matches && matches.length > 1) {
      return matches[1];
    }
    return "";
  }
  self.start_date = ko.observable("");
  self.end_date = ko.observable("");
  if (settings.start_date != null) {
    if (typeof settings.start_date != 'function') {
      self.start_date(normalize_date(settings.start_date));
    }
    else {
      self.start_date(settings.start_date);
    }
  }

  if (settings.end_date != null) {
    if (typeof settings.end_date != 'function') {
      self.end_date(normalize_date(settings.end_date));
    }
    else {
      self.end_date(settings.end_date);
    }
  }
  self.static_date = ko.observable("");
  self.day_of_month = ko.observable("");
  self.enroller_picks_default = ko.observable("");
  self.enroller_picks_no_less = ko.observable("");
  self.first_friday = ko.observable("");

  if (self.effective_date_type != "" && (typeof effective_date_settings != 'string')) {
    switch (self.effective_date_type) {
      case 'static_date':
        self.static_date(normalize_date(effective_date_settings.static_date));
        break;
      case 'day_of_month':
        self.day_of_month(effective_date_settings.day_of_month);
        break;
      case 'enroller_selects':
        self.enroller_picks_default(effective_date_settings.enroller_selects.default);
        self.enroller_picks_no_less(effective_date_settings.enroller_selects.no_less);
        break;
      case 'first_friday':
        self.first_friday(effective_date_settings.first_friday);
        break;
      default:
        break;
    }
  }

  self.serialize_effective_date = function () {
    switch (self.effective_date_type) {
      case 'static_date':
        return {"type": self.period_type, "method": self.effective_date_type, "static_date": self.static_date()};
      case 'day_of_month':
        return {"type": self.period_type, "method": self.effective_date_type, "day_of_month": self.day_of_month()};
      case 'enroller_selects':
        return {"type": self.period_type, "method": self.effective_date_type, "enroller_selects": {"default": self.enroller_picks_default(), "no_less": self.enroller_picks_no_less()}};
      case 'first_friday':
        return {"type": self.period_type, "method": self.effective_date_type, "first_friday": self.first_friday()};
      default:
        return {};
    }
  };

  self.get_start_observable = ko.computed(function () {
    if (typeof self.start_date() == 'function') {
        return self.start_date();
      }
    else {
     return self.start_date;
    }
  });

  self.get_end_observable = ko.computed(function () {
    if (typeof self.end_date() == 'function') {
        return self.end_date();
      }
    else {
     return self.end_date;
    }
  });


  self.get_start_date = function () {
    if (typeof self.start_date() == 'function') {
        return normalize_date(self.start_date()());
      }
      else {
       return normalize_date(self.start_date());
      }
  };

  self.get_end_date = function () {
    if (typeof self.end_date() == 'function') {
        return normalize_date(self.end_date()());
      }
      else {
       return normalize_date(self.end_date());
      }
  };

  // Validates month / day formatted as MM/DD
  self.is_valid_month_day = function(val) {
    return self.get_month_day(val).isValid();
  };
  self.get_month_day = function(val) {
    return parse_month_date_input(val);
  };
  
  self.valid_day = function (val) {
    return (0 < val && val < 32);
  };

  // Validates actual date formatted MM/DD/YYYY
  self.is_valid_date = function(val) {
    return is_valid_date(val);
  };

  self.is_valid = ko.computed(function() {
    if (self.is_open) {
        return self.is_valid_date(self.get_start_date()) && self.is_valid_date(self.get_end_date());
    } else {
      /* return (self.is_valid_month_day(self.start_date()) && self.is_valid_month_day(self.end_date())); */
      return true;
    }
  });

  self.serialize = function() {
    if(self.is_open) {
      return {
        period_type: self.period_type,
        case_id: self.case_id,
        start_date: self.get_start_date(),
        end_date: self.get_end_date()
      }
    } else {
      return {
        period_type: self.period_type,
        case_id: self.case_id,
      }
    }
  };
};
