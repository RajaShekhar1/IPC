var CaseEnrollmentPeriod = function CaseEnrollmentPeriod(period) {
  var self = this;
  var defaults = {
    period_type: "annual_period",
    case_id: null,
    start_date: "",
    end_date: ""
  };
  var settings = $.extend({}, defaults, period);

  self.period_type = settings.period_type;
  self.is_annual = ko.computed(function() {return self.period_type == "annual_period"});
  self.is_open = ko.computed(function() {return !self.is_annual()});

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


  if (self.is_open()) {
    // process start/end date normally
    self.start_date = ko.observable(normalize_date(settings.start_date));
    self.end_date = ko.observable(normalize_date(settings.end_date));
  } else {
    self.start_date = ko.observable(strip_year(settings.start_date));
    self.end_date = ko.observable(strip_year(settings.end_date));
  }

  // Validates month / day formatted as MM/DD
  self.is_valid_month_day = function(val) {
    return self.get_month_day(val).isValid();
  };
  self.get_month_day = function(val) {
    return parse_month_date_input(val);
  };

  // Validates actual date formatted MM/DD/YYYY
  self.is_valid_date = function(val) {
    return is_valid_date(val);
  };

  self.is_valid = ko.computed(function() {
    if (self.is_open()) {

      // End date is optional, but must be valid if present.
      if (self.end_date()) {
        return (
          self.is_valid_date(self.start_date())
          && self.is_valid_date(self.end_date())
          && parse_date(self.end_date()) >= parse_date(self.start_date())
        );
      } else {
        return self.is_valid_date(self.start_date());
      }
    } else {
      return (self.is_valid_month_day(self.start_date()) && self.is_valid_month_day(self.end_date()));
    }
  });

  self.serialize = function() {
    return {
      period_type: self.period_type,
      case_id: self.case_id,
      start_date: self.start_date(),
      end_date: self.end_date()
    }
  };
};