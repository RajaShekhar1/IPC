/* AddToCensusViewModel */
var AddToCensusViewModel = function AddToCensusViewModel(case_id) {
  var self = this;

  self.case_id = ko.observable(case_id);

  self.matched_employees = ko.observableArray([]);
  self.matched_employees.error = ko.observable("");
  self.selected_employee = ko.observable(null);

  self.matched_employee = ko.computed(function() {
    if (self.matched_employees().length == 0) {
      return null;
    }
    return self.matched_employees()[0];
  });

  self.loading_message = ko.observable("Loading...");

  self.ssn = ko.observable(null);
  self.ssn.error = ko.observable("");

  // Panel state
  self.PANEL_FORM = "find_employee";
  self.PANEL_LOADING = "loading";
  self.PANEL_NOMATCH = "no_matches";
  self.PANEL_ONEMATCH = "one_match";
  self.PANEL_MANYMATCHES = "matches";
  self.current_panel = ko.observable(self.PANEL_FORM);

  self.show_modal = function() {
    // Initialize the modal
    self.current_panel(self.PANEL_FORM);
    self.ssn("");
    self.ssn.error("");

    self.selected_employee(null);
  };

  self.is_match_form_showing = ko.computed(function() {
    return (self.current_panel() == self.PANEL_NOMATCH ||
    self.current_panel() == self.PANEL_MANYMATCHES ||
    self.current_panel() == self.PANEL_ONEMATCH);
  });
  self.add_form_showing = ko.computed(function() {
    return self.current_panel() == self.PANEL_FORM;
  });
  self.is_loading = ko.computed(function() {
    return self.current_panel() == self.PANEL_LOADING;
  });
  self.no_matches_found = ko.computed(function() {
    return self.current_panel() == self.PANEL_NOMATCH;
  });
  self.multiple_matches_found = ko.computed(function() {
    return self.current_panel() == self.PANEL_MANYMATCHES;
  });
  self.one_match_found = ko.computed(function() {
    return self.current_panel() == self.PANEL_ONEMATCH;
  });
  self.one_or_more_matches_found = ko.computed(function() {
    return self.multiple_matches_found() || self.one_match_found();
  });

  self.find_matches = function() {
    // Clear errors
    self.ssn.error("");

    if (!self.is_valid_ssn()) {
      self.ssn.error("A valid SSN is required");
      return;
    }

    // Submit search
    self.loading_message("Checking census for matches...");
    self.current_panel(self.PANEL_LOADING);
    $.get("/cases/" + self.case_id() + "/census_records",
    {filter_ssn: self.ssn()})
    .success(function(resp) {
      if (resp.data.length > 0) {
        // Load matches
        self.matched_employees(_.map(resp.data, function(e) {return new EmployeeMatchViewModel(e)}));

        // Display match panel
        if (resp.data.length == 1) {
          self.current_panel(self.PANEL_ONEMATCH);
        } else {
          self.current_panel(self.PANEL_MANYMATCHES);
        }
      } else {
        // No matches
        self.matched_employees([]);
        self.current_panel(self.PANEL_NOMATCH);
      }
    })
    .error(function(resp) {
      console.error(resp);
    });
  };

  self.is_valid_ssn = ko.computed(function() {
    var ssn_regex = /^\d\d\d-\d\d-\d\d\d\d$/;
    return ssn_regex.test(self.ssn());
  });

  self.add_to_census = function() {
    self.loading_message("Adding employee to census...");
    self.current_panel(self.PANEL_LOADING);
    ajax_post("/cases/"+self.case_id()+"/census_records",
    {ssn: self.ssn()},
    function(resp) {
      // Go to enrollment
      self.enroll_from_record(resp.data.id);
    }, function(resp) {
      bootbox.alert("There was a problem adding a record to the census");
    });
  };

  self.enroll_from_match = function() {
    if (self.matched_employees().length == 1) {
      self.enroll_from_record(self.matched_employees()[0].id);
    } else {
      // Use the selected record
      var id = self.selected_employee();
      if (id === null) {
        self.matched_employees.error("Please select the census record to enroll.");
        return;
      }
      self.enroll_from_record(id);
    }
  };

  self.enroll_from_record = function(id) {
    self.loading_message("Loading Enrollment Application...");
    self.current_panel(self.PANEL_LOADING);

    submit_to_url(urls.get_in_person_enrollment_url(), {
      record_id: id,
      enrollment_city: window.case_settings.get_enrollment_city_override(),
      enrollment_state: window.case_settings.get_enrollment_state_override()
    });
  }
};
/* END: AddToCensusViewModel */
