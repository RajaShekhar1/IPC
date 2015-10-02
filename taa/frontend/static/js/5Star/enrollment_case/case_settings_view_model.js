var CaseSettingsPanel = function CaseSettingsPanel(case_data, product_choices, can_edit_case, settings) {
  var self = this;
  self.case_id = case_data.id;
  self.case_token = case_data.case_token;

  self.can_edit_case = can_edit_case;

  self.report_data = ko.observable(null);

  self.report_viewmodel = ko.observable(null);

  self.report_viewmodel.subscribe(function(val) {
    if (val && window.location.hash == "#reports") {
      // try to load reports and enrollments
      val.load_reports();
      val.load_enrollments();
    }
  });

  self.company_name = ko.observable(case_data.company_name || "").extend({ rateLimit: 1000 });

  self.group_number = ko.observable(case_data.group_number || "");

  self.product_choices = ko.observableArray(product_choices);
  // Get the list of selected products from the product choices
  var matched_selected_products = _.filter(self.product_choices(), function(p) {
    return _.contains(_.pluck(case_data.products, "id"), p.id);
  });
  self.products = ko.observableArray(matched_selected_products);

  // Until multi-product is working, make a single-product layer on top of the products array
  self.single_product = ko.computed({
    read: function () {
      return (self.products().length > 0) ? self.products()[0]: null;
    },
    write: function (value) {
      if (value !== null && value !== undefined) {
        self.products([value]);
      } else {
        self.products([]);
      }
    },
    owner: self
  });


  self.emailSettings = {
    sender_name: ko.observable(case_data.self_enrollment_setup.email_sender_name),
    sender_email: ko.observable(case_data.self_enrollment_setup.email_sender_email),
    subject: ko.observable(case_data.self_enrollment_setup.email_subject || settings.default_email_subject),
    greeting_type: ko.observable(case_data.self_enrollment_setup.email_greeting_type),
    greeting_salutation: ko.observable(case_data.self_enrollment_setup.email_greeting_salutation),
    message: ko.observable(case_data.self_enrollment_setup.email_message || settings.default_email_message)
  };

  self.resetEmail = function() {
    self.emailSettings.message(settings.default_email_message);
    self.emailSettings.subject(settings.default_email_subject);
  };

  self.landing = {
    page_title: ko.observable(case_data.self_enrollment_setup.page_title || $("#page_title").html()),
    page_text: ko.observable(case_data.self_enrollment_setup.page_text || $("#page_text").html())
  };


  self.enrollment_period_type = ko.observable(case_data.enrollment_period_type);
  self.enrollment_periods = ko.observableArray($.map(case_data.enrollment_periods, function(p) {
        return new CaseEnrollmentPeriod(p)}
  ));
  self.situs_city = ko.observable(case_data.situs_city);

  self.state_choices = settings.all_states;
  self.situs_state = ko.observable(get_state_from_statecode(case_data.situs_state));

  function get_state_from_statecode(statecode) {
    return _.find(self.state_choices, function(c) {
      return c.statecode == statecode;
    })
  }

  // Overrides for city state when doing an in-person enrollment.
  //
  // Keep a session-storage copy of the last used override (for this case)
  self.enrollment_city_override = ko.observable(get_storage_or_default('enrollment_city_override.'+self.case_id, self.situs_city()));
  self.enrollment_state_override = ko.observable(get_default_state_override());

  // Return empty string rather than null or undefined for city and state
  self.get_enrollment_city_override = ko.pureComputed(function() {
    if (!self.enrollment_city_override()) {
      return "";
    }
    return self.enrollment_city_override();
  });

  self.get_enrollment_state_override = ko.pureComputed(function() {
    if (!self.enrollment_state_override()) {
      return "";
    }
    return self.enrollment_state_override().statecode;
  });

  // Save to session storage whenever the override values change
  set_storage_from_observable('enrollment_city_override.'+self.case_id, self.enrollment_city_override);

  // Note: saving just the statecode here, not the state object
  set_storage_from_observable('enrollment_state_override.'+self.case_id, self.get_enrollment_state_override);


  function get_default_state_override() {

    var case_default_statecode = (self.situs_state()) ? self.situs_state().statecode : "";

    // Use the default statecode unless we have session storage value for this case
    var statecode = get_storage_or_default('enrollment_state_override.'+self.case_id, case_default_statecode);

    // Lookup the state for this statecode
    if (!statecode || !get_state_from_statecode(statecode)) {
      return get_state_from_statecode(case_default_statecode);
    } else {
     return get_state_from_statecode(statecode);
    }
  }


  function get_storage_or_default(key, default_val) {
    if (window.sessionStorage.getItem(key)) {
      return window.sessionStorage.getItem(key);
    }

    return default_val;
  }
  function set_storage_from_observable(key, observable) {
    observable.subscribe(function(new_val) {
      window.sessionStorage.setItem(key, new_val);
    });
  }


  self.include_bank_draft_form = ko.observable(case_data.include_bank_draft_form);

  // Reset the overrides when the case values change
  self.situs_city.subscribe(function(new_val) {
    self.enrollment_city_override(new_val)
  });
  self.situs_state.subscribe(function(new_val) {
    self.enrollment_state_override(new_val);
  });

  self.selected_statecode = ko.pureComputed(function(){
    return (self.situs_state()) ? self.situs_state().statecode : null;
  });
  self.is_active = ko.observable(case_data.active);
  self.owner_agent_id = ko.observable(case_data.agent_id || "");
  var should_restrict_downloads = false;
  if (case_data.can_partners_download_enrollments === false) {
    should_restrict_downloads = true;
  }
  self.restrict_download_enrollments_to_owner = ko.observable(should_restrict_downloads);
  self.can_partners_download_enrollments = ko.pureComputed(function() {
    return !self.restrict_download_enrollments_to_owner();
  });

  self.can_download_enrollments = ko.pureComputed(function() {
    // We don't even show the button if the user is restricted from downloading,
    // this is just so we don't show the button if we don't have data.
    return self.has_census_data();
  });

  self.partner_agents = ko.observable(
      (case_data.partner_agents)? _.map(_.pluck(case_data.partner_agents, "id"), function(id) {return id+""}) : []);


  // Disable bad combos of states and products
  self.state_product_limiter = new ProductStatesLimiterViewModel(settings.product_state_mapping,
      self.situs_state,
      self.state_choices,
      self.products, self.product_choices
  );

  // Limit the states that can be chosen as 'enrolling from state' by the products
  self.state_product_override_limiter = new StatesLimiterViewModel(settings.product_state_mapping,
      self.enrollment_state_override,
      self.state_choices,
      self.products
  );


  self.is_open_enrollment = ko.computed(function() {
    return self.enrollment_period_type() === "open";
  });
  self.is_annual_enrollment = ko.computed(function() {
    return self.enrollment_period_type() === "annual";
  });
  self.get_open_enrollment_period = ko.computed(function() {
    return _.find(self.enrollment_periods(), function(p) {return p.is_open()});
  });

  self.annual_enrollment_periods = ko.computed(function() {
    return _.filter(self.enrollment_periods(), function(p) {return p.period_type === "annual_period"});
  });


  // Get payment modes
  self.payment_mode_choices = settings.payment_modes;
  self.payment_mode = ko.observable(_.find(self.payment_mode_choices, function(c) {
    return c.mode == case_data.payment_mode;
  }));
  self.selected_payment_mode = ko.pureComputed(function(){
    return (self.payment_mode()) ? parseInt(self.payment_mode().mode) : null;
  });

  // Get Rider information
  self.rider_choices = settings.riders;
  // self.riders = ko.observable(self.rider_choices)
  
  self.riders = ko.computed(function() {
    // Show only riders allowed for this product; depends on the product selected.
    return _.reject(self.rider_choices, function(rider) {
      if (!self.single_product()) {
        return false;
      }
      var current_product_name = self.single_product().base_product_type;
      return rider.restrict_to.indexOf(current_product_name) === -1;
    });
  });

  // Self-enrollment
  self.is_self_enrollment = ko.observable(case_data.is_self_enrollment);
  self.is_self_enrollment.subscribe(function() {
    // Confirm disabling self-enrollment
    if(!self.is_self_enrollment()) {
      bootbox.confirm('<h3>Any self-enrollment links you have sent out <u class="text-danger">will no longer work</u> if you turn off self-enrollment.</h3><p>Click <em>OK</em> to turn it off, or <em>Cancel</em> to leave it on.</p>',
          function(result) { self.is_self_enrollment(!result); });
    }
  });
  self.self_enrollment_type = ko.observable(case_data.self_enrollment_type);

  self.enrolling_agent_id = ko.observable(case_data.self_enrollment_setup.enrolling_agent_id);

  // Make sure there is at least one open enrollment period
  if (!self.get_open_enrollment_period()) {
    self.enrollment_periods.push(new CaseEnrollmentPeriod({case_id: case_data.id, period_type: "open_with_start"}))
  }
  // Make at least four annual enrollment periods by default
  if (self.annual_enrollment_periods().length < 4) {
    var num_missing = 4 - self.annual_enrollment_periods().length;
    _.each(_.range(num_missing), function() {
      var p = new CaseEnrollmentPeriod({
        case_id: case_data.id,
        period_type: "annual_period"
      });
      self.enrollment_periods.push(p);
    });
  }

  // utility methods for algorithm below
  function get_annual_periods_after_period(period) {
    var periods = self.annual_enrollment_periods();
    return _.filter(periods, function(p) {return periods.indexOf(p) > periods.indexOf(period)})
  }
  function compute_next_quarter_start(m) {
    return m.add(3, "months");
  }
  function compute_period_end(start, months) {
    return start.add(months, "months").subtract(1, "day");
  }
  function compute_quarterly_dates(period, val) {
    if (val && period.is_valid_month_day(val) && period.end_date() == "") {
      // Auto fill with end of next month approx. alg
      // Initial period is two months after start
      var computed_date = compute_period_end(period.get_month_day(val), 2);
      period.end_date(computed_date.format("MM/DD"));

      // Loop through remaining periods and fill in any that are blank
      var remaining_periods = get_annual_periods_after_period(period);
      var previous_period = period;
      $.each(remaining_periods, function() {
        var rp = this;
        if (!rp.is_valid()) {
          // Fill in this period
          var previous_start = previous_period.get_month_day(previous_period.start_date());
          var computed_start = compute_next_quarter_start(previous_start);
          rp.start_date(computed_start.format("MM/DD"));
          // one month after start
          var computed_finish = compute_period_end(computed_start, 1);
          rp.end_date(computed_finish.format("MM/DD"));
        } else {
          // break - don't continue checking any more periods
          return false;
        }
        previous_period = rp;
      });
    }
  }
  // observe the annual period start dates for auto-filling algorithm
  var first_period = self.annual_enrollment_periods()[0];
  first_period.start_date.extend({ notify: 'always' });
  first_period.start_date.subscribe(function(val) {
    compute_quarterly_dates(first_period, val);
  });

  // Add validation to the month/day inputs
  _.each(self.annual_enrollment_periods(), function(period) {
    _.each([period.start_date, period.end_date], function(date_observable){
      date_observable.subscribe(function(val) {
        if (val == "") return;

        var val_date = parse_month_date_input(val);
        if (!val_date.isValid()) {
          date_observable("");
          bootbox.dialog({title: "Invalid Date", message: "The value '"+val+"' is not a valid start or end date. Provide a month and date formatted as 'MM/DD'.",
            buttons: {main: {label: "OK"}}});
        }
      });
    });
  });

  // enrollment data
  self.census_data = ko.observable([]);
  self.has_census_data = ko.pureComputed(function() {
    return self.census_data().length > 0;
  });

  self.is_data_dirty = ko.observable();

  self.flash_messages = new FlashMessages();

  // subscribe to all changes to detect data changes
  if (self.can_edit_case) {
    var fields = [self.company_name, self.group_number, self.products, self.enrollment_period_type,
      self.enrollment_periods, self.situs_city, self.situs_state, self.payment_mode,
      self.is_active, self.owner_agent_id, self.can_partners_download_enrollments, self.is_self_enrollment
    ];
    _.each(self.enrollment_periods(), function(p) {
      fields.push(p.start_date);
      fields.push(p.end_date);
    });

    $.each(fields, function() {
      var field = this;
      field.subscribe(function() {

        self.is_data_dirty(true);
      });
    });
    // For now, trigger dirty on every change within the self-enroll setup if it is enabled.
    $("#edit-self-enrollment-form").on("change", "input, select, [contenteditable]", function() {
      if (self.is_self_enrollment()) {
        self.is_data_dirty(true);
      }
    });

  }

  self.email_batches = ko.observableArray([]);

  $.getJSON(urls.get_case_api_census_email_batches(case_data.id), function(data) {
    self.email_batches(data.data);
  });

  self.batch_preview = function(batch) {
    window.open(urls.get_case_api_census_email_batch_preview_url(case_data.id, batch.id), "batchView", "menubar=no,location=no,resizable=no,width=650,height=600,scrollbars=yes,status=yes");
  };

  self.load_logs = function(batch) {
    $("#log"+batch.id).html("Loading Logs...");
    $.get(urls.get_case_api_census_email_batch_logs_url(case_data.id, batch.id), function(data) {
      $("#log"+batch.id).html(data);
      $("#log"+batch.id+" .dt-responsive").dataTable();
      $("#log"+batch.id+" .dt-responsive .status").each(function() {
        $(this).html(format_enrollment_status_html($(this).text()))
      });
    });
  };

  self.can_activate_case = ko.computed(function() {
    var is_valid = (
        self.enrollment_period_type() != null &&
        self.enrollment_periods().length > 0 &&
        self.products().length > 0 &&
        $.trim(self.company_name()) != "" &&
        $.trim(self.situs_city()) != "" &&
        self.selected_statecode() != null &&
        self.selected_payment_mode() != null &&
        self.owner_agent_id() > 0
    );

    if (self.is_open_enrollment() && !self.get_open_enrollment_period().is_valid()) {
      is_valid = false;
    } else if (self.is_annual_enrollment()) {
      // Make sure there is at least one valid period date
      is_valid &= _.any(self.annual_enrollment_periods(), function(p) {
        return p.is_valid();
      });
    }

    return is_valid;
  });

  self.is_active.subscribe(function(value) {

    if (value && !self.can_activate_case()) {
      bootbox.alert("Cannot activate case for enrollment until settings are complete.");
      self.is_active(false);
    }

    self.toggle_enrollment_buttons();
  });

  self.toggle_enrollment_buttons = function() {
    // If the case is not active, we must remove enrollment buttons on the census.
    if (self.is_active()) {
      $("button.enroll-employee").prop('disabled', false);
    } else {
      $("button.enroll-employee").prop('disabled', true)
    }
  };
  self.toggle_enrollment_buttons();

  self.switch_label = ko.computed(function() {
    if (self.is_active()) {
      return "Case is Active";
    } else {
      return "Case is Not Active";
    }
  });

  self.get_form_error = ko.pureComputed(function() {
    if (self.company_name.is_unique !== undefined &&
        self.company_name.is_unique() === false) {
      return "The name '"+self.company_name()+"' is already used."
    }
    return "";
  });

  self.check_unique_name = function(current_value, callback) {
    $.get(urls.get_cases_api_url(case_data.id), {by_name: current_value}, function(result) {
      // Must have either 0 cases with this name, or 1 (the current case)
      var is_unique = (result.data.length === 0 || current_value === case_data.company_name);
      callback(is_unique);
    }, "json");

  };


  self.validate = function() {

    hide_all_errors();

    // hide missing date errors
    _.invoke(self.annual_enrollment_periods(), "error", "");


    // all other errors
    var errors = {};

    // unique name error
    var unique_name_error = self.get_form_error();
    if (unique_name_error !== "") {
      add_case_error(errors, "company_name", unique_name_error);
    }

    if ($.trim(self.company_name()) == "") {
      add_case_error(errors, "company_name", "Company name is required.");
    }

    // an invalid enrollment period (both dates can be blank, but if one is populated, both must be)
    if (self.is_open_enrollment() && !self.get_open_enrollment_period().is_valid()) {
      var start = self.get_open_enrollment_period().start_date();
      var end = self.get_open_enrollment_period().end_date();
      if(end != '') {
        if(!is_valid_date(end)) {
          add_case_error(errors, "open_enrollment_end_date", "Enter valid End Date")
        }
        else if(start == '') {
          add_case_error(errors, "open_enrollment_start_date", "Enter valid Start Date or both dates blank")
        }
      }
      if(start != '' && !is_valid_date(start)) {
        add_case_error(errors, "open_enrollment_start_date", "Enter valid Start Date")
      }
      if(start != '' && end != '' && parse_date(start) > parse_date(end)) {
        add_case_error(errors, "open_enrollment_start_date", "Start Date comes after End Date")
      }
    }

    // an invalid annual period (missing start or end)
    if (self.is_annual_enrollment() && self.any_annual_period_missing_a_component()) {
      var missing_observables = _.filter(self.annual_enrollment_periods(), self.missing_annual_period_predicate);
      _.invoke(missing_observables, "error", "Must fill in start and end date");
      return false;
    }

    if (Object.keys(errors).length > 0) {
      show_all_errors(errors);
      return false;
    } else {
      return true;
    }
  };

  self.missing_annual_period_predicate = function(period) {
    var start_date_present = period.start_date() != "";
    var end_date_present = period.end_date() != "";
    // XOR - one or the other, but not both the same
    return (start_date_present ? !end_date_present : end_date_present);
  };
  self.any_annual_period_missing_a_component = function() {
    return _.any(self.annual_enrollment_periods(), self.missing_annual_period_predicate);
  };

  self.serialize_case = function() {
    var partner_agents = _.map(self.partner_agents(), function(id_str) {
      return parseInt(id_str)
    });

    return {
      company_name: self.company_name(),
      group_number: self.group_number(),
      active: self.is_active(),
      products: self.products(),
      partner_agents: partner_agents,
      enrollment_period_type: self.enrollment_period_type(),
      situs_city: self.situs_city(),
      situs_state: self.selected_statecode() ? self.selected_statecode() : "",
      payment_mode: self.selected_payment_mode() ? self.selected_payment_mode() : null,
      agent_id: self.owner_agent_id(),
      can_partners_download_enrollments: self.can_partners_download_enrollments(),
      is_self_enrollment: self.is_self_enrollment(),
      riders: self.riders(),
      include_bank_draft_form: self.include_bank_draft_form()
    }
  };

  self.serialize_enrollment_periods = function() {
    if (self.is_annual_enrollment()) {
      // Only send valid periods
      return _.invoke(
          _.filter(self.annual_enrollment_periods(), function(p) {return p.is_valid()}),
          "serialize");
    } else {
      var period = self.get_open_enrollment_period();
      return [period.serialize()];
    }
  };

  self.serialize_self_enroll = function() {
    var settings = $.extend({}, self.emailSettings, self.landing);
    for(var key in settings) {
      if(settings.hasOwnProperty(key)) {
        settings[key] = settings[key]();
      }
    }
    return {
      self_enrollment_type: self.self_enrollment_type(),
      enrolling_agent_id: self.enrolling_agent_id(),
      email_sender_email: settings.sender_email,
      email_sender_name: settings.sender_name,
      email_subject: settings.subject,
      email_greeting_type: settings.greeting_type,
      email_greeting_salutation: settings.greeting_salutation,
      email_message: settings.message,
      page_title: settings.page_title,
      page_text: settings.page_text
    };
  };

  self.loading_modal = ko.observable(null);


  self.save_settings = function(cb) {
    self.flash_messages.clear();

    if (!self.can_save_case()) {
      return;
    }

    self.loading_modal({message: "Saving case..."});

    // Make sure case can be activated
    if (self.is_active() && !self.can_activate_case()) {
      self.is_active(false);
      self.flash_messages.flash_error("The case has been deactivated due to missing settings.");
      // Continue saving the case
    }

    var case_request = send_json_data(
        "PUT",
        urls.get_case_api_url(case_data.id),
        self.serialize_case()
    );

    var periods_request = send_json_data(
        "PUT",
        urls.get_case_api_enrollment_periods_url(case_data.id),
        self.serialize_enrollment_periods()
    );

    if (self.is_self_enrollment()) {
      // Also save self enrollment settings.
      var self_enroll_request = send_json_data(
          "PUT",
          urls.get_case_api_self_enrollment_url(case_data.id),
          self.serialize_self_enroll()
      );
    }

    // Self Enroll settings
    $('#save-success').hide();
    $('#save-fail').hide();

    var on_success = function(case_xhr, periods_xhr, self_enroll_xhr) {
      self.is_data_dirty(false);
      self.flash_messages.flash_success("Data Saved");
      $("#save-success").text("Data Saved");
      self.loading_modal(null);
    };
    var on_failure = function(failed_xhr) {
      var errors = failed_xhr.responseJSON.errors || [];
      show_all_errors(errors);
      self.loading_modal(null);
      self.flash_messages.flash_error("Save failed. Please correct any errors below.");
    };
    if (!self.is_self_enrollment()) {
      var done = $.when(case_request, periods_request);
    } else {
      var done = $.when(case_request, periods_request, self_enroll_request);
    }

    if(cb && typeof(cb) === "function") {
      cb(done);
      self.loading_modal(null);
      return;
    }

    done.then(on_success, on_failure);

    return false;
  };

  self.can_save_case = function() {
    if (!self.can_edit_case) {
      self.flash_messages.flash_error("Only the case owner agent or home office can make changes to the case.");
      return false;
    }

    if (!self.validate()) {
      self.flash_messages.flash_error("Please correct any errors below before saving.");
      return false;
    }

    // Make sure case can be activated
    if (self.is_active() && !self.can_activate_case()) {
      self.is_active(false);
      self.flash_messages.flash_error("The case has been deactivated due to missing settings.");
      // Continue saving the case
    }

    return true;
  };

  self.delete_case = function() {
    bootbox.confirm("Are you sure you want to the case '"+self.company_name()+"'? This is permanent and cannot be undone!", function(result) {
      if (!result) { return; }

      self.remote_delete();
    });
  };

  self.remote_delete = function() {
    var request = send_json_data("DELETE", urls.get_case_api_url(case_data.id));
    $.when(request).then(function() {
      bootbox.alert("Case deleted successfully");
      // Allow leaving the page (disables the page unload trigger prompt)
      self.is_data_dirty(false);
      // Go back to main page
      window.location.href = urls.get_manage_cases_url(case_data.id);
    }, function() {
      bootbox.alert("There was a problem removing this case.");
    });
  };

  self.is_permanent_delete_modal_showing = ko.observable(false);
  self.delete_confirmation_text = ko.observable("");
  self.is_delete_text_valid = ko.pureComputed(function() {
    return self.delete_confirmation_text() == "DELETE";
  });

  self.delete_case_with_enrollments = function() {
    // Show the dialog
    self.delete_confirmation_text("");
    // Toggle off, then on since we don't detect modal hide right now
    self.is_permanent_delete_modal_showing(false);
    self.is_permanent_delete_modal_showing(true);
  };

  if (self.can_edit_case) {
    $(window).bind("beforeunload", function() {
      return self.has_unsaved_data() ? "You have made changes without saving. Do you you wish to leave this page and lose all changes?" : undefined;
    });
  }

  self.show_upload_enrollment_form = function() {
    var el = $("#enrollment-csv-modal");
    reset_upload_modal(el);
    el.modal("show");
  };


  self.has_unsaved_data = ko.computed(function() {
    return self.is_data_dirty();
  });

  self.exit_print_mode = function() {
    if (self.report_viewmodel()) {
      self.report_viewmodel().exit_print_preview();
    }
  };

  // Tabs
  var setup_tab = $('#case-nav-tabs a[href="#setup"]');
  var enrollment_tab = $('#case-nav-tabs a[href="#enrollment"]');
  var history_tab = $('#case-nav-tabs a[href="#history"]');
  var reports_tab = $('#case-nav-tabs a[href="#reports"]');
  var api_tab = $('#case-nav-tabs a[href="#api"]');

  self.sammy_app = Sammy(function() {
    this.get("#setup", function() {
      self.exit_print_mode();

      setup_tab.tab('show');

      // Disable all data entry on case setup if case is not editable
      if (!self.can_edit_case) {
        $("#setup").find("input, select, button").prop("disabled", true);
      }

    });

    // If switching to another tab, we check for dirty and warn the user
    this.before({except: {path: '#setup'}}, function() {

      if (self.has_unsaved_data()) {
        bootbox.dialog({
          title: "Unsaved Changes",
          message: "You have made changes without saving. Please save your changes or discard before leaving this page.",
          buttons: {
            main: {label: "Save", className: 'btn-primary', callback: function() {
              self.save_settings();
            }},
            danger: {label: "Discard Changes", className: 'btn-danger', callback: function() {
              // disable the page unload check
              self.is_data_dirty(false);
              window.location.reload();
            }},
            cancel: {label: "Cancel", className: 'btn-default', callback: function() {}}
          }
        });

        this.redirect("#setup");
        return false;
      }
    });

    this.get("#enrollment", function() {
      self.exit_print_mode();

      enrollment_tab.tab('show');

      // Load census data when tab loads
      console.log("SetTimeout for load_initial_census_data")
      setTimeout(load_initial_census_data, 0);

      function load_initial_census_data() {
        case_management.refresh_census_table(case_data.id, urls.get_case_api_census_records_url(case_data.id),
            "#census-records-table", "#census-table-loading",
            handle_census_data_loaded_first_time,
            handle_no_census_data_loaded,
            handle_census_data_loaded
        );
      }

      function handle_census_data_loaded_first_time(table, data) {
        case_management.init_status_filter(table);
        case_management.init_alphabet_search(table);
      }

      function handle_census_data_loaded(table, data) {
        self.census_data(data);
        self.toggle_enrollment_buttons();
      }

      function handle_no_census_data_loaded() {
        self.census_data([]);
        self.toggle_enrollment_buttons();
      }
    });

    this.get("#history", function() {
      self.exit_print_mode();
      history_tab.tab('show');
      $.getJSON(urls.get_case_api_census_email_batches(case_data.id), function(data) {
        self.email_batches(data.data);
      });
    });

    this.get("#reports", function() {
      self.exit_print_mode();

      reports_tab.tab('show');
      if (self.report_viewmodel()) {
        self.report_viewmodel().load_reports();
        self.report_viewmodel().load_enrollments();
      }
    });

    this.get("#api", function() {
      self.exit_print_mode();
      api_tab.tab('show');
    });


    this.get("#print", function() {
      if (self.report_viewmodel()) {
        self.report_viewmodel().enter_print_preview();
        window.print();
      } else {
        // Go to report page
        window.location = "#reports";
      }
    });
  }).run();

};
