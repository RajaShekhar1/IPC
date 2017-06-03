var CaseViewModel = function CaseViewModel(case_data, product_choices, can_edit_case, settings, product_rate_levels) {
  var self = this;

  _.defaults(case_data, {
    omit_actively_at_work: false,
    requires_paylogix_export: false
  });

  self.case_id = case_data.id;
  self.case_token = case_data.case_token;
  self.product_rate_levels = product_rate_levels || {};
  self.state_override_view_models = ko.observableArray([]);

  self.product_state_mapping = ko.computed(function () {
    // Start with the default product-state map.
    var mapping = _.cloneDeep(settings.product_state_mapping);

    // If any product overrides the default state mapping, set the overrides here.
    //  This also triggers a dependency on the state_override_view_models() array and
    //  any list of state_overrides() that are enabled.
    _.each(self.state_override_view_models(), function (ovm) {
      if (ovm.restrict_state_availability()) {
        mapping[ovm.id] = ovm.state_overrides();
      }
    });

    return mapping;
  });


  self.can_edit_case = can_edit_case;

  self.report_data = ko.observable(null);

  self.report_viewmodel = ko.observable(null);

  self.is_data_dirty = ko.observable();

  self.report_viewmodel.subscribe(function (val) {
    if (val && window.location.hash == "#reports") {
      // try to load reports and enrollments
      val.load_reports();
      val.load_enrollments();
    }
  });

  self.company_name = ko.observable(case_data.company_name || "").extend({
    rateLimit: 1000
  });

  self.group_number = ko.observable(case_data.group_number || "");

  self.product_choices = ko.observableArray(product_choices);
  // Get the list of initially selected products from the product choices and sort it by ordinals
  var initially_selected_products = _.chain(self.product_choices())
    .filter(function (p) {
      return _.includes(_.map(case_data.products, "id"), p.id);
    })
    .value();
  initially_selected_products = _.sortBy(initially_selected_products, function (product) {
    return product.ordinal;
  });

  self.products = ko.observableArray(initially_selected_products);

  self.effective_products = ko.observableArray("");
  self.initialize_effective_products = function () {
    if (case_data.products.length < 1) {
      self.effective_products(_.map(self.product_choices(), function (p) {
        return new ProductEffectiveDateSettings(p, null);
      }));
    }
    else {
      if (case_data.product_settings != null) {
        self.effective_products(_.map(self.products(), function (p) {
          return new ProductEffectiveDateSettings(p, _.find(case_data.product_settings.effective_date_settings, function (ep) {
            return ep.product_id == p.id;
          }));
        }));
      }
    }
  };

  self.initialize_effective_products();

  self.should_show_coverage_limits = function(product) {
    return self.should_show_max_coverage(product) || self.should_show_max_age(product);
  };

  self.should_show_max_coverage = function(product) {
    return !_.includes(["HI", 'HIB01', 'HIA01', "HIAOBG", "ACC", 'ACAOF', "Static Benefit"], product.base_product_type);
  };

  self.should_show_max_age = function(product) {
    return !_.includes(["HI", 'HIB01', 'HIA01', "HIAOBG", "ACC", 'ACAOF', "Static Benefit"], product.base_product_type);
  };

  self.get_product_effective_date_settings = function (product) {
    var effective = _.find(self.effective_products(), function (ep) {
      return ep.id == product.id;
    });
    if (typeof effective == typeof undefined) {
      var effective = new ProductEffectiveDateSettings(product, null);
    }
    self.effective_products.push(effective);
    return effective;
  };

  self.should_show_coverage_limits = function(product) {
    return self.should_show_max_coverage(product) || self.should_show_max_age(product);
  };

  self.should_show_max_coverage = function(product) {
    return !_.includes(["HI", 'HIB01', 'HIA01', "HIAOBG", "ACC", 'ACAOF', "Static Benefit"], product.base_product_type);
  };

  self.should_show_max_age = function(product) {
    return !_.includes(["HI", 'HIB01', 'HIA01', "HIAOBG", "ACC", 'ACAOF', "Static Benefit"], product.base_product_type);
  };


  function get_coverage_limit_settings_for_product(product) {
    var product_settings = {};
    if (case_data.product_settings &&
        case_data.product_settings.coverage_limits &&
        case_data.product_settings.coverage_limits[product.id]) {
      product_settings = case_data.product_settings.coverage_limits[product.id];
    }
    return product_settings;
  }

  // Coverage limits
  self.get_maximum_age_vm_for_product = function(product) {
    return coverage_settings_module.get_max_age_vm_for_product({
      product: product,
      settings: get_coverage_limit_settings_for_product(product)
    });
  };


  self.get_maximum_coverage_vm_for_product = function(product) {
    return coverage_settings_module.get_max_coverage_vm_for_product({
      payment_mode: self.payment_mode,
      product: product,
      settings: get_coverage_limit_settings_for_product(product)
    });
  };


  self.sort_selected_products = ko.observableArray([]);
  self.products.subscribe(function () {
    self.is_data_dirty(true);
    self.update_product_ordinals();
  });

  self.has_fpp_products = ko.pureComputed(function () {
    return _.any(self.products(), function (product) {
      return _.startsWith(product.base_product_type, "FPP");
    });
  });

  self.omit_actively_at_work = ko.observable(case_data.omit_actively_at_work);
  self.omit_actively_at_work.subscribe(function () {
    self.is_data_dirty(true);
  });

  self.has_product_sort_selections = ko.pureComputed(function () {
    return !!self.sort_selected_products() && self.sort_selected_products().length > 0;
  });

  self.unselected_products = ko.pureComputed(function () {
      var sorted_unselected_productes = _.sortBy(_.difference(self.product_choices(), self.products()), function (product) {
          //return product.ordinal;
          return product.name;
      });
      return sorted_unselected_productes;
  });

  self.selected_product = ko.observable(null);

  self.has_selected_product = ko.pureComputed(function () {
    return !!self.selected_product();
  });

  self.add_selected_product = function () {
    var product = self.selected_product();
    if (!product) {
      return;
    }
    self.products.push(product);
  };

  self.remove_selected_products = function () {
    var products_to_remove = self.sort_selected_products();
    var products = self.products();
    var effective_products = self.effective_products();
    if (!products_to_remove || products_to_remove.length === 0) {
      return;
    }
    _.forEach(products_to_remove, function (product) {
      var effective_index =  _.findIndex(effective_products, function (ep) {
        return ep.id == product.id
      });
      if (effective_index !== -1) {
        effective_products.splice(effective_index, 1);
      }
    });
    self.effective_products(effective_products);

    _.forEach(products_to_remove, function (product) {
      var index = products.indexOf(product);
      if (index !== -1) {
        products.splice(index, 1);
      }
    });
    self.products(products);
    self.update_product_ordinals();
  };

  self.emailSettings = {
    sender_name: ko.observable(case_data.self_enrollment_setup.email_sender_name),
    sender_email: ko.observable(case_data.self_enrollment_setup.email_sender_email),
    subject: ko.observable(case_data.self_enrollment_setup.email_subject || settings.default_email_subject),
    greeting_type: ko.observable(case_data.self_enrollment_setup.email_greeting_type),
    greeting_salutation: ko.observable(case_data.self_enrollment_setup.email_greeting_salutation),
    message: ko.observable(case_data.self_enrollment_setup.email_message || settings.default_email_message)
  };

  self.resetEmail = function () {
    self.emailSettings.message(settings.default_email_message);
    self.emailSettings.subject(settings.default_email_subject);
  };

  self.landing = {
    page_title: ko.observable(case_data.self_enrollment_setup.page_title || $("#page_title").html()),
    page_text: ko.observable(case_data.self_enrollment_setup.page_text || $("#page_text").html())
  };

  //region Product Sorting
  self.update_product_ordinals = function () {
    for (var idx = 0; idx < self.products().length; idx++) {
      var product = self.products()[idx];
      product.ordinal = idx;
    }
  };

  self.move_products_up = function () {
    var productsToSort = self.sort_selected_products();
    if (!productsToSort || productsToSort.length === 0 || productsToSort.length === self.products().length) {
      return;
    }
    var selectedProducts = self.products();
    var startPosition = selectedProducts.indexOf(_.first(productsToSort));
    if (startPosition === 0) {
      return;
    }

    _.forEach(productsToSort, function (product) {
      var position = selectedProducts.indexOf(product);
      if (position === 0 || position === -1) {
        return;
      }
      selectedProducts.splice(position, 1);
      selectedProducts.splice(position - 1, 0, product);
      self.products(selectedProducts);
    });
    self.update_product_ordinals();
  };

  self.move_products_down = function () {
    var productsToSort = self.sort_selected_products();
    if (!productsToSort || productsToSort.length === 0 || productsToSort.length === self.products().length) {
      return;
    }
    var selectedProducts = self.products();

    var endPosition = selectedProducts.indexOf(_.last(productsToSort));
    if (endPosition === selectedProducts.length - 1) {
      return;
    }

    _.chain(productsToSort)
      .reverse()
      .forEach(function (product) {
        var position = selectedProducts.indexOf(product);
        if (position === -1) {
          return;
        }
        selectedProducts.splice(position, 1);
        selectedProducts.splice(position + 1, 0, product);
        self.products(selectedProducts);
      }).value();
    self.update_product_ordinals();
  };
  //endregion

  //region Product Rate Levels
  self.get_rate_levels_for_product_code = function (product_code) {
    if (_.has(self.product_rate_levels, product_code)) {
      return self.product_rate_levels[product_code];
    } else {
      return [];
    }
  };
  //endregion
  self.parent_start_date = ko.observable("");
  self.parent_end_date = ko.observable("");

  self.enrollment_periods = ko.observableArray(_.zipWith(case_data.enrollment_periods, case_data.effective_date_settings, function (p, e) {
    if (p.period_type == 'open_with_start') {
      self.parent_start_date(normalize_date(p.start_date));
      self.parent_end_date(normalize_date(p.end_date));
    }
    return new CaseEnrollmentPeriod(p, e);
  }));


  self.situs_city = ko.observable(case_data.situs_city);


  self.state_choices = settings.all_states;
  self.situs_state = ko.observable(get_state_from_statecode(case_data.situs_state));

  function get_state_from_statecode(statecode) {
    return _.find(self.state_choices, function (c) {
      return c.statecode == statecode;
    });
  }

  // Overrides for city state when doing an in-person enrollment.
  //
  // Keep a session-storage copy of the last used override (for this case)
  self.enrollment_city_override = ko.observable(get_storage_or_default('enrollment_city_override.' + self.case_id, self.situs_city()));
  self.enrollment_state_override = ko.observable(get_default_state_override());

  // Return empty string rather than null or undefined for city and state
  self.get_enrollment_city_override = ko.pureComputed(function () {
    if (!self.enrollment_city_override()) {
      return "";
    }
    return self.enrollment_city_override();
  });

  self.get_enrollment_state_override = ko.pureComputed(function () {
    if (!self.enrollment_state_override()) {
      return "";
    }
    return self.enrollment_state_override().statecode;
  });
  // Save to session storage whenever the override values change
  set_storage_from_observable('enrollment_city_override.' + self.case_id, self.enrollment_city_override);

  // Note: saving just the statecode here, not the state object
  set_storage_from_observable('enrollment_state_override.' + self.case_id, self.get_enrollment_state_override);

  function get_default_state_override() {

    var case_default_statecode = (self.situs_state()) ? self.situs_state().statecode : "";

    // Use the default statecode unless we have session storage value for this case
    var statecode = get_storage_or_default('enrollment_state_override.' + self.case_id, case_default_statecode);

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
    observable.subscribe(function (new_val) {
      window.sessionStorage.setItem(key, new_val);
    });
  }

  // Reset the overrides when the case values change
  self.situs_city.subscribe(function (new_val) {
    self.enrollment_city_override(new_val);
  });
  self.situs_state.subscribe(function (new_val) {
    self.enrollment_state_override(new_val);
  });

  self.selected_statecode = ko.pureComputed(function () {
    return (self.situs_state()) ? self.situs_state().statecode : null;
  });
  self.is_active = ko.observable(case_data.active);
  self.owner_agent_id = ko.observable(case_data.agent_id || "");
  var should_restrict_downloads = false;
  if (case_data.can_partners_download_enrollments === false) {
    should_restrict_downloads = true;
  }
  self.restrict_download_enrollments_to_owner = ko.observable(should_restrict_downloads);
  self.can_partners_download_enrollments = ko.pureComputed(function () {
    return !self.restrict_download_enrollments_to_owner();
  });

  self.can_download_enrollments = ko.pureComputed(function () {
    // We don't even show the button if the user is restricted from downloading,
    // this is just so we don't show the button if we don't have data.
    return self.has_census_data();
  });

  self.download_enrollments_modal = ko.observable(false);
  self.download_enrollments_url = ko.observable(null);
  self.download_enrollment_error = ko.observable(null);

  self.download_enrollments = function () {
    // reset dialogue, show modal
    self.download_enrollments_url(null);
    self.download_enrollment_error(null);
    self.download_enrollments_modal(true);

    var url = urls.get_case_api_enrollment_records_url(self.case_id) + "?format=csv&poll=true";
    $.getJSON(url, function (data) {

      window.setTimeout(function () {
        self.poll_export_file(data.data.poll_url)
      }, 5000);

    }).error(function () {
      self.download_enrollment_error("There was a problem generating the export file. Please try again later.");
    });
  };

  self.poll_export_file = function (poll_url) {
    $.get(poll_url).success(function (resp) {
      if (!resp.data.is_ready) {
        // Try again
        window.setTimeout(function () {
          self.poll_export_file(poll_url)
        }, 5000);
      } else {
        // Show the download button for fetching the file
        self.download_enrollments_url(resp.data.download_url);
      }
    }).error(function () {
      self.download_enrollment_error("There was a problem generating the export file. Please try again later.");
    });

  };

  self.partner_agents = ko.observable(
    (case_data.partner_agents) ? _.map(_.pluck(case_data.partner_agents, "id"), function (id) {
      return id + "";
    }) : []);

  // Disable bad combos of states and products
  self.state_product_limiter = new ProductStatesLimiterViewModel(self.product_state_mapping, self.situs_state, self.state_choices, self.products, self.product_choices);

  self.on_products_select_rendered = function (option, item) {
    if (!item) {
      return;
    }
    var is_valid = ko.pureComputed(function () {
      return self.state_product_limiter.is_valid_product_for_state(item, self.situs_state());
    });
    ko.applyBindingAccessorsToNode(option, {
      enable: is_valid
    }, item);
  };

  // Track whether or not we are editing products.
  self.is_editing_products = ko.observable(false);
  if (case_data.products.length === 0) {
    self.is_editing_products(true);
  }
  self.start_editing_products = function () {
    self.is_editing_products(true);
  };
  self.stop_editing_products = function () {
    self.is_editing_products(false);
  };

  // Limit the states that can be chosen as 'enrolling from state' by the products
  self.state_product_override_limiter = new StatesLimiterViewModel(self.product_state_mapping,
    self.enrollment_state_override,
    self.state_choices,
    self.products
  );

  self.on_state_selected = function () {
    self.selected_product(null);
  };

  self.state_product_limiter.selected_state.subscribe(self.on_state_selected);
  self.state_product_override_limiter.selected_state.subscribe(self.on_state_selected);

  self.enrollmentOptions = ko.observableArray([
    {
      type: "none",
      text: "(Select Enrollment Option)",
      value: "None"
    },
    {
      type: "open_with_start",
      text: "Enrollment Period",
      value: "Open Enrollment"
    },
    {
      type: "both",
      text: "Enrollment Period with Ongoing",
      value: "Open Enrollment and Ongoing thereafter"
    },
    {
      type: "ongoing",
      text: "Always Open",
      value: "Ongoing Enrollment"
    }
  ]);

  self.openEnrollmentOptions = ko.observableArray([
    new CaseEnrollmentPeriod({
      case_id: case_data.id,
      start_date: self.parent_start_date,
      end_date: self.parent_end_date,
      description: "(Select an Effective Date Type)"
    }, "default"),
    new CaseEnrollmentPeriod({
      period_type: "open_with_start",
      case_id: case_data.id,
      start_date: self.parent_start_date,
      end_date: self.parent_end_date,
      description: "Static Date"
    }, "static_date"),
    new CaseEnrollmentPeriod({
      period_type: "open_with_start",
      case_id: case_data.id,
      start_date: self.parent_start_date,
      end_date: self.parent_end_date,
      description: "Cutoff day of month"
    }, "day_of_month"),
    new CaseEnrollmentPeriod({
      period_type: "open_with_start",
      case_id: case_data.id,
      start_date: self.parent_start_date,
      end_date: self.parent_end_date,
      description: "Enroller selects"
    }, "enroller_selects"),
    new CaseEnrollmentPeriod({
      period_type: "open_with_start",
      case_id: case_data.id,
      start_date: self.parent_start_date,
      end_date: self.parent_end_date,
      description: "Friday grouping"
    }, "first_friday")
  ]);

  self.ongoingEnrollmentOptions = ko.observableArray([
    new CaseEnrollmentPeriod({
      case_id: case_data.id,
      description: "(Select an Effective Date Type)"
    }, "default"),
    new CaseEnrollmentPeriod({
      period_type: "ongoing",
      case_id: case_data.id,
      description: "Static Date"
    }, "static_date"),
    new CaseEnrollmentPeriod({
      period_type: "ongoing",
      case_id: case_data.id,
      description: "Cutoff day of month"
    }, "day_of_month"),
    new CaseEnrollmentPeriod({
      period_type: "ongoing",
      case_id: case_data.id,
      description: "Enroller selects"
    }, "enroller_selects"),
    new CaseEnrollmentPeriod({
      period_type: "ongoing",
      case_id: case_data.id,
      description: "Friday grouping"
    }, "first_friday")
  ]);

  self.open_enrollment_status = ko.observable(false);
  self.ongoing_enrollment_status = ko.observable(false);

  self.has_open_enrollment = ko.computed(function () {
    var has_open_enrollment = false;
    _.each(self.enrollment_periods(), function (p) {
      if (p.is_open) {
        has_open_enrollment = true;
        self.open_enrollment_status(true);
      }
    });
    return has_open_enrollment;
  });
  self.has_ongoing_enrollment = ko.computed(function () {
    var has_ongoing_enrollment = false;
    _.each(self.enrollment_periods(), function (p) {
      if (p.is_ongoing) {
        has_ongoing_enrollment = true;
        self.ongoing_enrollment_status(true);
      }
    });
    return has_ongoing_enrollment;
  });


  self.get_enrollment_options = ko.pureComputed(function () {
    if (self.open_enrollment_status() && !self.ongoing_enrollment_status()) {
      var open = _.find(self.enrollment_periods(), function (p) {
        return p;
      });
      if (open.effective_date_type != 'default') {
        self.selectedOpenOption = ko.observable(_.findWhere(self.openEnrollmentOptions(), {effective_date_type: open.effective_date_type}));
        self.set_enrollment_method_data(open, self.selectedOpenOption());
      }
      self.selectedEnrollment = ko.observable(self.enrollmentOptions()[1]);
      self.selectedOngoingOption = ko.observable(self.ongoingEnrollmentOptions()[0]);
    }
    else if (self.open_enrollment_status() && self.ongoing_enrollment_status()) {
      _.each(self.enrollment_periods(), function (p) {
        if (p.period_type == 'open_with_start') {
          self.selectedOpenOption = ko.observable(_.findWhere(self.openEnrollmentOptions(), {effective_date_type: p.effective_date_type}));
          self.set_enrollment_method_data(p, self.selectedOpenOption());
        }
        if (p.period_type == 'ongoing') {
          self.selectedOngoingOption = ko.observable(_.findWhere(self.ongoingEnrollmentOptions(), {effective_date_type: p.effective_date_type}));
          self.set_enrollment_method_data(p, self.selectedOngoingOption());
        }
      });
      self.selectedEnrollment = ko.observable(self.enrollmentOptions()[2]);
    }
    else if (!self.open_enrollment_status() && self.ongoing_enrollment_status()) {
      var ongoing = _.find(self.enrollment_periods(), function (p) {
        return p;
      });
      if (ongoing.effective_date_type != 'default') {
        self.selectedOngoingOption = ko.observable(_.findWhere(self.ongoingEnrollmentOptions(), {effective_date_type: ongoing.effective_date_type}));
        self.set_enrollment_method_data(ongoing, self.selectedOngoingOption());
      }
      self.selectedEnrollment = ko.observable(self.enrollmentOptions()[3]);
      self.selectedOpenOption = ko.observable(self.openEnrollmentOptions()[0]);
    }
    else {
      self.selectedOpenOption = ko.observable(self.openEnrollmentOptions()[0]);
      self.selectedOngoingOption = ko.observable(self.ongoingEnrollmentOptions()[0]);
      self.selectedEnrollment = ko.observable(self.enrollmentOptions()[0]);
    }
  });

  self.parent_start_date.subscribe(function (newDate) {
    if (self.get_open_enrollment_period()) {
      self.get_open_enrollment_period().start_date(newDate);
    }
  });

  self.parent_end_date.subscribe(function (newDate) {
    if (self.get_open_enrollment_period()) {
      self.get_open_enrollment_period().end_date(newDate);
    }
  });

  self.set_enrollment_method_data = function (ep, option) {
    switch (ep.effective_date_type) {
      case 'static_date':
        option.static_date(ep.static_date());
        break;
      case 'day_of_month':
        option.day_of_month(ep.day_of_month());
        break;
      case 'enroller_selects':
        option.enroller_picks_default(ep.enroller_picks_default());
        option.enroller_picks_no_less(ep.enroller_picks_no_less());
        break;
      case 'first_friday':
        option.first_friday(ep.first_friday());
        break;
    }
  };

  self.get_enrollment_options();

  self.enrollment_type = ko.computed(function () {
    switch (self.selectedEnrollment().type) {
      case "open_with_start":
        self.open_enrollment_status(true);
        self.ongoing_enrollment_status(false);
        break;
      case "both":
        self.open_enrollment_status(true);
        self.ongoing_enrollment_status(true);
        break;
      case "ongoing":
        self.open_enrollment_status(false);
        self.ongoing_enrollment_status(true);
        break;
      default:
        self.open_enrollment_status(false);
        self.ongoing_enrollment_status(false);
    }

  });
  self.open_type = ko.computed(function () {
    if (self.selectedEnrollment().type.indexOf("open_with_start") == -1 && self.selectedEnrollment().type.indexOf('both') == -1) {
      return 0;
    }
    switch (self.selectedOpenOption().effective_date_type) {
      case "static_date":
        return 1;
        break;
      case "day_of_month":
        return 2;
        break;
      case "enroller_selects":
        return 3;
        break;
      case "first_friday":
        return 4;
        break;
      default:
        return 0;
        break;
    }
  });
  self.ongoing_type = ko.computed(function () {
    if (self.selectedEnrollment().type.indexOf("ongoing") == -1 && self.selectedEnrollment().type.indexOf('both') == -1) {
      return 0;
    }
    switch (self.selectedOngoingOption().effective_date_type) {
      case "static_date":
        return 1;
        break;
      case "day_of_month":
        return 2;
        break;
      case "enroller_selects":
        return 3;
        break;
      case "first_friday":
        return 4;
        break;
      default:
        return 0;
        break;
    }
  });


  self.get_open_enrollment_period = ko.pureComputed(function () {
    if (self.open_enrollment_status()) {
      return self.selectedOpenOption();
    }
    return false;
  });

  self.get_ongoing_enrollment_period = ko.pureComputed(function () {
    if (self.ongoing_enrollment_status()) {
      return self.selectedOngoingOption();
    }
    return false;
  });

  self.today_between = ko.pureComputed(function () {
    if (!self.get_open_enrollment_period()) {
      return false;
    }

    var start = self.parent_start_date();
    var end = self.parent_end_date();
    return today_between(start, end);
  });

  self.today_before = ko.pureComputed(function(){
    if (!self.get_open_enrollment_period()){
      return false;
    }

    var start = self.parent_start_date();
    return today_before(start);
  });
  
  self.check_between_or_before = function(){
    return ((self.open_enrollment_status() && !self.today_between() && !self.ongoing_enrollment_status()) || (self.open_enrollment_status() && self.ongoing_enrollment_status() && self.today_before()))
  }

  self.open_enrollment_type = ko.observable(case_data.open_enrollment_type);
  self.ongoing_enrollment_type = ko.observable(case_data.ongoing_enrollment_type);

  // Get payment modes
  self.payment_mode_choices = settings.payment_modes;
  self.payment_mode = ko.observable(_.find(self.payment_mode_choices, function (c) {
    return c.mode == case_data.payment_mode;
  }));
  // This is just the Integer representation of the payment_mode observable, or null if none is selected.
  self.selected_payment_mode = ko.pureComputed(function () {
    return (self.payment_mode()) ? parseInt(self.payment_mode().mode) : null;
  });

  // Bank Draft Form option
  self.include_bank_draft_form = ko.observable(case_data.include_bank_draft_form);
  self.can_include_bank_draft_form = ko.pureComputed(function () {
    return self.selected_payment_mode() === 12;
  });
  // Disable the include_bank_draft_form option if mode is changed.
  self.can_include_bank_draft_form.subscribe(function (can_include) {
    if (!can_include) {
      // disable
      self.include_bank_draft_form(false);
    }
  });

  // Cover sheet options
  self.include_cover_sheet = ko.observable(case_data.include_cover_sheet);
  self.logo_url = ko.observable(case_data.cover_sheet_logo_url);
  self.logo_src = urls.get_case_api_logo_url(case_data.id);

  // Call Center Workflow
  self.should_use_call_center_workflow = ko.observable(case_data.should_use_call_center_workflow);

  // There should always be a default occupation class setting
  if (!Array.isArray(case_data.occupation_class_settings)) {
    case_data.occupation_class_settings = [];
  }
  if (!_.any(case_data.occupation_class_settings, function (occupation) {
      return occupation.label.toLowerCase() === DEFAULT_OCCUPATION_LABEL.toLowerCase();
    })) {
    case_data.occupation_class_settings.unshift({label: DEFAULT_OCCUPATION_LABEL, level: 1});
  }
  // Occupation classes
  self.occupation_classes = ko.observableArray(sort_occupations(_.map(case_data.occupation_class_settings, function (occupation) {
    return new OccupationVM(occupation.label, occupation.level, occupation.has_applicants);
  })));
  self.new_occupation_class = ko.observable('');

  self.addOccupationClass = function () {
    self.occupation_classes.push(new OccupationVM(self.new_occupation_class()));
    self.occupation_classes(sort_occupations(self.occupation_classes()));
    self.new_occupation_class('');
    self.is_data_dirty(true);
  };

  self.pending_delete_occupation_class = ko.observable(null);

  self.confirm_remove_occupation_class = function () {
    var occupation_class = self.pending_delete_occupation_class();
    if (occupation_class) {
      self.occupation_classes.remove(occupation_class);
      self.is_data_dirty(true);
    }
  };

  self.removeOccupationClass = function (occupation_class) {
    if (occupation_class.has_applicants) {
      self.pending_delete_occupation_class(occupation_class);
      $('#occupation-deletion-warning').modal('show');
    } else {
      self.occupation_classes.remove(occupation_class);
      self.is_data_dirty(true);
    }
  };

  self.is_new_occupation_class_valid = ko.pureComputed(function () {
    var index = _.findIndex(self.occupation_classes(), function (item) {
      return item.label.toLowerCase() === self.new_occupation_class().toLowerCase();
    });
    return (self.new_occupation_class() !== '' && index === -1);
  });

  self.hi_occupation_classes = [];

  // Product base codes that will display occupation class mapping widgets
  self.eligible_product_codes = ['ACC', 'ACAOF', 'HI', 'HIB01', 'HIA01', 'HIAOBG'];

  self.is_product_occupation_class_eligible = function (product) {
    return self.eligible_product_codes.indexOf(product.base_product_type) != -1;
  };

  self.is_case_occupation_class_eligible = function () {
    return _.any(_.map(
      case_settings.products(),
      function (p) {
        return self.is_product_occupation_class_eligible(p);
      }));
  };

  self.occ_mapping_cache = {};

  self.update_occ_vm = function (product_id, label, value) {
    var is_update = false;
    if (product_id in self.occ_mapping_cache) {
      if (!(label in self.occ_mapping_cache[product_id])) {
        is_update = true;
      }
    } else {
      self.occ_mapping_cache[product_id] = {};
      self.occ_mapping_cache[product_id][label] = {};
      is_update = true;
    }
    if (is_update) {
      self.occ_mapping_cache[product_id][label] = {
        product_id: product_id,
        label: label,
        value: ko.observable(value)
      };
      self.occ_mapping_cache[product_id][label].value.subscribe(function () {
        self.is_data_dirty(true);
      });
    }
  };

  function is_product_active(product) {
    for (var i in self.products()) {
      if (i == 'first') {
        continue;
      }
      if (self.products()[i].id == product.id) {
        return true;
      }
    }
    return false;
  }

  // TODO: Rewrite this to be clearer.
  function is_occupation_class_active(label) {
    for (var i in self.occupation_classes()) {
      if (i == 'first') {
        continue;
      }
      if (self.occupation_classes()[i].label == label) {
        return true;
      }
    }
    return false;
  }

  self.deserialize_occ_mapping = function () {
    self.occ_mapping_cache = {};
    if (!case_data.product_settings) {
      return;
    }
    var data = case_data.product_settings.classification_mappings;
    if (!data) {
      return;
    }
    for (var i = 0; i < case_data.products.length; i++) {
      for (var j = 0; j < self.occupation_classes().length; j++) {
        var product = case_data.products[i];
        var occ_label = self.occupation_classes()[j].label;
        var value = null;
        if (data[product.id] != undefined && data[product.id][occ_label] != undefined) {
          value = data[product.id][occ_label];
        }
        self.update_occ_vm(product.id, occ_label, value);
      }
    }
  };

  // Load initial occupation mappings from database
  self.deserialize_occ_mapping();

  self.serialize_occ_mapping = function () {
    var data = {};
    // TODO: Rewrite to be clearer
    for (var product_id in self.occ_mapping_cache) {
      if (!is_product_active({
          id: product_id
        })) {
        continue;
      }
      if (!(product_id in data)) {
        data[product_id] = {};
      }
      for (var label in self.occ_mapping_cache[product_id]) {
        if (!is_occupation_class_active(label)) {
          continue;
        }
        if (!(label in data)) {
          data[product_id][label] = self.occ_mapping_cache[product_id][label].value();
        }
      }
    }
    return data;
  };

  // Creates and removes viewmodels as self.products() changes, retaining existing viewmodels,
  // so that data remains consistent
  // self.update_state_override_view_models = ko.computed(function () {
  //   var viewmodels = self.state_override_view_models();
  //
  //   _.each(self.products(), function (product) {
  //     var vm_ids = _.map(self.state_override_view_models(), function (vm) {
  //       return  _.get(vm, 'id')
  //     });
  //     if (vm_ids.indexOf(product.id) !== -1) {
  //       viewmodels.push(new ProductOverrideViewModel(product, case_data, settings.state_overrides));
  //     }
  //   });
  //
  //   viewmodels = _.filter(viewmodels, function (vm) {
  //     return _.find(self.products(), function (prod) {
  //       return prod.id == vm.id;
  //     });
  //   });
  //
  //   self.state_override_view_models(viewmodels);
  // });

  self.get_state_override_view_model = function (product) {
    var vm = _.find(self.state_override_view_models(), function (view) {
      return view.id == product.id;
    });
    if (!vm) {
      vm = new ProductOverrideViewModel(product, case_data);
      self.state_override_view_models.push(vm);
    }
    return vm
  };

  self.product_can_override_states = function (product) {
    return product.can_override_states;
  };

  self.occupation_classes_for_product = function (product) {
    var occupation_mappings = [];
    if (!(product.id in self.occ_mapping_cache)) {
      for (var i in self.occupation_classes()) {
        if (i == 'first') {
          continue;
        }
        self.update_occ_vm(product.id, self.occupation_classes()[i].label, null);
      }
    }
    for (var i in self.occupation_classes()) {
      if (i == 'first') {
        continue;
      }
      if (!(self.occupation_classes()[i].label in self.occ_mapping_cache[product.id])) {
        self.update_occ_vm(product.id, self.occupation_classes()[i].label, null);
      }
    }
    for (var label in self.occ_mapping_cache[product.id]) {
      if (is_product_active(product) && is_occupation_class_active(label)) {
        occupation_mappings.push(self.occ_mapping_cache[product.id][label]);
      }
    }
    return sort_occupations(occupation_mappings);
  };

  // cache the instances of the riders here.
  self._case_rider_configurations_by_product = {};

  self.case_riders = ko.pureComputed(function () {
    var _case_rider_configs = [];
    _.each(self.products(), function (product) {
      if (!_.has(self._case_rider_configurations_by_product, product.id)) {
        // Instantiate a VM for each rider configuration.
        self._case_rider_configurations_by_product[product.id] = _.map(product.riders, function (r) {
          return new CaseRiderConfiguration(self, product, r, case_data.product_settings);
        });
      }
      // Concatenate the riders for this product.
      _case_rider_configs = _case_rider_configs.concat(self._case_rider_configurations_by_product[product.id]);
    });

    return _case_rider_configs;
  });

  self.case_riders_for_product = function (product) {
    return _.filter(self.case_riders(), function (rider) {
      return rider.product === product;
    });
  };

  self.enabled_case_riders = ko.pureComputed(function () {
    return _.filter(self.case_riders(), function (rider) {
      return rider.is_selected() === true;
    });
  });

  self.enabled_case_riders_for_product = function (product) {
    return _.filter(self.enabled_case_riders(), function (cr) {
      return cr.product === product;
    });
  };

  /* Agent Splits */

  if (!Array.prototype.first) {
    Array.prototype.first = function () {
      if (this.length > 0) {
        return this[0];
      } else {
        return undefined;
      }
    };
  }

  self.owner_agent = ko.pureComputed(function () {
    return _.find(settings.active_agents, function (elem) {
      return elem.id === parseInt(self.owner_agent_id(), 10);
    });
  });

  self.partner_agent_list = ko.pureComputed(function () {
    return settings.active_agents.filter(function (elem) {
      return self.partner_agents().indexOf(String(elem.id)) !== -1;
    });
  });

  self.case_agents = ko.pureComputed(function () {
    // Concatenate the owner agent with the partner agents.

    var owner = self.owner_agent();

    // The owner can be added to partner agent list, even though it is not sensible.
    if (owner && _.pluck(self.partner_agent_list(), "id").indexOf(owner.id) === -1) {
      // Sort the owner into the list
      var all_agents = self.partner_agent_list().concat(owner);
      all_agents.sort(function (a, b) {
        if (a.last == b.last) {
          return a.first.localeCompare(b.first);
        }
        return a.last.localeCompare(b.last);
      });
      return all_agents;
    } else {
      return self.partner_agent_list();
    }

  });

  self.has_agent_splits = ko.observable(case_data.has_agent_splits);
  self.is_stp = ko.observable(case_data.is_stp);

  // An array of AgentSplit viewmodels.
  self.selected_agent_splits = ko.observableArray(get_initial_splits());

  // Used for adding an agent to the split table.
  self.selected_agent = ko.observable();

  // Tracks which agents are in the split table.
  self.selected_agents = ko.observableArray(get_initial_split_agents());

  // Inverse of the selected_agents list compared to the available agents on this case.
  self.unused_agents = ko.pureComputed(function () {
    var selected_ids = self.selected_agents();
    return self.case_agents().filter(function (i) {
      return !~selected_ids.indexOf(i.id);
    });
  });

  self.show_product_subcount_modal = function (element_id) {
    $('#' + element_id).modal('show');
  };

  self.add_agent_split = function () {
    var selected_agent = self.selected_agent();
    if (selected_agent) {
      self.selected_agents.push(selected_agent.id);
    }
  };

  self.get_agent_name_from_id = function (agent_id) {
    if (agent_id === null) {
      return "Writing Agent";
    }
    var cur_agent = settings.active_agents.filter(function (elem) {
      return elem.id === agent_id;
    }).first();
    if (!cur_agent) {
      return "";
    }
    return cur_agent.first + " " + cur_agent.last;
  };

  self.get_agent_splits_for_product = function (product_id) {
    return _.filter(self.selected_agent_splits(), function (split) {
      return !!split.agent_id && split.product_id === product_id;
    });
  };

  self.are_agent_splits_complete = function (product_id) {
    if (!self.has_agent_splits()) {
      return true;
    }

    var splits = self.selected_agent_splits().filter(function (split) {
      return !!split.agent_id && split.product_id === product_id;
    });

    return _.reduce(splits, function (sum, n) {
      return sum && !!n.commission_subcount_code();
    }, true);
  };

  self.product_split_percentage_computeds = {};

  self.get_product_split_percentage_complete_computed = function (product_id) {
    if (!product_id) {
      return null;
    }

    var computed;
    if (_.has(self.product_split_percentage_computeds, product_id)) {
      computed = self.product_split_percentage_computeds[product_id];
    } else {
      computed = ko.pureComputed(function () {
        var total = _.chain(self.selected_agent_splits())
          .filter(function (split) {
            return split.product_id === product_id;
          })
          .sum(function (split) {
            return split.split_percentage();
          })
          .value();
        return total === 100;
      });
      self.product_split_percentage_computeds[product_id] = computed;
    }
    return computed;
  };

  self.is_split_percentage_complete = function (product_id) {
    if (!self.has_agent_splits()) {
      return true;
    }

    var total = _.chain(self.selected_agent_splits())
      .filter(function (split) {
        return split.product_id === product_id;
      })
      .sum(function (split) {
        return split.split_percentage;
      })
      .value();
    return total === 100;
  };

  self.get_agent_splits_completion_status = function (product_id) {
    if (!self.has_agent_splits()) {
      return "Complete";
    }
    var splits = self.selected_agent_splits().filter(function (split) {
      return !!split.agent_id && split.product_id === product_id;
    });
    var success = _.reduce(splits, function (sum, n) {
      return sum && !!n.commission_subcount_code();
    }, true);
    if (success) {
      return "Complete";
    } else {
      return "Incomplete";
    }
  };

  //region Percentage Functions

  self.agent_has_percentage = function (agent) {
    return _.some(self.selected_agent_splits(), {
      agent_id: agent.id
    });
  };

  self.agents_with_percentage = ko.pureComputed(function () {
    return _.filter(self.case_agents(), function (agent) {
      return self.agent_has_percentage(agent);
    });
  });

  //endregion

  // Basic viewmodel for an agent split percentage and commission code for a given product and agent.
  function AgentSplit(agent_id, product_id, commission_subcount_code, split_percentage, is_added) {
    this.agent_id = agent_id;
    this.product_id = product_id;
    this.commission_subcount_code = ko.observable(commission_subcount_code);
    this.split_percentage = ko.observable(split_percentage);
    this.is_added = ko.observable(!!is_added);
    this.valid = ko.pureComputed(function () {
      // Check if Writing Agent
      if (this.agent_id === null) {
        return true;
      }
      return self.case_agents().filter(function (elem) {
          return elem.id === this.agent_id;
        }.bind(this)).length > 0;
    }, this);
    this.product_name = (function () {
      var cur_product = self.products().filter(function (elem) {
        return elem.id === product_id;
      }).first();
      return cur_product.name;
    })();
    this.toJson = function () {
      return {
        agent_id: this.agent_id,
        product_id: this.product_id,
        split_percentage: this.split_percentage() || null,
        commission_subcount_code: this.commission_subcount_code()
      };
    };

    this.commission_subcount_code.subscribe(function () {
      self.is_data_dirty(true);
    });
    this.split_percentage.subscribe(function () {
      self.is_data_dirty(true);
    });
  }

  // Used for populating the agent split UI grid. Checks the selected_agent_splits() array first to see if a viewmodel
  //  exists, otherwise creates a new one.
  self.get_or_create_split = function (agent_id, product_id) {

    var existing_split = _.find(self.selected_agent_splits.peek(), function (elem) {
      return elem.product_id === product_id && elem.agent_id === agent_id;
    });

    if (existing_split) {
      return existing_split;
    } else {
      var split = new AgentSplit(agent_id, product_id);
      self.selected_agent_splits.push(split);
      return split;
    }
  };

  self.has_products = ko.pureComputed(function () {
    return !!self.products() && self.products().length > 0;
  });

  function get_initial_splits() {
    return case_data.agent_splits.filter(function (elem) {
      // Ignore splits for products we don't have.
      return _.find(self.products(), function (product) {
          return product.id === elem.product_id;
        }) !== undefined;
    }).map(function (elem) {
      return new AgentSplit(elem.agent_id, elem.product_id, elem.commission_subcount_code, elem.split_percentage);
    });
  }

  function get_initial_split_agents() {
    var agentIds = _.chain(case_data.agent_splits)
      .filter(function (split) {
        // Grab only splits that have a split percentage
        return !!split.split_percentage && _.find(self.case_agents(), {
            id: split.agent_id
          });
      })
      .map(function (split) {
        // Grab the agent ids
        return split.agent_id;
      })
      .uniq() // Get unique agent ids
      .value();

    // Add a null value if one does not exist to represent the writing agent
    if (agentIds.indexOf(null) === -1) {
      agentIds.splice(0, 0, null);
    }
    return agentIds;
  }

  // Self-enrollment
  self.is_self_enrollment = ko.observable(case_data.is_self_enrollment);
  self.is_self_enrollment.subscribe(function () {
    // Confirm disabling self-enrollment
    if (!self.is_self_enrollment()) {
      bootbox.confirm('<h3>Any self-enrollment links you have sent out <u class="text-danger">will no longer work</u> if you turn off self-enrollment.</h3><p>Click <em>OK</em> to turn it off, or <em>Cancel</em> to leave it on.</p>',
        function (result) {
          self.is_self_enrollment(!result);
        });
    }
  });
  self.self_enrollment_type = ko.observable(case_data.self_enrollment_type);
  self.enrolling_agent_id = ko.observable(case_data.self_enrollment_setup.enrolling_agent_id);


  // enrollment data
  self.census_data = ko.observable(true);
  self.has_census_data = ko.pureComputed(function () {
    return self.census_data();
  });

  self.flash_messages = new FlashMessages();

  // subscribe to all changes to detect data changes
  if (self.can_edit_case) {
    var fields = [self.company_name, self.group_number, self.products,
      self.enrollment_periods, self.situs_city, self.situs_state, self.payment_mode,
      self.is_active, self.owner_agent_id, self.can_partners_download_enrollments, self.is_self_enrollment,
      self.selected_agent_splits, self.is_stp,
      self.has_agent_splits
    ];

    _.each(self.enrollment_periods(), function (p) {
      if (p.is_open) {
        fields.push(p.start_date);
        fields.push(p.end_date);
      }
    });

    $.each(fields, function (field) {
      var field = this;
      field.subscribe(function () {
        self.is_data_dirty(true);
      });
    });
    // For now, trigger dirty on every change within the self-enroll setup if it is enabled.
    $("#edit-self-enrollment-form").on("change", "input, select, [contenteditable]", function () {
      if (self.is_self_enrollment()) {
        self.is_data_dirty(true);
      }
    });

  }

  self.email_batches = ko.observableArray([]);

  $.getJSON(urls.get_case_api_census_email_batches(case_data.id), function (data) {
    self.email_batches(data.data);
  });

  self.batch_preview = function (batch) {
    window.open(urls.get_case_api_census_email_batch_preview_url(case_data.id, batch.id), "batchView", "menubar=no,location=no,resizable=no,width=650,height=600,scrollbars=yes,status=yes");
  };

  self.load_logs = function (batch) {
    $("#log" + batch.id).html("Loading Logs...");
    $.get(urls.get_case_api_census_email_batch_logs_url(case_data.id, batch.id), function (data) {
      $("#log" + batch.id).html(data);
      $("#log" + batch.id + " .dt-responsive").dataTable();
      $("#log" + batch.id + " .dt-responsive .status").each(function () {
        $(this).html(format_enrollment_status_html($(this).text()));
      });
    });
  };

  self.can_activate_case = ko.pureComputed(function () {
    var is_valid = (
      (self.open_enrollment_status() || self.ongoing_enrollment_status()) &&
      self.products().length > 0 &&
      $.trim(self.company_name()) !== "" &&
      $.trim(self.situs_city()) !== "" &&
      self.selected_statecode() !== null &&
      self.selected_payment_mode() !== null &&
      self.owner_agent_id() > 0
    );

    if (self.selectedEnrollment().type == 'none') {
      return false;
    }

    if (!self.open_enrollment_status() && !self.ongoing_enrollment_status()) {
      return false;
    }

    if (self.open_enrollment_status() && !self.get_open_enrollment_period().is_valid()) {
      is_valid = false;
    } else if (self.ongoing_enrollment_status()) {
      // Make sure there is at least one valid period date

    }
    if ((self.open_enrollment_status() && self.selectedOpenOption().effective_date_type == 'default') || (self.ongoing_enrollment_status() && self.selectedOngoingOption().effective_date_type == 'default')) {
      return false;
    }
    return is_valid;
  });

  self.is_active.subscribe(function (value) {
    if (value && !self.can_activate_case()) {
      bootbox.alert("Cannot activate case for enrollment until settings are complete.");
      self.is_active(false);
    }

    self.toggle_enrollment_buttons();
  });

  self.toggle_enrollment_buttons = function () {
    // If the case is not active, we must remove enrollment buttons on the census.
    if (self.is_active()) {
      $("button.enroll-employee").prop('disabled', false);
    } else {
      $("button.enroll-employee").prop('disabled', true);
    }
  };
  self.toggle_enrollment_buttons();

  self.switch_label = ko.pureComputed(function () {
    if (self.is_active()) {
      return "Case is Active";
    } else {
      return "Case is Not Active";
    }
  });

  self.get_form_error = ko.pureComputed(function () {
    if (self.company_name.is_unique !== undefined &&
      self.company_name.is_unique() === false) {
      return "The name '" + self.company_name() + "' is already used.";
    }
    return "";
  });

  self.check_unique_name = function (current_value, callback) {
    $.get(urls.get_cases_api_url(case_data.id), {
      by_name: current_value
    }, function (result) {
      // Must have either 0 cases with this name, or 1 (the current case)
      var is_unique = (result.data.length === 0 || current_value === case_data.company_name);
      callback(is_unique);
    }, "json");

  };

  self.validate_splits = function (callback) {
    if (!self.has_agent_splits()) {
      return true;
    }

    if (!_.all(self.products(), self.has_proper_split_percentage_sum)) {
      var invalid_product = _.find(self.products(), _.negate(self.has_proper_split_percentage_sum));
      var bad_sum = self.sum_split_percentages_for_product(invalid_product);
      bootbox.alert("Agent splits for " + invalid_product.name + " must total 100%; the current total percentage is " + bad_sum + "%", function () {
        $('.bootbox.modal').modal('hide');
        if (callback) {
          callback();
        }
      });
      return false;
    }

    return true;
  };

  self.has_proper_split_percentage_sum = function (product) {
    var product_split_sum = self.sum_split_percentages_for_product(product);
    return product_split_sum === 100;
  };

  self.sum_split_percentages_for_product = function (product) {
    var splits = self.selected_agent_splits(); //self.serialize_agent_splits();
    var splits_for_product_with_percentage = splits.filter(function (elem) {
      return elem.product_id === product.id && parseInt(elem.split_percentage()) > 0;
    });
    return splits_for_product_with_percentage.reduce(function (acc, split) {
      return acc + parseInt(split.split_percentage(), 0);
    }, 0);
  };

  self.validate = function () {

    hide_all_errors();


    // all other errors
    var errors = {};

    // unique name error
    var unique_name_error = self.get_form_error();
    if (unique_name_error !== "") {
      add_case_error(errors, "company_name", unique_name_error);
    }

    if ($.trim(self.company_name()) === "") {
      add_case_error(errors, "company_name", "Company name is required.");
    }

    // an invalid enrollment period (both dates can be blank, but if one is populated, both must be)

    if (self.open_enrollment_status() && self.selectedOpenOption().effective_date_type == 'default') {
      add_case_error(errors, "open_enrollment_option", "Must Select an Open Enrollment Option")
    }

    if (self.ongoing_enrollment_status() && self.selectedOngoingOption().effective_date_type == 'default') {
      add_case_error(errors, "ongoing_enrollment_option", "Must Select an Ongoing Enrollment Option")
    }

    if (self.open_enrollment_status() && !self.get_open_enrollment_period().is_valid()) {
      var open = self.get_open_enrollment_period();
      var start = normalize_date(open.get_start_date());
      var end = normalize_date(open.get_end_date());
      if (end !== '' || !is_valid_date(end)) {
        add_case_error(errors, "open_enrollment_end_date", "Enter valid End Date");
      }
      if (start !== '' || !is_valid_date(start)) {
        add_case_error(errors, "open_enrollment_start_date", "Enter valid Start Date");
      }
      if (start !== '' && end !== '' && parse_date(start) > parse_date(end)) {
        add_case_error(errors, "open_enrollment_start_date", "Start Date comes after End Date");
      }
    }

    if (!self.open_enrollment_status() && !self.ongoing_enrollment_status()) {
      add_case_error(errors, "enrollment_option", "Must Select an Enrollment Option");
    }

    if (self.open_enrollment_status()) {
      switch (self.get_open_enrollment_period().effective_date_type) {
        case 'static_date':
          if (!self.get_open_enrollment_period().is_valid_date(self.get_open_enrollment_period().static_date())) {
            add_case_error(errors, "open_static_date", "Enter valid Static Date");
          }
          break;
        case 'day_of_month':
          if (!self.get_open_enrollment_period().valid_day(self.get_open_enrollment_period().day_of_month())) {
            add_case_error(errors, "open_day_of_month", "Enter valid Day of Month");
          }
          break;
        case 'enroller_selects':
          if (!/^[0-9]+$/.test(self.get_open_enrollment_period().enroller_picks_default())) {
            add_case_error(errors, "open_enroller_default", "Enter valid number of days");
          }
          if (!/^[0-9]+$/.test(self.get_open_enrollment_period().enroller_picks_no_less())) {
            add_case_error(errors, "open_enroller_no_less", "Enter valid number of days");
          }
          break;
        case 'first_friday':
          if (!/^[0-9]+$/.test(self.get_open_enrollment_period().first_friday())) {
            add_case_error(errors, "open_first_friday", "Enter valid number of days");
          }
          break;
      }
    }

    if (self.ongoing_enrollment_status()) {
      switch (self.get_ongoing_enrollment_period().effective_date_type) {
        case 'static_date':
          if (!self.get_ongoing_enrollment_period().is_valid_date(self.get_ongoing_enrollment_period().static_date())) {
            add_case_error(errors, "ongoing_static_date", "Enter valid Static Date");
          }
          break;
        case 'day_of_month':
          if (!self.get_ongoing_enrollment_period().valid_day(self.get_ongoing_enrollment_period().day_of_month())) {
            add_case_error(errors, "ongoing_day_of_month", "Enter valid Day of Month");
          }
          break;
        case 'enroller_selects':
          if (!/^[0-9]+$/.test(self.get_ongoing_enrollment_period().enroller_picks_default())) {
            add_case_error(errors, "ongoing_enroller_default", "Enter valid number of days");
          }
          if (!/^[0-9]+$/.test(self.get_ongoing_enrollment_period().enroller_picks_no_less())) {
            add_case_error(errors, "ongoing_enroller_no_less", "Enter valid number of days");
          }
          break;
        case 'first_friday':
          if (!/^[0-9]+$/.test(self.get_ongoing_enrollment_period().first_friday())) {
            add_case_error(errors, "ongoing_first_friday", "Enter valid number of days");
          }
          break;
      }
    }


    if (Object.keys(errors).length > 0) {
      show_all_errors(errors);
      return false;
    } else {
      return true;
    }
  };


  //region Third Party Bank Draft Export
  self.requires_third_party_bank_draft = ko.observable(case_data.requires_paylogix_export);

  self.requires_third_party_bank_draft.subscribe(function (value) {
    self.is_data_dirty(true);
  });

  self.should_show_third_party_bank_draft_checkbox = ko.pureComputed(function () {
    return !!self.can_include_bank_draft_form() && !!self.include_bank_draft_form();
  });
  //endregion

  self.serialize_case = function () {
    var partner_agents = _.map(self.partner_agents(), function (id_str) {
      return parseInt(id_str);
    });

    if (self.open_enrollment_status() && self.ongoing_enrollment_status()) {
      self.open_enrollment_type(self.get_open_enrollment_period().effective_date_type);
      self.ongoing_enrollment_type(self.get_ongoing_enrollment_period().effective_date_type);
    }
    if (self.open_enrollment_status() && !self.ongoing_enrollment_status()) {
      self.open_enrollment_type(self.get_open_enrollment_period().effective_date_type);
      self.ongoing_enrollment_type(null);
    }
    if (!self.open_enrollment_status() && self.ongoing_enrollment_status()) {
      self.ongoing_enrollment_type(self.get_ongoing_enrollment_period().effective_date_type);
      self.open_enrollment_type(null);
    }


    return {
      company_name: self.company_name(),
      group_number: self.group_number(),
      active: self.is_active(),
      products: self.products(),
      partner_agents: partner_agents,
      enrollment_period_type: self.selectedEnrollment().type,
      open_enrollment_type: self.open_enrollment_type(),
      ongoing_enrollment_type: self.ongoing_enrollment_type(),
      effective_date_settings: self.serialize_effective_date_settings(),
      situs_city: self.situs_city(),
      situs_state: self.selected_statecode() ? self.selected_statecode() : "",
      payment_mode: self.selected_payment_mode() ? self.selected_payment_mode() : null,
      agent_id: self.owner_agent_id(),
      can_partners_download_enrollments: self.can_partners_download_enrollments(),
      is_self_enrollment: self.is_self_enrollment(),
      include_bank_draft_form: self.include_bank_draft_form(),
      should_use_call_center_workflow: self.should_use_call_center_workflow(),
      product_settings: self.serialize_product_settings(),
      has_agent_splits: Boolean(self.has_agent_splits()),
      is_stp: Boolean(self.is_stp()),
      occupation_class_settings: _.map(self.occupation_classes(), function (occupation_class) {
        return occupation_class.serialize_object();
      }),
      omit_actively_at_work: self.omit_actively_at_work(),
      include_cover_sheet: self.include_cover_sheet(),
      requires_paylogix_export: self.requires_third_party_bank_draft()
    };
  };

  self.serialize_product_settings = function () {
    return {
      riders: self.serialize_riders(),
      classification_mappings: self.serialize_occ_mapping(),
      effective_date_settings: self.serialize_product_effective_date(),
      state_overrides: self.serialize_state_overrides(),
      coverage_limits: self.serialize_coverage_limits()
    };
  };

  self.serialize_state_overrides = function () {
    var overrides = {};
    _.each(self.products(), function(product) {
      var vm = self.get_state_override_view_model(product);
      overrides[product.id] = vm.serialize_overridden_states();
    });

    return overrides;
  };

  self.serialize_coverage_limits = function() {
    var limits = {};
    _.each(self.products(), function(product) {

      limits[product.id] = {};

      if (self.should_show_max_age(product)) {
        var max_age_vm = self.get_maximum_age_vm_for_product(product);
        // Save the data only if enabled.
        if (max_age_vm.is_enabled()) {
          limits[product.id]['max_age'] = max_age_vm.serialize();
        }
      }

      if (self.should_show_max_coverage(product)) {
        var max_coverage_vm = self.get_maximum_coverage_vm_for_product(product);

        if (max_coverage_vm.is_enabled()) {
          limits[product.id]['max_coverage'] = max_coverage_vm.serialize();
        }
      }
    });
    return limits;
  };

  self.serialize_riders = function () {
    return _.invoke(self.case_riders(), "serialize");
  };

  self.serialize_product_effective_date = function () {
    return _.map(self.products(), function (product) {
      var ep = _.find(self.effective_products(), function (ep) {
          return ep.id == product.id;
      });
      return ep.serialize();
    });
  };

  self.serialize_enrollment_periods = function () {
    var periods = [];
    if (self.open_enrollment_status() && self.ongoing_enrollment_status()) {
      var open = self.get_open_enrollment_period();
      var ongoing = self.get_ongoing_enrollment_period();
      periods.push(open.serialize(), ongoing.serialize());
    }
    if (self.ongoing_enrollment_status() && !self.open_enrollment_status()) {
      var period = self.get_ongoing_enrollment_period();
      periods.push(period.serialize());
    }
    if (self.open_enrollment_status() && !self.ongoing_enrollment_status()) {
      var period = self.get_open_enrollment_period();
      periods.push(period.serialize());
    }
    return periods;
  };

  self.serialize_effective_date_settings = function () {
    var effective_date_settings = [];
    if (self.open_enrollment_status() && self.ongoing_enrollment_status()) {
      var open = self.get_open_enrollment_period();
      var ongoing = self.get_ongoing_enrollment_period();
      effective_date_settings.push(open.serialize_effective_date(), ongoing.serialize_effective_date());
    }
    if (!self.open_enrollment_status() && self.ongoing_enrollment_status()) {
      var ongoing = self.get_ongoing_enrollment_period();
      effective_date_settings.push(ongoing.serialize_effective_date());
    }
    if (self.open_enrollment_status() && !self.ongoing_enrollment_status()) {
      var open = self.get_open_enrollment_period();
      effective_date_settings.push(open.serialize_effective_date());
    }
    return effective_date_settings;
  };

  function removeHarmfulEmailTags(message) {
    var allowedTags = ["br", "span", "strong", "b", "i", "em", "emphasis", "img", "a"];
    var base = $("<div>" + message + "</div>")[0].childNodes;

    var mapped = Array.prototype.map.call(base, function (elem, a) {
      if (elem.tagName) {
        var tagName = elem.tagName.toLowerCase();
        if (allowedTags.indexOf(tagName) === -1) {
          elem = $(elem).contents().unwrap()[0]
        }
      }
      return elem;
    });

    var htmlstr = Array.prototype.reduce.call(base, function (html, node) {
      return html + (node.outerHTML || node.nodeValue);
    }, "");

    return htmlstr;
  }

  self.serialize_self_enroll = function () {
    self.emailSettings.message(removeHarmfulEmailTags(self.emailSettings.message()));
    var settings = $.extend({}, self.emailSettings, self.landing);
    for (var key in settings) {
      if (settings.hasOwnProperty(key)) {
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

  self.serialize_agent_splits = function () {
    var serialized_records = self.selected_agent_splits();
    // Filter out any splits for agents that have been removed from the case
    serialized_records = _.filter(serialized_records, function (split) {
      // The null check ensures that the Writing Agent, which is denoted by a null id
      return _.some(self.case_agents(), {
          'id': split.agent_id
        }) || split.agent_id === null;
    });
    return serialized_records.reduce(function (start, elem) {
      if (elem.split_percentage() || elem.commission_subcount_code()) {
        start.push(elem.toJson());
      }
      return start;
    }, []);
  };

  self.loading_modal = ko.observable(null);

  self.save_settings = function (cb) {
    self.flash_messages.clear();

    if (!self.can_save_case()) {
      return;
    }

    function show_save_dialog() {
      self.loading_modal({
        message: "Saving case..."
      });

      // Make sure case can be activated
      if (self.is_active() && !self.can_activate_case()) {
        self.is_active(false);
        self.flash_messages.flash_error("The case has been deactivated due to missing settings.");
        // Continue saving the case
      }

      var requests = [];

      // The main PUT request for the case data.
      requests.push(send_json_data(
        "PUT",
        urls.get_case_api_url(case_data.id),
        self.serialize_case()
      ));

      // Update the logo image if it was changed
      var file_select = $("#cover-logo-file-input").get(0);
      var files = file_select.files;
      if (files.length === 1) {
        // Add the file upload to the request.
        var form_data = new FormData();
        form_data.append('cover-logo', files[0], files[0].name);
        requests.push(send_file_data(
          "POST",
          urls.get_case_api_logo_url(case_data.id),
          form_data
        ));
      }

      // Enrollment periods
      requests.push(send_json_data(
        "PUT",
        urls.get_case_api_enrollment_periods_url(case_data.id),
        self.serialize_enrollment_periods()
      ));

      if (self.is_self_enrollment()) {
        // Also save self enrollment settings.
        requests.push(send_json_data(
          "PUT",
          urls.get_case_api_self_enrollment_url(case_data.id),
          self.serialize_self_enroll()
        ));
      }

      if (self.has_agent_splits()) {
        // Also save agent split settings.
        requests.push(send_json_data(
          "PUT",
          urls.get_case_api_agent_splits_url(case_data.id),
          self.serialize_agent_splits()
        ));
      }

      // Self Enroll settings
      $('#save-success').hide();
      $('#save-fail').hide();

      // Callbacks for when all the PUT requests are done.
      var on_success = function (case_xhr, periods_xhr, self_enroll_xhr) {
        self.is_data_dirty(false);
        self.flash_messages.flash_success("Data Saved");
        $("#save-success").text("Data Saved");
        self.loading_modal(null);
      };
      var on_failure = function (failed_xhr) {
        var errors = failed_xhr.responseJSON.errors || [];
        show_all_errors(errors);
        self.loading_modal(null);
        self.flash_messages.flash_error("Save failed. Please correct any errors below.");
      };

      // Actually send the requests
      var done = $.when.apply($.when, requests);

      if (cb && typeof (cb) === "function") {
        cb(done);
        self.loading_modal(null);
        return;
      }

      done.then(on_success, on_failure);

      return false;
    }

    if (!self.validate_splits(show_save_dialog)) {
      return;
    }

    return show_save_dialog();
  };

  self.can_save_case = function () {
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

  self.delete_case = function () {
    bootbox.confirm("Are you sure you want to delete the case '" + self.company_name() + "'? This is permanent and cannot be undone!", function (result) {
      if (!result) {
        return;
      }

      self.remote_delete();
    });
  };

  self.remote_delete = function () {
    var request = send_json_data("DELETE", urls.get_case_api_url(case_data.id));
    $.when(request).then(function () {
      bootbox.alert("Case deleted successfully");
      // Allow leaving the page (disables the page unload trigger prompt)
      self.is_data_dirty(false);
      // Go back to main page
      window.location.href = urls.get_manage_cases_url(case_data.id);
    }, function () {
      bootbox.alert("There was a problem removing this case.");
    });
  };

  self.is_permanent_delete_modal_showing = ko.observable(false);
  self.delete_confirmation_text = ko.observable("");
  self.is_delete_text_valid = ko.pureComputed(function () {
    return self.delete_confirmation_text() == "DELETE";
  });

  self.delete_case_with_enrollments = function () {
    // Show the dialog
    self.delete_confirmation_text("");
    // Toggle off, then on since we don't detect modal hide right now
    self.is_permanent_delete_modal_showing(false);
    self.is_permanent_delete_modal_showing(true);
  };

  if (self.can_edit_case) {
    $(window).bind("beforeunload", function () {
      return self.has_unsaved_data() ? "You have made changes without saving. Do you you wish to leave this page and lose all changes?" : undefined;
    });
  }

  self.show_upload_enrollment_form = function () {
    var el = $("#enrollment-csv-modal");
    reset_upload_modal(el);
    el.modal("show");
  };

  self.has_unsaved_data = ko.pureComputed(function () {
    return self.is_data_dirty();
  });

  self.exit_print_mode = function () {
    if (self.report_viewmodel()) {
      self.report_viewmodel().exit_print_preview();
    }
  };

  self.has_general_product_configuration_options = ko.pureComputed(function () {
    return self.has_products() || self.is_case_occupation_class_eligible();
  });

  // Tabs
  var setup_tab = $('#case-nav-tabs a[href="#setup"]');
  var enrollment_tab = $('#case-nav-tabs a[href="#enrollment"]');
  var history_tab = $('#case-nav-tabs a[href="#history"]');
  var reports_tab = $('#case-nav-tabs a[href="#reports"]');
  var api_tab = $('#case-nav-tabs a[href="#api"]');

  self.sammy_app = Sammy(function () {
    this.get("#setup", function () {
      self.exit_print_mode();

      setup_tab.tab('show');

      // Disable all data entry on case setup if case is not editable
      if (!self.can_edit_case) {
        $("#setup").find("input, select, button").prop("disabled", true);
      }

    });

    // If switching to another tab, we check for dirty and warn the user
    this.before({
      except: {
        path: '#setup'
      }
    }, function () {

      if (self.has_unsaved_data()) {
        bootbox.dialog({
          title: "Unsaved Changes",
          message: "You have made changes without saving. Please save your changes or discard before leaving this page.",
          buttons: {
            main: {
              label: "Save",
              className: 'btn-primary',
              callback: function () {
                self.save_settings();
              }
            },
            danger: {
              label: "Discard Changes",
              className: 'btn-danger',
              callback: function () {
                // disable the page unload check
                self.is_data_dirty(false);
                window.location.reload();
              }
            },
            cancel: {
              label: "Cancel",
              className: 'btn-default',
              callback: function () {
              }
            }
          }
        });

        this.redirect("#setup");
        return false;
      }
    });

    this.get("#enrollment", function () {
      self.exit_print_mode();

      enrollment_tab.tab('show');

      // Load census data when tab loads
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
        self.census_data(false);
        self.toggle_enrollment_buttons();
      }
    });

    this.get("#history", function () {
      self.exit_print_mode();
      history_tab.tab('show');

      setTimeout(function () {
        $.getJSON(urls.get_case_api_census_email_batches(case_data.id), function (data) {
          self.email_batches(data.data);
        });
      }, 0);
    });

    this.get("#reports", function () {
      self.exit_print_mode();

      reports_tab.tab('show');
      if (self.report_viewmodel()) {
        self.report_viewmodel().load_reports();
        self.report_viewmodel().load_enrollments();
      }
    });

    this.get("#api", function () {
      self.exit_print_mode();
      api_tab.tab('show');
    });

    this.get("#print", function () {
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
