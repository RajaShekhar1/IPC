var CaseViewModel = function CaseViewModel(case_data, product_choices, can_edit_case, settings, product_rate_levels) {
  var self = this;

  //region Member Variable Initialization
  _.defaults(case_data, {
    omit_actively_at_work: false,
    requires_paylogix_export: false
  });

  self.case_id = case_data.id;
  self.case_token = case_data.case_token;
  self.product_rate_levels = product_rate_levels || {};
  self.state_override_view_models = ko.observableArray([]);

  self.product_state_mapping = ko.computed(function () {
    var mapping = settings.product_state_mapping;
    _.each(self.state_override_view_models(), function (ovm) {
      if (ovm.restrict_state_availability()) {
        mapping[ovm.id] = ovm.state_overrides();
      }
    });
    return mapping;
  });

  self.can_edit_case = can_edit_case;
  //endregion

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
  self.omit_actively_at_work.subscribe(function () { self.is_data_dirty(true); });

  self.has_product_sort_selections = ko.pureComputed(function () {
    return !!self.sort_selected_products() && self.sort_selected_products().length > 0;
  });

  self.unselected_products = ko.pureComputed(function () {
    return _.sortBy(_.difference(self.product_choices(), self.products()), function (product) {
      return product.ordinal;
    });
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
    if (!products_to_remove || products_to_remove.length === 0) {
      return;
    }
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
  }
  //endregion

  self.enrollment_period_type = ko.observable(case_data.enrollment_period_type);
  self.enrollment_periods = ko.observableArray($.map(case_data.enrollment_periods, function (p) {
    return new CaseEnrollmentPeriod(p);
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

    var case_default_statecode = (self.situs_state())? self.situs_state().statecode : "";

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
    return (self.situs_state())? self.situs_state().statecode : null;
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

  self.download_enrollments = function() {
    // reset dialogue, show modal
    self.download_enrollments_url(null);
    self.download_enrollment_error(null);
    self.download_enrollments_modal(true);

    var url = urls.get_case_api_enrollment_records_url(self.case_id) + "?format=csv&poll=true";
    $.getJSON(url, function (data) {

      window.setTimeout(function() {self.poll_export_file(data.data.poll_url)}, 5000);

    }).error(function() {
      self.download_enrollment_error("There was a problem generating the export file. Please try again later.");
    });
  };

  self.poll_export_file = function(poll_url) {
    $.get(poll_url).success(function(resp) {
      if (!resp.data.is_ready) {
        // Try again
        window.setTimeout(function() {self.poll_export_file(poll_url)}, 5000);
      } else {
        // Show the download button for fetching the file
        self.download_enrollments_url(resp.data.download_url);
      }
    }).error(function() {
      self.download_enrollment_error("There was a problem generating the export file. Please try again later.");
    });

  };

  self.partner_agents = ko.observable(
    (case_data.partner_agents)? _.map(_.pluck(case_data.partner_agents, "id"), function (id) {
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

  self.is_open_enrollment = ko.pureComputed(function () {
    return self.enrollment_period_type() === "open";
  });
  self.is_annual_enrollment = ko.pureComputed(function () {
    return self.enrollment_period_type() === "annual";
  });
  self.get_open_enrollment_period = ko.pureComputed(function () {
    return _.find(self.enrollment_periods(), function (p) {
      return p.is_open();
    });
  });

  self.today_between = ko.pureComputed(function () {
    if (!self.get_open_enrollment_period()) {
      return false;
    }

    var start = self.get_open_enrollment_period().start_date();
    var end = self.get_open_enrollment_period().end_date();
    return today_between(start, end);
  });

  self.annual_enrollment_periods = ko.pureComputed(function () {
    return _.filter(self.enrollment_periods(), function (p) {
      return p.period_type === "annual_period";
    });
  });

  self.annual_today_between = ko.pureComputed(function () {
    if (self.annual_enrollment_periods().length >= 4 && self.annual_enrollment_periods()[0].start_date() !== "") {
      return self.annual_enrollment_periods().reduce(function (a, period) {
        var start_date = moment(period.start_date(), "MM/DD");
        var end_date = moment(period.end_date(), "MM/DD");
        return today_between(start_date, end_date) || a;
      }, false);
    }
    return true;
  });

  // Get payment modes
  self.payment_mode_choices = settings.payment_modes;
  self.payment_mode = ko.observable(_.find(self.payment_mode_choices, function (c) {
    return c.mode == case_data.payment_mode;
  }));
  // This is just the Integer representation of the payment_mode observable, or null if none is selected.
  self.selected_payment_mode = ko.pureComputed(function () {
    return (self.payment_mode())? parseInt(self.payment_mode().mode) : null;
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
  if (!_.any(case_data.occupation_class_settings, function (occupation) { return occupation.label.toLowerCase() === DEFAULT_OCCUPATION_LABEL.toLowerCase(); })) {
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
  self.eligible_product_codes = ['ACC', 'HI'];

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

  //Creates and removes viewmodels as self.products() changes, retaining existing viewmodels,
  //so that data remains consistent
  self.update_state_override_view_models = ko.computed(function () {
    var viewmodels = self.state_override_view_models();
    _.each(self.products(), function (product) {
      var vm_ids = _.map(self.state_override_view_models(), function (vm) {
        return  _.get(vm, 'id')
      });
      if (!!(_.find(vm_ids, function (vm_id) {return vm_id == product.id}))) {
        return;
      }
      else {
        viewmodels.push(new ProductOverrideViewModel(product, case_data, settings.state_overrides, self.product_state_mapping));
      }
    });
    viewmodels = _.filter(viewmodels, function (vm) {
      return _.find(self.products(), function (prod) {
        return prod.id == vm.id;
      });
    });
    console.log(viewmodels);
    self.state_override_view_models(viewmodels);
  });

  self.get_state_override_view_model = function (product) {
    var vm = _.find(self.state_override_view_models(), function (view) {
      return view.id == product.id;
    });
    if (!!vm) {
      return vm;
    }
    else {
      return false;
    }
  };

  self.product_can_override_states = function (product) {
    var vm = self.get_state_override_view_model(product);
    return vm.can_override_states;
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

  self.has_agent_splits = ko.observable(case_data.is_stp);

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

  // Make sure there is at least one open enrollment period
  if (!self.get_open_enrollment_period()) {
    self.enrollment_periods.push(new CaseEnrollmentPeriod({
      case_id: case_data.id,
      period_type: "open_with_start"
    }));
  }
  // Make at least four annual enrollment periods by default
  if (self.annual_enrollment_periods().length < 4) {
    var num_missing = 4 - self.annual_enrollment_periods().length;
    _.each(_.range(num_missing), function () {
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
    return _.filter(periods, function (p) {
      return periods.indexOf(p) > periods.indexOf(period);
    });
  }

  function compute_next_quarter_start(m) {
    return m.add(3, "months");
  }

  function compute_period_end(start, months) {
    return start.add(months, "months").subtract(1, "day");
  }

  function compute_quarterly_dates(period, val) {
    if (val && period.is_valid_month_day(val) && period.end_date() === "") {
      // Auto fill with end of next month approx. alg
      // Initial period is two months after start
      var computed_date = compute_period_end(period.get_month_day(val), 2);
      period.end_date(computed_date.format("MM/DD"));

      // Loop through remaining periods and fill in any that are blank
      var remaining_periods = get_annual_periods_after_period(period);
      var previous_period = period;
      $.each(remaining_periods, function () {
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
  first_period.start_date.extend({
    notify: 'always'
  });
  first_period.start_date.subscribe(function (val) {
    compute_quarterly_dates(first_period, val);
  });

  // Add validation to the month/day inputs
  _.each(self.annual_enrollment_periods(), function (period) {
    _.each([period.start_date, period.end_date], function (date_observable) {
      date_observable.subscribe(function (val) {
        if (val === "") {
          return;
        }

        var val_date = parse_month_date_input(val);
        if (!val_date.isValid()) {
          date_observable("");
          bootbox.dialog({
            title: "Invalid Date",
            message: "The value '" + val + "' is not a valid start or end date. Provide a month and date formatted as 'MM/DD'.",
            buttons: {
              main: {
                label: "OK"
              }
            }
          });
        }
      });
    });
  });

  // enrollment data
  self.census_data = ko.observable(true);
  self.has_census_data = ko.pureComputed(function () {
    return self.census_data();
  });

  self.flash_messages = new FlashMessages();

  // subscribe to all changes to detect data changes
  if (self.can_edit_case) {
    var fields = [self.company_name, self.group_number, self.products, self.enrollment_period_type,
      self.enrollment_periods, self.situs_city, self.situs_state, self.payment_mode,
      self.is_active, self.owner_agent_id, self.can_partners_download_enrollments, self.is_self_enrollment,
      self.selected_agent_splits,
      self.has_agent_splits
    ];
    _.each(self.enrollment_periods(), function (p) {
      fields.push(p.start_date);
      fields.push(p.end_date);
    });

    $.each(fields, function () {
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
      self.enrollment_period_type() !== null &&
      self.enrollment_periods().length > 0 &&
      self.products().length > 0 &&
      $.trim(self.company_name()) !== "" &&
      $.trim(self.situs_city()) !== "" &&
      self.selected_statecode() !== null &&
      self.selected_payment_mode() !== null &&
      self.owner_agent_id() > 0
    );
    // if (self.is_open_enrollment() && !self.get_open_enrollment_period().is_valid()) {
    //   is_valid = false;
    // } else if (self.is_annual_enrollment()) {
    //   // Make sure there is at least one valid period date
    //   is_valid &= _.any(self.annual_enrollment_periods(), function(p) {
    //     return p.is_valid();
    //   });
    // }

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

    // hide missing date errors
    _.invoke(self.annual_enrollment_periods(), "error", "");

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
    if (self.is_open_enrollment() && !self.get_open_enrollment_period().is_valid()) {
      var start = self.get_open_enrollment_period().start_date();
      var end = self.get_open_enrollment_period().end_date();
      if (end !== '') {
        if (!is_valid_date(end)) {
          add_case_error(errors, "open_enrollment_end_date", "Enter valid End Date");
        } else if (start === '') {
          add_case_error(errors, "open_enrollment_start_date", "Enter valid Start Date or both dates blank");
        }
      }
      if (start !== '' && !is_valid_date(start)) {
        add_case_error(errors, "open_enrollment_start_date", "Enter valid Start Date");
      }
      if (start !== '' && end !== '' && parse_date(start) > parse_date(end)) {
        add_case_error(errors, "open_enrollment_start_date", "Start Date comes after End Date");
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

  self.missing_annual_period_predicate = function (period) {
    var start_date_present = period.start_date() !== "";
    var end_date_present = period.end_date() !== "";
    // XOR - one or the other, but not both the same
    return (start_date_present? !end_date_present : end_date_present);
  };
  self.any_annual_period_missing_a_component = function () {
    return _.any(self.annual_enrollment_periods(), self.missing_annual_period_predicate);
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

    return {
      company_name: self.company_name(),
      group_number: self.group_number(),
      active: self.is_active(),
      products: self.products(),
      partner_agents: partner_agents,
      enrollment_period_type: self.enrollment_period_type(),
      situs_city: self.situs_city(),
      situs_state: self.selected_statecode()? self.selected_statecode() : "",
      payment_mode: self.selected_payment_mode()? self.selected_payment_mode() : null,
      agent_id: self.owner_agent_id(),
      can_partners_download_enrollments: self.can_partners_download_enrollments(),
      is_self_enrollment: self.is_self_enrollment(),
      include_bank_draft_form: self.include_bank_draft_form(),
      should_use_call_center_workflow: self.should_use_call_center_workflow(),
      product_settings: self.serialize_product_settings(),
      is_stp: Boolean(self.has_agent_splits()),
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
      state_overrides: self.serialize_state_overrides()
    };
  };

  self.serialize_state_overrides = function () {
    return _.invoke(self.state_override_view_models(), "serialize");
  };

  self.serialize_riders = function () {
    return _.invoke(self.case_riders(), "serialize");
  };

  self.serialize_enrollment_periods = function () {
    if (self.is_annual_enrollment()) {
      // Only send valid periods
      return _.invoke(
        _.filter(self.annual_enrollment_periods(), function (p) {
          return p.is_valid();
        }),
        "serialize");
    } else {
      var period = self.get_open_enrollment_period();
      return [period.serialize()];
    }
  };

  function removeHarmfulEmailTags(message) {
    var allowedTags = ["br", "span", "strong", "b", "i", "em", "emphasis", "img", "a"]
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
    bootbox.confirm("Are you sure you want to the case '" + self.company_name() + "'? This is permanent and cannot be undone!", function (result) {
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
      return self.has_unsaved_data()? "You have made changes without saving. Do you you wish to leave this page and lose all changes?" : undefined;
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
              callback: function () {}
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
