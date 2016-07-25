// Global cache-buster for AJAX GET requests.
$(function () {
  $.ajaxSetup({cache: false});
});

// form_data is
function send_form_data(method, url, data, on_success, on_error) {
  return submit_data(method, url, data, true, on_success, on_error);
}

// <data> is a FormData object that contains a file upload
function send_file_data(method, url, data, on_success, on_error) {
  return submit_data(method, url, data, false, on_success, on_error, false);
}

// <data> is a plain javascript object
function send_json_data(method, url, data, on_success, on_error) {
  return submit_data(method, url, JSON.stringify(data), false, on_success, on_error, 'application/json');
}

function get_loading_html(message) {
  var text = message || "Loading data...";
  //return "<span class='icon-spinner icon-spin grey bigger-200'></span> <span class='bigger-175'> "+text+"</span>";
  return '<div class="text-center">' +
    "<h4>" + text + "</h4>" +
    '<i class="icon-spinner icon-spin grey bigger-200"></i>' +
    "</div>";
}

// Misc Formatting
function format_enrollment_status_text(status) {
  if (status === "enrolled") {
    return "Enrolled";
  } else if (status === "declined") {
    return "Declined";
  } else if (status === "pending_employee") {
    return "Pending Employee";
  } else if (status === "pending_agent") {
    return "Pending Agent";
  } else {
    return "Not Enrolled";
  }
}

function format_enrollment_status_html(status) {
  var status_text = format_enrollment_status_text(status);
  if (status_text === "Not Enrolled") {
    return status_text;
  } else {
    if (status_text === "Enrolled") {
      return "<span class='enroll-status ace-icon glyphicon glyphicon-ok'> </span><span class='enroll-status'> Enrolled</span>";
    } else if (status_text === "Pending Employee" || status_text === "Pending Agent") {
      return "<span class='enroll-status pending icon glyphicon glyphicon-pencil'></span><span class='enroll-status pending'>" + status_text + "</span>";
    } else {
      return "<span class='ace-icon glyphicon glyphicon-remove error'></span> <span class='enroll-status declined'> Declined</span>";
    }
  }
}

//Specific Date handling
function parse_month_date_input(val) {
  return parse_date(val, "MM/DD");
}

// check if today is between a start and end date
function today_between(start, end) {
  var today = moment();
  var is_after_start = today.isSameOrAfter(moment(start), 'day');
  if (!moment(end).isValid()) {
    return is_after_start;
  } else {
    var is_before_end = today.isSameOrBefore(moment(end), 'day');
    return is_after_start && is_before_end
  }

}


// Date handling
function parse_date(date_str, format_str) {
  // Parse a date as a moment object from the given string, according to the format string.
  // Defaults format to server-sent date format, and falls back to MM/DD/YYYY otherwise.
  if (format_str === undefined) {
    format_str = ["YYYY-MM-DD", "MM/DD/YYYY"];
  }
  return moment(date_str, format_str);
}

function normalize_date(date_str) {
  if (date_str != '' && is_valid_date(date_str)) {
    return format_date(parse_date(date_str));
  } else {
    return '';
  }
}

function normalize_date_of_birth(date_of_birth_string) {
  'use strict';
  //var date = moment(date_of_birth_string);
  //if (date.isValid()) {
  //  return format_date(date);
  //} else {
  //  return '';
  //}
  if (date_of_birth_string && is_valid_date_of_birth(date_of_birth_string)) {
    return format_date(parse_date(date_of_birth_string));
  } else {
    return '';
  }
}

function is_valid_date(date_str, format_str) {
  // Is the given string valid according to the format string? Defaults format to server-sent date format.
  var date = parse_date(date_str, format_str);
  return date.isValid();
}

function is_valid_date_of_birth(date_of_birth) {
  'use strict';
  if (!date_of_birth) {
    return false;
  }
  var dob_moment = moment.isMoment(date_of_birth) ? date_of_birth : moment(date_of_birth, 'MM/DD/YYYY');
  if (!dob_moment.isValid()) {
    dob_moment = moment(date_of_birth, 'YYYY-MM-DD');
  }
  var today = moment({hour: 0, minute: 0, seconds: 0, milliseconds: 0});
  return dob_moment.isValid() && dob_moment.isBefore(today);
}

function get_date_of_birth_validation_error(date_of_birth) {
  'use strict';
  if (!date_of_birth) {
    return null;
  }
  var dob_moment = moment.isMoment(date_of_birth) ? date_of_birth : moment(date_of_birth, 'MM/DD/YYYY');
  if (!dob_moment.isValid()) {
    return 'Date is invalid.';
  }
  var today = moment({hour: 0, minute: 0, seconds: 0, milliseconds: 0});
  if (dob_moment.isAfter(today)) {
    return 'Date must be before today.';
  }
  return null;
}

function valid_enroller_selects(minimum, input) {
  var today = moment({hour: 0, minute: 0, seconds: 0, milliseconds: 0});
  var input_moment = moment(input);
  var minimum_moment = today.clone().add(minimum, 'day');
  return minimum_moment.isSameOrBefore(input_moment);
}

function format_date(moment_date) {
  // Given a moment object, format it the same across the site
  return moment_date.format("MM/DD/YYYY");
}
function now() {
  return moment();
}

function get_responsive_datatables_breakpoints() {
  // We use these standard breakpoints throughout the site for DataTable tables

  // Usage: add the class 'min-breakV' to a column to hide it below 480,
  //         'min-breakIV' to hide it below 560,
  //          etc.
  return [
    {name: 'breakI', width: Infinity},
    {name: 'breakII', width: 1024},
    {name: 'breakIII', width: 768},
    {name: 'breakIV', width: 600},
    {name: 'breakV', width: 560},
    {name: 'smallphone', width: 480}
  ];
}


// The shortcut functions above use this method to wrap the jquery ajax call in slightly different ways
function submit_data(method, url, data, should_process_data, on_success, on_error, contentType) {
  on_success = on_success || function () {
    };
  on_error = on_error || function () {
    };

  var options = {
    url: url,
    type: method,
    data: data,
    // return data type expected is always json for this app
    dataType: 'json',
    success: function (results) {
      on_success(results);
    },
    error: function (xhr) {
      on_error(xhr);
    }
  };

  if (!should_process_data) {
    options.processData = false;
  }

  if (contentType !== undefined) {
    options.contentType = contentType;
  }

  return $.ajax(options);
}

// Forces the page to submit a post as if a form were submitted, without needing a form on the page
function submit_to_url(url, data) {
  var form = document.createElement('form');
  for (k in data) {
    if (data.hasOwnProperty(k)) {
      $("<input>", {name: k, value: data[k]}).appendTo(form);
    }
  }

  form.method = "POST";
  form.action = url;

  // Need to add to the DOM for firefox and ie
  var wrapper = $("<div>");
  wrapper.appendTo("body").hide();
  $(form).appendTo(wrapper).submit();
}


// Error handling / validation
function show_all_errors(all_errors) {
  if (Object.keys(all_errors).length == 0) {
    return;
  }

  $(".submit-message").addClass("error").html("Please fix the indicated problems and resubmit.").show();
  $.each(all_errors, function (field_name, errors) {
    show_errors(field_name, errors);
  });
  focus_first_error(all_errors);
}

function show_errors(field_name, field_error_messages) {
  get_field(field_name).after(
    $("<div>").addClass("error").html(field_error_messages.join("<br>"))
  );
}

function get_field(field_name) {
  return $("textarea[name=" + field_name + "], select[name=" + field_name + "], input[name=" + field_name + "], #" + field_name);
}

function focus_first_error(errors) {
  $.each(errors, function (field_name, errors) {
    get_field(field_name).focus();
  })
}

function hide_all_errors() {
  $(".submit-message").removeClass("error").hide();
  $(".error").remove();
}


// Common formatting, etc

// http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
function numberWithCommas(x) {
  return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}


// Custom bindings

// Show or hide a modal based on a boolean observable
ko.bindingHandlers.modal = {
  update: function (element, valueAccessor) {
    if (ko.unwrap(valueAccessor())) {
      $(element).modal('show');
    } else {
      $(element).modal('hide');
    }
  }
};

// Simple DataTable binding using a javascript data source (usually observable)
ko.bindingHandlers.dataTable = {


  update: function (element, valueAccessor) {
    var bind_opts = ko.unwrap(valueAccessor());
    var datatable_opts = $.extend({}, bind_opts.options);
    var data_observable = bind_opts.data;

    var updated_data = data_observable();

    if (!$.fn.DataTable.fnIsDataTable(element) && updated_data.length > 0) {
      // Initialize DataTable for the first time.

      // Add data to table.
      datatable_opts.data = updated_data;

      // Create table
      $(element).//wrap("<div class='dataTables_borderWrap' />").
      DataTable(datatable_opts);

    } else if (updated_data.length > 0) {
      var table = $(element).DataTable();
      table.clear();
      table.rows.add(updated_data).draw();
    }
  }
};

ko.bindingHandlers.flashMessage = {
  update: function (element, valueAccessor) {
    $(element).html(ko.unwrap(valueAccessor())).delay(5000).hide('fade')
  }
};

// Uses the mask plugin provided with the ACE template to control what can be typed in an input.
ko.bindingHandlers.maskedInput = {
  init: function (element, valueAccessor) {
    $(element).mask(ko.unwrap(valueAccessor()));
  }
};

// Reveal a panel by sliding down when the observable is true; slide back up if false.
ko.bindingHandlers.slideDownIf = {
  init: function (element, value_accessor) {
    var val = ko.unwrap(value_accessor());
    // Initially hide or show without animation
    $(element).toggle(val);
  },
  update: function (element, value_accessor) {
    // value should be a boolean
    var val = ko.unwrap(value_accessor());
    if (val) {
      $(element).slideDown(400);
    } else {
      $(element).slideUp(400);
    }
  }
};

ko.bindingHandlers.fadeInIf = {
  init: function (element, value_accessor) {
    var val = ko.unwrap(value_accessor());
    // Initially hide or show without animation
    $(element).toggle(val);
  },
  update: function (element, value_accessor) {
    // value should be a boolean
    var val = ko.unwrap(value_accessor());
    if (val) {
      $(element).fadeIn(400);
    } else {
      $(element).fadeOut(400);
    }
  }
};

// Wrap the ace multiselect plugin
ko.bindingHandlers.multiSelect = {
  init: function (element, valueAccessor, allBindings, viewModel) {
    // Expects the value to have initial plugin options using the 'options' key,
    //  and an observableArray passed in as the 'observed' key

    // hook into observed value so we get updates
    valueAccessor().observed.subscribe(function (newVal) {
      console.log(newVal);
      $(element).multiselect('deselectAll');
      $(element).multiselect('select', newVal);

    });
    $(element).multiselect(valueAccessor().options);
  },
  update: function (element, valueAccessor, allBindings, viewModel) {
    $(element).multiselect('refresh');
  }
};

// Wrap the dual-listbox plugin
ko.bindingHandlers.dualListbox = {
  init: function (element, valueAccessor) {
    // Expects the value to have initial plugin options using the 'options' key,
    //  and an observableArray passed in as the 'observed' key

    // hook into observed value so we get updates
    valueAccessor().observed();
    $(element).bootstrapDualListbox(valueAccessor().options);
  },
  update: function (element, valueAccessor) {
    $(element).bootstrapDualListbox("refresh");
  }
};

// Works with any ajax validation
ko.bindingHandlers.uniqueNameValidation = {
  init: function (element, valueAccessor, allBindings, viewModel, bindingContext) {
    // This will be called when the binding is first applied to an element
    // Set up any initial state, event handlers, etc. here

    var value = valueAccessor();
    var remoteInvocation = value.remoteMethod;
    var tracked_observable = value.uniqueObservable;

    tracked_observable.has_been_checked = ko.observable(false);
    tracked_observable.is_checking = ko.observable(false);
    tracked_observable.is_unique = ko.observable(null);
    tracked_observable.unique_value = null;

    // Subscribe to changes
    tracked_observable.subscribe(function (current_value) {
      // Reset tracking vars
      tracked_observable.is_unique(null);
      tracked_observable.is_checking(true);
      tracked_observable.has_been_checked(false);

      // Check uniqueness on server
      remoteInvocation(current_value, function (is_unique) {
        tracked_observable.unique_value = current_value;
        tracked_observable.is_unique(is_unique);
        tracked_observable.is_checking(false);
        tracked_observable.has_been_checked(true);
      });
    });
  }
};


// Trigger a change event on any contenteditable fields for easier tracking.
// http://stackoverflow.com/questions/1391278/contenteditable-change-events
$(function () {
  $('body').on('focus', '[contenteditable]', function () {
    var $this = $(this);
    $this.data('before', $this.html());
    return $this;
  }).on('blur keyup paste input', '[contenteditable]', function () {
    var $this = $(this);
    if ($this.data('before') !== $this.html()) {
      $this.data('before', $this.html());
      $this.trigger('change');
    }
    return $this;
  });
});

// Components

// Flash message component
// params should have a FlashMessages object named "messages".
// Use this object to communicate with the flash message components.
var FlashMessages = function () {
  var self = this;
  self.messages = ko.observableArray();

  self.clear = function () {
    _.invoke(self.messages(), "dismiss");
  };

  self.flash_error = function (message) {
    self.messages.push(new FlashMessage({message: message, type: "error"}));
  };
  self.flash_success = function (message) {
    self.messages.push(new FlashMessage({message: message, type: "success"}));
  };
};

var FlashMessage = function (message_obj) {
  var self = this;
  self.message = message_obj.message;
  self.type = message_obj.type;
  self.is_visible = ko.observable(true);

  self.is_error = function () {
    return self.type === "error";
  };
  self.is_success = function () {
    return self.type === "success";
  };
  self.dismiss = function () {
    self.is_visible(false);
  }
};

ko.components.register('flash-messages', {
  viewModel: function (params) {
    var self = this;
    self.flash_messages = params.messages;
  },
  template: '\
  <!--ko foreach: flash_messages.messages-->\
  <div class="alert alert-block" \
  data-bind="visible: is_visible, \
  css: {\'alert-success\': is_success(), \'alert-danger\': is_error()}">\
  <button type="button" class="close" data-bind="click: dismiss">\
  <i class="ace-icon fa fa-times"></i>\
  </button>\
  \
  <p>\
  <strong data-bind="visible: is_success()">\
  <i class="ace-icon fa fa-check"></i>\
  </strong>\
  \
  <strong data-bind="visible: is_error()">\
  <i class="ace-icon fa fa-exclamation-triangle"></i>\
  </strong>\
  <span data-bind="html: message"></span>\
  </p>\
  </div>\
  <!--/ko-->'
});


// Loading modal component
// params should have an "options" key that is an observable object
// with a "message" string and optional "title".
// if the value of the observable is set to null, it hides the modal
ko.components.register('loading-modal', {
  viewModel: function (params) {
    var self = this;
    self.options = params.options;

    self.defaults = {
      message: "Loading...",
      title: "Please Wait"
    };

    self._current_settings = ko.pureComputed(function () {
      // Use current options if available, else fall back to defaults
      return $.extend({}, self.defaults, self.options() || {});
    });

    self.message = ko.pureComputed(function () {
      return self._current_settings().message;
    });

    self.title = ko.pureComputed(function () {
      return self._current_settings().title;
    });

    self.is_showing = ko.pureComputed(function () {
      return self.options() !== null;
    });
  },
  template: '\
  <div class="modal fade" data-bind="modal: is_showing">\
  <div class="modal-dialog">\
  <div class="modal-content">\
  <div class="modal-header">\
  <button type="button" class="close" data-dismiss="modal"><span\
  aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>\
  <h4 class="modal-title" data-bind="html: title">Please wait</h4>\
  </div>\
  <div class="modal-body">\
  <div class="form-panel modal-panel">\
  <div class="text-center">\
  <h4 data-bind="html: message"></h4>\
  <i class="ace-icon fa fa-spinner fa-spin grey bigger-200"></i>\
  </div>\
  </div>\
  </div>\
  <div class="modal-footer">\
  </div>\
  </div>\
  <!-- /.modal-content -->\
  </div>\
  <!-- /.modal-dialog -->\
  </div>\
  '
});

// Height select
ko.components.register('height-select', {
  viewModel: function (params) {
    var self = this;

    // Data: value is an observable that can be null or an int (inches)
    self.height = params.value;
    self.required = params.required || false;
    self.name_suffix = params.name_suffix || null;

    self.height_feet_part = ko.observable("" + get_feet_part(self.height()));
    self.height_inches_part = ko.observable("" + get_inches_part(self.height()));

    // Update the observed value when one of the selectors changes
    self.update_height = function () {
      var feet = parseInt(self.height_feet_part());
      var inches = parseInt(self.height_inches_part());
      if (feet === null || isNaN(feet) || inches === null || isNaN(inches)) {
        self.height(null);
      } else {
        self.height((12 * feet) + inches);
      }
    };

    self.height_feet_part.subscribe(self.update_height);
    self.height_inches_part.subscribe(self.update_height);
  },
  template: '\
  <label>\
  Height:\
  <select data-bind="value: height_feet_part, attr: {name: \'height_feet_\'+name_suffix}">\
  <option></option>\
  <option>4</option>\
  <option>5</option>\
  <option>6</option>\
  </select> Feet\
  </label>\
  <label>\
  <select data-bind="value: height_inches_part, attr: {name: \'height_inches_\'+name_suffix}">\
  <option></option>\
  <option>0</option>\
  <option>1</option>\
  <option>2</option>\
  <option>3</option>\
  <option>4</option>\
  <option>5</option>\
  <option>6</option>\
  <option>7</option>\
  <option>8</option>\
  <option>9</option>\
  <option>10</option>\
  <option>11</option>\
  </select> Inches\
  </label>'
});

function get_feet_part(val) {
  if (val === null || val === undefined) {
    return null;
  } else {
    return Math.floor(parseInt(val) / 12);
  }
}
function get_inches_part(val) {
  if (val === null || val === undefined) {
    return null;
  } else {
    return Math.floor(parseInt(val) % 12);
  }
}

// The following two components work together with the ProductStatesLimiterViewModel below
//  to ensure that all applications have valid product/state combos.

ko.components.register('limited-state-select', {
  viewModel: function (params) {
    this.limiter = params.limiter;
    this.is_disabled = ko.observable(params.disabled || false);
  },
  template: '<select name="enrollmentState" id="enrollmentState" data-bind="\
  value: limiter.selected_state,\
  options: limiter.available_states,\
  optionsCaption: \'(Select State)\',\
  optionsText: \'statecode\',\
  optionsAfterRender: limiter.disable_state_option_if_invalid,\
  disable: is_disabled\
  "></select>'
});

// Used when only one product is selected (no multi-product)
ko.components.register('limited-product-select', {
  viewModel: function (params) {
    this.limiter = params.limiter;
    this.is_disabled = ko.observable(params.disabled || false);
    this.is_mulit_select = params.is_multi_select || false;
  },
  template: '<select multiple name="productID" id="productID" data-bind="\
  selectedOptions: limiter.selected_products, \
  options: limiter.available_products,\
  optionsText: \'name\', \
  optionsAfterRender: limiter.disable_product_option_if_invalid,\
  disable: is_disabled\
  "> \
  </select>'
});
var ProductStatesLimiterViewModel = function (product_statecode_mapping,
                                              selected_state, available_states,
                                              selected_products, available_products) {
  // product_state_mapping: links a given product_id to a list of valid state codes
  // selected_state: an observable that can be null, or a two-letter statecode
  // available_states: the states we can select from
  // selected_products: an observable array tracking the selected products
  // available_products: an observable array with the products to choose from
  var self = this;


  self.product_state_mapping = map_states_to_products_from_statecode_map(available_states, product_statecode_mapping);

  self.selected_state = selected_state;
  self.available_states = available_states;

  self.selected_products = selected_products;
  self.available_products = available_products;

  // Until multi-product is working, use a single selected product
  //self.selected_product = ko.computed({
  //  read: function () {
  //    if (self.selected_products().length > 0) {
  //      return self.selected_products()[0];
  //    } else {
  //      return null;
  //    }
  //  },
  //  write: function (val) {
  //    if (val) {
  //      self.selected_products([val]);
  //    } else {
  //      self.selected_products([]);
  //    }
  //  },
  //  owner: self
  //});


  self.is_valid_product_for_state = function (product, state) {
    if (!state) {
      return true;
    }
    if (product.base_product_type === 'Static Benefit') {
      return true;
    }
    return _.contains(self.product_state_mapping[product.id], state);
  };


  self.is_state_disabled = function (state) {
    return !_.contains(self.enabled_states(), state);
  };
  self.is_product_disabled = function (product) {
    return !_.contains(self.enabled_products(), product);
  };

  self.disable_product_option_if_invalid = function (option, product) {
    // skip the caption
    if (product === undefined) {
      return;
    }

    ko.applyBindingsToNode(option, {
      disable: ko.computed(function () {
        return self.is_product_disabled(product);
      })
    }, product);
  };

  self.disable_state_option_if_invalid = function (option, state) {
    if (state === undefined) {
      // Skip over the top option in the select that doesn't have a value.
      return;
    }
    ko.applyBindingsToNode(option, {
      disable: ko.computed(function () {
        return self.is_state_disabled(state);
      })
    }, state);
  };

  // Based on product selection, change which states are enabled
  self.enabled_states = ko.computed(function () {
    return _.filter(self.available_states, function (state) {
      // Should be _.all(), but restriction lessened for now to allow multiproduct 
      return _.any(self.selected_products(), function (product) {
        return self.is_valid_product_for_state(product, state);
      });
    });
  });

  // List the products that can be selected given the current selected state
  self.enabled_products = ko.computed(function () {

    if (self.selected_state() !== null && self.selected_state()) {
      return _.filter(self.available_products(), function (product) {
        return self.is_valid_product_for_state(product, self.selected_state());
      });
    }

    return self.available_products();
  });

};


// Create a mapping of products to states using the state objects in the all_states list
//  This is necessary because knockout compares state objects with === comparison
function map_states_to_products_from_statecode_map(available_states, product_state_mapping) {
  var states_for_products = {};
  _.each(product_state_mapping, function (statecodes, product_id) {
    states_for_products[product_id] = [];
    _.each(statecodes, function (statecode) {
      var matched_state = _.find(available_states, function (s) {
        return s.statecode == statecode;
      });
      if (matched_state) {
        states_for_products[product_id].push(matched_state);
      }
    });
  });
  return states_for_products;
}


var StatesLimiterViewModel = function (product_statecode_mapping,
                                       selected_state, available_states,
                                       selected_products) {
  var self = this;
  self.available_states = available_states;
  self.selected_state = selected_state;
  self.product_state_mapping = map_states_to_products_from_statecode_map(available_states, product_statecode_mapping);
  self.is_valid_product_for_state = function (product, state) {
    return _.contains(self.product_state_mapping[product.id], state);
  };

  self.is_state_disabled = function (state) {
    return !_.contains(self.enabled_states(), state);
  };

  self.disable_state_option_if_invalid = function (option, state) {
    if (state === undefined) {
      // Skip over the top option in the select that doesn't have a value.
      return;
    }
    ko.applyBindingsToNode(option, {
      disable: ko.computed(function () {
        return self.is_state_disabled(state);
      })
    }, state);
  };

  // Based on product selection, change which states are enabled
  self.enabled_states = ko.computed(function () {
    return _.filter(self.available_states, function (state) {
      // SHOULD be _.all(), but we slackened this restriction temporarily
      return _.any(selected_products(), function (product) {
        return self.is_valid_product_for_state(product, state);
      });
    });
  });

  // When the products change, remove any ineligible states from the selection
  selected_products.subscribe(function (products) {
    // reset the selected state if not valid for the new product(s).
    if (!_.any(products, function (product) {
        return self.is_valid_product_for_state(product, self.selected_state());
      })) {
      self.selected_state(null);
    }
  });
};


// IE8 and below polyfill for object.create
if (typeof Object.create != 'function') {
  (function () {
    var F = function () {
    };
    Object.create = function (o) {
      if (arguments.length > 1) {
        throw Error('Second argument not supported');
      }
      if (o === null) {
        throw Error('Cannot set a null [[Prototype]]');
      }
      if (typeof o != 'object') {
        throw new TypeError('Argument must be an object');
      }
      F.prototype = o;
      return new F();
    };
  })();
}

// Show a special delete modal with custom message and callback.
ko.components.register('delete-confirm-modal', {
  viewModel: function (params) {
    var self = this;

    self.title = params.title;
    self.message = params.message;
    self.callback = params.callback;
    self.modal_observable = params.modal_observable;

    // Built-in behavior has user type "DELETE"
    self.delete_confirmation_text = ko.observable("");
    self.is_delete_text_valid = ko.pureComputed(function () {
      return self.delete_confirmation_text() == "DELETE";
    });

  },
  template: '<div data-bind="modal: modal_observable" class="modal fade">\
    <div class="modal-dialog modal-lg">\
      <div class="modal-content">\
        <div class="modal-header">\
          <button type="button" class="close" data-dismiss="modal"><span\
                                              aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>\
          <h4 class="modal-title" data-bind="text: title"></h4>\
        </div>\
        <div class="modal-body">\
          <div class="form-panel modal-panel">\
            <div class="row">\
              <div class="col-xs-12">\
                <p data-bind="text: message">\
                \
                </p>\
                <br>\
                <label>\
                  Type DELETE in the following box to confirm this is what you want:\
                  <input data-bind="textInput: delete_confirmation_text">\
                </label>\
              </div>\
            </div>\
          </div>\
        </div>\
\
        <div class="modal-footer">\
          <div class="form-buttons buttons-panel">\
            <button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button>\
            <button class="btn btn-danger"\
                    data-bind="enable: is_delete_enrollment_modal_showing.is_delete_text_valid, click: do_delete_enrollment_record">PERMANENT DELETE</button>\
          </div>\
        </div>\
      </div>\
    </div>\
  </div>'
});
