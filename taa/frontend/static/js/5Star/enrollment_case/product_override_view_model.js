function ProductOverrideViewModel(product, case_data, initial_state_overrides) {
  var self = this;

  self.product = product;
  self.case_data = case_data;
  self.id = product.id;
  self.can_override_states = product.can_override_states;
  self.base_product_type = product.base_product_type;
  self.initial_state_overrides = initial_state_overrides;
  self.restrict_state_availability = ko.observable(false);
  self.state_overrides = ko.observableArray([]);


  self.get_overridden_states = function () {
    return self.state_overrides();
  };

  self.is_selected_state = function (statecode) {
    var initial_overrides = _.find(self.initial_state_overrides, function (so) {
      return _.get(so, String(self.id))
    });
    if (!!initial_overrides) {
      var product_states = _.get(initial_overrides, String(self.id));
      var exists = _.findIndex(product_states, statecode);
      if (exists > 1) {
        return true;
      }
      else {
        return null;
      }
    }
    else {
      return null;
    }
  };

  self.initialize_states_selection = function () {
    $('#states-list-product-' + self.id).bootstrapDualListbox({
      infoTextFiltered: '<span class="label label-purple label-lg">Filtered</span>',
      showFilterInputs: true,
      moveOnSelect: false,
      nonSelectedListLabel: 'Available States:',
      selectedListLabel: 'States Allowed:',
      selectorMinimalHeight: 250
    });
  };

  self.initialize_state_overrides = function () {
    var states = [];
    var id_string = String(self.id);
    if (!!self.case_data.product_settings) {
      states = _.find(self.case_data.product_settings.state_overrides, function (so) {
        return _.has(so, id_string);
      });
    }
    if (!!states) {
      states = _.get(states, String(self.id), []);
      if (states.length > 0) {
        self.restrict_state_availability(true);
      }
      self.state_overrides(states);
    }
  };

  self.initialize_state_overrides();

  self.can_product_override_states = function () {
    return self.can_override_states;
  };
}

ProductOverrideViewModel.prototype.serialize = function() {
  var states = this.get_overridden_states();
  var object = {};
  object[this.id] = states;
  return object;
};

ProductOverrideViewModel.prototype.initialize = function() {
  this.initialize_states_selection();
  this.initialize_state_overrides();
};