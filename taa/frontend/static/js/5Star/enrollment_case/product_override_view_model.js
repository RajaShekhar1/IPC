function ProductOverrideViewModel(product, case_data) {
  var self = this;

  self.product = product;
  self.case_data = case_data;
  self.id = product.id;
  self.base_product_type = product.base_product_type;


  var initial_overrides = [];
  if (case_data.product_settings && case_data.product_settings.state_overrides) {
    initial_overrides = case_data.product_settings.state_overrides[product.id] || [];
  }
  self.state_overrides = ko.observableArray(initial_overrides);
  self.restrict_state_availability = ko.observable(self.state_overrides().length > 0);

  //
  // self.is_selected_state = function (statecode) {
  //   var initial_overrides = _.find(self.initial_state_overrides, function (so) {
  //     return _.get(so, String(self.id))
  //   });
  //   if (!!initial_overrides) {
  //     var product_states = _.get(initial_overrides, String(self.id));
  //     var exists = _.findIndex(product_states, statecode);
  //     if (exists > 1) {
  //       return true;
  //     }
  //     else {
  //       return null;
  //     }
  //   }
  //   else {
  //     return null;
  //   }
  // };
  //
  // self.initialize_states_selection = function () {
  //   $('#states-list-product-' + self.id).bootstrapDualListbox({
  //     infoTextFiltered: '<span class="label label-purple label-lg">Filtered</span>',
  //     showFilterInputs: true,
  //     moveOnSelect: false,
  //     nonSelectedListLabel: 'Available States:',
  //     selectedListLabel: 'States Allowed:',
  //     selectorMinimalHeight: 250
  //   });
  // };

  self.can_product_override_states = function () {
    return product.can_override_states;
  };

  self.serialize_overridden_states = function() {
    if (!self.restrict_state_availability()) {
      return [];
    }
    return self.state_overrides();
  };

  //self.initialize_states_selection();
}


ProductOverrideViewModel.prototype.initialize = function() {
  this.initialize_states_selection();
  this.initialize_state_overrides();
};