
function CaseRiderConfiguration(case_settings, product, rider, initial_product_settings) {
  var self = this;
  self.case_settings = case_settings;
  self.product = product;
  self.rider = rider;

  self.name = rider.name;
  self.code = rider.code;

  self.is_rider_initially_selected = function(initial_product_settings) {
    if (!initial_product_settings || !initial_product_settings.riders) {
      return false;
    }

    return _.any(initial_product_settings.riders, function(r) {
      return (self.product.id === r.product_id &&
          self.rider.code === r.rider_code &&
          r.is_selected)
    });
  };

  self.is_selected = ko.observable(self.is_rider_initially_selected(initial_product_settings));

  self.can_enable_rider = ko.pureComputed(function() {

    var is_enabled = true;

    // Check the list of enabled riders. If any of the enabled riders are in our
    // restricted list, we can't enable.
    _.each(case_settings.enabled_case_riders_for_product(self.product), function(r) {
      if (_.includes(rider.disallowed_rider_combinations, r.code)){
        is_enabled = false;
        // Break out of loop.
        return false;
      }
    });

    return is_enabled;
  });

}

CaseRiderConfiguration.prototype.serialize = function() {
  return {
      product_id: this.product.id,
      rider_code: this.rider.code,
      is_selected: this.is_selected()
    }
};
