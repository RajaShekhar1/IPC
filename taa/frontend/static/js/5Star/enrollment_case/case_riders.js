
function CaseRiderConfiguration(case_settings, product, rider) {
  var self = this;
  self.case_settings = case_settings;
  self.product = product;
  self.rider = rider;

  self.name = rider.name;
  self.code = rider.code;
  self.is_selected = ko.observable(false);

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