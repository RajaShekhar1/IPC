function RiderOption(product_coverage, product, rider, initial_product_settings) {
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
      return (self.product.base_product_type === r.product_code &&
          self.rider.code === r.rider_code &&
          r.is_selected)
    });
  };

  self.is_selected = ko.observable(self.is_rider_initially_selected(initial_product_settings));
