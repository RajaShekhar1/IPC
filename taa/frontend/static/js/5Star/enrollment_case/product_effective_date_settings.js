function ProductEffectiveDateSettings(product, effective_date_settings) {
  var self = this;
  var defaults = {
    effective_date_override: false,
    effective_date: ""
  };
  var settings = $.extend({}, defaults, effective_date_settings);
  self.id = product.id;
  self.effective_date_override = ko.observable(settings.effective_date_override);
  self.effective_date_input = ko.observable(settings.effective_date);

}

ProductEffectiveDateSettings.prototype.serialize = function() {
  return {
      product_id: this.id,
      effective_date_override: this.effective_date_override(),
      effective_date: this.effective_date_input()
    }
};
