var OccupationVM = (function (label, level, product_id) {
  'use strict';
  var self = this;

  self.product_id = product_id || null;
  self.label = label;

  self.serialize_object = function () {
    return {
      product_id: self.product_id,
      label: self.label
    };
  };
});