var OccupationVM = (function (label, level, has_applicants) {
  'use strict';
  var self = this;

  self.has_applicants = !!has_applicants;
  self.label = label;

  self.serialize_object = function () {
    return {
      product_id: self.product_id,
      label: self.label
    };
  };
});