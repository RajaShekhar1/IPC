var DEFAULT_OCCUPATION_LABEL = 'Default';

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

function sort_occupations(occupations) {
  if (!Array.isArray(occupations)) {
    return null;
  }
  var new_occupations = [];
  var default_occupation = _.find(occupations, function (occupation) { return occupation.label === DEFAULT_OCCUPATION_LABEL; });
  if (default_occupation) {
    new_occupations.push(default_occupation);
  }
  var sorted_occupations = _.chain(occupations)
    .filter(function (occupation) { return occupation.label !== DEFAULT_OCCUPATION_LABEL;  })
    .sortBy('label')
    .value();
  _.forEach(sorted_occupations, function (occupation) {
    new_occupations.push(occupation);
  });
  return new_occupations;
}