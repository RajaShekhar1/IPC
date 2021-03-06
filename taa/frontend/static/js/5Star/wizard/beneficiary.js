var beneficiary = (function () {
  "use strict";

  // ViewModel for Beneficiaries
  function Beneficiary(options) {
    var self = this;

    var defaults = {
      name: "",
      relationship: "",
      ssn: "",
      date_of_birth: "",
      is_nonperson_entity: false
    };
    options = $.merge({}, defaults, options);

    self.name = ko.observable(options.name);
    self.relationship = ko.observable(options.relationship);
    self.ssn = ko.observable(options.ssn);
    self.date_of_birth = ko.observable(options.date_of_birth);
    self.is_nonperson_entity = ko.observable(options.is_nonperson_entity);

    self.date_of_birth_validation_error = ko.observable(null);

    self.date_of_birth.subscribe(function (value) {
      self.date_of_birth_validation_error(get_date_of_birth_validation_error(value));
    });

    self.serialize = function () {
      return {
        name: self.name(),
        relationship: self.relationship(),
        ssn: self.ssn(),
        date_of_birth: self.date_of_birth(),
        is_nonperson_entity: self.is_nonperson_entity()
      };
    };
  }

  // Exports
  return {
    Beneficiary: Beneficiary
  };
})();
