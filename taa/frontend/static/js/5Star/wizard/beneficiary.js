var beneficiary = (function () {

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
  }
})();
