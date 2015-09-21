/* EmployeeMatchViewModel */
var EmployeeMatchViewModel = function(data) {
  var self = this;
  self.id = data.id;
  self.first = data.employee_first || "(No First Name)";
  self.last = data.employee_last || "(No Last Name)";
  self.ssn = data.employee_ssn;
  self.birthdate = data.employee_birthdate;

  self.formatted_birthdate = function() {
    return moment(self.birthdate, "YYYY-MM-DD").format("MM/DD/YYYY");
  };

  self.is_selected = ko.observable(false);
};
/* END: EmployeeMatchViewModel */
