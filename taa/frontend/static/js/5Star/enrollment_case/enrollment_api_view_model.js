var EnrollmentAPIPanel = function EnrollmentAPIPanel(user_token) {
  var self = this;
  self.user_token = user_token;
  self.error_records = ko.observableArray();
  self.num_records = ko.observable();
}
