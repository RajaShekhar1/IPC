var EnrollmentAPIPanel = function EnrollmentAPIPanel(user_token) {
  var self = this;
  self.user_token = user_token;
  self.error_records = ko.observableArray();
  self.num_records = ko.observable();

  self.display_error = function(error_record) {
    var msg = "";
    if (error_record.record_num) {
      msg += "Error in record " + error_record.record_num + ": ";
    }

    msg += error_record.fields + ': ' + error_record.message;

    return msg;
  }
};
