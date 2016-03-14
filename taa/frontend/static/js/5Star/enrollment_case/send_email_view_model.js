var SendEmailsModalViewModel = function SendEmailsModalViewModel(case_settings_vm, settings) {
  var self = this;
  self.case_settings_vm = case_settings_vm;

  self.numbers = {
    not_sent: ko.pureComputed(function() {
      // FIXME: now that census data not loaded on client side, need to fetch these numbers.
      return 0;
      return self.case_settings_vm.census_data().reduce(function addNotSent(acc, app) {
        return !app.sent_email ? acc+1 : acc;
      },0);
    }),
    not_enrolled: ko.pureComputed(function() {
      // FIXME: now that census data not loaded on client side, need to fetch these numbers.
      return 0;
      return self.case_settings_vm.census_data().reduce(function addNotApplied(acc, app) {
        return app.enrollment_status===null ? acc+1 : acc;
      },0);
    }),
    declined: ko.pureComputed(function() {
      // FIXME: now that census data not loaded on client side, need to fetch these numbers.
      return 0;
      return self.case_settings_vm.census_data().reduce(function addDeclined(acc, app) {
        return app.enrollment_status==='declined' ? acc+1 : acc;
      },0);
    })
  };

  self.send_type = ko.observable("");
  self.emailSettings = self.case_settings_vm.emailSettings;

  self.num_emails_to_send = ko.pureComputed(function() {
    if (!self.send_type()) return 0;

    return self.numbers[self.send_type().replace("-", "_")]();
  });

  self.resetEmail = function() {
    self.emailSettings.message(settings.default_email_message);
    self.emailSettings.subject(settings.default_email_subject);
  };

  self.errors = ko.observableArray();

  self.send_emails = function() {

    self.errors([]);
    if(self.num_emails_to_send() == 0) {
      self.errors.push("There are no census records that match the selected criteria.");
      return false;
    }

    if(!confirm("You are about to send "+self.num_emails_to_send()+" emails. Are you sure?")) {
      return false;
    }

    self.show_loading_panel();

    if (window.case_settings.can_edit_case) {
      // Save the case (along with any changes the email) and then send emails.
      window.case_settings.save_settings(save_callback);
    } else {
      // Todo: send email settings overrides
      // Just send the emails.
      send_emails({});
    }

    function save_callback(promise) {
      promise.then(send_emails, handle_failed_save);
    }

    function handle_failed_save() {
      bootbox.alert("Sorry, there was a problem saving the case data before sending the emails. No emails were sent.");
    }

    function send_emails(data) {
      $.ajax({
        url: urls.get_case_email_self_enrollment_batches_url(self.case_settings_vm.case_id)+ '?send_type=' + self.send_type(),
        method: 'POST',
        dataType: 'json',
        success: function(results) {
          self.show_finished_panel();
          var sent_status_list = results.data;
          $('#email-target-links-modal #email-status-wrap').html(sent_status_list.join('<br>'));
        },
        error: function(xhr) {
          self.show_error_panel();
        }
      });
    }

  };

  self.show_form_panel = function() {
    $('#email-target-links-modal .success-buttons').hide();
    $('#email-target-links-modal .form-buttons').show();
    $('#email-target-links-modal .modal-panel').hide();
    $('#email-target-links-modal .form-panel').show();
  };
  self.show_loading_panel = function () {
    $('#email-target-links-modal .form-panel').hide();
    $('#email-target-links-modal .form-buttons').hide();
    $('#email-target-links-modal .loading-panel').show();
    $('#email-target-links-modal .processing-buttons').show();
  };
  self.show_finished_panel = function() {
    $('#email-target-links-modal .loading-panel').hide();
    $('#email-target-links-modal .success-panel').show();
    $('#email-target-links-modal .processing-buttons').hide();
    $('#email-target-links-modal .success-buttons').show();
    // Show the first panel when we click again
    $("#email-target-links-modal .success-buttons button").click(function() {
      self.show_form_panel();
    });
  };
  self.show_error_panel = function() {
    $('#email-target-links-modal .loading-panel').hide();
    $('#email-target-links-modal .processing-buttons').hide();
    $('#email-target-links-modal .form-buttons').hide();
    $('#email-target-links-modal .error-panel').show();
    $('#email-target-links-modal .error-buttons').show();
    $("#email-target-links-modal .error-buttons button:not(.back-btn)").click(function() {
      $('#email-target-links-modal .error-buttons').hide();
      $('#email-target-links-modal .processing-buttons').show();
      $('#email-target-links-modal .modal-panel').hide();
      $('#email-target-links-modal .form-panel').show();
    });
  };
};
