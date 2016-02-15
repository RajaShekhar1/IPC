var agent_inbox = (function() {

  var AgentInboxViewModel = function(agent_id) {
    var self = this;
    self.current_agent_id = agent_id;

    self.is_loading = ko.observable(true);
    self.envelopes = ko.observableArray([]);

    self.pending_envelopes = ko.pureComputed(function() {
      return _.filter(self.envelopes(), function(e) {
        return e.agent_signing_status !== "signed";
      });
    });

    self.completed_envelopes = ko.pureComputed(function() {
      return _.filter(self.envelopes(), function(e) {
        return e.agent_signing_status === "signed";
      });
    });

    self.sign_envelope = function() {
      // Invoked using jquery due to data tables control of table.
      var envelope_id = $(this).attr("data-id");
      $.post("/envelopes/"+envelope_id+"/sign"
        ).success(self.handle_signing_redirect
        ).error(self.handle_signing_failure);

      bootbox.alert("Redirecting to signing page, please wait...");
    };

    self.handle_signing_redirect = function(data) {
      window.location.href = data.data.url;
    };

    self.handle_signing_failure = function(data) {
      bootbox.hideAll();
      bootbox.alert("There was a problem redirecting to the signing page.");
    };

    self.table_options = {
      order: [[2, "asc"]],
      columns: [
        {
          sortable: false,
          data: function(row) {
            var icon_class;
            if (row.should_show_sign_button(self.current_agent_id)) {
              icon_class = "glyphicon glyphicon-pencil";
            } else if (row.should_show_view_button(self.current_agent_id)) {
              icon_class = "glyphicon glyphicon-file";
            } else {
              // No button if status is pending and we are not the agent on the envelope.
              return "";
            }
            return '<button class="btn btn-primary btn-xs sign-button" data-id="'+row.id+'"><span class="ace-icon '+icon_class+'"></span> '+row.button_text+'</button>';
          }
        },
        {
          data: "status"
        },
        {
          data: "timestamp",
          visible: false
        },
        {
          data: "formatted_timestamp",
          orderData: [1]
        },
        {
          data: "group"
        },
        {
          data: "employee_first"
        },
        {
          data: "employee_last"
        },
        {
          data: "products"
        },
        {
          data: "coverage"
        }

      ]
    };

    fetch_envelopes(self.is_loading, self.envelopes);
    // observe clicks
    $("body").on("click", "button.sign-button", self.sign_envelope);
  };


  var EnvelopeViewModel = function(envelope) {
    var self = this;

    self.id = envelope.id;

    self.agent_signing_status = envelope.agent_signing_status;
    self.employee_signing_status = envelope.employee_signing_status;
    self.employee_signing_datetime = envelope.employee_signing_datetime;

    if (self.employee_signing_status === "pending" || self.employee_signing_status === "declined_to_sign") {
      self.status = "Pending Employee";
      self.button_text = "Sign";
      self.timestamp = envelope.timestamp;
      self.formatted_timestamp = format_time();
    } else if (self.agent_signing_status === null || self.agent_signing_status === "pending") {
      self.status = "Pending Agent";
      self.button_text = "Sign";
      // TODO: on pending tab still, just uses created timestamp.
      self.timestamp = envelope.timestamp;
      self.formatted_timestamp = format_time();
    } else if (self.agent_signing_status === "signed") {
      self.status = "Complete";
      self.button_text = "View";
      self.timestamp = self.employee_signing_datetime;
      self.formatted_timestamp = format_time();
    } else {
      self.status = "Unknown";
      self.button_text = "";
      self.timestamp = envelope.timestamp;
      self.formatted_timestamp = format_time();
    }

    //self.timestamp = envelope.timestamp;
    //self.formatted_timestamp = moment(envelope.timestamp).format("MM/DD/YYYY h:mma");
    self.group = envelope.group;
    self.employee_first = envelope.employee_first;
    self.employee_last = envelope.employee_last;
    self.products = envelope.products;
    self.coverage = envelope.coverage;
    self.agent_id = envelope.agent_id;

    self.should_show_sign_button = function(current_agent_id) {
      return self.button_text === "Sign" && (current_agent_id === self.agent_id || self.agent_id === null);
    };

    self.should_show_view_button = function(current_agent_id) {
      return self.button_text === "View";
    };

    function format_time() {
      return moment(self.timestamp).format("MM/DD/YYYY h:mma");
    }

  };

  function fetch_envelopes(loading_observable, envelopes_observable) {

    loading_observable(true);

    $.ajax("/envelopes").success(function(data) {
      loading_observable(false);

      var envelopes = _.map(data.data, function(envelope_data) {
        return new EnvelopeViewModel(envelope_data);
      });
      envelopes_observable(envelopes);


    })
  }

  return {
    init_viewmodel: function(params) {
      return new AgentInboxViewModel(params);
    }
  }
})();