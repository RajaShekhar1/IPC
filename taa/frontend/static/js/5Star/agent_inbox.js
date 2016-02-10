var agent_inbox = (function() {

  var AgentInboxViewModel = function() {
    var self = this;
    self.is_loading = ko.observable(true);
    self.envelopes = ko.observableArray([]);

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
      order: [[2, "desc"]],
      columns: [
        {
          sortable: false,
          data: function(row) {
            return '<button class="btn btn-primary btn-xs sign-button" data-id="'+row.id+'"><span class="ace-icon glyphicon glyphicon-pencil"></span> Sign</button>';
          }
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
    self.timestamp = envelope.timestamp;
    self.formatted_timestamp = moment(envelope.timestamp).format("MM/DD/YYYY h:mma");
    self.group = envelope.group;
    self.employee_first = envelope.employee_first;
    self.employee_last = envelope.employee_last;
    self.products = envelope.products;
    self.coverage = envelope.coverage;
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