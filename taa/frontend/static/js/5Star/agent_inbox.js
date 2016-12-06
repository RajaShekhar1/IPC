var agent_inbox = (function() {

  var AgentInboxViewModel = function(agent_id) {
    var self = this;
    self.current_agent_id = agent_id;

    self.is_loading = ko.observable(true);
    self.envelopes = ko.observableArray([]);

    self.pending_envelopes = ko.pureComputed(function() {
      return _.filter(self.envelopes(), function(e) {
        return e.is_pending();
      });
    });

    self.completed_envelopes = ko.pureComputed(function() {
      return _.filter(self.envelopes(), function(e) {
        return e.is_completed();
      });
    });

    self.signing_enrollment_id = ko.observable(null);

    self.existing_insurance = ko.observable(false);
    self.replacing_insurance = ko.observable(false);

    self.sign_envelope = function() {
      // Get the envelope ID from the button.
      self.signing_enrollment_id($(this).attr("data-id"));

      //sign_envelope(envelope_id, {from_inbox: true}).success(get_finished_signing_callback(envelope_id));

      // Show signing ceremony popup
      $("#modal-signing-enroller-in-person").modal("show");

    };

    self.should_show_other_insurance_questions = function() {
      return true;
    };

    self.show_pdf_preview = function() {
      self.pdf_url = '/enrollments/records/' + self.signing_enrollment_id() + '/pdf';

      bootbox.dialog({
          title: "Reviewing Application",
          className: 'pdf-preview',
          message: '<script>vm.renderPDF(vm.pdf_url, ".pdf-container", {canvasClass: "pdf-preview", loadingSel: ".loading-pdf-preview"});</script><div class="row">  ' +
          '<div class="col-md-12">' +
          '<h2 class="loading-pdf-preview text-center">' +
          'Loading application preview<br>' +
          '<i class="fa fa-spinner fa-spin fa-3x fa-fw"></i>' +
          '<span class="sr-only">Loading...</span>' +
          '</h2>' +
          '<div class="pdf-container text-center">' +
            //'<canvas id="pdf-preview"></canvas>' +
          '</div>' +
          '</div></div>',
          buttons: {
            confirm: {
              label: "Close",
              className: "width-25 pull-right btn btn-primary",
            }
          }
        });
    };
    
    var MAX_PDF_PREVIEW_SCALE = 1.25;
    self.renderPDF = function(url, targetSel, options) {
      options = options || {};
      var container = $(targetSel);
      var loadingSel = options.loadingSel || null;
      var canvasClass = options.canvasClass || '';
      var scale = options.scale || MAX_PDF_PREVIEW_SCALE;

      function renderPage(page) {
        scale = Math.min(MAX_PDF_PREVIEW_SCALE, container[0].clientWidth / page.getViewport(1).width);
        var viewport = page.getViewport(scale);
        var canvas = document.createElement('canvas');
        canvas.className = canvasClass;
        var ctx = canvas.getContext('2d');
        var renderContext = {
          canvasContext: ctx,
          viewport: viewport
        };

        canvas.height = viewport.height;
        canvas.width = viewport.width;
        $.each(container, function(idx, el) { el.appendChild(canvas); });
        if(loadingSel) {
          $(loadingSel).hide();
        }
        page.render(renderContext);
      }

      function renderPages(pdfDoc) {
        for (var num = 1; num <= pdfDoc.numPages; num++) {
          pdfDoc.getPage(num).then(renderPage);
        }
      }
      PDFJS.disableWorker = true;
      PDFJS.getDocument(url).then(renderPages);
    };

    self.view_envelope = function() {
      // Get the envelope ID from the button.
      var envelope_id = $(this).attr("data-id");

      view_envelope(envelope_id, {from_inbox: true}).success(get_finished_signing_callback(envelope_id));
    };

    function get_finished_signing_callback(envelope_id) {
      return function(resp) {
        var data = resp.data;
        // Remove voided envelope from the inbox immediately.
        if (data.errors && data.errors.length > 0 && data.errors[0].reason === "voided_envelope") {
          var envelope = _.find(self.envelopes(), function(e) { return e.id === envelope_id});
          if (envelope) {
            self.envelopes.remove(envelope);
          }
        }

      }
    }

    self.handle_agent_signing = function() {
      sign_envelope(self.signing_enrollment_id(), {from_inbox: true});
    };

    self.table_options = {
      order: [[2, "asc"]],
      columns: [
        {
          sortable: false,
          data: function(row) {
            var icon_class, btn_class;
            if (row.should_show_sign_button(self.current_agent_id)) {
              icon_class = "glyphicon glyphicon-pencil";
              btn_class = "sign-button";
            } else if (row.should_show_view_button(self.current_agent_id)) {
              icon_class = "glyphicon glyphicon-file";
              btn_class = "";//"view-button";
              // Just return a link to the PDF
              var pdf_url = '/enrollments/records/' + row.id + '/pdf';
              return '<a target="_blank" class="btn btn-primary btn-xs ' + btn_class+ '" href="'+pdf_url+'" data-id="'+row.id+'"><span class="ace-icon '+icon_class+'"></span> '+row.button_text+'</a>';
            } else {
              // No button if status is pending and we are not the agent on the envelope.
              return "";
            }
            return '<button class="btn btn-primary btn-xs ' + btn_class+ '" data-id="'+row.id+'"><span class="ace-icon '+icon_class+'"></span> '+row.button_text+'</button>';
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
          orderData: [2]
        },
        {
          data: "agent"
        },
        {
          data: "group"
        },
        {
          data: function(row) {
            return "<a href='/enrollment-case/" + row.case_id + "/census/" + row.census_record_id +"'>" + row.employee_first + "</a>";
          }
        },
        {
          data: function(row) {
            return "<a href='/enrollment-case/" + row.case_id + "/census/" + row.census_record_id +"'>" + row.employee_last + "</a>";
          }
        }
      ]
    };

    fetch_envelopes(self.is_loading, self.envelopes);
    // observe clicks
    $("body").on("click", "button.sign-button", self.sign_envelope);
    $("body").on("click", "button.view-button", self.view_envelope);

  };


  var EnvelopeViewModel = function(envelope) {
    var self = this;

    self.id = envelope.id;
    self.case_id = envelope.case_id;
    self.census_record_id = envelope.census_record_id;

    // Overall status of the application.
    self.application_status = envelope.application_status;

    // Individual signing statuses and timestamps.
    self.agent_signing_status = envelope.agent_signing_status;
    self.employee_signing_status = envelope.employee_signing_status;
    self.employee_signing_datetime = envelope.employee_signing_datetime;
    self.agent_signing_datetime = envelope.agent_signing_datetime;

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
      self.timestamp = (self.employee_signing_datetime) ?  self.employee_signing_datetime: self.agent_signing_datetime;
      self.formatted_timestamp = format_time();
    } else if (self.agent_signing_status === "declined_to_sign") {
      self.status = "Agent Declined";
      self.timestamp = envelope.timestamp;
      self.formatted_timestamp = format_time();
    } else {
      self.status = "Unknown";
      self.button_text = "";
      self.timestamp = envelope.timestamp;
      self.formatted_timestamp = format_time();
    }

    self.group = envelope.group;
    self.agent = envelope.agent;
    self.employee_first = envelope.employee_first;
    self.employee_last = envelope.employee_last;
    // TODO: Re-enable after pagination is addded
    // self.products = envelope.products;
    // self.coverage = envelope.coverage;
    self.agent_id = envelope.agent_id;

    self.is_pending = function() {
      return self.agent_signing_status !== "signed"
            // Never show voided or declines here
            && self.agent_signing_status !== "declined_to_sign"
            && self.application_status !== "voided";
    };

    self.is_completed = function() {
      return self.agent_signing_status === "signed"
            // Never show voided envelopes
            && self.application_status !== "voided";
    };


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


  function sign_envelope(envelope_id, options) {
    var url = "/envelopes/sign-enrollment/"+envelope_id;
    if (options && options.from_inbox) {
      url += "?from=inbox";
    }
    var req = $.post(url
      ).success(function(data) {return handle_signing_redirect(envelope_id, data)}
      ).error(handle_signing_failure);

    bootbox.alert("Please wait...");
    return req;
  }

  function view_envelope(enrollment_id, options) {
    //var url = "/envelopes/"+envelope_id+"/sign";

    var url = '/enrollments/records/' + enrollment_id + '/pdf';
    if (options && options.from_inbox) {
      url += "?from=inbox";
    }
    var req = $.post(url
      ).success(function(data) {return handle_signing_redirect(enrollment_id, data)}
      ).error(handle_signing_failure);

    bootbox.alert("Loading document, please wait...");
    return req;
  }

  function handle_signing_redirect(envelope_id, resp) {
      var data = resp.data;
      if (data.errors.length > 0) {
        bootbox.hideAll();
        bootbox.alert(data.errors[0].message);

        return;
      }

      // Perform the redirection.
      if (data.url) {
        window.location.href = data.url;
      } else {
        window.location.reload();
      }

    }

    function handle_signing_failure(data) {
      bootbox.hideAll();
      bootbox.alert("There was a problem signing the document.");
    }

  return {
    init_viewmodel: function(params) {
      return new AgentInboxViewModel(params);
    },

    sign_envelope: function(envelope_id) {
      return sign_envelope(envelope_id);
    },

    view_envelope: function(envelope_id) {
      return view_envelope(envelope_id);
    }
  }
})();