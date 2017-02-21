/* CaseReportsViewModel */
var CaseReportsViewModel = function(params) {
  var self = this;

  self.is_loading = ko.observable(true);
  self.error_message = ko.observable("");

  self.company_name = ko.observable({});
  self.group_number = ko.observable({});

  self.any_enrollments = ko.observable(false);

  self.summary_data = ko.observable({});
  self.product_data = ko.observable([]);
  self.enrollment_dates = ko.observable({});
  self.enrollment_methods = ko.observableArray([]);
  self.case_agents = ko.observableArray([]);

  self.load_reports = function() {
    self.is_loading(true);
    var url = urls.get_case_api_enrollment_report_url(params.settings_viewmodel.case_id);
    $.get(url).success(function(resp) {
      // Pull out the report data
      self.company_name(resp.data.company_name);
      self.group_number(resp.data.group_number);
      self.enrollment_dates(resp.data.enrollment_period);
      self.enrollment_methods(resp.data.enrollment_methods);
      self.summary_data(resp.data.summary);
      self.any_enrollments(self.summary_data().processed_enrollments > 0);

      // Reformat the product data to fit the display better
      var pdata = [];
      $.each(resp.data.product_report, function() {
        var data = this;
        pdata.push({
          name: data.product_name,
          num_taken: data.enrolled_count,
          percent_taken: (data.enrolled_count / resp.data.summary.total_census * 100).toFixed(2),
          annualized_premium: numberWithCommas(parseFloat(data.total_annualized_premium))
        });
      });
      self.product_data(pdata);
      // Case agents
      var case_agents = [];
      if (resp.data.case_owner) {
        case_agents.push(resp.data.case_owner.first + " "+resp.data.case_owner.last);
      }
      _.each(resp.data.case_agents, function(agent) {
        case_agents.push(agent.first + " "+agent.last);
      });
      self.case_agents(case_agents);
      self.is_loading(false);
    }).error(function(e) {
      console.error(e);
      self.error_message("There was a problem loading the enrollment report data.");
      self.is_loading(false);
    });
  };

  self.enter_print_preview = function () {
    $(".header ul").hide();
    $(".container.page").hide();
    $("#print-wrap").html($("#reports .report-wrap").clone()
  ).prepend("<div><a class='btn btn-default hidden-print' href='#reports'>"+
  "<span class='glyphicon glyphicon-chevron-left'></span> Back to report</a></div>").show();
};

self.exit_print_preview = function() {
  $(".header ul").show();
  $(".container.page").show();
  $("#print-wrap").hide();
};

self.load_enrollments = function() {
  case_management.refresh_enrollments_table(params.settings_viewmodel.case_id,
      urls.get_case_api_enrollment_records_url(params.settings_viewmodel.case_id),
  "#enrollment-records-table", "#enrollment-table-loading", {},
  function(table) {}
);
};

self.format_enrollment_methods = ko.computed(function() {
  var display_map = {
    in_person: 'In Person',
    self_enroll_email: 'Self-Enroll by Email',
    api_import: 'Imported'
  };

  return _.map(self.enrollment_methods(), function(m) {return display_map[m]}).join(", ");
});

self.format_enrollment_dates = ko.computed(function() {
  var start = self.enrollment_dates().start;
  var end = self.enrollment_dates().end;
  if (is_valid_date(end)) {
    return normalize_date(start) + " - " + normalize_date(end);
  } else {
    return "Open enrollment starting " + normalize_date(start);
  }
});

self.is_census_report = ko.computed(function() {
  return self.summary_data().is_census_report;
});

self.only_one_product = ko.computed(function() {
  return self.summary_data().only_one_product;
});

self.product_name = ko.computed(function() {
  return (self.only_one_product())? self.summary_data().product_names[0] : "";
});

self.format_total_percent_processed = ko.computed(function() {
  var percent = self.summary_data().processed_enrollments / self.summary_data().total_census * 100;
  return percent.toFixed(2) + "%";
});

self.format_total_num_census = ko.pureComputed(function() {
  return self.summary_data().total_census;
});

self.format_total_num_processed = ko.computed(function() {
  return self.summary_data().processed_enrollments;
});

self.format_total_percent_taken = ko.computed(function() {
  var percent = self.summary_data().taken_enrollments / self.summary_data().total_census * 100;
  return percent.toFixed(2) + "%";
});

self.format_total_num_taken = ko.computed(function() {
  return self.summary_data().taken_enrollments;
});

self.format_total_num_declined = ko.computed(function() {
  return self.summary_data().declined_enrollments;
});

self.format_total_percent_declined = ko.computed(function() {
  var percent = self.summary_data().declined_enrollments / self.summary_data().total_census * 100;
  return percent.toFixed(2) + "%";
});

self.format_total_annualized_premium = ko.computed(function() {
  return '$' + numberWithCommas(parseFloat(self.summary_data().total_annualized_premium));
});

self.format_case_agents = ko.computed(function() {
  return self.case_agents().join(", ");
});

params.settings_viewmodel.report_viewmodel(self);
};

/* END: CaseReportsViewModel */
