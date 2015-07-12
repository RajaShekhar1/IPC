
/* CaseEnrollmentsViewModel */
var CaseEnrollmentsViewModel = function(case_data) {
  var self = this;

  self.add_to_census_modal = new AddToCensusViewModel(case_data.id);
  ko.applyBindings(self.add_to_census_modal, $("#add-to-census-modal")[0]);
  $("#add-to-census-btn").on('click', function() {

    $("#add-to-census-modal").modal("show");
    self.add_to_census_modal.show_modal();
  });

  $("#enrollment").on('click', "button.enroll-employee", function() {
    var record_id = $(this).attr('data-id');

    submit_to_url(urls.get_in_person_enrollment_url(), {
      record_id: record_id,
      enrollment_city: window.case_settings.get_enrollment_city_override(),
      enrollment_state: window.case_settings.get_enrollment_state_override()
    });
  });
};
/* END: CaseEnrollmentsViewModel */
