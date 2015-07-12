var urls = (function urls() {
  return {
    get_manage_cases_url: function() {
      return '/enrollment-cases';
    },
    get_manage_case_url: function(case_id) {
      return '/enrollment-case/' + case_id;
    },
    get_cases_api_url: function() {
      return "/cases";
    },
    get_case_api_url: function(case_id) {
      return '/cases/' + case_id;
    },
    get_case_api_enrollment_periods_url: function(case_id) {
      return '/cases/' + case_id + '/enrollment_periods';
    },
    get_case_api_enrollment_report_url: function(case_id) {
      return '/cases/' + case_id + '/enrollment_report';
    },
    get_case_api_self_enrollment_url: function(case_id) {
      return '/cases/' + case_id + '/self_enrollment_setup';
    },
    get_case_api_enrollment_records_url: function(case_id) {
      return '/cases/' + case_id + '/enrollment_records';
    },
    get_case_api_enrollment_record_url: function(case_id, census_record_id) {
      return '/cases/' + case_id + '/enrollment_records/' + census_record_id;
    },
    get_case_api_census_records_url: function(case_id) {
      return '/cases/' + case_id + '/census_records';
    },
    get_case_api_census_record_url: function(case_id, census_record_id) {
      return '/cases/' + case_id + '/census_records/' + census_record_id;
    },
    get_case_api_census_email_batches: function(case_id) {
      return "/cases/"+case_id+"/self_enroll_email_batches"
    },
    get_case_api_census_email_batch_preview_url: function(case_id, batch_id) {
      return "/batch-info/" + case_id + "/preview/" + batch_id
    },
    get_case_api_census_email_batch_logs_url: function(case_id, batch_id) {
      return "/batch-info/" + case_id + "/logs/" + batch_id;
    },
    get_case_email_self_enrollment_batches_url: function(case_id) {
      return '/cases/' + case_id + '/self_enroll_email_batches';
    },
    get_case_email_self_enrollment_batches_url: function(case_id, batch_id) {
      return '/cases/' + case_id + '/self_enroll_email_batches/' + batch_id;
    },
    get_product_api_url: function(product_id) {
      return '/products/' + case_id;
    },
    get_in_person_enrollment_url: function() {
      return '/in-person-enrollment';
    },
  }
})();
