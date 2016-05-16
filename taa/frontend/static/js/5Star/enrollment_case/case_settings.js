
function add_case_error(errors, field_name, message) {
  if (field_name in errors) {
    errors[field_name].push(message);
  } else {
    errors[field_name] = [message];
  }
}

function get_form_data(form) {
  // Returns the form data as a javascript object.
  var data = $(form).serializeArray();
  var data_object = {};
  $.each(data, function() {
    var key = this.name;
    var val = this.value;
    if (key in data_object && data_object.length === undefined) {
      // Convert multi-selects to a list (key present more than once)
      data_object[key] = [data_object[key]];
      data_object[key].push(val);
    } else if (key in data_object) {
      data_object[key].push(val);
    } else {
      data_object[key] = val;
    }
  });
  return data_object;
}

function show_loading_panel(modal) {
  $(".form-panel", modal).hide("slide");
  $(".loading-panel", modal).show("slide");
  $(".processing-buttons", modal).show("fade");
  $(".form-buttons", modal).hide("fade");
}

function show_error_panel(modal) {
  $(".loading-panel", modal).hide("slide");
  $(".error-panel", modal).show("slide");
  $(".error-buttons", modal).show("fade");
  $(".processing-buttons", modal).hide("fade");
}

function show_success_panel(modal) {
  $(".loading-panel", modal).hide("slide");
  $(".success-panel", modal).show("slide");
  $(".success-buttons", modal).show("fade");
  $(".processing-buttons", modal).hide("fade");
}

function show_form_panel(modal) {
  $(".modal-panel", modal).hide("slide");
  $(".buttons-panel", modal).hide("slide");
  $(".form-panel", modal).show("slide");
  $(".form-buttons", modal).show("slide");
}

function reset_upload_modal() {
  $(".modal-panel").hide();
  $(".buttons-panel").hide();
  $(".form-panel").show();
  $(".form-buttons").show();
}

function handle_upload_success(resp, modal) {
  census_errors_panel.error_records(resp.data.errors);
  census_errors_panel.num_data_records(resp.data.records.length);
  show_success_panel($(modal));

  window.case_settings.census_data(resp.data.records);

  // Update the datatable
  case_management.refresh_census_table(window.case_id, urls.get_case_api_census_records_url(window.case_id),
  "#census-records-table", "#census-table-loading");
}

function handle_upload_error(xhr, modal) {
  if (xhr.status >= 400 && xhr.status < 500) {
    show_error_panel($(modal));
    var data = $.parseJSON(xhr.responseText).data;
    census_errors_panel.error_records(data.errors);

    // Show error tooltips
    $('.upload-error-table td.error').tooltip({placement: "auto left"});
  } else {
    alert("Sorry, there was a problem communicating with the server.");
    reset_upload_modal($(modal));
  }
}

function observe_settings_events(settings) {
  $("#upload-btn").on("click", reset_upload_modal);
  $(".back-btn").on("click", function() {
    show_form_panel($(this).parents(".modal"));
  });

  // Buttons to reset text
  $('#reset-page-text').click(function() {
    $('#page_text').html(settings.default_page_text).focus();
  });

  // Show or hide landing page as necessary
  $('#self_enrollment_type').change(function() {
    if($('#self_enrollment_type option:selected').val() == 'case-generic') {
      $('.self-enroll-email').hide();
      $('.self-enroll-email-off').show();
    } else {
      $('.self-enroll-email').show();
      $('.self-enroll-email-off').hide();
    }
  }).change();

  $('#edit-self-enrollment-form').on('submit', function() {
    window.case_settings.save_settings();
    return false;
  });
}

function observe_census_record_form_submit() {
  $("#census-record-form").on("submit", function() {
    var form = this;
    var file_select = $(this).find("input[type=file]").get(0);
    var files = file_select.files;
    var form_data = new FormData();
    if (files.length !== 1) {
      alert("Please select a file");
      return false;
    }

    // Add the file upload to the request.
    form_data.append('csv-file', files[0], files[0].name);
    form_data.append('upload_type', $("input[name=upload_type]:checked").val());


    send_file_data("POST", urls.get_case_api_census_records_url(window.case_id),
      form_data, function(data) {
        handle_upload_success(data, form);
      }, function(err) {
          handle_upload_error(err, form);
      }, false
    );

    show_loading_panel($(this));

    return false;
  });
}

function handle_enrollment_upload_success(data, modal) {
    window.enrollment_api_panel.num_records(data.data.num_processed);
    show_success_panel($(modal));
}

function handle_enrollment_upload_error(err, modal) {
  if (err.status >= 400 && err.status < 500) {
    var data = $.parseJSON(err.responseText).data;
    window.enrollment_api_panel.error_records(data.errors);
    show_error_panel($(modal));
  } else {
    alert("Sorry, there was a problem communicating with the server.");
    reset_upload_modal($(modal));
  }
}

function observe_enrollment_upload_form_submit() {

  $("#enrollment-csv-form").on("submit", function() {
    var form = this;

    var file_select = $(this).find("input[type=file]").get(0);
    var files = file_select.files;
    var form_data = new FormData();
    if (files.length !== 1) {
      alert("Please select a file");
      return false;
    }
    // Add the file upload to the request.
    form_data.append('api-upload-file', files[0], files[0].name);

    file_extension = files[0].name.split(".").slice(-1)[0];
    if (window.case_settings) {
      form_data.append('case_token', window.case_settings.case_token);
    }
    form_data.append('auth_token', window.enrollment_api_panel.user_token);
    form_data.append('email_errors', false);
    form_data.append('upload_source', 'api');
    form_data.append('format', file_extension);
    send_file_data("POST", urls.get_submit_enrollment_records_url(), form_data, function success(data) {
      handle_enrollment_upload_success(data, form);
    }, function error(err) {
      handle_enrollment_upload_error(err, form);
    }, false);
    show_loading_panel($(this));
    return false;
  });
}

function init_settings_fields() {
  $('#csv-file-input').ace_file_input({
    no_file:'No File ...',
    btn_choose:'Choose File',
    btn_change:'Change File',
    droppable:false,
    onchange:null,
    thumbnail:false,
    whitelist:'csv'
  });

  $('#csv-file-input-enrollment').ace_file_input({
    no_file:'No File ...',
    btn_choose:'Choose File',
    btn_change:'Change File',
    droppable:false,
    onchange:null,
    thumbnail:false,
    whitelist:'csv'
  });

  $(".input-mask-date").mask("99/99/9999");
  $(".input-mask-month-day").mask("99/99");
  $('[data-rel=tooltip]').tooltip();

  $('#partner-agents').bootstrapDualListbox({
    infoTextFiltered: '<span class="label label-purple label-lg">Filtered</span>',
    showFilterInputs: true,
    moveOnSelect: false,
    nonSelectedListLabel: 'Available Agents:',
    selectedListLabel: 'Case Visible to Agents:'
  });
}

function init_enrollment_import_fields() {
  $('#csv-file-input-enrollment').ace_file_input({
    no_file:'No File ...',
    btn_choose:'Choose File',
    btn_change:'Change File',
    droppable:false,
    onchange:null,
    thumbnail:false,
    whitelist:'csv'
  });
}

function apply_default_values() {
  // Set default values if applicable
  $.each(['#email_sender_name', '#email_sender_email', '#email_message',
  '#page_title', '#page_text'], function(index, value) {
    if($(value).prop('tagName') == 'DIV') {
      if($(value).html() == '') {
        $(value).html($(value).attr('default'));
        $(value).trigger('change');
      }
    } else {
      if($(value).val() == '') {
        $(value).val($(value).attr('default'));
        $(value).trigger('change');
      }
    }
  });
}

function handle_url_hashs() {
  // Show success message if required
  if(window.location.hash == '#success') {
    window.location.href = window.location.href.replace(/#.*$/, '#');
    $('#save-success').show();
  }
  // If we don't have a location hash, go to #setup
  if (window.location.hash == "") {
    window.location.hash = "setup";
  }
}
