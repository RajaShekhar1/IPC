
function format_face_value(val) {
  if (val == null) {
    return "(NA)";
  }
  return "$" + numberWithCommas(val);
}

function format_premium_value(val) {
  if (val == null) {
    return "(NA)";
  }
  return "$" + numberWithCommas(val.toFixed(2));
}

function handle_remote_error() {
  alert("Sorry, an error occurred communicating with the server.");
}

function ajax_post(url, data, on_success, on_error, is_json) {
  var options = {
    data: data,
    error: on_error,
    success: on_success,
    // expected return data type
    dataType: "json",
    method: "POST"
  };
  if (is_json === true) {
    options.contentType = "application/json; charset=utf-8";
    options.processData = false;
    options.data = JSON.stringify(data);
  }
  $.ajax(url, options);
}


function init_case_riders(riders) {
  window.ui.selected_riders()["emp"](riders);
  window.ui.selected_riders()["sp"](riders);
  window.ui.case_riders = riders;
}

function init_enrollment_riders(riders) {
  window.ui.enrollment_riders = riders;
}

