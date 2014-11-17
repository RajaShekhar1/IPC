
// form_data is 
function send_form_data(method, url, data, on_success, on_error) {
    return submit_data(method, url, data, true, on_success, on_error);
}

// <data> is a FormData object that contains a file upload
function send_file_data(method, url, data, on_success, on_error) {
    return submit_data(method, url, data, false, on_success, on_error, false);
}

// <data> is a plain javascript object 
function send_json_data(method, url, data, on_success, on_error) {
    return submit_data(method, url, JSON.stringify(data), false, on_success, on_error, 'application/json');
}

// The shortcut functions above use this method to wrap the jquery ajax call in slightly different ways
function submit_data(method, url, data, should_process_data, on_success, on_error, contentType) {
    on_success = on_success || function() {};
    on_error = on_error || function() {};
    
    var options = {
        url: url,
        type: method,
        data: data,
        // return data type expected is always json for this app
        dataType: 'json',
        success: function(results) {
            on_success(results);   
        },
        error: function(xhr) {
            on_error(xhr);
        }
    };
    
    if (!should_process_data) {
        options.processData = false;
    } 
    
    if (contentType !== undefined) {
        options.contentType = contentType;
    } 
    
    return $.ajax(options);
}

// Forces the page to submit a post as if a form were submitted, without needing a form on the page 
function submit_to_url(url, data) {
    var form = document.createElement('form');
    for (k in data) {
        if (data.hasOwnProperty(k)) {
            $("<input>", {name: k, value: data[k]}).appendTo(form);
        }
    }
    
    form.method = "POST"; 
    form.action = url;
    form.submit();
}


// Errors
function show_all_errors(all_errors) {
    if (all_errors.length == 0) {
        return;
    }
        
    $(".submit-message").addClass("error").html("Please fix the indicated problems and resubmit.").show();
    $.each(all_errors, function(field_name, errors) {
        show_errors(field_name, errors);
        return false;
    });
    focus_first_error(all_errors);
}

function show_errors(field_name, field_error_messages) {
    get_field(field_name).after(
        $("<div>").addClass("error").html(field_error_messages.join("<br>"))
    );
}

function get_field(field_name) {
    return $("textarea[name="+field_name+"], select[name="+field_name+"], input[name="+field_name+"]");
}

function focus_first_error(errors) {
    $.each(errors, function(field_name, errors) {
        get_field(field_name).focus();
    })
}

function hide_all_errors() {
    $(".error").hide();
    $(".submit-message").removeClass("error").hide();
}