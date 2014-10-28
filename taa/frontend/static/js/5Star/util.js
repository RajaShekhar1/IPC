
function send_form_data(method, url, form_data, on_success, on_error) {
    $.ajax(url, {
        type: method,
        data: form_data,//JSON.stringify(data),
        dataType: 'json',
        success: function(results) {
            on_success(results);   
        },
        error: function(xhr) {
            on_error(xhr);
            //console.log(xhr.responseJSON.errors)
        }
    });
}
function send_json_data(method, url, data, on_success, on_error) {
    $.ajax(url, {
        type: method,
        processData: false,
        contentType:'application/json',
        data: JSON.stringify(data),
        dataType: 'json',
        success: function(results) {
            on_success(results);   
        },
        error: function(xhr) {
            on_error(xhr);
            //console.log(xhr.responseJSON.errors)
        }
    });
}

function submit_to_url(url, data) {
    var form = document.createElement('form');
    for (k in data) {
        $("<input>", {name: k, value: data[k]}).appendTo(form);
    }
    
    form.method = "POST"; 
    form.action = url;
    form.submit();
}