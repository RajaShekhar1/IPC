


function init_rate_form() {
    // Bind event handlers
    $("button.show-rates").on("click", handle_show_rates);
    
}


function handle_show_rates() {
    
    var params = {
        gender: "male",
        employee_birthdate: $("#eeDOB").val()
    };
    
    ajax_post("/get_rates", params, handle_rate_response, handle_remote_error);
}

function handle_rate_response(data) {
    var rate_table = $(".recommended-coverage-table");
    for (var i = 0; i < data.rates.length; i++) {
        var rate = data.rates[i];
        
        // do something with the data
        console.log("got quote: $"+rate.weekly+" for $"+rate.coverage);
        var rate_div = $("<div></div>").addClass("rate").html("$"+rate.weekly+"/wk for $"+rate.coverage+" in coverage");
        rate_table.append(rate_div);
    }
    
    rate_table.slideDown();
}

function handle_remote_error() {
    console.error("Server-side error");    
}

function ajax_post(url, data, on_success, on_error) {
    $.ajax(url, {
        data: data,
        error: on_error,
        success: on_success,
        // expected return data type
        dataType: "json",
        method: "POST"
    });
}

