

function init_rate_form() {
    // Bind event handlers
    $("button.show-rates").on("click", update_rates);
    $("#eeDOB").on("change", update_rates_if_showing);
    $("#spDOB").on("change", update_rates_if_showing);
    $("#qtyChildren").on("change", update_rates_if_showing);
}

function update_rates_if_showing() {
    if (!rate_table_showing()) {
        return;
    }
    update_rates();
}

function update_rates() {
    var employee_info = get_employee_info();
    ajax_post("/get_rates", employee_info, show_rate_comparison_table, handle_remote_error);
}


function get_employee_info() {
    return {
        employee_first: $("#eeFName").val(),
        employee_last: $("#eeLName").val(),
        employee_birthdate: $("#eeDOB").val(),
        spouse_first: $("#spFName").val(),
        spouse_last: $("#spLName").val(),
        spouse_birthdate: $("#spDOB").val(),
        num_children: $("#qtyChildren").val()
    };
}

function show_rate_comparison_table(data) {
    var rate_table = get_rate_table();
    for (var i = 0; i < data.rates.length; i++) {
        var rate = data.rates[i];
        
        // do something with the data
        var rate_div = $("<div></div>").addClass("rate").html("$"+rate.weekly+"/wk for $"+rate.coverage+" in coverage");
        rate_table.append(rate_div);
    }
    
    reveal_table()
}

function get_rate_table() {
    return $(".recommended-coverage-table");
}
function reveal_table() {
    if (!rate_table_showing()) {
        get_rate_table().slideDown();    
    }
}

function rate_table_showing() {
    return get_rate_table().is(":visible");
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


