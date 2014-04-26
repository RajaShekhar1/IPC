

function init_rate_form() {
    // Bind event handlers
    get_rate_button().on("click", handle_button_click);
    
    var watch_elements = [
        $("#eeDOB"),
        $("#spDOB"),
        $("#qtyChildren"),
        $("#eeFName"),
        $("#spFName")
    ];
    $.each(watch_elements, function() {
        this.on('change', update_rates_if_showing)
    });
}

function get_rate_button() {
    return $("button.show-rates");
}

function handle_button_click() {
    //get_rate_button().hide();
    
    update_rates();
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
        employee_first: get_employee_first(),
        employee_last: $("#eeLName").val(),
        employee_birthdate: get_employee_birthday(),
        spouse_first: get_spouse_first(),
        spouse_last: $("#spLName").val(),
        spouse_birthdate: $("#spDOB").val(),
        num_children: get_num_children()
    };
}

function get_employee_first() {
    return $("#eeFName").val();
}
function get_employee_birthday() {
    return $("#eeDOB").val();
}
function get_spouse_first() {
    return $("#spFName").val();
}
function get_num_children() {
    return $("#qtyChildren").val();
}

function show_rate_comparison_table(data) {
    var rate_table_ctn = get_rate_table();
    var name_ctn = get_rate_table_row_names();
    
    var show_spouse = ('weekly_byface' in data.spouse_rates && data.spouse_rates.weekly_byface); 
    var show_children = get_num_children() > 0;
    
    // Names
    var spacer = "&nbsp;";
    name_ctn.html("");
    name_ctn.append("<li>"+(get_employee_first()|| spacer)+"</li>");
    name_ctn.append("<li>"+(get_spouse_first()|| spacer)+"</li>");
    var children_label = (get_num_children() > 0)? "Children" : spacer;
    name_ctn.append("<li class='children'>"+(children_label || "&nbsp")+"</li>");
    
    // 50, 100, 150 for good, better, best
    clear_table(rate_table_ctn);
    
    $.each([data.employee_rates, data.spouse_rates], function() {
        var premium_data = this;
        
        $.each([["good", 50000],["better", 100000], ["best",150000]], function() {
            var rate_type = this[0];
            var coverage = this[1];
            var col = rate_table_ctn.find("."+rate_type+" .pricing-table");
            if (!('weekly_byface' in premium_data && premium_data['weekly_byface'].length > 0)) {
                col.append("<li></li>");
                return true;
            }
            
            var premium = get_coverage_premium(premium_data.weekly_byface, coverage);
            
            var coverage_fmt = format_face_value(coverage);
            var premium_fmt = format_premium_value(premium);
            
            col.append("<li><strong>"+coverage_fmt+"</strong><br>"+premium_fmt+"</li>");
        });
    });
    
    // children
    if (show_children) {
        var premium_data = data.children_rates.weekly_premiums;
        $.each([["good", premium_data[0]],["better", premium_data[1]], ["best",premium_data[1]]], function() {
            var rate_type = this[0];
            var data = this[1];
            var premium = data.premium;
            var coverage = data.coverage;
            var coverage_fmt = format_face_value(coverage);
            var premium_fmt = format_premium_value(premium);
            var col = rate_table_ctn.find("."+rate_type+" .pricing-table");
            col.append("<li><strong>"+coverage_fmt+"</strong><br>"+premium_fmt+"</li>");
        });
    }
    
    
    
    // Custom rates
    var custom_col = rate_table_ctn.find(".custom .pricing-table");
    var item = $("<li></li>");
    item.append(build_rate_select(data.employee_rates));
    custom_col.append(item);
    
    var spouse_select = (show_spouse)? build_rate_select(data.spouse_rates) : spacer;
    var item = $("<li></li>");
    item.append(spouse_select);
    custom_col.append(item);
    
    var children_select = (show_children) ? build_children_select(data.children_rates.weekly_premiums): spacer;
    var item = $("<li></li>");
    item.append(children_select);
    custom_col.append(item);
    
    reveal_table()
}

function format_face_value(val) {
    if (val == null) {
        return "(NA)";
    }
    return "$"+numberWithCommas(val);
}

function format_premium_value(val) {
    if (val == null) {
        return "(NA)";
    }
    return "$"+numberWithCommas(val.toFixed(2));
}

// http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function clear_table(rate_table_ctn) {
    rate_table_ctn.find(".pricing-table").html("");
}

function build_rate_select(data) {
    var sel = $("<select></select>");
    $.each(data.weekly_bypremium, function() {
        var opt = $("<option></option>").text(format_premium_value(this.premium) + "/week");
        sel.append(opt);
    });
    $.each(data.weekly_byface, function() {
        var opt = $("<option></option>").text(format_face_value(this.coverage));
        sel.append(opt);
    });
    
    return sel;
}

function build_children_select(rates) {
    var sel = $("<select></select>");
    $.each(rates, function() {
        var opt = $("<option></option>").text(format_premium_value(this.premium) + "/week for "+
            format_face_value(this.coverage));
        sel.append(opt);
    });
    return sel;
}

function get_coverage_premium(data, coverage) {
    var premium = null;
    $.each(data, function() {
        if (this.coverage == coverage) {
            premium = this.premium;
            return false;
        } 
    });
    return premium;
}


function get_rate_table() {
    return $(".recommended-coverage-table");
}
function get_rate_table_row_names() {
    return get_rate_table().find("ul.compare-row-names");
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


