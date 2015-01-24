

/* testing */

var case_management = (function() {
    
    var loading_html = "<span class='icon-spinner icon-spin grey bigger-200'></span> <span class='bigger-175'> Loading data...</span>";
    
    function refresh_census_table(case_id, url, table_selector, loading_selector, table_options, init_callback) {
        // show loading message under the table
        var loading = $(loading_selector);
        var table = $(table_selector);
        var table_defaults = {
            "aoColumnDefs":[
                {"bSortable": false, 
                 "aTargets":[0], "mData":function(source) {
                    return "<a href='/manage-case/"+case_id+"/census/"+source.id+"'><span class='glyphicon glyphicon-edit'></span></a>";
                }},
                {"aTargets":[1], "mData": "employee_first"},
                {"aTargets":[2], "mData": "employee_last"},
                {"aTargets":[3], "mData": function(source) {
                    var d = moment(source.employee_birthdate, "YYYY-MM-DD");
                    return d.format("MM/DD/YYYY");
                }},
                {"aTargets":[4], "mData": "employee_email"}
            ],
            "aaSorting": [[ 2, "asc" ]],
            "iDisplayLength": 25
        };
        var table_settings = $.extend({}, table_defaults, table_options || {});
        
        // Show loading
        loading.html(loading_html);
        
        // Clear table if it exists
        if ($.fn.DataTable.fnIsDataTable(table.get(0))) {
            table.dataTable().fnClearTable();
        }
        // Make the remote call
        $.get(url, {}, function(resp) {
            // Clear loading
            loading.html("");
            // If table exists, add new data. Otherwise, initialize
            if ($.fn.DataTable.fnIsDataTable(table.get(0))) {
                table.dataTable().fnAddData(resp.data);
            } else if (resp.data.length > 0){
                table.show();
                $(".no-census-header").hide();
                // Initialize DataTable
                table_settings.aaData = resp.data;
                table.dataTable(table_settings);
                if (init_callback !== undefined) {
                    init_callback(table);
                }
            }
        });
    }
    
    function refresh_enrollments_table(case_id, url, table_selector, loading_selector, table_options, init_callback) {
        // show loading message under the table
        var loading = $(loading_selector);
        var table = $(table_selector);
        var table_defaults = {
            "aoColumnDefs":[
                
                {"aTargets":[0], "mData": "employee_first"},
                {"aTargets":[1], "mData": "employee_last"},
                {"aTargets":[2], "mData": function(source) {
                    var d = moment(source.employee_birthdate, "YYYY-MM-DD");
                    return d.format("MM/DD/YYYY");
                }},
                {"aTargets":[3], "mData": "employee_email"},
                {"aTargets":[4], "mData": "application_status"},
                {"aTargets":[5], "mData": function(source) {
                    return '$'+source.total_annual_premium;
                }, "sClass": "text-right"}
            ],
            "aaSorting": [[ 1, "asc" ]],
            "iDisplayLength": 25
        };
        var table_settings = $.extend({}, table_defaults, table_options || {});
        
        // Show loading
        loading.html(loading_html);
        
        // Clear table if it exists
        clear_table(table);
        
        // Make the remote call
        $.get(url, {}, function(resp) {
            // Clear loading
            loading.html("");
            
            // If table exists, add new data. Otherwise, initialize
            if (table_exists(table)) {
                update_table_data(table, resp.data);
            } else if (resp.data.length > 0) {
                // Initialize DataTable
                init_data_table(table, table_settings, resp.data, init_callback);
            }
        });
    }
    function table_exists(table) {
        return $.fn.DataTable.fnIsDataTable(table.get(0));
    }
    function clear_table(table) {
        if ($.fn.DataTable.fnIsDataTable(table.get(0))) {
            table.dataTable().fnClearTable();
        }
    }
    function update_table_data(table, data) {
        table.dataTable().fnAddData(data);
    }
    
    function init_data_table(table, table_settings, data, init_callback) {
        table.show();
        table_settings.aaData = data;
        table.dataTable(table_settings);
        if (init_callback !== undefined) {
            init_callback(table);
        }
    }
    
    return {
        refresh_census_table: refresh_census_table,
        refresh_enrollments_table: refresh_enrollments_table
    };
})();
