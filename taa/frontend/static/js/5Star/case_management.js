var case_management = (function() {

    var loading_html = "<span class='icon-spinner icon-spin grey bigger-200'></span> <span class='bigger-175'> Loading data...</span>";

    function refresh_census_table(case_id, url, table_selector, loading_selector, init_callback, no_data_cb, success_callback) {

        // show loading message under the table
        var loading = $(loading_selector);
        var table = $(table_selector);
        var table_settings = {
            "responsive": {breakpoints: get_responsive_datatables_breakpoints()},
            "autoWidth": false,
            "aoColumnDefs":[
              // Show enroll button in first column
              {"aTargets":[0], "bSortable": false, "mData":function(source) {
                if (source.enrollment_status !== null) {
                  return '<button class="btn btn-primary btn-xs enroll-employee" data-id="'+source.id+'"><span class="ace-icon glyphicon glyphicon-plus"></span> Add Coverage</button>';
                }

                return '<button class="btn btn-primary btn-sm enroll-employee" data-id="'+source.id+'">Enroll</button>';
              }},
              {"aTargets":[1], "mData":function(source) {
                return format_enrollment_status_html(source.enrollment_status);
              }},
              {"aTargets":[2], "mData":function(source) {
                return "<a href='/enrollment-case/"+source.case_id+"/census/" + source.id + "'>"+ source.employee_first + "</a>";
              }},
              {"aTargets":[3], "mData":function(source) {
                return "<a href='/enrollment-case/"+source.case_id+"/census/" + source.id + "'>"+ source.employee_last + "</a>";
              }},
              {"aTargets":[4], "mData": function(source) {
                return normalize_date(source.employee_birthdate);
              }},
              {"aTargets":[5], "mData": "agent", className: "min-breakIII"}
              //{"aTargets":[5], "mData":"employee_email", className: "min-breakIII"}
            ],
            "aaSorting": [[ 3, "asc" ]]
          };

        // Show loading
        loading.html(loading_html);

        // Clear table if it exists
        clear_table(table);

        // Make the remote call
        $.get(url, {}, function(resp) {
            // Clear loading
            loading.html("");
            // If table exists, add new data. Otherwise, initialize
            if ($.fn.DataTable.fnIsDataTable(table[0])) {
                table.dataTable().fnAddData(resp.data);
                if (success_callback !== undefined) {
                    success_callback(table, resp.data);
                }
            } else if (resp.data.length > 0){
                table.show();
                $(".no-census-header").hide();
                // Initialize DataTable
                table_settings.aaData = resp.data;
                table.wrap("<div class='dataTables_borderWrap' />").dataTable(table_settings);
                if (init_callback !== undefined) {
                    init_callback(table, resp.data);
                }
                if (success_callback !== undefined) {
                    success_callback(table, resp.data);
                }
            } else if (no_data_cb !== undefined) {
                no_data_cb();
            }
        });
    }

    function refresh_enrollments_table(case_id, url, table_selector, loading_selector, table_options, init_callback) {
        // show loading message under the table
        var loading = $(loading_selector);
        var table = $(table_selector);

        var table_defaults = {
            "responsive": {breakpoints: get_responsive_datatables_breakpoints()},
            "autoWidth": false,
            "aoColumnDefs":[
                {"aTargets":[0], "mData": function(source) {
                    return format_date(parse_date(source.signature_time));
                },
                    "className":"min-breakII"
                },
                {"aTargets":[1], "mData": "employee_first"},
                {"aTargets":[2], "mData": "employee_last"},
                {"aTargets":[3], "mData": function(source) {
                    return format_date(parse_date(source.employee_birthdate, "YYYY-MM-DD"));
                },
                    "className":"min-breakII"
                },
                {"aTargets":[4], "mData": "employee_email",
                    "className":"min-breakI"},
                {"aTargets":[5], "mData": function(source) {
                    return format_enrollment_status_html(source.enrollment_status)
                },
                    className: "min-breakV"
                },
                {"aTargets":[6], "mData": function(source) {
                    return '$'+source.total_annual_premium;
                }, "sClass": "text-right"
                }
            ],
            "aaSorting": [[ 2, "asc" ]],
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
        table.DataTable().columns.adjust().draw();
    }

    function init_data_table(table, table_settings, data, init_callback) {
        table.show();
        table_settings.aaData = data;
        table.wrap("<div class='dataTables_borderWrap' />").dataTable(table_settings);
        if (init_callback !== undefined) {
            init_callback(table);
        }
        table.DataTable().columns.adjust().draw();
    }



    // Custom alphabet search for datatable
    var _alphabet_search_letter;

    $.fn.dataTableExt.afnFiltering.push(
        function( oSettings, aData, iDataIndex ) {
            if (!_alphabet_search_letter) {
                return true;
            }
            var last_name_col = 3;

            return (aData[last_name_col] && aData[last_name_col].charAt(0) === _alphabet_search_letter);
        }
    );

    function init_alphabet_search(table) {
        var alphabet = $('<div class="alphabet hidden-sm hidden-xs"/>').append('Last Name: ');
        $('<span class="clear active"/>')
            .data('letter', '')
            .html('Reset')
            .appendTo(alphabet);

        for (var i = 0; i < 26; i++) {
            var letter = String.fromCharCode(65 + i);

            $('<span class="letter"/>')
                .data('letter', letter)
                .html(letter)
                .appendTo(alphabet);
        }

        alphabet.insertBefore( table );

        alphabet.on('click', 'span', function() {
            alphabet.find('.active').removeClass('active');
            $(this).addClass('active');

            _alphabet_search_letter = $(this).data('letter');
            table.fnDraw();
        });
    }

    // Checkbox filter
    var _should_show_enrolled = true;
    $.fn.dataTableExt.afnFiltering.push(
        function(oSettings, aData, iDataIndex ) {
            if (_should_show_enrolled) {
                return true;
            }

            return (aData[1] === "Not Enrolled");
        }
    );

    function init_status_filter(table) {
        var ctn = $('<div class="status-filter">');
        ctn.html("<label><input type='checkbox' class='ace' checked='checked'> <span class='lbl'> Show Enrolled</span></label>");
        ctn.on('change', 'input', function() {
            _should_show_enrolled = $(this).prop("checked");
            table.fnDraw();
        });
        ctn.insertBefore(table);
    }


    return {
        refresh_census_table: refresh_census_table,
        refresh_enrollments_table: refresh_enrollments_table,
        init_alphabet_search: init_alphabet_search,
        init_status_filter: init_status_filter

    };
})();
