var case_management = (function() {

    var loading_html = "<span class='icon-spinner icon-spin grey bigger-200'></span> <span class='bigger-175'> Loading data...</span>";

    function refresh_census_table(case_id, url, table_selector, loading_selector, init_callback, no_data_cb, success_callback) {

        // show loading message under the table
        var loading = $(loading_selector);
        var table = $(table_selector);
        var table_settings = {
            "responsive": {breakpoints: get_responsive_datatables_breakpoints()},
            "autoWidth": false,
            "processing": true,
            "pagingType": "full",
            "serverSide": true,
            "ajax": "/cases/"+case_id+"/census_records",
            "columnDefs":[
              // Show enroll button in first column
              {targets: [0], sortable: false, name: "action", data: function(row) {
                if (row.enrollment_status === "enrolled") {
                    return '<button class="btn btn-primary btn-xs enroll-employee" data-id="'+row.id+'"><span class="ace-icon glyphicon glyphicon-plus"></span> Add Coverage</button>';
                } else if (row.enrollment_status === "pending_employee" || row.enrollment_status === "pending_agent") {
                    //return '<button class="btn btn-primary btn-xs enroll-employee" data-id="'+source.id+'"><span class="ace-icon fa fa-pencil"></span> Sign</button>'
                    // No button because only the agent who created it can enroll, and we don't have that agent id in this record right now.
                    return "";//"<a class='btn btn-warning btn-xs' href='/enrollment-case/"+source.case_id+"/census/" + source.id + "'><span class='icon fa fa-pencil'></span> Sign</a>";
                } else {
                    return '<button class="btn btn-primary btn-sm enroll-employee" data-id="'+row.id+'">Enroll</button>';
                }
              }},
              {targets: [1], name: "status", data: function(row) {
                return format_enrollment_status_html(row.enrollment_status);
              }},
              {targets: [2], name: "employee_first", data: function(row) {
                return "<a href='/enrollment-case/"+row.case_id+"/census/" + row.id + "'>"+ row.employee_first + "</a>";
              }},
              {targets: [3], name: "employee_last", data: function(source) {
                return "<a href='/enrollment-case/"+source.case_id+"/census/" + source.id + "'>"+ source.employee_last + "</a>";
              }},
              {targets: [4], sortable: false, name: "employee_birthdate", data: function(source) {
                return normalize_date(source.employee_birthdate);
              }},
              //{"aTargets":[5], "data": "agent", className: "min-breakIII"}
              {targets:[5], sortable: false, name: "employee_email", data: "employee_email", className: "min-breakIII"}
            ],
            sorting: [[3, "asc"]]
        };


        table.show();
        $(".no-census-header").hide();

        if (!$.fn.DataTable.fnIsDataTable(table[0])) {
            // Initialize DataTable
            //table_settings.aaData = resp.data;
            table.wrap("<div class='dataTables_borderWrap' />").DataTable(table_settings);
        } else {
            table.DataTable().columns.adjust().draw();
        }

    }

    function refresh_enrollments_table(case_id, url, table_selector, loading_selector, table_options, init_callback) {
        // show loading message under the table
        var loading = $(loading_selector);
        var table = $(table_selector);

        var table_defaults = {
            "responsive": {breakpoints: get_responsive_datatables_breakpoints()},
            "autoWidth": false,
            "processing": true,
            "pagingType": "full",
            "serverSide": true,
            "ajax": "/cases/"+case_id+"/enrollment_records",
            "columnDefs":[
                {"targets":[0], name: "date", "data": function(source) {
                    return format_date(parse_date(source.date));
                },
                    "className":"min-breakII"
                },
                {"targets":[1], name: "employee_first", "data": function(row) {
                    return "<a href='/enrollment-case/"+row.case_id+"/census/" + row.census_record_id + "'>"+ row.employee_first + "</a>";
                }},
                {"targets":[2], name: "employee_last", "data": function(row) {
                    return "<a href='/enrollment-case/"+row.case_id+"/census/" + row.census_record_id + "'>"+ row.employee_last + "</a>";
                }},
                {"targets":[3], name: "employee_birthdate",  sortable: false, "data": function(source) {
                    return format_date(parse_date(source.employee_birthdate, "YYYY-MM-DD"));
                    },
                    "className":"min-breakI"
                },
                //{"targets":[4], name: "employee_email", "data": "employee_email",
                //    "className":"min-breakI"},
                {targets: [4], name: "agent_name", data: "agent_name", "className": "min-breakII"},
                {"targets":[5], name: "enrollment_status", "data": function(source) {
                    return format_enrollment_status_html(source.enrollment_status)
                    },
                    className: "min-breakV"
                },
                // {"targets": [6], sortable: false, name: "effective_date", "data": function (source) {
                //   console.log(source);
                //   return normalize_date(source.effective_date);
                //   },
                //   className: "min-breakI"
                // },
                {"targets":[6], name: "total_premium", "data": function(source) {
                    return '$'+source.total_premium;
                    },
                    className: "text-right"
                }
            ],
            "sorting": [[ 2, "asc" ]],
            "displayLength": 25
        };
        var table_settings = $.extend({}, table_defaults, table_options || {});

        // Show loading
        //loading.html(loading_html);

        // Clear table if it exists
        //clear_table(table);

        if (!table_exists(table)) {
            table.show();
            table.wrap("<div class='dataTables_borderWrap' />").DataTable(table_settings);
            init_callback(table);
        } else {
            table.DataTable().columns.adjust().draw();
        }
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
