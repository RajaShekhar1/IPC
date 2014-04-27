

function init_rate_form() {
    var ui = new BenefitsUI();
    ko.applyBindings(ui);
}

// Root model of the Benefits wizard User Interface
function BenefitsUI(product) {
    var self = this;
    
    self.insurance_product = product;
    
    self.employee = ko.observable(new Beneficiary({}));
    
    // Marital and Spouse info
    var MS_MARRIED = 'Married',
        MS_UNMARRIED = 'Unmarried';
    self.marital_statuses = [MS_MARRIED, MS_UNMARRIED];
    self.employee.marital_status = ko.observable(null);
    
    self.should_show_spouse = ko.computed(function() {
       return self.employee.marital_status() == MS_MARRIED; 
    });
    self.spouse = ko.observable(new Beneficiary({}));
    self.show_spouse_name = ko.computed(function() {
        return (self.spouse().is_valid()) ? self.spouse().name() : "";
    });
    
    // Children
    self.should_include_children = ko.observable(false);
    self.children = ko.observableArray([
        // Start with two blank child entries
        new Beneficiary({}),
        new Beneficiary({})
    ]);
    self.show_children_names = ko.computed(function() {
        if (self.should_include_children()) {
            var valid_children = self.get_valid_children();
            var firstnames = $.map(valid_children, function(child) {
                return child.first();
            });
            return firstnames.join(", ") + " (Each)";
        } else {
            return "";
        } 
    });
    self.get_valid_children = function() {
        var children = [];
        $.each(self.children(), function() {
            if (this.is_valid()) {
                children.push(this);
            } 
        });
        return children;
    };
    
    self.add_child = function() {
        self.children.push(new Beneficiary({}));  
    };
    self.rendered_child = function(element) {
        $(element).hide().slideDown(400);
    };
    self.removing_child = function(element) {
        $(element).slideUp(function() {$(element).remove();});
    };
    
    // Recommended and selected benefits
    self.is_recommended_table_visible = ko.observable(false);
    self.show_recommendations_table = function() {
        self.is_recommended_table_visible(true);
    };
}

function Product(product_type) {
    var self = this;
    
    self.product_type = product_type;
}

function Beneficiary(options) {
    var self = this;
    
    self.first = ko.observable(options.first || "");
    self.last = ko.observable(options.last || "");
    self.birthdate = ko.observable(options.birthdate || null);
    
    self.is_valid = function() {
        return (
            $.trim(self.first()) != "" &&
            $.trim(self.last()) != "" &&
            $.trim(self.birthdate()) != ""
        );  
    };
    
    self.name = ko.computed(function() {
        // Only show if valid
        if (self.is_valid()) {
            return self.first(); 
        } else {
            return "";
        }
    });
    
    self.benefit_selection = ko.observable(null);
}

function Benefit(face_value, weekly_premium) {
    var self = this;
    
    self.weekly_premium = weekly_premium;
    self.face_value = face_value
}

ko.bindingHandlers.slideDownIf = {
    init: function(element, value_accessor) {
        var val = ko.unwrap(value_accessor());
        $(element).toggle(val);
    },
    update: function(element, value_accessor) {
        // value should be a boolean
        var val = ko.unwrap(value_accessor());
        if (val) {
            $(element).slideDown(400);
        } else {
            $(element).slideUp(400);
        }
    }
};






function _init_rate_form() {
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


function init_validation() {
    $(document).on('change', 'input:radio[id^="eeOwner-"]', function () {
        var other = $('#eeOtherOwner');
        var inp = $('#eeOtherOwnerName').get(0);
        var inpss = $('#eeOtherOwnerSSN').get(0);

        if (other.hasClass('hide')) {
            other.fadeIn("medium");
            other.removeClass('hide');
            inp.removeAttribute('disabled');
            inp.placeholder = "Full Name";
            inpss.removeAttribute('disabled');
        } else {
            $('#eeOtherOwner').addClass('hide');
            inp.setAttribute('disabled', 'disabled');
            inp.placeholder = "employee is policy owner";
            inp.value = "";
            inpss.setAttribute('disabled', 'disabled');
            inpss.value = "";
        }
    });

    $('[data-rel=tooltip]').tooltip();

    var $validation = true;
    $('#fuelux-wizard').ace_wizard().on('change', function (e, info) {
        if (info.step == 2 && $validation) {
            if (!$('#step2-form').valid()) return false;
        }
    }).on('finished', function (e) {
        bootbox.dialog({
            message: "Thank you! Your information was successfully saved!",
            buttons: {
                "success": {
                    "label": "OK",
                    "className": "btn-sm btn-primary"
                }
            }
        });
    }).on('stepclick', function (e) {
        //return false;//prevent clicking on steps
    });


    //documentation : http://docs.jquery.com/Plugins/Validation/validate


    $.mask.definitions['~'] = '[+-]';
    $('#phone').mask('(999) 999-9999');
    $('#eeDOB').mask('99/99/1999');
    $('.input-mask-date').mask('99/99/9999');
    $('.input-mask-ssn').mask('999-99-9999');
    $('.input-mask-zip').mask('99999');


    jQuery.validator.addMethod("phone", function (value, element) {
        return this.optional(element) || /^\(\d{3}\) \d{3}\-\d{4}( x\d{1,6})?$/.test(value);
    }, "Enter a valid phone number.");

    $('#step2-form').validate({
        errorElement: 'div',
        errorClass: 'help-block',
        focusInvalid: false,
        rules: {
            email: {
                required: true,
                email: true
            },
            eeFName: {
                required: true
            },
            eeLName: {
                required: true
            },
            phone: {
                required: true,
                phone: 'required'
            },
            comment: {
                required: true
            },
            state: {
                required: true
            },
            gender: 'required',
            agree: 'required'
        },

        messages: {
            email: {
                required: "Please provide a valid email.",
                email: "Please provide a valid email."
            },
            subscription: "Please choose at least one option",
            gender: "Please choose gender",
            agree: "Please confirm your agreement"
        },

        

        highlight: function (e) {
            $(e).closest('.form-group').removeClass('has-info').addClass('has-error');
        },

        success: function (e) {
            $(e).closest('.form-group').removeClass('has-error').addClass('has-info');
            $(e).remove();
        },

        errorPlacement: function (error, element) {
            if (element.is(':checkbox') || element.is(':radio')) {
                var controls = element.closest('div[class*="col-"]');
                if (controls.find(':checkbox,:radio').length > 1) controls.append(error);
                else error.insertAfter(element.nextAll('.lbl:eq(0)').eq(0));
            }
            else if (element.is('.select2')) {
                error.insertAfter(element.siblings('[class*="select2-container"]:eq(0)'));
            }
            else if (element.is('.chosen-select')) {
                error.insertAfter(element.siblings('[class*="chosen-container"]:eq(0)'));
            }
            else error.insertAfter(element.parent());
        },

        submitHandler: function (form) {
        },
        
        invalidHandler: function (event, validator) { 
            // display error alert on form submit   
            $('.alert-danger', $('.login-form')).show();
        }
        
    });
	
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



