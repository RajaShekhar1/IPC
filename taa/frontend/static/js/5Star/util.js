
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

function get_loading_html(message) {
    var text = message || "Loading data...";
    //return "<span class='icon-spinner icon-spin grey bigger-200'></span> <span class='bigger-175'> "+text+"</span>";
    return '<div class="text-center">' +
                "<h4>"+text+"</h4>"+
                '<i class="icon-spinner icon-spin grey bigger-200"></i>'+  
            "</div>";
}

// Misc Formatting
function format_enrollment_status_text(status) {
    if (status === "enrolled") {
        return "Enrolled";
    } else if (status === "declined") {
        return "Declined";
    } else {
        return "Not Enrolled";
    }
}

function format_enrollment_status_html(status) {
    var status_text = format_enrollment_status_text(status);
    if (status_text === "Not Enrolled") {
        return status_text;
    } else {
        if (status_text === "Enrolled") {
            return "<span class='enroll-status ace-icon glyphicon glyphicon-ok'> </span><span class='enroll-status'> Enrolled</span>";
        } else {
            return "<span class='ace-icon glyphicon glyphicon-remove error'></span> <span class='enroll-status declined'> Declined</span>";
        }
    }
}

// Date handling
function parse_date(date_str, format_str) {
    // Parse a date as a moment object from the given string, according to the format string. 
    // Defaults format to server-sent date format, and falls back to MM/DD/YYYY otherwise.
    if (format_str === undefined) {
        format_str = ["YYYY-MM-DD","MM/DD/YYYY"];
    }
    return moment(date_str, format_str);
}
function normalize_date(date_str) {
    return format_date(parse_date(date_str));
}
function is_valid_date(date_str, format_str) {
    // Is the given string valid according to the format string? Defaults format to server-sent date format.
    var date = parse_date(date_str, format_str);
    return date.isValid();
}
function format_date(moment_date) {
    // Given a moment object, format it the same across the site
    return moment_date.format("MM/DD/YYYY");    
}
function now() {
    return moment();
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


// Error handling / validation
function show_all_errors(all_errors) {
    if (Object.keys(all_errors).length == 0) {
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
    return $("textarea[name="+field_name+"], select[name="+field_name+"], input[name="+field_name+"], #"+field_name);
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


// Common formatting, etc

// http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}



// Custom bindings

// Show or hide a modal based on a boolean observable
ko.bindingHandlers.modal = {
    update: function(element, valueAccessor) {
        if (ko.unwrap(valueAccessor())) {
            $(element).modal('show');
        } else {
            $(element).modal('hide');
        }
    }
};

ko.bindingHandlers.flashMessage = {
    update: function(element, valueAccessor) {
        $(element).html(ko.unwrap(valueAccessor())).delay(5000).hide('fade')
    } 
};

ko.bindingHandlers.maskedInput = {
    init: function(element, valueAccessor) {
        $(element).mask(ko.unwrap(valueAccessor()));
    }
};

// Reveal a panel by sliding down when the observable is true; slide back up if false.
ko.bindingHandlers.slideDownIf = {
    init: function(element, value_accessor) {
        var val = ko.unwrap(value_accessor());
        // Initially hide or show without animation
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

ko.bindingHandlers.fadeInIf = {
    init: function(element, value_accessor) {
        var val = ko.unwrap(value_accessor());
        // Initially hide or show without animation
        $(element).toggle(val);
    },
    update: function(element, value_accessor) {
        // value should be a boolean
        var val = ko.unwrap(value_accessor());
        if (val) {
            $(element).fadeIn(400);
        } else {
            $(element).fadeOut(400);
        }
    } 
};


// Wrap the ace multiselect plugin 
ko.bindingHandlers.multiSelect = {
    init: function(element, valueAccessor, allBindings, viewModel) {
        // Expects the value to have initial plugin options using the 'options' key, 
        //  and an observableArray passed in as the 'observed' key
        
        // hook into observed value so we get updates
        valueAccessor().observed();
        $(element).multiselect(valueAccessor().options);
    },
    update: function(element, valueAccessor, allBindings, viewModel) {
        $(element).multiselect('refresh');
    }
};

// Wrap the dual-listbox plugin
ko.bindingHandlers.dualListbox = {
    init: function(element, valueAccessor) {
        // Expects the value to have initial plugin options using the 'options' key, 
        //  and an observableArray passed in as the 'observed' key
        
        // hook into observed value so we get updates
        valueAccessor().observed();
        $(element).bootstrapDualListbox(valueAccessor().options);
    },
    update: function(element, valueAccessor) {
        $(element).bootstrapDualListbox("refresh");
    }
};

// Works with any ajax validation
ko.bindingHandlers.uniqueNameValidation = {
    init: function(element, valueAccessor, allBindings, viewModel, bindingContext) {
        // This will be called when the binding is first applied to an element
        // Set up any initial state, event handlers, etc. here
        
        var value = valueAccessor();
        var remoteInvocation = value.remoteMethod;
        var tracked_observable = value.uniqueObservable;
        
        tracked_observable.has_been_checked = ko.observable(false);
        tracked_observable.is_checking = ko.observable(false);
        tracked_observable.is_unique = ko.observable(null);
        
        // Subscribe to changes
        tracked_observable.subscribe(function(current_value) {
            // Reset tracking vars
            tracked_observable.is_checking(true);
            tracked_observable.has_been_checked(false);
            
            // Check uniqueness on server
            remoteInvocation(current_value, function(is_unique) {
                tracked_observable.is_unique(is_unique);
                tracked_observable.is_checking(false);
                tracked_observable.has_been_checked(true);
            });
        });
    }
};


// Components

// Flash message component
// params should have a FlashMessages object named "messages". 
// Use this object to communicate with the flash message components.
var FlashMessages = function() {
    var self = this;
    self.messages = ko.observableArray();
    
    self.clear = function() {
        _.invoke(self.messages(), "dismiss");  
    };
    
    self.flash_error = function(message) {
        self.messages.push(new FlashMessage({message: message, type: "error"}));
    };
    self.flash_success = function(message) {
        self.messages.push(new FlashMessage({message: message, type: "success"}));  
    };
};

var FlashMessage = function(message_obj) {
    var self = this;
    self.message = message_obj.message;
    self.type = message_obj.type;
    self.is_visible = ko.observable(true);
    
    self.is_error = function() {
        return self.type === "error";
    };
    self.is_success = function() {
        return self.type === "success";  
    };
    self.dismiss = function() {
        self.is_visible(false);
    }
};

ko.components.register('flash-messages', {
    viewModel: function(params) {
        var self = this;
        self.flash_messages = params.messages;
    }, 
    template: 
        '\
        <!--ko foreach: flash_messages.messages-->\
        <div class="alert alert-block" \
            data-bind="visible: is_visible, \
            css: {\'alert-success\': is_success(), \'alert-danger\': is_error()}">\
            <button type="button" class="close" data-bind="click: dismiss">\
                <i class="ace-icon fa fa-times"></i>\
            </button>\
            \
            <p>\
                <strong data-bind="visible: is_success()">\
                    <i class="ace-icon fa fa-check"></i>\
                </strong>\
                \
                <strong data-bind="visible: is_error()">\
                    <i class="ace-icon fa fa-exclamation-triangle"></i>\
                </strong>\
                <span data-bind="html: message"></span>\
            </p>\
        </div>\
        <!--/ko-->'
});


// Loading modal component
// params should have an "options" key that is an observable object
// with a "message" string and optional "title".
// if the value of the observable is set to null, it hides the modal
ko.components.register('loading-modal', {
    viewModel: function(params) {
        var self = this;
        self.options = params.options;
        
        self.defaults = {
            message: "Loading...",
            title: "Please Wait"
        };
        
        self._current_settings = ko.pureComputed(function() {
            // Use current options if available, else fall back to defaults
            return $.extend({}, self.defaults, self.options() || {});
        });
        
        self.message = ko.pureComputed(function() {
            return self._current_settings().message;
        });
        
        self.title = ko.pureComputed(function() {
            return self._current_settings().title;
        });
        
        self.is_showing = ko.pureComputed(function() {
            return self.options() !== null; 
        });
    }, 
    template: '\
        <div class="modal fade" data-bind="modal: is_showing">\
            <div class="modal-dialog">\
                <div class="modal-content">\
                    <div class="modal-header">\
                        <button type="button" class="close" data-dismiss="modal"><span\
                            aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>\
                        <h4 class="modal-title" data-bind="html: title">Please wait</h4>\
                    </div>\
                    <div class="modal-body">\
                        <div class="form-panel modal-panel">\
                            <div class="text-center">\
                                <h4 data-bind="html: message"></h4>\
                                <i class="icon-spinner icon-spin grey bigger-200"></i>\
                            </div>\
                        </div>\
                    </div>\
                    <div class="modal-footer">\
                    </div>\
                </div>\
                <!-- /.modal-content -->\
            </div>\
            <!-- /.modal-dialog -->\
        </div>\
    '
});

// Height select
ko.components.register('height-select', {
    viewModel: function(params) {
        var self = this;
        
        // Data: value is an observable that can be null or an int (inches)
        self.height = params.value;
        self.required = params.required || false;
        self.name_suffix = params.name_suffix || null;
        
        self.height_feet_part = ko.observable(get_feet_part(self.height()));
        self.height_inches_part = ko.observable(get_inches_part(self.height()));
        
        // Update the observed value when one of the selectors changes
        self.update_height = function() {
            var feet = parseInt(self.height_feet_part());
            var inches = parseInt(self.height_inches_part());
            if (feet === null || inches === null) {
                self.height(null);
            } else {
                self.height((12 * feet) + inches);
            }
        };
        
        self.height_feet_part.subscribe(self.update_height);
        self.height_inches_part.subscribe(self.update_height);
    },
    template:
        '\
        <label>\
        Height:\
            <select data-bind="value: height_feet_part, attr: {name: \'height_feet_\'+name_suffix}">\
                <option></option>\
                <option>4</option>\
                <option>5</option>\
                <option>6</option>\
            </select> Feet\
        </label>\
        <label>\
            <select data-bind="value: height_inches_part, attr: {name: \'height_inches_\'+name_suffix}">\
                <option></option>\
                <option>1</option>\
                <option>2</option>\
                <option>3</option>\
                <option>4</option>\
                <option>5</option>\
                <option>6</option>\
                <option>7</option>\
                <option>8</option>\
                <option>9</option>\
                <option>10</option>\
                <option>11</option>\
            </select> Inches\
        </label>'
});

function get_feet_part(val) {
    if (val === null || val === undefined) {
        return null;
    } else {
        return parseInt(val) / 12;
    }
}
function get_inches_part(val) {
    if (val === null || val === undefined) {
        return null;
    } else {
        return parseInt(val) % 12;
    }
}



