
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

ko.bindingHandlers.flashMessage = {
    update: function(element, valueAccessor) {
        $(element).html(ko.unwrap(valueAccessor())).delay(5000).hide('fade')
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

// Height select
ko.components.register('height-select', {
    viewModel: function(params) {
        var self = this;
        
        // Data: value is an observable that can be null or an int (inches)
        self.height = params.value;
        
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
            <select data-bind="value: height_feet_part">\
                <option></option>\
                <option>4</option>\
                <option>5</option>\
                <option>6</option>\
            </select> Feet\
        </label>\
        <label>\
            <select data-bind="value: height_inches_part">\
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



