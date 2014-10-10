var product_state_select = (function() {
    
    var state_select, 
        product_select,
        product_states;
    
    function init(state_selector, product_selector, product_states_mapping) {
        
        state_select = $(state_selector);
        product_select = $(product_selector);
        product_states = product_states_mapping;
        
        // Update states when product changes
        product_select.on("change", update_states_for_product);
        
        // Initial view
        update_states_for_product();
    }
    
    function update_states_for_product() {
        var current_state = state_select.val();
        var current_product = product_select.val();
        
        if (!current_product) return;
        
        // Remove current options
        state_select.find("option").remove();
        
        // Add the correct options to the select
        $.each(product_states[current_product], function () {
            var statecode = this[0];
            var statename = this[1];
            var is_disabled = this[2];
            
            var option = $("<option></option>"
                ).attr("value", statecode
                ).prop("disabled", is_disabled
                ).text(statename
                );
            state_select.append(option);
        });
        
        // If the selected value is no longer enabled, set the value to what it was before
        if (current_state) {
            var is_disabled = state_select.find("option[value="+current_state+"]").prop("disabled");
            if (!is_disabled) {
                state_select.val(current_state);
            }
        }
            
    }
    
    return {
        init: init
    };
    
})();