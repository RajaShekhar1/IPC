

function build_product(root, products) {

    if (products.length == 0) {
        alert("Error: No products to enroll.");
        return null;
    }

    // use the first product until multi-product 
    var product_data = products[0];

    var base_type = product_data.base_product_type;
    var base_product;
    if (base_type == "FPPTI") {
        base_product = new FPPTIProduct(product_data);
    } else if (base_type == "FPPCI") {
        base_product = new FPPCIProduct(product_data);
    } else if (base_type == "Group CI") {
        base_product = new GroupCIProduct(root, product_data);
    } else if (base_type == "FPP-Gov") {
        base_product = new FPPGovProduct(product_data);
    } else {
        // default product?
        alert("Invalid product type '"+base_type+"'");
        base_product = new FPPTIProduct(product_data);
    }

    // Check if this is a Guaranteed Issue product
    if (product_data.is_guaranteed_issue) {
        return new GIProductDecorator(base_product, product_data);
    } else {
        return base_product;
    }
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

function handle_remote_error() {
    alert("Sorry, an error occurred communicating with the server.");
}

function ajax_post(url, data, on_success, on_error, is_json) {
    var options = {
        data: data,
        error: on_error,
        success: on_success,
        // expected return data type
        dataType: "json",
        method: "POST"
    };
    if (is_json === true) {
        options.contentType = "application/json; charset=utf-8";
        options.processData = false;
        options.data = JSON.stringify(data);
    }
    $.ajax(url, options);
}


function init_case_riders(riders) {
  window.ui.selected_riders()["emp"](riders);
  window.ui.selected_riders()["sp"](riders);
  window.ui.case_riders = riders;
}

function init_enrollment_riders(riders) {
  window.ui.enrollment_riders = riders;
}

