var product_rates_service = (function() {

  function get_product_rates(products, payment_mode, applicant_list, callback, error_callback) {
    var requests = _.map(products, function(product) {
      return  $.ajax({
        url: "/products/"+product.product_data.id+"/rates",
        dataType: "json",
        method: "POST",
        contentType: "application/json; charset=utf-8",
        processData: false,
        data: JSON.stringify(build_rate_parameters(payment_mode, applicant_list))
      });
    });

    $.when.apply($, requests).done(callback).fail(error_callback);
  }

  function build_rate_parameters(payment_mode, applicant_list) {
    var params = {
      payment_mode: payment_mode.frequency
    };
    return $.extend({}, params, build_applicant_parameters(applicant_list));
  }

  function build_applicant_parameters(applicant_list) {
    return {
      employee: applicant_list.get_employee().serialize_data(),
      spouse: (applicant_list.has_valid_spouse())? applicant_list.get_spouse().serialize_data() : null,
      num_children: applicant_list.get_valid_children().length
    };
  }

  return {
    get_product_rates: get_product_rates
  };
})();