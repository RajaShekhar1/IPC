var remote_service = (function() {
  function get_product_rates(product_id, data) {
    return $.ajax({
        url: "/products/"+product_id+"/rates",
        dataType: "json",
        method: "POST",
        contentType: "application/json; charset=utf-8",
        processData: false,
        data: JSON.stringify(data)
      });
  }

  return {
    get_product_rates: get_product_rates
  };
})();