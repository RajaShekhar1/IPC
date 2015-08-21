var payment_mode = (function() {
  function PaymentMode(options) {
    this.frequency = options.frequency;
    this.label = options.label;
  }
  PaymentMode.prototype = {
    serialize: function() {
      return this.frequency;
    }
  };
  return {
    create_payment_mode: function(options) {
      return new PaymentMode(options);
    }
  }
})();