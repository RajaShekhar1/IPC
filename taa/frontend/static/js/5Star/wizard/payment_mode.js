var payment_mode_module = (function() {
  function PaymentMode(options) {
    this.frequency = options.frequency;
    this.label = options.label;
  }
  PaymentMode.prototype = {
    serialize: function() {
      return this.frequency;
    },
    display_lowercase: function() {
      return this.label.toLowerCase();
    },
    display: function() {
      return this.label;
    }
  };

  var FREQUENCY_LABELS = {
    12: 'Monthly',
    24: 'Semimonthly',
    26: 'Biweekly',
    52: 'Weekly'
  };

  return {
    create_payment_mode: function(options) {
      return new PaymentMode(options);
    },

    create_payment_mode_by_frequency: function(frequency) {
      var options = {
        frequency: frequency,
        label: FREQUENCY_LABELS[frequency]
      };
      return new PaymentMode(options);
    },

    select_initial_payment_mode: function(case_data, payment_modes) {
      var initial_payment_mode = null;
      if (case_data && case_data.payment_mode >= 0) {
        // initialize to the correct payment mode
        initial_payment_mode = _.find(payment_modes, function(pm) {
          return pm.frequency === case_data.payment_mode;
        });

        // If we didn't find a match, set it to null.
        if (initial_payment_mode === undefined) {
          initial_payment_mode = null;
        }
      }
      return initial_payment_mode;
    },

    can_change_payment_mode: function(case_data) {
      // -1 is the sentinel for whether the user must select a mode.
      return (case_data.payment_mode === -1 ||
              // If not specified on case, allow user to change
              !case_data.payment_mode);
    }
  }
})();