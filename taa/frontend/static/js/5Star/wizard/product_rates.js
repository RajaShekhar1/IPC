var product_rates_service = (function() {

  // externally visible observable to express when we have finished loading rates.
  var is_loading_rates = ko.observable(false);

  function ProductRatesVM(product, payment_mode) {
    this.product = product;
    this.payment_mode = payment_mode;
    this.rates = ko.observableArray([]);
    this.recommendations = ko.observableArray([]);

    // Rates for each applicant type are derived from the rates observable.
    this.employee_options = ko.pureComputed(this._get_employee_options, this);
    this.spouse_options = ko.pureComputed(this._get_spouse_options, this);
    this.children_options = ko.pureComputed(this._get_children_options, this);

    // Put the observables in a dictionary for easy lookup by applicant type.
    this.applicant_coverage_options = {};
    this.applicant_coverage_options[wizard_applicant.Applicant.EmployeeType] = this.employee_options;
    this.applicant_coverage_options[wizard_applicant.Applicant.SpouseType] = this.spouse_options;
    this.applicant_coverage_options[wizard_applicant.Applicant.ChildType] = this.children_options;
  }
  ProductRatesVM.prototype = {
    update_rates_data: function(rates_data) {
      this.update_product_rates(rates_data);
      this.update_recommendations(rates_data);
      // TODO: init rider rates
    },

    update_product_rates: function(rates_data) {
      this.rates(this.get_coverage_options_from_api_data(rates_data));
    },

    get_coverage_options_from_api_data: function(rates_data) {
      var rates = [];
      rates = $.merge(rates, this.create_coverage_options(wizard_applicant.Applicant.EmployeeType, rates_data.employee_rates));
      rates = $.merge(rates, this.create_coverage_options(wizard_applicant.Applicant.SpouseType, rates_data.spouse_rates));
      rates = $.merge(rates, this.create_coverage_options(wizard_applicant.Applicant.ChildType, rates_data.children_rates));
      return rates;
    },

    create_coverage_options: function(applicant_type, applicant_rates_data) {
      var options = [];
      if (applicant_rates_data.bypremium) {
        options = $.merge(options, this.create_coverage_options_by_face(false, applicant_type, applicant_rates_data.bypremium));
      }
      if (applicant_rates_data.byface) {
        options = $.merge(options, this.create_coverage_options_by_face(true, applicant_type, applicant_rates_data.byface));
      }
      return options;
    },

    create_coverage_options_by_face: function(is_by_face, applicant_type, options) {
      return _.map(options, function(data) {
        return new CoverageOption({
          applicant_type: applicant_type,
          is_by_face: is_by_face,
          face_value: data.coverage,
          premium: data.premium,
          payment_mode: this.payment_mode
        });
      }, this);
    },

    update_recommendations: function(rates_data) {
      if (rates_data.recommendations) {
        this.recommendations(this.process_recommendations(rates_data.recommendations));
      }
    },

    process_recommendations: function(recommendations) {
      var processed_recommendations = [];
      _.each(recommendations, function(rec_data) {
        // Find the coverage option objects that match the recommendations for each applicant type
        var emp_option = this.find_applicant_coverage_option(wizard_applicant.Applicant.EmployeeType, rec_data.coverages.employee);
        var sp_option = this.find_applicant_coverage_option(wizard_applicant.Applicant.SpouseType, rec_data.coverages.spouse);
        var ch_option = this.find_applicant_coverage_option(wizard_applicant.Applicant.ChildType, rec_data.coverages.children);

        // Package each recommendation into a RecommendationSet
        processed_recommendations.push(new RecommendationSet(
            rec_data.name,
            [
              new Recommendation(rec_data.name, wizard_applicant.Applicant.EmployeeType, emp_option),
              new Recommendation(rec_data.name, wizard_applicant.Applicant.SpouseType, sp_option),
              new Recommendation(rec_data.name, wizard_applicant.Applicant.ChildType, ch_option)
            ]
        ));
      }, this);
      return processed_recommendations;
    },

    find_applicant_coverage_option: function(applicant_type, coverage_amount) {
      if (!coverage_amount) {
        return new NullCoverageOption();
      }
      var option = _.find(this.applicant_coverage_options[applicant_type](), function(opt) {
        return opt.face_value === parseInt(coverage_amount);
      });
      if (option) {
        return option;
      } else {
        return new NullCoverageOption();
      }
    },

    get_coverage_options_observable_for_applicant: function(applicant) {
      return this.applicant_coverage_options[applicant.type];
    },

    _get_employee_options: function() {
      return this._get_options_by_applicant_type(wizard_applicant.Applicant.EmployeeType);
    },
    _get_spouse_options: function() {
      return this._get_options_by_applicant_type(wizard_applicant.Applicant.SpouseType);
    },
    _get_children_options: function() {
      return this._get_options_by_applicant_type(wizard_applicant.Applicant.ChildType);
    },

    _get_options_by_applicant_type: function(applicant_type) {
      return _.select(this.rates(), function(option) {
        return option.applicant_type === applicant_type;
      })
    }

  };


  var rates_by_product_id = {};

  function update_product_rates(products, payment_mode, applicant_list, error_callback, statecode) {

    // Signal we have started updating rates data.
    is_loading_rates(true);

    var requests = _.map(products, function(product) {
      var data = _build_rate_parameters(payment_mode, applicant_list, statecode);
      return remote_service.get_product_rates(product.product_data.id, data);
    });

    function process_product_rates() {
      for (var i = 0; i < arguments.length; i++) {
        // each product sends a separate rates request. We pull it out here.
        var product_rate_response = arguments[i][0];
        var product_rates = product_rate_response.data;

        // Find the matching product, call set_rates
        var product = _.find(products, function (p) {
          return p.product_data.id === product_rates.product_id
        }, this);

        var product_rates_viewmodel = get_or_create_product_rates_viewmodel(product, payment_mode);
        product_rates_viewmodel.update_rates_data(product_rates);
      }

      // Signal that we have updated the rates data.
      is_loading_rates(false);
    }

    $.when.apply($, requests).done(process_product_rates).fail(error_callback);
  }

  function _build_rate_parameters(payment_mode, applicant_list, statecode) {
    var params = {
      payment_mode: payment_mode.frequency,
      statecode: statecode
    };
    return $.extend({}, params, _build_applicant_parameters(applicant_list));
  }

  function _build_applicant_parameters(applicant_list) {
    return {
      employee: applicant_list.get_employee().serialize_data(),
      spouse: (applicant_list.has_valid_spouse())? applicant_list.get_spouse().serialize_data() : null
    };
  }

  function get_or_create_product_rates_viewmodel(product, payment_mode) {
    if (!(product.product_data.id in rates_by_product_id)) {
      rates_by_product_id[product.product_data.id] = new ProductRatesVM(product, payment_mode);
    }
    return rates_by_product_id[product.product_data.id];
  }


  function get_product_recommendations(product, payment_mode) {
    var rates = get_or_create_product_rates_viewmodel(product, payment_mode);
    return rates.recommendations;
  }

  function get_product_coverage_options_for_applicant(product, applicant) {
    var rates = get_or_create_product_rates_viewmodel(product);
    return rates.get_coverage_options_observable_for_applicant(applicant);
  }


  return {
    update_product_rates: update_product_rates,
    is_loading_rates: is_loading_rates,

    // Returns observable array that yields CoverageOption instances.
    get_product_coverage_options_for_applicant: get_product_coverage_options_for_applicant,
    get_product_recommendations: get_product_recommendations,
    get_or_create_product_rates_viewmodel: get_or_create_product_rates_viewmodel
  };
})();