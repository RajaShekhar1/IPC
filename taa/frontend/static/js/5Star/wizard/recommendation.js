
function RecommendationSet(name, recommendations) {
  var self = this;
  self.name = name;
  self.recommendations = recommendations;

  self.get_applicant_recommendation = function(applicant_type) {
    return _.find(self.recommendations, function(rec) {return rec.applicant_type === applicant_type;})
  };

  self.get_recommended_applicant_coverage = function(applicant_type) {
    var rec = self.get_applicant_recommendation(applicant_type);
    if (!rec) {
      return new NullCoverageOption();
    }
    return rec.recommended_coverage;
  };

  self.formatted_payment_mode = function() {
    if (self.recommendations.length === 0) {
      return "";
    }
    if (!_.any(_.invoke(self.recommendations, "is_valid"))) {
      return "";
    }
    var valid_recommendation = _.find(self.recommendations, function(rec) {return rec.is_valid()});
    return valid_recommendation.recommended_coverage.payment_mode().display_lowercase();
  };

  self.formatted_total_premium = function() {
    return format_premium_value(self.get_total_premium());
  };

  self.get_total_premium = function() {
    var total = 0.0;
    _.each(self.recommendations, function(rec) {
      total += rec.get_total_premium();
    });
    return total;
  };
}


function Recommendation(name, applicant_type, coverage_option, product) {
  var self = this;
  self.name = name;
  self.applicant_type = applicant_type;
  self.recommended_coverage = coverage_option;
  self.product = product;

  self.is_valid = function() {
    return self.recommended_coverage.is_valid();
  };

  self.get_total_premium = function() {
    // FPP products multiply by # children.
    if (self.product.is_fpp_product() && self.applicant_type === wizard_applicant.Applicant.ChildType) {
      return self.recommended_coverage.premium * window.vm.coverage_vm.applicants.get_valid_children().length;
    }

    return self.recommended_coverage.premium;
  };

  self.format_total_premium = function() {
    if (!self.recommended_coverage.is_valid()) {
      return "";
    }
    return format_premium_value(self.get_total_premium());
  };

  self.format_premium_option = function() {

    return self.recommended_coverage.format_premium_option()
  };
  self.format_coverage = function() {
    return self.recommended_coverage.format_face_value();
  };
}

function NullRecommendation(name) {
  var self = this;
  self.name = name;
  self.recommended_coverage = new NullCoverageOption();
  self.is_valid = function() {return false;};
  self.get_total_premium = function() {
    return 0.0;
  };
  self.format_premium_option = function() {
    return "";
  };
  self.format_total_premium = function() {
    return "";
  };
  self.format_coverage = function() {
    return "";
  };
}