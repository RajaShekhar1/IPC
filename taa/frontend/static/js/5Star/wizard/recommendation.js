
function RecommendationSet(name, recommendations) {
  var self = this;
  self.name = name;
  self.recommendations = recommendations;

  self.get_recommended_applicant_coverage = function(applicant_type) {
    var rec = _.find(self.recommendations, function(rec) {return rec.applicant_type === applicant_type;});
    return rec.recommended_coverage;
  };

  self.formatted_payment_mode = function() {
    if (self.recommendations.length === 0) {
      return "";
    }
    return self.recommendations[0].recommended_coverage.payment_mode().display_lowercase();
  };

  self.formatted_total_premium = function() {
    return format_premium_value(self.get_total_premium());
  };

  self.get_total_premium = function() {
    var total = 0.0;
    _.each(self.recommendations, function(rec) {
      total += rec.recommended_coverage.premium;
    });
    return total;
  };
}


function Recommendation(name, applicant_type, coverage_option) {
  var self = this;
  self.name = name;
  self.applicant_type = applicant_type;
  self.recommended_coverage = coverage_option;

  self.is_valid = function() {
    return true;
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
  self.format_premium_option = function() {
    return "";
  };
  self.format_coverage = function() {
    return "";
  };
}