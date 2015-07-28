
function Recommendation(recommended_benefit) {
  var self = this;
  self.recommended_benefit = recommended_benefit;

  self.is_valid = function() {
    return true;
  };

  self.format_premium_option = function() {
    return self.recommended_benefit.format_premium_option()
  };
  self.format_face_value = function() {
    return self.recommended_benefit.format_face_value();
  };
}

function NullRecommendation(option) {
  var self = this;
  self.recommended_benefit = option || new NullBenefitOption();
  self.is_valid = function() {return false;};

}