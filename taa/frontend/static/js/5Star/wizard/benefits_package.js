function BenefitsPackage(root, name) {
  var self = this;
  self.root = root;
  self.name = ko.observable(name);

  self.employee_recommendation = ko.observable(new NullRecommendation());
  self.spouse_recommendation = ko.observable(new NullRecommendation());
  self.children_recommendation = ko.observable(new NullRecommendation());

  self.set_recommendations = function (recommendations) {

    if (root.employee().is_valid()) {
      self.employee_recommendation(self.build_recommendation(self.root.employee(), recommendations['employee']));
    } else {
      self.employee_recommendation(new NullRecommendation());
    }

    if (root.should_include_spouse_in_table()) {
      self.spouse_recommendation(self.build_recommendation(self.root.spouse(), recommendations['spouse']));
    } else {
      self.spouse_recommendation(new NullRecommendation());
    }

    if (root.should_include_children_in_table()) {
      self.children_recommendation(self.build_recommendation(self.root.child_benefits(), recommendations['children']));
    } else {
      self.children_recommendation(new NullRecommendation());
    }
  };

  self.build_recommendation = function (applicant, recommended_val) {
    var benefit = self.get_recommended_benefit(applicant, recommended_val);
    return new Recommendation(benefit);
  };

  self.get_recommended_benefit = function (applicant, recommended_val) {
    if (recommended_val == null || recommended_val == "") {
      return new NullCoverageOption();
    } else {
      var applicant_type;
      if (applicant == root.employee()) {
        applicant_type = "employee";
      } else if (applicant == root.spouse()) {
        applicant_type = "spouse";
      } else {
        applicant_type = "children";
      }

      return root.insurance_product.find_recommended_coverage_benefit(applicant_type, recommended_val);
    }
  };

  self.get_package_benefits = function () {
    var benefits = [
      self.employee_recommendation().recommended_benefit,
      self.spouse_recommendation().recommended_benefit
    ];
    // For each child, push our child benefit recommendation
    if (self.children_recommendation().recommended_benefit) {
      for (var i = 0; i < self.root.get_valid_children().length; i++) {
        benefits.push(self.children_recommendation().recommended_benefit);
      }
    }
    return benefits;
  };

  self.get_total_riders = ko.computed(function () {
    total_rider_amount = 0;
    if (window.ui) {
      var riders = window.ui.get_selected_riders();
      for (var i = 0; i < riders.length; i++) {
        total_rider_amount += window.ui.riders()['emp'][riders[i].code];
      }
    }

    return total_rider_amount;
  });


  self.get_total_premium = ko.computed(function () {
    var benefits = self.get_package_benefits();

    // Sum the benefit premiums
    var total = 0.0;
    $.each(benefits, function () {
      if (this.premium != null) {
        total += this.premium;
      }
    });

    // If total rider premium is greater than zero, add it
    if (self.get_total_riders() > 0.0) {
      total += self.get_total_riders();
    }
    return total;
  });

  self.formatted_total_premium = ko.computed(function () {
    if (self.get_total_premium() > 0.0) {
      return format_premium_value(self.get_total_premium());
    } else {
      return "";
    }
  });

  self.is_valid = function () {
    return true
  };


  self.did_select_employee_coverage = ko.computed(function () {
    return self.employee_recommendation().recommended_benefit.is_valid();
  });

  self.did_select_spouse_coverage = ko.computed(function () {
    return self.spouse_recommendation().recommended_benefit.is_valid();
  });

  self.did_select_children_coverage = ko.computed(function () {
    return self.children_recommendation().recommended_benefit.is_valid();
  });


  self.get_all_people = ko.computed(function () {
    var employee = root.employee();


    var people = [employee];
    if (root.should_include_spouse_in_table()) {
      var spouse = root.spouse();
      people.push(spouse);
    }

    if (root.should_include_children_in_table()) {
      $.each(root.get_valid_children(), function () {
        var child = this;
        people.push(child);
      });
    }
    return people;
  });

  self.get_all_covered_people = ko.computed(function () {
    var people = [];
    //
    //if (root.did_select_employee_coverage()) {
    //  var employee = root.employee();
    //  people.push(employee);
    //}
    //
    //if (root.did_select_spouse_coverage()) {
    //  var spouse = root.spouse();
    //  people.push(spouse);
    //}
    //
    //if (root.did_select_children_coverage()) {
    //  $.each(root.get_valid_children(), function () {
    //    var child = this;
    //    people.push(child);
    //  });
    //}
    return people;
  });

  self.get_all_people_labels = ko.computed(function () {
    var labels = [root.employee().name()];
    if (root.should_include_spouse_in_table()) {
      labels.push(root.spouse().name());
    }
    if (root.should_include_children_in_table()) {
      $.each(root.get_valid_children(), function () {
        var child = this;
        labels.push(child.name());
      });
    }
    return labels;
  });

  self.get_all_covered_people_labels = ko.computed(function () {
    var labels = [];
    //if (root.did_select_employee_coverage()) {
    //  labels.push(root.employee().name());
    //}
    //if (root.did_select_spouse_coverage()) {
    //  labels.push(root.spouse().name());
    //}
    //if (root.did_select_children_coverage()) {
    //  $.each(root.get_valid_children(), function () {
    //    var child = this;
    //    labels.push(child.name());
    //  });
    //}
    return labels;
  });

  self.get_covered_applicants_with_type = ko.computed(function () {
    var applicants = [];
    if (root.did_select_employee_coverage()) {

      applicants.push({
        applicant: root.employee(),
        type: "Employee",
        coverage: self.employee_recommendation().recommended_benefit
      });
    }
    if (root.did_select_spouse_coverage()) {
      applicants.push({
        applicant: root.spouse(),
        type: "Spouse",
        coverage: self.spouse_recommendation().recommended_benefit
      });
    }
    if (root.did_select_children_coverage()) {
      $.each(root.get_valid_children(), function () {
        var child = this;
        applicants.push({
          applicant: child,
          type: "Child",
          coverage: self.children_recommendation().recommended_benefit
        });
      });
    }
    return applicants;
  });

  self.get_covered_children = ko.computed(function () {
    return _.map(_.filter(self.get_covered_applicants_with_type(), function (d) {
      return d.type == "Child";
    }), function (d) {
      return d.applicant;
    });
  });

  self.get_people_with_labels = ko.computed(function () {
    var people = self.get_all_people();
    var labels = self.get_all_people_labels();

    var out = [];
    for (var i = 0; i < people.length; i++) {
      out.push({person: people[i], label: labels[i]});
    }
    return out;
  });

  self.has_at_least_one_benefit_selected = function () {
    var any_benefits = false;
    $.each(self.get_package_benefits(), function () {
      if (this.is_valid()) {
        any_benefits = true;
        return false;
      }
    });
    return any_benefits;
  };
}

function NullBenefitsPackage(root) {
  var self = this;

  self.root = root;
  self.employee_recommendation = ko.observable(new NullRecommendation());
  self.spouse_recommendation = ko.observable(new NullRecommendation());
  self.children_recommendation = ko.observable(new NullRecommendation());
  self.get_total_premium = function () {
    return null;
  };
  self.formatted_total_premium = function () {
    return "";
  };
  self.is_valid = function () {
    return false
  };
  self.get_all_people = function () {
    return [];
  };
  self.get_all_covered_people = function () {
    return [];
  };
  self.get_covered_applicants_with_type = function () {
    return [];
  };
  self.get_all_people_labels = function () {
    return [];
  };
  self.get_all_covered_people_labels = function () {
    return [];
  };
  self.get_people_with_labels = function () {
    return [];
  };
  self.has_at_least_one_benefit_selected = function () {
    return false;
  };

  self.did_select_employee_coverage = function () {
    return false;
  };
  self.did_select_spouse_coverage = function () {
    return false;
  };
  self.did_select_children_coverage = function () {
    return false;
  };
  self.get_covered_children = function () {
    return [];
  }
}