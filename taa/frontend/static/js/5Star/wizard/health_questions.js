


function process_health_question_data(root, health_question_data_by_product, product_data) {
  // Build up the master list of health questions for the product
  var questions = [];

  _.each(health_question_data_by_product, function (product_health_questions, product_id) {
    if (product_id == product_data.id) {
      var question_factory;
      if (product_data.is_guaranteed_issue) {
        question_factory = function (question_data) {
          return new GIHealthQuestion(root.insurance_product, question_data, root.selected_plan,
              product_data.gi_criteria,
              product_data.statement_of_health_bypass_type, product_data.bypassed_soh_questions);
        };
      } else if (root.insurance_product.product_type == "Group CI") {
        question_factory = function (question_data) {
          return new GIHealthQuestion(root.insurance_product, question_data, root.selected_plan,

              [
                // Employees must answer if >= 10000 coverage
                {
                  guarantee_issue_amount: 10000,
                  applicant_type: 'Employee',
                  age_max: null,
                  age_min: null,
                  weight_min: null,
                  weight_max: null,
                  height_min: null,
                  height_max: null
                },
                // Spouse always has to answer, so don't put criteria in for spouse.
                {
                  // Children never have to answer, make the max GI amount bigger to get this effect.
                  guarantee_issue_amount: 25000,
                  applicant_type: 'Child',
                  age_max: null,
                  age_min: null,
                  weight_min: null,
                  weight_max: null,
                  height_min: null,
                  height_max: null
                }
              ],
              // Skip over these questions (all but first two )
              "selected",
              [
                {question_type_label: "5yr Heart"},
                {question_type_label: "5yr Hypertension / Cholesterol"},
                {question_type_label: "5yr Lung / Colon"},
                {question_type_label: "5yr Skin Cancer"},
                {question_type_label: "5yr HPV/HSV"},
                {question_type_label: "Abnormal Results"},
                {question_type_label: "Ever been rejected"}
              ]
          );
        }
      } else {
        question_factory = function (question_data) {
          return new StandardHealthQuestion(question_data, root.selected_plan);
        }
      }

      questions = _.map(product_health_questions, question_factory);
    }
  });
  return questions;
}

function process_spouse_question_data(root, question_data_by_product, product_data) {
  // Build up the master list of questions for the product
  var questions = [];

  _.each(question_data_by_product, function (product_questions, product_id) {
    if (product_id == product_data.id) {
      var question_factory;
      if (product_data.is_guaranteed_issue) {
        question_factory = function (question_data) {
          return new GIHealthQuestion(root.insurance_product, question_data, root.selected_plan,
              product_data.gi_criteria,
              product_data.statement_of_health_bypass_type, product_data.bypassed_soh_questions);
        };
      } else {
        question_factory = function (question_data) {
          return new StandardHealthQuestion(question_data, root.selected_plan);
        }
      }

      questions = _.map(product_questions, question_factory);
    }
  });
  return questions;
}


var GlobalSOHQuestion = function(question_text) {
  var self = this;
  self.question_text = question_text;
};
GlobalSOHQuestion.prototype.get_question_text = function() {
  return this.question_text;
};


var NonHealthQuestion = function(question_text) {
  var self = this;
  self.question_text = question_text;
};
NonHealthQuestion.prototype.get_question_text = function() {
  return this.question_text;
};


var StandardHealthQuestion = function(question, selected_plan) {
  // A viewmodel that keeps track of which applicants need to answer which health questions
  var self = this;

  // should be an observableArray of Applicant objects
  self.selected_plan = selected_plan;

  // Simple object with .question_text and .label
  self.question = question;

  self.does_employee_need_to_answer = ko.computed(function() {
    if (self.question.is_spouse_only) {
      return false;
    }
    return self.selected_plan().did_select_employee_coverage();
  });
  self.does_spouse_need_to_answer = ko.computed(function() {
    return self.selected_plan().did_select_spouse_coverage();
  });

  self.does_child_need_to_answer = function(child) {
    if (self.question.is_spouse_only) {
      return false;
    }
    return self.selected_plan().did_select_children_coverage();
  };

  self.can_employee_skip_due_to_GI = function() {return false};
  self.can_spouse_skip_due_to_GI = function() {return false};

  self.show_yes_dialogue_employee = function() {return self.show_yes_dialogue(); };
  self.show_yes_dialogue_spouse = function() {return self.show_yes_dialogue(); };
  self.show_yes_dialogue_child = function(child) {return self.show_yes_dialogue(); };


  self.show_yes_dialogue = function() {
    if (self.does_yes_stop_app()) {
      handle_question_yes();
    } else {
      // do nothing
    }
  };

  self.does_any_applicant_need_to_answer = ko.computed(function() {

    return _.any(self.selected_plan().get_covered_applicants_with_type(), function(data) {
      return self.does_applicant_need_to_answer(data.type, data.applicant);
    });
  });
};
StandardHealthQuestion.prototype.get_question_text = function() {
  return this.question.question_text;
};
StandardHealthQuestion.prototype.get_question_label = function() {
  return this.question.label;
};

StandardHealthQuestion.prototype.does_applicant_need_to_answer = function(applicant_type, applicant) {
  var self = this;
  if (applicant_type == "Employee") {
    return self.does_employee_need_to_answer();
  } else if (applicant_type == "Spouse") {
    return self.does_spouse_need_to_answer();
  } else if (applicant_type == "Child") {
    return self.does_child_need_to_answer(applicant);
  }
  console.error("Got unknown applicant type '"+applicant_type+"'");
  return true;
};
StandardHealthQuestion.prototype.get_yes_highlight = function() {
  return (this.does_yes_stop_app()) ? 'stop' : 'checkmark';
};
StandardHealthQuestion.prototype.does_yes_stop_app = function() {
  return !this.question.is_ignored;
};


var GIHealthQuestion = function(product, question, selected_plan, applicant_criteria, skip_mode, skipped_questions) {
  var self = this;
  self.product = product;
  self.selected_plan = selected_plan;
  self.question = question;
  self.applicant_criteria = applicant_criteria;
  self.skip_mode = skip_mode;
  self.skipped_questions = skipped_questions;


  self.does_employee_need_to_answer = ko.computed(function() {
    if (!self.selected_plan()) return false;
    if (!self.selected_plan().did_select_employee_coverage()) {
      return false;
    }
    if (self.can_employee_skip_due_to_GI()) {
      return false;
    }
    if (self.question.is_spouse_only) {
      return false;
    }

    return true;
  });



  self.does_spouse_need_to_answer = ko.computed(function() {
    if (!self.selected_plan()) return false;
    if (!self.selected_plan().did_select_spouse_coverage()) {
      return false;
    }
    if (self.can_spouse_skip_due_to_GI()) {
      return false;
    }

    return true;
  });


  self.does_child_need_to_answer = function(child_applicant) {
    if (!self.selected_plan()) return false;
    if (!self.selected_plan().did_select_children_coverage()) {
      return false;
    }

    if (self.has_child_met_GI_criteria(child_applicant) && self.should_skip_if_GI_criteria_met()) {
      return false;
    }

    if (self.question.is_spouse_only) {
      return false;
    }

    return true;
  };

  self.get_met_gi_criteria_for_employee = ko.computed(function() {
    if (!self.selected_plan().did_select_employee_coverage()) {
      return undefined;
    }

    var coverage = self.selected_plan().employee_recommendation().recommended_benefit;
    var applicant = self.selected_plan().root.employee();
    var employee_criteria = self.get_criteria("Employee");
    return _.find(employee_criteria, function(criterion) {
      return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
    });
  });

  self.has_employee_met_GI_criteria = ko.computed(function() {
    return self.get_met_gi_criteria_for_employee() !== undefined;
  });


  self.get_met_gi_criteria_for_spouse = ko.computed(function() {
    if (!self.selected_plan().did_select_spouse_coverage()) {
      return undefined;
    }
    var coverage = self.selected_plan().spouse_recommendation().recommended_benefit;
    var applicant = self.selected_plan().root.spouse();
    var criteria = self.get_criteria("Spouse");
    return _.find(criteria, function(criterion) {
      return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
    });
  });

  self.has_spouse_met_GI_criteria = ko.computed(function() {
    return self.get_met_gi_criteria_for_spouse() !== undefined;
  });

  self.can_employee_skip_due_to_GI = ko.computed(function() {
    return self.has_employee_met_GI_criteria() && self.should_skip_if_GI_criteria_met();
  });

  self.can_spouse_skip_due_to_GI = ko.computed(function() {
    return self.has_spouse_met_GI_criteria() && self.should_skip_if_GI_criteria_met();
  });

  self.get_met_gi_criteria_for_child = function(child_applicant) {
    if (!self.selected_plan().did_select_children_coverage()) {
      return undefined;
    }
    var criteria = self.get_criteria("Child");
    var coverage = self.selected_plan().children_recommendation().recommended_benefit;
    return _.find(criteria, function(criterion) {
      return self.does_applicant_meet_GI_criteria(child_applicant, coverage, criterion);
    });
  };


  self.has_child_met_GI_criteria = function(child_applicant) {
    return self.get_met_gi_criteria_for_child(child_applicant) !== undefined;
  };

  self.does_any_applicant_need_to_answer = ko.computed(function() {
    return _.any(self.selected_plan().get_covered_applicants_with_type(), function(data) {
      return self.does_applicant_need_to_answer(data.type, data.applicant);
    });
  });

  self.show_yes_dialogue_employee = function() {
    self.show_yes_dialogue('Employee', self.selected_plan().root.employee());
  };

  self.show_yes_dialogue_spouse = function() {
    self.show_yes_dialogue('Spouse', self.selected_plan().root.spouse());
  };

  self.show_yes_dialogue_child = function(child) {
    self.show_yes_dialogue('Children', child);
  };

  self.get_best_GI_criteria = function(applicant, coverage, criteria) {
    // sort by gi amount descending then pick the one, if any, that is strictly less than
    //  the selected coverage. Returns undefined if there is no qualifying option.
    var descendingCriteria = _.sortBy(criteria, function(c) {return -c.guarantee_issue_amount;});
    return _.find(descendingCriteria, function(criterion) {
      return (
          self.does_applicant_meet_demographic_GI_criteria(applicant, criterion) &&
          criterion.guarantee_issue_amount < coverage.face_value
      );
    });
  };

  self.show_yes_dialogue = function(applicant_type, applicant) {
    // If we get here, we know the applicant doesn't completely qualify for GI
    //   with the coverage selected, and has answered 'Yes' to a question.
    //   We need to determine if the applicant can bypass this question by
    //   reducing coverage.

    // If this is a required question, we show the normal 'you must answer no' dialogue
    if (!self.should_skip_if_GI_criteria_met()) {
      handle_question_yes();
      return;
    }

    // See if there is a GI condition with a lower coverage amount.
    var criteria = self.get_criteria(applicant_type);
    var coverage, reduced_gi_criterion, applicant_coverage_options, coverage_applicant;
    // Does the applicant meet any of the demographic GI criteria?
    if (applicant_type == "Employee") {
      coverage = self.selected_plan().employee_recommendation().recommended_benefit;
      reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
      applicant_coverage_options = self.product.get_coverage_options_for_applicant('employee');
      coverage_applicant = applicant;
    } else if (applicant_type == "Spouse") {
      coverage = self.selected_plan().spouse_recommendation().recommended_benefit;
      reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
      applicant_coverage_options = self.product.get_coverage_options_for_applicant('spouse');
      coverage_applicant = applicant;
    } else if (applicant_type == "Children") {
      coverage = self.selected_plan().children_recommendation().recommended_benefit;
      reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
      applicant_coverage_options = self.product.get_coverage_options_for_applicant('children');
      // The 'coverage applicant' is a fake applicant representing all child coverage
      coverage_applicant = self.selected_plan().root.child_benefits();
    }

    // If no option was found, present the normal yes dialogue.
    if (!reduced_gi_criterion) {
      handle_question_yes();
      return;
    }

    // Otherwise, show a special dialogue that gives a reduce benefit option for continuing

    var face_amount = coverage.format_face_value();
    var gi_amount = reduced_gi_criterion.guarantee_issue_amount;
    var formatted_gi_amount = format_face_value(gi_amount);

    var button_options = {
      reduce: {label: "Reduce the coverage", className: 'btn-success', callback: function() {
        //
        var max_option = _.max(
            _.filter(applicant_coverage_options(), function(o) {
              return o.face_value <= gi_amount && o.face_value > 0
            }),
            function(o) {return o.face_value}
        );
        coverage_applicant.selected_custom_option(max_option);
        self.selected_plan().root.apply_selected_customization();

      }},
      remove: {label: "Remove this applicant", className: 'btn-danger', callback: function() {
        // If child, we remove only the selected child, not all child coverage
        if (applicant_type == "Children") {
          self.selected_plan().root.children.remove(applicant);

        } else {
          var null_option = _.find(applicant_coverage_options(), function(o) {
            return o.face_value == 0
          });

          coverage_applicant.selected_custom_option(null_option);
          self.selected_plan().root.apply_selected_customization();
        }
      }},
      ignore: {label: "Ignore and Continue", className: 'btn-default', callback: function() {
        // Nothing to do in this case
      }}
    };

    bootbox.dialog({
      message: 'A "yes" response to this question prohibits this person from obtaining the selected '+face_amount+' of coverage. You may proceed, however, by reducing your coverage to the guaranteed coverage amount of '+formatted_gi_amount+'.'+
      '<br><br>Alternatively, you may remove this individual from the coverage selection altogether (in Step 1) before proceeding with the rest of the application.',
      buttons: button_options
    });
  };


  self.get_criteria = function(applicant_type) {
    if (applicant_type == "Children") {
      applicant_type = "Child";
    }
    return _.filter(self.applicant_criteria, function(c) {return c.applicant_type == applicant_type;});
  };

  self.does_applicant_meet_GI_criteria = function(applicant, coverage, criterion) {
    return (
        self.does_applicant_meet_coverage_GI_criteria(applicant, coverage, criterion) &&
        self.does_applicant_meet_demographic_GI_criteria(applicant, criterion)
    );
  };

  self.does_applicant_meet_coverage_GI_criteria = function(applicant, coverage, criterion) {
    return coverage.face_value <= criterion.guarantee_issue_amount;
  };

  self.does_applicant_meet_demographic_GI_criteria = function(applicant, criterion) {
    // Checks all the criteria _except_ for the coverage so we know if the applicant
    //  can reduce his coverage to meet this criterion

    var does_meet = true;

    if (criterion.age_max !== null) {
      does_meet &= applicant.get_age() <= criterion.age_max;
    }
    if (criterion.age_min !== null) {
      does_meet &= applicant.get_age() >= criterion.age_min;
    }
    if (criterion.height_max !== null) {
      does_meet &= applicant.height() !== null && applicant.height() <= criterion.height_max
    }
    if (criterion.height_min !== null) {
      does_meet &= applicant.height() !== null && applicant.height() >= criterion.height_min;
    }
    if (criterion.weight_max !== null) {
      does_meet &= applicant.weight() !== null && applicant.weight() <= criterion.weight_max
    }
    if (criterion.weight_min !== null) {
      does_meet &= applicant.weight() !== null && applicant.weight() >= criterion.weight_min;
    }

    return does_meet;
  };
};
GIHealthQuestion.prototype = Object.create(StandardHealthQuestion.prototype);

GIHealthQuestion.prototype.should_skip_if_GI_criteria_met = function() {
  var self = this;
  if (self.skip_mode == "all") {
    return true;
  } else {
    return _.any(self.skipped_questions, function(q) {
      return self.get_question_label() == q.question_type_label
    });
  }
};
GIHealthQuestion.prototype.does_yes_stop_app = function() {
  var self = this;

  // If GI, clicking YES always stops (but will show the reduce/remove dialogue if optional).
  return true;
};

function HealthQuestionAnswer(question, button_group, question_object) {
  var self = this;

  self.question = question;

  // This has all the additional attributes for the question
  self.question_object = question_object;

  self.button_group = ko.observable(null);


  self.answer = ko.computed(function() {
    if (self.button_group() == null) {
      return null;
    } else {
      return self.button_group().get_val();
    }
  });

  self.serialize = function() {
    // question: text,
    // answer: [Yes|No|GI] (GI means it was skipped due to GI)
    var answer;
    if (self.button_group()) {
      answer = (self.button_group().is_required()) ? self.button_group().get_val() : "GI";
    } else {
      answer = null;
    }

    return {
      question: self.question.question_text,
      label: self.question.label,
      is_spouse_only: self.question.is_spouse_only || false,
      answer: answer
    }
  };
}
