var health_questions = (function() {
  // View model for step 2
  function ProductHealthQuestions(product_coverage, spouse_questions, all_health_questions) {
    var self = this;
    self.product_coverage = product_coverage;

    // SOH Questions (depends on product)
      // TODO: Double check that this need to be computed since the ProductHealthQuestions object is instantiated once for each product.
    self.health_questions = ko.pureComputed(function() {
      var questions = health_questions.process_spouse_question_data(spouse_questions, self.product_coverage);
      var soh_questions = health_questions.process_health_question_data(all_health_questions, self.product_coverage);
      $.merge(questions, soh_questions);
      return questions;
    });

    self.health_button_rows = ko.pureComputed(function() {
      return _.map(self.health_questions(), function(question) {
        return new health_question_buttons.HealthButtonRow(question, self.product_coverage, self);
      });
    });

    self.do_any_health_questions_need_answering = function() {
      if (self.health_questions().length == 0) {
        return false;
      }
      return _.any(self.health_questions(), function(question) {
        return question.does_any_applicant_need_to_answer();
      });
    };

    self._health_question_responses = [];

    self.get_applicant_answer_for_question = function(applicant, question) {

      var found_response = _.find(self._health_question_responses, function(response) {
        return response.question === question && response.applicant === applicant;
      });
      if (found_response === undefined) {
        var resp = new HealthQuestionResponse(question, applicant, null);
        self._health_question_responses.push(resp);
        return resp;
      } else {
        return found_response;
      }
    };

    self.serialize_answers_for_applicant = function(applicant) {
      return _.map(self.health_questions(), function(question) {
        var response = self.get_applicant_answer_for_question(applicant, question);
        return response.serialize();
      });
    }

  }

  function HealthQuestionResponse(question, applicant, initial_response) {
    var self = this;
    self.question = question;
    self.applicant = applicant;
    self.value = ko.observable(initial_response);

    self.serialize = function() {

      var answer = null;
      if (self.question.does_applicant_need_to_answer(self.applicant)) {
        answer = self.value();
      } else if (self.question.can_applicant_skip_due_to_GI(self.applicant)) {
        answer = 'GI';
      }

      return {
        question: self.question.get_question_text(),
        label: self.question.get_question_label(),
        is_spouse_only: self.question.is_spouse_only(),
        answer: answer
      }
    };
  }


  // Create the spouse-specific HealthQuestion objects from the raw data provided by the server.
  function process_spouse_question_data(question_data_by_product, product_coverage) {
    // Build up the master list of questions for this product
    var questions = [];
    var product_data = product_coverage.product.product_data;

    _.each(question_data_by_product, function (product_questions, product_id) {
      if (product_id == product_data.id) {
        var question_factory;
        if (product_coverage.product.product_data.is_guaranteed_issue) {
          question_factory = function(question_data) {
            return new GIHealthQuestion(product_coverage.product, question_data, product_coverage,
                product_data.gi_criteria,
                product_data.statement_of_health_bypass_type, product_data.bypassed_soh_questions);
          };
        } else {
          question_factory = function (question_data) {
            return new StandardHealthQuestion(question_data, product_coverage);
          }
        }

        questions = _.map(product_questions, question_factory);
      }
    });
    return questions;
  }

  // Create the general Statement of Health HealthQuestion objects from the raw data provided by the server.
  function process_health_question_data(health_question_data_by_product, product_coverage) {
    // Build up the master list of health questions for the product
    var questions = [];
    var product_data = product_coverage.product.product_data;

    _.each(health_question_data_by_product, function (product_health_questions, product_id) {
      if (product_id == product_data.id) {
        var question_factory;
        if (product_data.is_guaranteed_issue) {
          question_factory = function (question_data) {
            return new GIHealthQuestion(product_coverage.product, question_data, product_coverage,
                product_data.gi_criteria,
                product_data.statement_of_health_bypass_type, product_data.bypassed_soh_questions);
          };
        } else if (product_coverage.product.product_type == "Group CI") {
          question_factory = function (question_data) {
            return new GIHealthQuestion(product_coverage.product, question_data, product_coverage,

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
            return new StandardHealthQuestion(question_data, product_coverage);
          }
        }

        questions = _.map(product_health_questions, question_factory);
      }
    });
    return questions;
  }


  return {
    ProductHealthQuestions: ProductHealthQuestions,
    HealthQuestionResponse: HealthQuestionResponse,
    process_health_question_data: process_health_question_data,
    process_spouse_question_data: process_spouse_question_data
  };
})();


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


var StandardHealthQuestion = function(question, product_coverage) {
  // A viewmodel that keeps track of which applicants need to answer which health questions
  var self = this;

  self.product_coverage = product_coverage;

  // Simple object with .question_text and .label
  self.question = question;

  self.does_employee_need_to_answer = ko.computed(function() {
    if (self.question.is_spouse_only) {
      return false;
    }
    return self.product_coverage.did_select_employee_coverage();
  });
  self.does_spouse_need_to_answer = ko.computed(function() {
    return self.product_coverage.did_select_spouse_coverage();
  });

  self.does_child_need_to_answer = function(child) {
    if (self.question.is_spouse_only) {
      return false;
    }
    return self.product_coverage.did_select_children_coverage();
  };

  // Non-GI questions can't be skipped.
  self.can_employee_skip_due_to_GI = function() {return false};
  self.can_spouse_skip_due_to_GI = function() {return false};
  self.can_child_skip_due_to_GI = function(child) {return false};

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

  self.does_any_applicant_need_to_answer = ko.pureComputed(function() {

    return _.any(self.product_coverage.applicant_coverage_selections(), function (app_cov) {
      return self.does_applicant_need_to_answer(app_cov.applicant);
    });
  });
};

StandardHealthQuestion.prototype.is_spouse_only = function() {
  return this.question.is_spouse_only;
};

StandardHealthQuestion.prototype.get_question_text = function() {
  return this.question.question_text;
};
StandardHealthQuestion.prototype.get_question_label = function() {
  return this.question.label;
};

StandardHealthQuestion.prototype.does_applicant_need_to_answer = function(applicant) {
  if (applicant.type === wizard_applicant.Applicant.EmployeeType) {
    return this.does_employee_need_to_answer();
  }
  if (applicant.type === wizard_applicant.Applicant.SpouseType) {
    return this.does_spouse_need_to_answer();
  }
  if (applicant.type === wizard_applicant.Applicant.ChildType) {
    return this.does_child_need_to_answer(applicant);
  }

  return false;
};

StandardHealthQuestion.prototype.can_applicant_skip_due_to_GI = function(applicant) {
  if (applicant.type === wizard_applicant.Applicant.EmployeeType) {
    return this.can_employee_skip_due_to_GI();
  } else if (applicant.type === wizard_applicant.Applicant.SpouseType) {
    return this.can_spouse_skip_due_to_GI();
  } else if (applicant.type === wizard_applicant.Applicant.ChildType) {
    return this.can_child_skip_due_to_GI(applicant);
  } else {
    console.error("Invalid applicant type "+applicant.type, applicant);
  }
};

StandardHealthQuestion.prototype.get_yes_highlight = function() {
  return (this.does_yes_stop_app()) ? 'stop' : 'checkmark';
};
StandardHealthQuestion.prototype.does_yes_stop_app = function() {
  return !this.question.is_ignored;
};


var GIHealthQuestion = function(product, question, product_coverage, applicant_criteria, skip_mode, skipped_questions) {
  var self = this;
  self.product = product;
  self.product_coverage = product_coverage;
  self.question = question;
  self.applicant_criteria = applicant_criteria;
  self.skip_mode = skip_mode;
  self.skipped_questions = skipped_questions;


  self.get_criteria = function(applicant_type) {
    if (applicant_type == "Children") {
      applicant_type = "Child";
    }
    return _.filter(self.applicant_criteria, function(c) {return c.applicant_type == applicant_type;});
  };

  self.get_met_gi_criteria_for_employee = ko.computed(function() {
    if (!self.product_coverage.did_select_employee_coverage()) {
      return undefined;
    }

    var applicant = self.product_coverage.applicant_list.get_employee();
    var coverage = self.product_coverage.__get_coverage_for_applicant(applicant).coverage_option();
    var employee_criteria = self.get_criteria("Employee");
    return _.find(employee_criteria, function(criterion) {
      return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
    });
  });

  self.has_employee_met_GI_criteria = ko.computed(function() {
    return self.get_met_gi_criteria_for_employee() !== undefined;
  });

  self.get_met_gi_criteria_for_spouse = ko.computed(function() {
    if (!self.product_coverage.did_select_spouse_coverage()) {
      return undefined;
    }
    var applicant = self.product_coverage.applicant_list.get_spouse();
    var coverage = self.product_coverage.__get_coverage_for_applicant(applicant).coverage_option();
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
    if (!self.product_coverage.did_select_children_coverage()) {
      return undefined;
    }
    var criteria = self.get_criteria("Child");
    var children = self.product_coverage.applicant_list.get_children_group();
    var coverage = self.product_coverage.__get_coverage_for_applicant(children).coverage_option();
    return _.find(criteria, function(criterion) {
      return self.does_applicant_meet_GI_criteria(child_applicant, coverage, criterion);
    });
  };


  self.has_child_met_GI_criteria = function(child_applicant) {
    return self.get_met_gi_criteria_for_child(child_applicant) !== undefined;
  };


  self.show_yes_dialogue_employee = function() {
    self.show_yes_dialogue('Employee', self.product_coverage.applicant_list.get_employee());
  };

  self.show_yes_dialogue_spouse = function() {
    self.show_yes_dialogue('Spouse', self.product_coverage.applicant_list.get_spouse());
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

  self._clean_criteria_applicant_type = function(applicant_type) {
    // The database applicant types from the server don't match client enum. Fix that here as a band-aid.
    if (applicant_type === 'employee') {
      return 'Employee';
    } else if (applicant_type === 'spouse') {
      return 'Spouse';
    } else if (applicant_type === 'child' || applicant_type === 'children') {
      return 'Children';
    } else {
      return applicant_type;
    }
  };

  self.show_yes_dialogue = function(applicant_type, applicant) {
    // Cleanup applicant type so that it matches what the DB has stored on the server.
    applicant_type = self._clean_criteria_applicant_type(applicant_type);

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
    var reduced_gi_criterion;
    var criteria = self.get_criteria(applicant_type);
    var coverage = self.product_coverage.__get_coverage_for_applicant(applicant).coverage_option();

    // Does the applicant meet any of the demographic GI criteria?
    if (applicant_type == "Employee") {
      reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
    } else if (applicant_type == "Spouse") {
      reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
    } else if (applicant_type == "Children") {
      reduced_gi_criterion = self.get_best_GI_criteria(applicant, coverage, criteria);
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
        var applicant_coverage_options = self.product_coverage.get_coverage_options_for_applicant(applicant);

        // Find the option that gives the most coverage but is below the GI threshold.
        var max_option = _.max(
            _.filter(applicant_coverage_options, function(o) {
              return o.face_value <= gi_amount && o.face_value > 0
            }),
            function(o) {return o.face_value}
        );

        // Select this coverage.
        self.product_coverage.__get_coverage_for_applicant(applicant).customized_coverage_option(max_option);
      }},
      remove: {label: "Remove this applicant", className: 'btn-danger', callback: function() {
        var applicant_coverage_options = self.product_coverage.get_coverage_options_for_applicant(applicant);

        // If child, we remove only the selected child, not all child coverage.
        if (applicant_type == "Children") {
          self.product_coverage.applicant_list.remove_applicant(applicant);
        } else {
          var null_option = _.find(applicant_coverage_options, function(o) {
            return o.face_value == 0
          });

          self.product_coverage.__get_coverage_for_applicant(applicant).customized_coverage_option(null_option);
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

  self.does_employee_need_to_answer = ko.computed(function() {
    if (!self.product_coverage.did_select_employee_coverage()) {
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
    if (!self.product_coverage.did_select_spouse_coverage()) {
      return false;
    }
    if (self.can_spouse_skip_due_to_GI()) {
      return false;
    }

    return true;
  });

  self.can_child_skip_due_to_GI = function(child_applicant) {
    return self.has_child_met_GI_criteria(child_applicant) && self.should_skip_if_GI_criteria_met();
  };

  self.does_child_need_to_answer = function(child_applicant) {
    if (!self.product_coverage.did_select_children_coverage()) {
      return false;
    }

    if (self.can_child_skip_due_to_GI(child_applicant)) {
      return false;
    }

    if (self.question.is_spouse_only) {
      return false;
    }

    return true;
  };

  self.does_any_applicant_need_to_answer = ko.computed(function() {
    return _.any(self.product_coverage.applicant_coverage_selections(), function(app_cov) {
      return self.does_applicant_need_to_answer(app_cov.applicant);
    });
  });



};
GIHealthQuestion.prototype = Object.create(StandardHealthQuestion.prototype);


GIHealthQuestion.prototype.does_applicant_meet_GI_criteria = function(applicant, coverage, criterion) {
  return (
      this.does_applicant_meet_coverage_GI_criteria(applicant, coverage, criterion) &&
      this.does_applicant_meet_demographic_GI_criteria(applicant, criterion)
  );
};
GIHealthQuestion.prototype.does_applicant_meet_coverage_GI_criteria = function(applicant, coverage, criterion) {
  return coverage.face_value <= criterion.guarantee_issue_amount;
};
GIHealthQuestion.prototype.does_applicant_meet_demographic_GI_criteria = function(applicant, criterion) {
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
