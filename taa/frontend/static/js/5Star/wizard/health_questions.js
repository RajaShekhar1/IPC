var health_questions = (function () {
  'use strict';
  // View model for step 2
  function ProductHealthQuestions(product_coverage, spouse_questions, all_health_questions, employee_questions, applicant_list, omit_actively_at_work, omit_actively_at_work_observable) {
    var self = this;
    self.product_coverage = product_coverage;
    self.applicant_list = applicant_list;
    self.employee = _.find(applicant_list.applicants(), function (applicant) { return applicant.type === wizard_applicant.Applicant.EmployeeType; });
    self.spouse = _.find(applicant_list.applicants(), function (applicant) { return applicant.type === wizard_applicant.Applicant.SpouseType; });

    // SOH Questions (depends on product)
    // TODO: Double check that this need to be computed since the ProductHealthQuestions object is instantiated once for each product.
    self.health_questions = ko.pureComputed(function () {
      var questions = [];
      var emp_questions = health_questions.process_employee_question_data(employee_questions, self.product_coverage, self.employee, omit_actively_at_work, omit_actively_at_work_observable);
      var sp_questions = health_questions.process_spouse_question_data(spouse_questions, self.product_coverage, self.spouse, omit_actively_at_work, omit_actively_at_work_observable);
      var soh_questions = health_questions.process_health_question_data(all_health_questions, self.product_coverage);
      $.merge(questions, emp_questions);
      $.merge(questions, sp_questions);
      $.merge(questions, soh_questions);
      return questions;
    });

    self.show_yes_dialogue = function (applicant, message) {
      var button_options = {
        remove: {
          label: "Remove this applicant", className: 'btn-danger', callback: function () {
            var applicant_coverage_options = self.product_coverage.get_coverage_options_for_applicant(applicant);

            // If child, we remove only the selected child, not all child coverage.
            if (applicant.type === wizard_applicant.Applicant.ChildType) {
              self.product_coverage.applicant_list.remove_applicant(applicant);
            } else {
              var null_option = _.find(applicant_coverage_options, function (o) {
                return o.face_value === 0;
              });

              self.product_coverage.__get_coverage_for_applicant(applicant).customized_coverage_option(null_option);
            }

            decline_product_if_no_coverage(self.product_coverage);
          }
        },
        ignore: {
          label: "Ignore and Continue", className: 'btn-default', callback: function () {
            // Nothing to do in this case
          }
        }
      };

      var product_name = self.product_coverage.format_product_name();

      // Default the message to the one used for knockout questions
      message = message || "A \"" + self.action_name + "\" response to this question disqualifies this person from obtaining coverage for " +
        "'" + product_name + "'. You may proceed with this application after removing this individual from the " +
        "coverage selection before proceeding.";

      bootbox.dialog({
        message: message,
        buttons: button_options
      });
    };

    self.health_button_rows = ko.pureComputed(function () {
      return _.map(self.health_questions(), function (question) {
        return new health_question_buttons.HealthButtonRow(question, self.product_coverage, self);
      });
    });

    self.do_any_health_questions_need_answering = function () {
      if (self.health_questions().length == 0) {
        return false;
      }

      return _.any(self.health_questions(), function (question) {
        return question.does_any_applicant_need_to_answer();
      });
    };

    self._health_question_responses = [];

    self.get_applicant_answer_for_question = function (applicant, question) {

      var found_response = _.find(self._health_question_responses, function (response) {
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

    self.serialize_answers_for_applicant = function (applicant) {
      return _.map(self.health_questions(), function (question) {
        var response = self.get_applicant_answer_for_question(applicant, question);
        return response.serialize();
      });
    };

  }

  function HealthQuestionResponse(question, applicant, initial_response) {
    var self = this;
    self.question = question;
    self.applicant = applicant;
    self.value = ko.observable(initial_response);

    self.serialize = function () {

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
      };
    };
  }


  // Create the spouse-specific HealthQuestion objects from the raw data provided by the server.
  function process_applicant_question_data(question_data_by_product, product_coverage, applicant, omit_actively_at_work, actively_at_work_observable) {
    // Build up the master list of questions for this product
    var questions = [];
    var product_data = product_coverage.product.product_data;
    var omit_actively_at_work_question = typeof omit_actively_at_work !== 'undefined'? !!omit_actively_at_work : false;

    _.each(question_data_by_product, function (product_questions, product_id) {
      if (product_id == product_data.id) {
        var question_factory;
        if (product_coverage.product.product_data.is_guaranteed_issue) {
          question_factory = function (question_data) {
            if (question_data.label === 'Employee Actively at Work') {
              return null;
            }
            return new GIHealthQuestion(
              product_coverage.product,
              question_data,
              product_coverage,
              product_data.gi_criteria,
              product_data.statement_of_health_bypass_type,
              product_data.bypassed_soh_questions
            );
          };
        } else {
          question_factory = function (question_data) {
            if (question_data.label === 'Employee Actively at Work') {
              return null;
            }
            return new StandardHealthQuestion(question_data, product_coverage);
          }
        }
        questions = _.chain(product_questions)
          .filter(function (question) {
            if (question.label === 'Employee Actively at Work') {
              return !omit_actively_at_work_question;
            }
            return true;
          })
          .map(question_factory)
          .value();
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
                  // Children never have to answer, make the max GI amount bigger than is possible to select to achieve this effect.
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
          };
        } else {
          question_factory = function (question_data) {
            return new StandardHealthQuestion(question_data, product_coverage);
          };
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
    process_spouse_question_data: process_applicant_question_data,
    process_employee_question_data: process_applicant_question_data
  };
})();


var GlobalSOHQuestion = function (question_text) {
  var self = this;
  self.question_text = question_text;
};
GlobalSOHQuestion.prototype.get_question_text = function () {
  return this.question_text;
};


var NonHealthQuestion = function (question_text) {
  var self = this;
  self.question_text = question_text;
};
NonHealthQuestion.prototype.get_question_text = function () {
  return this.question_text;
};


function decline_product_if_no_coverage(product_coverage) {
  'use strict';
  // If we have removed the last applicant for this product, set it as declined.
  if (!product_coverage.did_select_valid_coverage()) {
    if (window.vm.coverage_vm.has_multiple_products()) {
      product_coverage.did_decline(true);
    } else {
      window.vm.did_decline(true);
    }
  }

  // If all products are declined now, go back to step 1.
  if (window.vm.did_decline_all_products()) {
    window.vm.set_wizard_step(1);
  }
}

var HealthQuestions = {
  Responses: {
  }
};

Object.defineProperty(HealthQuestions.Responses, 'Yes', { value: 'yes' });
Object.defineProperty(HealthQuestions.Responses, 'No', { value: 'no' });

var StandardHealthQuestion = function (question, product_coverage) {
  // A viewmodel that keeps track of which applicants need to answer which health questions
  var self = this;

  self.product_coverage = product_coverage;

  // Simple object with .question_text and .label
  self.question = question;

  self.does_employee_need_to_answer = ko.computed(function () {
    if (self.question.is_spouse_only) {
      return false;
    }
    return self.product_coverage.did_select_employee_coverage();
  });
  self.does_spouse_need_to_answer = ko.computed(function () {
    return self.product_coverage.did_select_spouse_coverage();
  });

  self.does_child_need_to_answer = function (child) {
    if (self.question.is_spouse_only) {
      return false;
    }
    return self.product_coverage.did_select_children_coverage();
  };

  // Non-GI questions can't be skipped.
  self.can_employee_skip_due_to_GI = function () {
    return false
  };
  self.can_spouse_skip_due_to_GI = function () {
    return false
  };
  self.can_child_skip_due_to_GI = function (child) {
    return false
  };

  self.show_yes_dialogue_employee = function () {
    return self.show_yes_dialogue();
  };
  self.show_yes_dialogue_spouse = function () {
    return self.show_yes_dialogue();
  };
  self.show_yes_dialogue_child = function (child) {
    return self.show_yes_dialogue();
  };

  self.show_yes_dialogue = function (applicant, message) {
    if (!self.does_question_stop_app()) {
      // No need to show anything, return.
      return;
    }

    // If we get here, the applicant has answered 'Yes' to a question.
    //   We offer a choice to remove the applicant directly from step 2.

    var button_options = {
      remove: {
        label: "Remove this applicant", className: 'btn-danger', callback: function () {
          var applicant_coverage_options = self.product_coverage.get_coverage_options_for_applicant(applicant);

          // If child, we remove only the selected child, not all child coverage.
          if (applicant.type === wizard_applicant.Applicant.ChildType) {
            self.product_coverage.applicant_list.remove_applicant(applicant);
          } else {
            var null_option = _.find(applicant_coverage_options, function (o) {
              return o.face_value === 0;
            });

            self.product_coverage.__get_coverage_for_applicant(applicant).customized_coverage_option(null_option);
          }

          decline_product_if_no_coverage(self.product_coverage);
        }
      },
      ignore: {
        label: "Ignore and Continue", className: 'btn-default', callback: function () {
          // Nothing to do in this case
        }
      }
    };

    var product_name = self.product_coverage.format_product_name();

    // Default the message to the one used for knockout questions
    message = message || "A \"" + self.action_name + "\" response to this question disqualifies this person from obtaining coverage for '" + product_name + "'.";

    bootbox.dialog({
      message: message + " You may proceed with this application after removing this individual from the coverage selection before proceeding.",
      buttons: button_options
    });
  };


  self.does_any_applicant_need_to_answer = ko.computed(function () {

    return _.any(self.product_coverage.get_covered_applicants(), function (applicant) {
      return self.does_applicant_need_to_answer(applicant);
    });
  });
  Object.defineProperty(self, 'action_name', {value: HealthQuestions.Responses.Yes, configurable: true});
  Object.defineProperty(self, 'has_static_value', {value: false, configurable: true});
};

StandardHealthQuestion.prototype.is_spouse_only = function () {
  return this.question.is_spouse_only;
};

StandardHealthQuestion.prototype.get_question_text = function () {
  return this.question.question_text;
};
StandardHealthQuestion.prototype.get_question_label = function () {
  return this.question.label;
};

StandardHealthQuestion.prototype.does_applicant_need_to_answer = function (applicant) {
  if (applicant.type === wizard_applicant.Applicant.EmployeeType) {
    return this.does_employee_need_to_answer();
  }
  if (applicant.type === wizard_applicant.Applicant.SpouseType) {
    return this.does_spouse_need_to_answer();
  }
  if (applicant.type === wizard_applicant.Applicant.ChildType) {
    if (!applicant.is_group()) {
      return this.does_child_need_to_answer(applicant);
    } else {
      return _.any(applicant.valid_applicants(), function (child) {
        return this.does_child_need_to_answer(child);
      }, this);
    }
  }

  return false;
};

StandardHealthQuestion.prototype.can_applicant_skip_due_to_GI = function (applicant) {
  if (applicant.type === wizard_applicant.Applicant.EmployeeType) {
    return this.can_employee_skip_due_to_GI();
  } else if (applicant.type === wizard_applicant.Applicant.SpouseType) {
    return this.can_spouse_skip_due_to_GI();
  } else if (applicant.type === wizard_applicant.Applicant.ChildType) {
    return this.can_child_skip_due_to_GI(applicant);
  } else {
    console.error("Invalid applicant type " + applicant.type, applicant);
  }
};

StandardHealthQuestion.prototype.does_question_stop_app = function () {
  return this.does_yes_stop_app() || this.does_no_stop_app();
};

StandardHealthQuestion.prototype.get_yes_highlight = function () {
  return this.does_yes_stop_app()? 'stop' : 'checkmark';
};
StandardHealthQuestion.prototype.get_no_highlight = function () {
  return 'checkmark';
};
StandardHealthQuestion.prototype.does_yes_stop_app = function () {
  return !this.question.is_ignored;
};
StandardHealthQuestion.prototype.does_no_stop_app = function () {
  return this.action_name === HealthQuestions.Responses.No;
};

var GIHealthQuestion = function (product, question, product_coverage, applicant_criteria, skip_mode, skipped_questions) {
  'use strict';
  var self = this;
  self.product = product;
  self.product_coverage = product_coverage;
  self.question = question;
  self.applicant_criteria = applicant_criteria;
  self.skip_mode = skip_mode;
  self.skipped_questions = skipped_questions;
  Object.defineProperty(self, 'action_name', {value: HealthQuestions.Responses.Yes, configurable: true});
  Object.defineProperty(self, 'has_static_value', {value: false, configurable: true});


  self.get_criteria = function (applicant_type) {
    return _.filter(self.applicant_criteria, function (c) {
      // make the JS applicant type match the product criteria applicant type.
      return self._clean_criteria_applicant_type(c.applicant_type) === applicant_type;
    });
  };

  self._clean_criteria_applicant_type = function (applicant_type) {
    // The database applicant types from the server don't match client enum. Fix that here as a band-aid.
    if (applicant_type === 'Employee') {
      return wizard_applicant.Applicant.EmployeeType;
    } else if (applicant_type === 'Spouse') {
      return wizard_applicant.Applicant.SpouseType;
    } else if (applicant_type === 'Child') {
      return wizard_applicant.Applicant.ChildType;
    } else {
      return applicant_type;
    }
  };

  self.get_met_gi_criteria_for_employee = ko.computed(function () {
    var applicant = self.product_coverage.applicant_list.get_employee();
    var coverage = self.product_coverage.__get_coverage_for_applicant(applicant);
    var employee_criteria = self.get_criteria(applicant.type);
    return _.find(employee_criteria, function (criterion) {
      return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
    });
  });

  self.has_employee_met_GI_criteria = ko.computed(function () {
    return self.get_met_gi_criteria_for_employee() !== undefined;
  });

  self.get_met_gi_criteria_for_spouse = ko.computed(function () {
    var applicant = self.product_coverage.applicant_list.get_spouse();
    var coverage = self.product_coverage.__get_coverage_for_applicant(applicant);
    var criteria = self.get_criteria(applicant.type);
    return _.find(criteria, function (criterion) {
      return self.does_applicant_meet_GI_criteria(applicant, coverage, criterion);
    });
  });

  self.has_spouse_met_GI_criteria = ko.computed(function () {
    return self.get_met_gi_criteria_for_spouse() !== undefined;
  });

  self.can_employee_skip_due_to_GI = ko.computed(function () {
    return self.has_employee_met_GI_criteria() && self.should_skip_if_GI_criteria_met();
  });

  self.can_spouse_skip_due_to_GI = ko.computed(function () {
    return self.has_spouse_met_GI_criteria() && self.should_skip_if_GI_criteria_met();
  });

  self.get_met_gi_criteria_for_child = function (child_applicant) {
    var criteria = self.get_criteria(child_applicant.type);

    var coverage;
    if (self.product_coverage.product.is_children_coverage_grouped()) {
      var children = self.product_coverage.applicant_list.get_children_group();
      coverage = self.product_coverage.__get_coverage_for_applicant(children);
    } else {
      coverage = self.product_coverage.__get_coverage_for_applicant(child_applicant);
    }

    return _.find(criteria, function (criterion) {
      return self.does_applicant_meet_GI_criteria(child_applicant, coverage, criterion);
    });
  };


  self.has_child_met_GI_criteria = function (child_applicant) {
    return self.get_met_gi_criteria_for_child(child_applicant) !== undefined;
  };


  self.show_yes_dialogue_employee = function () {
    self.show_yes_dialogue(self.product_coverage.applicant_list.get_employee());
  };

  self.show_yes_dialogue_spouse = function () {
    self.show_yes_dialogue(self.product_coverage.applicant_list.get_spouse());
  };

  self.show_yes_dialogue_child = function (child) {
    self.show_yes_dialogue(child);
  };

  self.get_best_GI_criteria = function (applicant, coverage, criteria) {
    // sort by gi amount descending then pick the one, if any, that is strictly less than
    //  the selected coverage. Returns undefined if there is no qualifying option.
    // We don't do <= here since we are assuming that the selected coverage is already greater than
    //  any GI amount, otherwise we wouldn't be searching for the best GI coverage to replace the selected coverage.
    var descendingCriteria = _.sortBy(criteria, function (c) {
      return -c.guarantee_issue_amount;
    });
    return _.find(descendingCriteria, function (criterion) {
      return (
        self.does_applicant_meet_demographic_GI_criteria(applicant, criterion) &&
        criterion.guarantee_issue_amount < coverage
      );
    });
  };

  self.show_yes_dialogue = function (applicant, message) {

    if (!self.does_question_stop_app()) {
      // No need to show anything, return.
      return;
    }

    // If we get here, we know the applicant doesn't completely qualify for GI
    //   with the coverage selected, and has answered 'Yes' to a question.
    //   We need to determine if the applicant can bypass this question by
    //   reducing coverage.

    // We will always allow the user to remove the applicant's coverage directly from this dialogue.
    var button_options = {
      remove: {
        label: "Remove this applicant", className: 'btn-danger', callback: function () {
          var applicant_coverage_options = self.product_coverage.get_coverage_options_for_applicant(applicant);

          // If child, we remove only the selected child, not all child coverage.
          if (applicant.type === wizard_applicant.Applicant.ChildType) {
            self.product_coverage.applicant_list.remove_applicant(applicant);
          } else {
            var null_option = _.find(applicant_coverage_options, function (o) {
              return o.face_value === 0;
            });

            self.product_coverage.__get_coverage_for_applicant(applicant).customized_coverage_option(null_option);
          }

          decline_product_if_no_coverage(self.product_coverage);
        }
      },
      ignore: {
        label: "Ignore and Continue", className: 'btn-default', callback: function () {
          // Nothing to do in this case
        }
      }
    };


    var display_message;
    if (should_show_reduce_coverage_option(applicant)) {
      add_reduce_option(applicant, button_options);
      display_message = create_reduce_dialogue_message(applicant, self.action_name);
    } else {
      var product_name = self.product_coverage.format_product_name();
      message = message || "A \"" + self.action_name + "\" response to this question disqualifies this person from obtaining coverage for '" + product_name + "'.";
      display_message = message + " You may proceed with this application after removing this individual from the coverage selection before proceeding.";
    }

    bootbox.dialog({
      message: display_message,
      buttons: button_options
    });
  };


  function create_reduce_dialogue_message(applicant, action_name) {
    var face_amount = format_face_value(get_cumulative_coverage(applicant));
    var gi_amount = get_reduced_coverage_criteria(applicant).guarantee_issue_amount;
    var formatted_gi_amount = format_face_value(gi_amount);
    action_name = typeof action_name !== 'undefined'? action_name : 'yes';

    return 'A "' + action_name + '" response to this question prohibits this person from obtaining the selected ' + face_amount + ' of coverage. You may proceed, however, by reducing your coverage to the guaranteed coverage amount of ' + formatted_gi_amount + '.' +
      '<br><br>Alternatively, you may remove this individual from the coverage selection altogether (in Step 1) before proceeding with the rest of the application.';
  }


  function get_cumulative_coverage(applicant) {
    var applicant_coverage = self.product_coverage.__get_coverage_for_applicant(applicant);
    return applicant_coverage.get_cumulative_coverage_amount();
  }

  function get_reduced_coverage_criteria(applicant) {
    var criteria = self.get_criteria(applicant.type);
    var coverage_amount = get_cumulative_coverage(applicant);

    return self.get_best_GI_criteria(applicant, coverage_amount, criteria);
  }

  function should_show_reduce_coverage_option(applicant) {
    // If this a required question, we can't skip it
    if (!self.should_skip_if_GI_criteria_met()) {
      return false;
    }

    // See if there is a GI condition with a lower coverage amount.
    // If no option was found, present the normal yes dialogue.
    if (!get_reduced_coverage_criteria(applicant)) {
      return false;
    }

    // If there is no option that will fit the reduced criterion, also don't show the reduce option.
    if (!find_reduced_coverage_option(applicant)) {
      return false;
    }

    return true;
  }

  function add_reduce_option(applicant, button_options) {
    // Add the option to reduce coverage to the dialogue.
    button_options["reduce"] = {
      label: "Reduce the coverage", className: 'btn-success', callback: function () {
        // Select the reduced coverage.
        self.product_coverage.__get_coverage_for_applicant(applicant).customized_coverage_option(find_reduced_coverage_option(applicant));
      }
    };
  }

  function find_reduced_coverage_option(applicant) {

    var applicant_coverage_options = self.product_coverage.get_coverage_options_for_applicant(applicant);
    var applicant_coverage = self.product_coverage.__get_coverage_for_applicant(applicant);
    var previous_coverage_amount = applicant_coverage.get_previous_coverage_amount();
    var reduced_gi_criterion = get_reduced_coverage_criteria(applicant);

    var gi_amount = reduced_gi_criterion.guarantee_issue_amount;
    // Find an option, if possible, that gives the most coverage but is below the GI threshold.
    //  Note that we include any previously applied coverage in this calculation.
    var filtered_options = _.filter(applicant_coverage_options, function (o) {
      return o.face_value > 0 && (previous_coverage_amount + o.face_value) <= gi_amount;
    });

    // If no option exists below the current coverage, return null.
    if (filtered_options.length == 0) {
      return null;
    }

    // Otherwise return the option with the largest coverage.
    return _.max(
      filtered_options, function (o) {
        return o.face_value;
      }
    );
  }

  self.does_employee_need_to_answer = ko.computed(function () {
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

  self.does_spouse_need_to_answer = ko.computed(function () {
    if (!self.product_coverage.did_select_spouse_coverage()) {
      return false;
    }
    if (self.can_spouse_skip_due_to_GI()) {
      return false;
    }

    return true;
  });

  self.can_child_skip_due_to_GI = function (child_applicant) {
    return self.has_child_met_GI_criteria(child_applicant) && self.should_skip_if_GI_criteria_met();
  };

  self.does_child_need_to_answer = function (child_applicant) {
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

  self.does_any_applicant_need_to_answer = ko.computed(function () {
    return _.any(self.product_coverage.get_covered_applicants(), function (app) {
      return self.does_applicant_need_to_answer(app);
    });
  });
};
GIHealthQuestion.prototype = Object.create(StandardHealthQuestion.prototype);


GIHealthQuestion.prototype.does_applicant_meet_GI_criteria = function (applicant, coverage, criterion) {
  return (
    this.does_applicant_meet_coverage_GI_criteria(applicant, coverage, criterion) &&
    this.does_applicant_meet_demographic_GI_criteria(applicant, criterion)
  );
};
GIHealthQuestion.prototype.does_applicant_meet_coverage_GI_criteria = function (applicant, applicant_coverage, criterion) {
  var self = this;
  return applicant_coverage.get_cumulative_coverage_amount() <= criterion.guarantee_issue_amount;
};
GIHealthQuestion.prototype.does_applicant_meet_demographic_GI_criteria = function (applicant, criterion) {
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


GIHealthQuestion.prototype.should_skip_if_GI_criteria_met = function () {
  var self = this;
  if (self.skip_mode == "all") {
    return true;
  } else {
    return _.any(self.skipped_questions, function (q) {
      return self.get_question_label() == q.question_type_label
    });
  }
};
GIHealthQuestion.prototype.does_yes_stop_app = function () {
  var self = this;

  if (self.question.is_ignored) {
    // Some questions are always ignored.
    return false;
  }

  // Otherwise, clicking YES always stops (but will show the reduce/remove dialogue if optional).
  return true;
};

function ActivelyAtWorkHealthQuestion(question, product_coverage, employee, actively_at_work_observable) {
  "use strict";
  var self = this;
  StandardHealthQuestion.call(self, question, product_coverage);
  self.employee = employee;
  self.value = actively_at_work_observable || ko.observable(null);
  Object.defineProperty(self, 'action_name', {value: HealthQuestions.Responses.No});
  Object.defineProperty(self, 'has_static_value', {value: true});
  self.does_spouse_need_to_answer = function () {
    return false;
  };
  self.does_child_need_to_answer = function () {
    return false;
  };
}

ActivelyAtWorkHealthQuestion.prototype = Object.create(StandardHealthQuestion.prototype);

ActivelyAtWorkHealthQuestion.prototype.get_question_text = function () {
  return 'Is ' + this.employee.first() + ' <a href="#actively_at_work_modal" data-toggle="modal">actively at work?</a>';
};

ActivelyAtWorkHealthQuestion.prototype.get_yes_highlight = function () {
  return 'checkmark';
};

ActivelyAtWorkHealthQuestion.prototype.get_no_highlight = function () {
  return 'stop';
};

ActivelyAtWorkHealthQuestion.prototype.does_yes_stop_app = function () {
  return false;
};

function ActivelyAtWorkGiHealthQuestion(product, question, product_coverage, applicant_criteria, skip_mode, skipped_questions, employee, actively_at_work_observable) {
  'use strict';
  var self = this;
  GIHealthQuestion.call(self, product, question, product_coverage, applicant_criteria, skip_mode, skipped_questions);
  self.employee = employee;
  self.value = actively_at_work_observable || ko.observable(null);
  Object.defineProperty(self, 'action_name', {value: HealthQuestions.Responses.No});
  Object.defineProperty(self, 'has_static_value', {value: true});
  self.does_spouse_need_to_answer = function () {
    return false;
  };
  self.does_child_need_to_answer = function () {
    return false;
  };
}

ActivelyAtWorkGiHealthQuestion.prototype = Object.create(GIHealthQuestion.prototype);

ActivelyAtWorkGiHealthQuestion.prototype.get_question_text = function () {
  return 'Is ' + this.employee.first() + ' <a href="#actively_at_work_modal" data-toggle="modal">actively at work?</a>';
};

ActivelyAtWorkGiHealthQuestion.prototype.get_yes_highlight = function () {
  return 'checkmark';
};

ActivelyAtWorkGiHealthQuestion.prototype.get_no_highlight = function () {
  return 'stop';
};

ActivelyAtWorkGiHealthQuestion.prototype.does_yes_stop_app = function () {
  return false;
};

function HealthQuestionAnswer(question, button_group, question_object) {
  var self = this;

  self.question = question;

  // This has all the additional attributes for the question
  self.question_object = question_object;

  self.button_group = ko.observable(null);


  self.answer = ko.computed(function () {
    if (self.button_group() == null) {
      return null;
    } else {
      return self.button_group().get_val();
    }
  });

  self.serialize = function () {
    // question: text,
    // answer: [Yes|No|GI] (GI means it was skipped due to GI)
    var answer;
    if (self.button_group()) {
      answer = (self.button_group().is_required())? self.button_group().get_val() : "GI";
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
