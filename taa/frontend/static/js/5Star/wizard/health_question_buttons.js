
function QuestionButton(element, val, highlight_func, unhighlight_func) {
  var self = this;

  self.elements = [$(element)];
  self.val = val;

  self.highlight = function () {
    $.each(self.elements, function () {
      highlight_func(this);
    });
  };
  self.unhighlight = function () {
    $.each(self.elements, function () {
      unhighlight_func(this);
    });
  };
}
function QuestionButtonGroup(question, is_required) {
  var self = this;
  self.question = question;
  self.buttons = [];
  self.selected_btn = ko.observable(null);
  self.is_required = is_required;

  self.get_val = ko.computed(function () {
    if (self.selected_btn() == null) {
      return false;
    } else {
      return self.selected_btn().val;
    }
  });


  self.add_button = function (element, val, high_func, unhigh_func) {
    var btn = null;
    $.each(self.buttons, function () {
      if (this.val == val) {
        btn = this;
      }
    });
    if (btn) {
      btn.elements.push(element);
    } else {
      self.buttons.push(new QuestionButton(element, val, high_func, unhigh_func));
    }
  };
  self.click_button = function (val) {
    var btn = null;
    $.each(self.buttons, function () {
      if (this.val == val) {
        btn = this;
      }
      this.unhighlight();
    });

    btn.highlight();
    self.selected_btn(btn);
  };
}

var general_questions_by_id = {};
ko.bindingHandlers.flagBtn = {
  init: function (element, value_accessor) {

    var val = ko.unwrap(value_accessor());

    // for testing purpose, mark each element with a CSS class and yes or no value
    $(element).addClass("flagBtn").addClass("val_" + val.val);

    var btn_group;
    if (val.applicant) {
      var applicant = val.applicant;
      var product_health_questions = val.product_health_questions;
      var applicant_coverage = product_health_questions.product_coverage.get_coverage_for_applicant(applicant);

      var question_text = val.question.get_question_text();
      /*var applicant_health_answer = product_health_questions.get_applicant_answer_for_question(applicant, question_text);

      btn_group = applicant_health_answer.button_group();
      */
      btn_group = null;
      if (!btn_group) {
        btn_group = new QuestionButtonGroup(val.question, val.is_required);
        //applicant_health_answer.button_group(btn_group);
      }
      //if (applicant_health_answer.answer() == val.val) {
      //  btn_group.click_button(val.val);
      //}
    } else {
      var group_lookup = general_questions_by_id;

      if (val.question.get_question_text() in group_lookup) {
        btn_group = group_lookup[val.question.get_question_text()];
      } else {
        btn_group = new QuestionButtonGroup(val.question, val.is_required);
        group_lookup[val.question.get_question_text()] = btn_group;
      }

      if (val.is_selected === true) {
        btn_group.click_button(val.val);
      }
    }

    btn_group.add_button(element, val.val, function (el) {
      var highlight_type;
      if (typeof val.highlight === "function") {
        highlight_type = val.highlight();
      } else {
        highlight_type = val.highlight;
      }
      if (highlight_type === "flag") {
        $(el
        ).prepend('<i class="icon glyphicon glyphicon-flag"></i>'
        ).addClass("btn btn-warning"
        );
      } else if (highlight_type === "checkmark") {
        $(el
        ).prepend('<i class="icon glyphicon glyphicon-ok"></i>'
        ).addClass("btn-success"
        );
      } else if (highlight_type === "stop") {
        $(el
        ).prepend('<i class="icon glyphicon glyphicon-remove"></i>'
        ).addClass("btn-danger"
        );
      }

    }, function (el) {
      $(el).removeClass("btn-success btn-warning btn-danger"
      ).addClass("btn-default"
      );
      $(el).find(".glyphicon").remove();
    });

    $(element).on("click", function () {
      btn_group.click_button(val.val);
      if (val.onclick) {
        val.onclick();
      }
    });

    // Initial value, if they are revisiting this page
    if (btn_group.selected_btn && btn_group.selected_btn.val == val.val) {
      btn_group.click_button(val.val);
    }

  },
  update: function (element, value_accessor) {

  }
};

function handle_existing_insurance_modal() {
  if (window.vm.did_select_any_fpp_product()) {

  } else {
    $("#modal_text_existing_warning_title").show();
    $("#modal_text_replacement_warning_title").hide();
    $("#modal_text_soh_warning_title").hide();

    $("#modal_text_existing_warning").show();
    //$("#modal_text_existing_warning_remote").hide();
    $("#modal_text_replacement_warning").hide();
    $("#modal_text_soh_warning").hide();

    $("#health_modal").modal('show');
    $("#existing_warning_text").show();

  }

  window.vm.existing_insurance(true);
}

function reset_existing_insurance_warning() {
  window.vm.existing_insurance(false);
  $("#existing_warning_text_remote").hide();
  $("#existing_warning_text").hide();
}

function handle_replacement_insurance_modal() {
  if (window.vm.did_select_any_fpp_product()) {

  } else {
    $("#modal_text_existing_warning_title").hide();
    $("#modal_text_replacement_warning_title").show();
    $("#modal_text_soh_warning_title").hide();

    $("#modal_text_existing_warning").hide();
    $("#modal_text_existing_warning_remote").hide();
    $("#modal_text_replacement_warning").show();
    $("#modal_text_soh_warning").hide();

    $("#health_modal").modal('show');
    $("#replacement_warning_text").show();
  }

  window.vm.replacing_insurance(true);

}
function reset_replacement_insurance_warning() {
  $("#replacement_warning_text").hide();
  window.vm.replacing_insurance(false);
}


function handle_question_yes() {
  $("#modal_text_existing_warning_title").hide();
  $("#modal_text_replacement_warning_title").hide();
  $("#modal_text_soh_warning_title").show();

  $("#modal_text_existing_warning").hide();
  $("#modal_text_existing_warning_remote").hide();
  $("#modal_text_replacement_warning").hide();
  $("#modal_text_soh_warning").show();

  $("#health_modal").modal('show');
}

function are_health_questions_valid() {
  // TODO: will need to improve visual designation of which questions are failing validation.

  // this one can be yes or no
  if (window.vm.should_show_other_insurance_questions() &&
      window.vm.is_in_person_application() &&
      general_questions_by_id['existing_insurance'].get_val() === null) {
    return false;
  }

  if (window.vm.should_show_other_insurance_questions()
      && (
          !window.vm.did_select_any_fpp_product()
          && general_questions_by_id['replace_insurance'].get_val() != "No"
      ) ||
      (
          window.vm.did_select_any_fpp_product()
          && general_questions_by_id['replace_insurance'].get_val() === null
      )
  ) {
    //el = $(general_questions_by_id['existing_insurance'].buttons[0].elements[0]);
    return false;
  }

  // fpp form
  if (window.vm.did_select_any_fpp_product()) {
    if (window.vm.is_employee_actively_at_work() === null) {
      return false;
    }
  }

  var valid = true;


  _.each(window.vm.selected_product_health_questions(), function(product_health_questions) {
    _.each(product_health_questions.health_button_rows(), function(health_button_row) {
      _.each(health_button_row.button_groups(), function(button_group) {
        if (button_group.does_applicant_need_to_answer()) {
          // If a no-op question, but still required, must select yes or no.
          if (!button_group.question.does_yes_stop_app() && button_group.response.value() === null) {
            valid = false;
            // break
            return false;
          // If this is required, the answer must be no.
          } else if (button_group.question.does_yes_stop_app() && button_group.response.value() !== "No") {
            valid = false;
            // break
            return false;
          }
        }
      });
      if (!valid) {
        // break
        return false;
      }
    });
    if (!valid) {
      // break
      return false;
    }
  });

  //$.each(window.vm.coverage_vm.get_all_covered_people(), function () {
  //  var covered_person = this;
  //  $.each(covered_person.health_questions(), function () {
  //    if (this.button_group()
  //        && this.button_group().is_required()
  //        && (
  //          // If a no-op question, but still required, must select yes or no.
  //            (this.question.is_ignored
  //                && this.button_group().get_val() === null
  //            )
  //              // If this is required, must be no.
  //            || (!this.question.is_ignored
  //                && this.button_group().get_val() !== "No"
  //            )
  //        )) {
  //      valid = false;
  //      // break
  //      return false;
  //    }
  //  });
  //  if (!valid) {
  //    // break
  //    return false;
  //  }
  //});

  return valid;
}
