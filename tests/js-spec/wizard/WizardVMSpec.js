describe("Wizard ViewModel", function() {

  // Some test data to initialize an Applicant
  var test_data = {};

  beforeEach(function() {
    // set test_data to a valid set of data.
    test_data = {
      case: {
        id: 1,
        situs_state: 'IL',
        situs_city: 'Chicago',
        group_riders: [],
        payment_mode: 52
      },
      applicants: [
        {
          type: wizard_applicant.Applicant.EmployeeType,
          first: "Joe",
          last: "Johnson",
          gender: "male",
          birthdate: "1990-01-22"
        },
        {
          type: wizard_applicant.Applicant.SpouseType,
          first: "Jane",
          last: "Johnson",
          gender: "female",
          birthdate: "1989-11-11"
        }
      ],
      products: [
        {
          base_product_type: 'FPPTI',
          code: 'FPPTI',
          soh_questions: [{'question': 'Have you had a heart attack in the last 5 years?'}]
        }
      ],
      payment_modes: [
        {'frequency': 52, 'label': 'Weekly'},
        {'frequency': 26, 'label': 'Biweekly'},
        {'frequency': 24, 'label': 'Semimonthly'},
        {'frequency': 12, 'label': 'Monthly'}
      ],
      beneficiaries: []
    };
  });

  function create_wizard(options) {
    return new wizard_viewmodel.WizardVM(options);
  }

  it("should access the employee through an employee() observable", function() {
    var wizard = create_wizard(test_data);
    var emp = new wizard_applicant.Applicant({type: wizard_applicant.Applicant.EmployeeType});

    expect(wizard.employee().first()).toEqual(test_data.applicants[0].first);
  });

  it("should create an empty employee applicant if no employee is given", function() {
    test_data.applicants = [];
    var wizard = create_wizard(test_data);

    expect(wizard.employee()).not.toBeUndefined();
  });

  it("should access the spouse through a spouse() observable", function() {
    var wizard = create_wizard(test_data);

    expect(wizard.spouse().first()).toBe(test_data.applicants[1].first);
  });

  it("should create an empty spouse applicant if no spouse is given", function() {
    test_data.applicants = [];
    var wizard = create_wizard(test_data);

    expect(wizard.spouse()).not.toBeUndefined();
  });

  it("should not show the spouse by default if no spouse data is provided", function() {
    // Test data has a spouse
    test_data.applicants = [];
    var wizard = create_wizard(test_data);

    expect(wizard.should_show_spouse()).toEqual(false);
  });

  it("should show the spouse by default if a spouse is provided", function() {
    var wizard = create_wizard(test_data);
    expect(wizard.should_show_spouse()).toEqual(true);
  });

  it("should create an empty ApplicantGroup if no child data is provided", function() {
    test_data.applicants = [];
    var wizard = create_wizard(test_data);
    expect(wizard.children_group.type).toEqual(wizard_applicant.Applicant.ChildType);
    expect(wizard.children_group.applicants()).toEqual([]);
  });

  it("should not show children by default if no children data", function() {
    test_data.applicants = [];
    var wizard = create_wizard(test_data);
    expect(wizard.should_include_children()).toEqual(false);
  });

  it("should remove a child when remove_child is called", function() {
    var ch = {type: wizard_applicant.Applicant.ChildType};
    test_data.applicants.push(ch);
    var wizard = create_wizard(test_data);

    var child = wizard.children()[0];
    wizard.remove_child(child);

    expect(wizard.children().length).toEqual(0);
  });

  it("should set should_include_children to false when the last child is removed", function() {
    var ch = {type: wizard_applicant.Applicant.ChildType};
    test_data.applicants.push(ch);
    var wizard = create_wizard(test_data);
    wizard.should_include_children(true);

    var child = wizard.children()[0];
    wizard.remove_child(child);

    expect(wizard.should_include_children()).toEqual(false);
  });

  it("should add a child with default last name", function() {
    var wizard = create_wizard(test_data);

    wizard.add_child();

    var emp_last = test_data.applicants[0].last;
    expect(wizard.children()[0].last()).toEqual(emp_last);
  });

  it("should default the spouse last name to employee's last name if given", function() {
    test_data.applicants = [{type: wizard_applicant.Applicant.EmployeeType, last: "Johnson"}];
    var wizard = create_wizard(test_data);
    expect(wizard.spouse().last()).toEqual("Johnson");
  });

  it("should allow the payment mode to be changed if the setting is user selects", function() {
    // "User selects" is -1
    test_data.case.payment_mode = -1;

    var wizard = create_wizard(test_data);

    expect(wizard.can_change_payment_mode()).toEqual(true);
  });

  it("should allow the payment mode to be changed if the payment mode is not set", function() {
    test_data.case.payment_mode = null;

    var wizard = create_wizard(test_data);
    expect(wizard.can_change_payment_mode()).toEqual(true);
  });

  it("should not allow the payment mode to be changed if the payment mode is already set", function() {
    test_data.case.payment_mode = 52;

    var wizard = create_wizard(test_data);
    expect(wizard.can_change_payment_mode()).toEqual(false);
  });

  it("should get the available payment modes from the input options", function() {
    var wizard = create_wizard(test_data);
    expect(wizard.payment_modes.length).toBe(test_data.payment_modes.length);
  });

  it("should have a valid payment mode if the case has a default selected", function() {
    test_data.case.payment_mode = 52;
    var wizard = create_wizard(test_data);
    expect(wizard.is_payment_mode_valid()).toEqual(true);
  });

  it("should not show the recommendation table initially", function() {
    var wizard = create_wizard(test_data);
    expect(wizard.has_show_rates_been_clicked()).toEqual(false);
    expect(wizard.is_coverage_selection_visible()).toEqual(false);
  });

  it("should show the coverage selection table when the data is valid", function() {
    var wizard = create_wizard(test_data);
    wizard.show_coverage_selection_table();
    expect(wizard.is_coverage_selection_visible()).toEqual(true);
  });

  it("should hide the show rates button when the coverage selection table is showing", function() {
    var wizard = create_wizard(test_data);
    expect(wizard.should_display_show_rates_button()).toEqual(true);
    wizard.show_coverage_selection_table();
    expect(wizard.should_display_show_rates_button()).toEqual(false);
  });

  it("should not show extended questions if there aren't any products requiring them", function() {
    var wizard = create_wizard(test_data);
    expect(wizard.should_show_extended_questions()).toEqual(false);
  });

  it("should show the extended questions if any product requires extended questions", function() {
    // GroupCI requires extended questions
    test_data.products.push({
      code: 'Group CI',
      base_product_type: 'Group CI',
      soh_questions: [{'question': 'Have you had a heart attack in the last 5 years?'}]
    });
    var wizard = create_wizard(test_data);
    expect(wizard.should_show_extended_questions()).toEqual(true);
  });

  xit("should require the minimum age to be greater than 18", function() {

  });

  it("should allow a user to decline coverage by default", function() {
    var wizard = create_wizard(test_data);
    expect(wizard.can_decline()).toEqual(true);
  });

  xit("should not allow a user to decline a product if he has previously taken coverage for that product", function() {

  });

});