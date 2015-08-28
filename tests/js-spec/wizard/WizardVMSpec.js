describe("Wizard ViewModel", function() {

  // Some test data to initialize an Applicant
  var test_data = {};

  beforeEach(function() {
    // set test_data to a valid set of data.
    test_data = {
      case_data: {
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

  // Moving the children group to the product coverage, as this will be more flexible since we can do this
  // on a product-by-product basis.
  it("should add two empty children to the form by default", function() {
    var wizard = create_wizard(test_data);
    expect(wizard.applicant_list.get_children().length).toEqual(2);
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

    // The wizard adds an extra blank child, so it will still be there when we remove this child
    expect(wizard.children().length).toEqual(1);
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
    test_data.case_data.payment_mode = -1;

    var wizard = create_wizard(test_data);

    expect(wizard.coverage_vm.can_change_payment_mode()).toEqual(true);
  });

  it("should allow the payment mode to be changed if the payment mode is not set", function() {
    test_data.case_data.payment_mode = null;

    var wizard = create_wizard(test_data);
    expect(wizard.coverage_vm.can_change_payment_mode()).toEqual(true);
  });

  it("should not allow the payment mode to be changed if the payment mode is already set", function() {
    test_data.case_data.payment_mode = 52;

    var wizard = create_wizard(test_data);
    expect(wizard.coverage_vm.can_change_payment_mode()).toEqual(false);
  });

  it("should get the available payment modes from the input options", function() {
    var wizard = create_wizard(test_data);
    expect(wizard.coverage_vm.payment_modes.length).toBe(test_data.payment_modes.length);
  });

  it("should have a valid payment mode if the case has a default selected", function() {
    test_data.case_data.payment_mode = 52;
    var wizard = create_wizard(test_data);
    expect(wizard.coverage_vm.is_payment_mode_valid()).toEqual(true);
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

  it("should show the names of the valid applicants in the coverage selection table", function() {
    var ch1 = {first: 'Jack', last: 'Johnson', birthdate: '2000-01-01', type: wizard_applicant.Applicant.ChildType};
    var ch2 = {first: 'Jill', last: 'Johnson', birthdate: '2002-11-01', type: wizard_applicant.Applicant.ChildType};
    test_data.applicants.push(ch1);
    test_data.applicants.push(ch2);
    var wizard = create_wizard(test_data);
    var offered_product_vm = wizard.product_coverage_viewmodels()[0];

    var emp_coverage_selection = offered_product_vm.get_coverage_for_applicant(wizard.applicant_list.get_employee());
    var spouse_coverage_selection = offered_product_vm.get_coverage_for_applicant(wizard.applicant_list.get_spouse());
    var children_coverage_selection = offered_product_vm.get_coverage_for_applicant(wizard.applicant_list.get_children_group());

    expect(emp_coverage_selection.format_name()).toEqual(wizard.employee().name());
    expect(spouse_coverage_selection.format_name()).toEqual(wizard.spouse().name());
    expect(children_coverage_selection.format_name()).toEqual(ch1.first + ", "+ch2.first);
  });

  it("should show that an applicant has not selected coverage for a given product", function() {
    var wizard = create_wizard(test_data);

    var offered_product_vm = wizard.product_coverage_viewmodels()[0];

    var applicant_coverage_selection = offered_product_vm.get_coverage_for_applicant(wizard.applicant_list.get_employee());
    expect(applicant_coverage_selection.has_selected_coverage()).toEqual(false);
  });

  it("should show an applicant's coverage level after selecting a specific coverage for an applicant", function() {
    var wizard = create_wizard(test_data);
    var offered_product_vm = wizard.product_coverage_viewmodels()[0];
    var emp_selected_cov = offered_product_vm.get_coverage_for_applicant(wizard.applicant_list.get_employee());

    var option = new CoverageOption({coverage: 10000, premium: 3.59});
    emp_selected_cov.select_custom_coverage(option);

    expect(emp_selected_cov.has_selected_coverage()).toEqual(true);
    expect(emp_selected_cov.get_selected_coverage()).toBe(option);
  });

  it("should allow the user to select a recommended set of coverages for all applicants on a product", function() {
    var wizard = create_wizard(test_data);
    var offered_product_vm = wizard.product_coverage_viewmodels()[0];
    var recommendations = [
      {name: 'good', coverages: {employee: 10000, spouse: null, children: null}}
    ];

    offered_product_vm.select_recommended_coverage(recommendations[0].coverages);

    var emp_selected_cov = offered_product_vm.get_coverage_for_applicant(wizard.applicant_list.get_employee());
    expect(emp_selected_cov.get_selected_coverage()).toBe(recommendations[0].coverages.employee);
  });

});