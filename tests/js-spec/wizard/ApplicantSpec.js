describe("Applicant ViewModel", function() {

  // Some test data to initialize an Applicant
  var test_data = {};

  beforeEach(function() {
    // set test_data to a valid set of data.
    test_data = {
      first: "Joe",
      last: "Johnson",
      gender: "male",
      type: wizard_applicant.Applicant.EmployeeType,
      email: "joe@joe.com",
      phone: "1-123-123-1234",
      birthdate: "1990-11-11",
      ssn: '111-22-3333',
      height: 72,
      weight: 150,
      is_smoker: true,

      street_address: '123 Sesame',
      street_address2: '',
      city: 'Chicago',
      state: 'IL',
      zip: '11123',

      // TODO: what format are these?
      existing_coverages: []
    }

  });

  it("should populate applicant observables from provided options dictionary.", function() {

    var applicant = wizard_applicant.create_applicant(test_data);

    expect(applicant.type).toEqual(wizard_applicant.Applicant.EmployeeType);
    expect(applicant.first()).toEqual(test_data.first);
    expect(applicant.last()).toEqual(test_data.last);
    expect(applicant.gender()).toEqual(test_data.gender);
    expect(applicant.email()).toEqual(test_data.email);
    // Wizard reformats date using normalize_date()
    expect(applicant.birthdate()).toEqual(normalize_date(test_data.birthdate));
    expect(applicant.phone()).toEqual(test_data.phone);
    expect(applicant.ssn()).toEqual(test_data.ssn);
    expect(applicant.height()).toEqual(test_data.height);
    expect(applicant.weight()).toEqual(test_data.weight);
    expect(applicant.is_smoker()).toEqual(test_data.is_smoker);
    expect(applicant.address1()).toEqual(test_data.street_address);
    expect(applicant.address2()).toEqual(test_data.street_address2);
    expect(applicant.city()).toEqual(test_data.city);
    expect(applicant.zip()).toEqual(test_data.zip);
    // Not an observable
    expect(applicant.existing_coverages).toEqual(test_data.existing_coverages);
  });

  it("should be valid if name and birthdate are provided", function() {
    var applicant = wizard_applicant.create_applicant(test_data);

    expect(applicant.is_valid()).toEqual(true);
  });

  it("should not be valid if first name is blank", function() {
    test_data.first = "";

    var applicant = wizard_applicant.create_applicant(test_data);

    expect(applicant.is_valid()).toEqual(false);
  });

  it("should not be valid if last name is blank", function() {
    test_data.last = "";

    var applicant = wizard_applicant.create_applicant(test_data);

    expect(applicant.is_valid()).toEqual(false);
  });

  it("should not be valid if birthdate is blank", function() {
    test_data.birthdate = "";

    var applicant = wizard_applicant.create_applicant(test_data);

    expect(applicant.is_valid()).toEqual(false);
  });

  it("should serialize the data", function() {
    var applicant = wizard_applicant.create_applicant(test_data);

    var serialized = applicant.serialize_data();

    expect(serialized.first).toEqual(test_data.first);
    expect(serialized.last).toEqual(test_data.last);
    // add more checks if needed.
  });

  it("should report if any of the main fields are valid", function() {
    var applicant = wizard_applicant.create_applicant(test_data);

    expect(applicant.any_valid_field()).toEqual(true);
  });

  it("should report false if not any of the main fields are valid", function() {
    test_data.first = "";
    test_data.last = "";
    test_data.birthdate = "";
    var applicant = wizard_applicant.create_applicant(test_data);

    expect(applicant.any_valid_field()).toEqual(false);
  });

});

describe("ApplicantList", function() {

  var emp, sp, ch1, ch2;

  beforeEach(function() {
    emp = wizard_applicant.create_applicant({type: wizard_applicant.Applicant.EmployeeType});
    sp = wizard_applicant.create_applicant({type: wizard_applicant.Applicant.SpouseType});
    ch1 = wizard_applicant.create_applicant({type: wizard_applicant.Applicant.ChildType});
    ch2 = wizard_applicant.create_applicant({type: wizard_applicant.Applicant.ChildType});
  });

  function build_app_list(apps) {
    return new wizard_applicant.ApplicantList(apps);
  }

  it("should be able to find the employee", function() {
    var app_list = build_app_list([emp, sp]);

    var found_employee = app_list.get_employee();

    expect(found_employee).toBe(emp);
  });

  it("should be able to find the spouse", function() {
    var app_list = build_app_list([emp, sp]);
    var found_spouse = app_list.get_spouse();
    expect(found_spouse).toBe(sp);
  });

  it("should check to see if spouse is invalid", function() {
    var app_list = build_app_list([emp, sp]);
    expect(app_list.has_valid_spouse()).toEqual(false);
  });

  it("should check to see if spouse is valid", function() {
    sp.first("Jane");
    sp.last("Johnson");
    sp.birthdate("01/10/1980");
    var app_list = build_app_list([emp, sp]);
    expect(app_list.has_valid_spouse()).toEqual(true);
  });

  it("should check to see if employee is invalid", function() {
    var app_list = build_app_list([emp, sp]);
    expect(app_list.has_valid_employee()).toEqual(false);
  });

  it("should check to see if employee is valid", function() {
    emp.first("Jane");
    emp.last("Johnson");
    emp.birthdate("01/10/1980");
    var app_list = build_app_list([emp, sp]);
    expect(app_list.has_valid_employee()).toEqual(true);
  });

  it("should check to see if there are no children", function() {
    var app_list = build_app_list([emp, sp]);
    expect(app_list.has_valid_children()).toEqual(false);
  });

  it("should return true when asked if there is a valid child and there is", function() {
    var app_list = build_app_list([emp, sp, ch1]);
    ch1.first("John");
    ch1.last("Johnson");
    ch1.birthdate("01/01/2000");
    expect(app_list.has_valid_children()).toEqual(true);

  });

  it("should create a default employee if none is given", function() {
    var app_list = build_app_list();
    expect(app_list.get_employee()).not.toBeUndefined();
  });

  it("should create a default spouse if none is given", function() {
    var app_list = build_app_list([emp]);
    expect(app_list.get_spouse()).not.toBeUndefined();
  });
});