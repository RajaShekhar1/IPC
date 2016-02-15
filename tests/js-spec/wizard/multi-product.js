describe("Enroll multiple products ", function () {
  var vm;

  beforeEach(function () {

    var options = {
      case: {
        id: 1,
        situs_state: 'IL',
        situs_city: 'Chicago',
        group_riders: []
      },
      applicants: [],
      products: [
        {
          code: 'FPPTI',
          soh_questions: [{'question': 'Have you had a heart attack in the last 5 years?'}]
        }
      ],
      beneficiaries: []
    };

    vm = new wizard_viewmodel.WizardUI({
      products: [{
        is_fpp_product: true,
      }],
      children_data: []
    });
  });

});