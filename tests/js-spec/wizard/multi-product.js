describe("Enroll multiple products ", function(){
    var vm;
    beforeEach(function() {
        vm = new wizard_viewmodel.WizardUI({
          products: [{
            is_fpp_product: true,
          }],
          children_data: [],
        });
    });

    it("should format 0 seconds as '00:00'", function() {

        var result = vm.NAIC_AND_MI[0];

        expect(result).toEqual("AK");
    });
});