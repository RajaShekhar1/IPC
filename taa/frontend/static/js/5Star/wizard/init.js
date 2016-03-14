var enrollment_wizard = (function() {
  return {
    init: function(data) {
      console.log("init with args: ", arguments);

      var ui = new wizard_viewmodel.WizardUI(data);
      // Allow other JS functions to access the ui object
      window.ui = ui;
      ko.applyBindings(ui);

      init_validation();
    }
  };
})();