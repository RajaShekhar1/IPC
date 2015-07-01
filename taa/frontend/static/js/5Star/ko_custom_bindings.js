ko.bindingHandlers.wysiwyg = (function() {
  this.toolbars = {
    simple: [ 'bold','italic','underline',null,'createLink','unlink',null,null,null,'undo','redo'],
    full: [
      {
        name: 'font',
        title: 'Change the font',
        values: ['Times New Roman', 'Arial', 'Verdana', 'Open Sans', 'Tahoma']
      },
      null,
      {
        name: 'fontSize',
        title: 'Change the font size',
        values:{1 : 'Small' , 3 : 'Normal' , 5 : 'Huge'}
      },
      null,
      'bold',
      'italic',
      'strikethrough',
      'underline',
      null,
      'insertunorderedlist',
      'insertorderedlist',
      null,
      'outdent',
      'indent',
      null,
      'justifyleft',
      'justifycenter',
      'justifyright',
      'justifyfull',
      null,
      {
        name: 'createLink',
        placeholder: 'URL',
        button_class: 'btn-purple',
        button_text: 'Add'
      },
      'unlink',
      {
        name: 'createLink',
        placeholder: 'Image URL',
        button_insert_class: 'btn-purple',
        button_insert: 'Add',
        choose_file: true,
        button_class: 'btn-success',
        button_text: 'Choose an Image'
      },
      null,
      'foreColor',
      null,
      'undo',
      'redo'
    ]
  };
  return {
    init: function wysiwyg_init(element, valueAccessor, allBindings, viewModel, bindingContext) {
      var va = valueAccessor();
      //va object has type and initial value observable
      var html = va.value, type = va.type;
      var toolbar = this.toolbars[type] || this.toolbars.simple;
      $(element).ace_wysiwyg({
        toolbar: toolbar
      });
      $(element).html(html());
      $(element).on("change", function() {
        html($(element).html());
      });
    },
  };
})();
