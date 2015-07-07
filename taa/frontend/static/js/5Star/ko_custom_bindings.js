
// wysiwyg editor. Takes an object like: {value: [observable value], type: ['simple'|'full']}
//  The simple editor has fewer toolbar options, suitable for email body editing.
//  The full editor has more control over the text in the body and can insert images.
//  Defaults to simple type toolbar.
ko.bindingHandlers.wysiwyg = (function() {
  var toolbars = {
    simple: [ 'bold','italic','underline',null,'createLink','unlink',null,null,null,'undo','redo'],
    full: [
      null,
      null,
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
      null,
      {
        name: 'insertImage',
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
    init: function(element, valueAccessor, allBindings, viewModel, bindingContext) {
      // object has type and initial value observable
      var va = valueAccessor();
      var html = va.value, type = va.type;
      var toolbar = toolbars[type] || toolbars.simple;
      $(element).ace_wysiwyg({
        toolbar: toolbar
      });

      // Set initial value.
      $(element).html(ko.unwrap(html));

      // Update the observable when the page changes.
      $(element).on("change", function() {
        html($(element).html());
      });
    },

    // What to do when the observed value changes.
    update: function(element, valueAccessor) {
      var updated_html = ko.unwrap(valueAccessor().value);

      // We only need to update the HTML if the content has changed (if the observable was changed by a script).
      if ($(element).html() != updated_html) {
        $(element).html(updated_html);
      }
    }
  };
})();
