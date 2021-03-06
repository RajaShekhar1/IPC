
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


/* Case Report component */
ko.components.register('case-report', {
  viewModel: {
    createViewModel: function(params, componentInfo) {
      return new CaseReportsViewModel(params);
    }
  },
  template: { element: 'case-report-template' }
});

/*
    Adding a reactive watcher to knockout out.
    From: https://github.com/ZiadJ/knockoutjs-reactor/tree/master/dist
*/
ko.subscribable.fn.watch=function(a,b,c,d){var e=typeof a;return"boolean"===e||"undefined"===e?ko.watch(this,{enabled:a!==!1}):"function"!==e||ko.isSubscribable(a)?ko.watch(a,b,c,d||this):ko.watch(this,b||{},a,d||this),this},ko.watch=function(a,b,c,d){function e(h,i,j,k,l,m){if(h&&0!==b.depth&&(-1===b.depth||j.length<(b.depth||1))){if(b.watchedOnly&&!h.watchable&&h!=a)return;if((b.enabled===!1||b.enabled===!0)&&(h.watchable=b.enabled),h.watchable===!1)return;b.seal===!0&&(h.watchable=!1);var n=typeof h;if("object"===n||"function"===n){if(h._watcher===d)return;if(b.hide&&ko.utils.arrayIndexOf(b.hide,h)>-1)return;var o=[].concat(j,i&&i!==a?i:[]);if("function"!==n){if("[object Object]"===Object.prototype.toString.call(h))ko.utils.objectForEach(h,function(a,c){if(c=b.getter?b.getter.call(d,o,h,a):c){if(b.wrap){var f=Object.prototype.toString.call(c);"[object Function]"!==f&&"[object Object]"!==f&&(b.beforeWrap&&b.beforeWrap.call(d,o,h,c)===!1||(c=h[a]="[object Array]"===f?ko.observableArray(c):ko.observable(c)))}b.unloop&&(c._watcher=k?void 0:d);var g=e(c,l?null:h,o,k,null,a);b.tagFields&&void 0===c._fieldName&&(g||"parentsOnly"!==b.tagFields&&"function"==typeof c||"object"==typeof c)&&(c._fieldName=a)}});else if(b.hideArrays!==!0)for(var p=0;p<h.length;p++)e(h[p],l?null:h,o,k);return!0}if("function"==typeof h.notifySubscribers&&c){if(b.enabled===!0&&h.watchable===!1)return;if(k||!b.beforeWatch||b.beforeWatch.call(d,o,h,m)!==!1){var q="function"==typeof h.pop;if(k?f(h):g(h,q,o,l),q)return e(h(),l?null:h,o,k,!0),!0;if(b.hideWrappedValues!==!0)return e(h(),l?null:h,o,k,!0)}}}}}function f(a){var c=a[h];if(!c)throw"Subscriptions field (."+h+") not defined for observable child "+(a._fieldName||"");if(c.change)for(var e=c.change.length-1;e>=0;e--)c.change[e]._watcher===d&&c.change[e].dispose();if(c.beforeChange&&(b.mutable||b.oldValues>0))for(var e=c.beforeChange.length-1;e>=0;e--)c.beforeChange[e]._watcher===d&&c.beforeChange[e].dispose();if(c.arrayChange)for(var e=c.arrayChange.length-1;e>=0;e--)c.arrayChange[e]._watcher===d&&c.arrayChange[e].dispose()}function g(a,f,g,h){f?a.subscribe(function(b){ko.utils.arrayForEach(b,function(b){var f=c.call(d,g,a,b);void 0!==f&&d(f),b.moved||setTimeout(function(){e(b.value,h?null:a,g,"deleted"===b.status)},0)})},void 0,"arrayChange")._watcher=d:(a.subscribe(function(){if(a.watchable!==!1){var f=c.call(d,g,a);void 0!==f&&d(f),b.mutable&&"object"==typeof a()&&e(a(),h?null:a,g)}},null,"change")._watcher=d,(b.oldValues>0||b.mutable)&&(a.subscribe(function(c){if(b.oldValues>0){var d=a.oldValues?a.oldValues:a.oldValues=[];for(d.unshift(c);d.length>b.oldValues;)d.pop()}b.mutable&&"object"==typeof c&&e(c,h?null:a,g,!1,!0)},null,"beforeChange")._watcher=d))}"function"==typeof b&&(d=d||c,c=b,b={}),d=d||this;var h;switch(ko.DEBUG||ko.version){case!0:h="_subscriptions";break;case"3.0.0":h="F";break;case"3.1.0":h="H";break;case"3.2.0":h="M";break;case"3.3.0":h="G";case"3.4.0":h="K";break;default:throw"Unsupported Knockout version. Only v3.0.0 to v3.4.0 are supported when minified. Current version is "+ko.version}return"function"!=typeof a||ko.isSubscribable(a)?(e(a,null,[]),{dispose:function(){e(a,null,[],!0)}}):ko.computed(a,c,b)};window.foo = "1.3.6";
