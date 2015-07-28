// Karma configuration
// Generated on Mon Jul 27 2015 22:30:19 GMT+0000 (UTC)

module.exports = function(config) {
  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',


    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['jasmine'],


    // list of files / patterns to load in the browser
    files: [
      'taa/frontend/static/js/jquery-2.1.1.min.js',
      'taa/frontend/static/js/bootstrap.js',
      'taa/frontend/static/js/jquery.dataTables.min.js',
      'taa/frontend/static/js/jquery.dataTables.bootstrap.js',
      'taa/frontend/static/js/jquery.maskedinput.min.js',
      'taa/frontend/static/js/jquery.validate.min.js',
      'taa/frontend/static/js/fuelux/fuelux.wizard.min.js',
      'taa/frontend/static/js/jquery.rcrumbs.min.js',

      'taa/frontend/static/js/ace-elements.min.js',
      'taa/frontend/static/js/ace.min.js',
      'taa/frontend/static/js/additional-methods.js',

      'taa/frontend/static/js/moment.min.js',
      'taa/frontend/static/js/bootbox.min.js',
      'taa/frontend/static/js/jquery.mobile.custom.min.js',
      'taa/frontend/static/js/typeahead-bs2.min.js',
      'taa/frontend/static/js/knockout-3.3.0.js',
      'taa/frontend/static/js/underscore-min.js',

      'taa/frontend/static/js/5Star/**/*.js',

      // tests
      'tests/js-spec/**/*.js'
    ],


    // list of files to exclude
    exclude: [
    ],


    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
    },


    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['progress'],


    // web server port
    port: 9876,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,


    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
    browsers: ['Chrome'],


    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
    singleRun: false
  })
}
