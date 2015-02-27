
from flask_assets import Environment, Bundle

# application css bundle
#css_taa_less = Bundle("less/*.less",
#                       filters="less", output="css/taa.css",
#                       debug=False)

from taa import app

if app.config['ASSETS_DEBUG']:
    # Put any debug configurations for JS and CSS here
    knockout_js = Bundle('js/knockout-3.3.0.debug.js')
else:
    knockout_js = Bundle('js/knockout-3.3.0.js', filters='rjsmin')

css_taa = Bundle(
    "css/five_star_ace_overlay.css",
    "css/five_star.css",
    filters="cssmin",
)

# consolidated css bundle - old ace (v1.2)
css_all = Bundle(
    "css/bootstrap.min.css", 
    "css/font-awesome.min.css",
    Bundle("css/ace-fonts.css", filters="cssmin"),
    "css/ace.min.css",
    # Put any ace CSS plugins here
    
    'css/jquery.rcrumbs.min.css',
    
    #css_taa_less,
    css_taa, 
    output="generated_assets/taa-old.min.css"
)

css_ace_latest = Bundle(
    # Ace dependencies
    "ace-v1.3.3/css/bootstrap.min.css",
    "ace-v1.3.3/css/font-awesome.min.css",
    "ace-v1.3.3/css/ace-fonts.min.css",
    
    # Put any 3rd party CSS plugins here (chosen, ui.jqgrid, etc)
    "ace-v1.3.3/css/bootstrap-multiselect.min.css",
    "ace-v1.3.3/css/bootstrap-duallistbox.min.css",
    Bundle('ace-v1.3.3/js/dataTables/dataTables.responsive.css', filters='cssmin'),
    
    # Main ace files 
    "ace-v1.3.3/css/ace.min.css",
    "ace-v1.3.3/css/ace-part2.min.css",

    'css/jquery.rcrumbs.min.css',

    css_taa,
    output="generated_assets/taa.min.css"
)

js_header = Bundle(
    "js/ace-extra.min.js",
)

# vendor js bundle
# (jquery is included directly on the page to allow CDN delivery or IE detection)
js_vendor = Bundle(

    # ace template requires bootstrap first
    Bundle('js/bootstrap.js', filters='rjsmin'),
    
    # js ace plugins for wizard and such go here
    'js/jquery.dataTables.min.js',
    'js/jquery.dataTables.bootstrap.js',
    'js/jquery.maskedinput.min.js',
    'js/jquery.validate.min.js',
    'js/fuelux/fuelux.wizard.min.js',
    'js/jquery.rcrumbs.min.js',
    
    # The rest of ace template libs
    'js/ace-elements.min.js',
    'js/ace.min.js',
    'js/additional-methods.js',
    
    # 3rd party libs
    'js/moment.min.js',
    
    'js/bootbox.min.js',
    'js/jquery.mobile.custom.min.js',
    'js/typeahead-bs2.min.js',
    knockout_js,
    'js/underscore-min.js',
    
    #output='generated_assets/js_vendor_old.min.js',
    
)

js_vendor_latest = Bundle(
    
    # ace template requires bootstrap first
    Bundle('ace-v1.3.3/js/bootstrap.js', filters='rjsmin'),
    
    # js ace plugins for wizard and such go here
    'ace-v1.3.3/js/dataTables/jquery.dataTables.min.js',
    'ace-v1.3.3/js/dataTables/jquery.dataTables.bootstrap.min.js',
    Bundle('ace-v1.3.3/js/dataTables/dataTables.responsive.js', filters='rjsmin', 
           #output='generated_assets/datatables.responsive.min.js'
    ),
    
    'ace-v1.3.3/js/jquery.maskedinput.min.js',
    'ace-v1.3.3/js/jquery.validate.min.js',
    'ace-v1.3.3/js/jquery.bootstrap-duallistbox.min.js',
    'ace-v1.3.3/js/fuelux/fuelux.wizard.min.js',
    'ace-v1.3.3/js/jquery.mobile.custom.min.js',
    # This file doesn't seem to be minified
    Bundle('ace-v1.3.3/js/typeahead.jquery.min.js', filters='rjsmin'),
    'ace-v1.3.3/js/bootstrap-multiselect.min.js',
    
    # The rest of ace template libs
    'ace-v1.3.3/js/ace-elements.min.js',
    'ace-v1.3.3/js/ace.min.js',
    'ace-v1.3.3/js/additional-methods.min.js',
    
    # 3rd party libs
    'js/moment.min.js',
    'js/bootbox.min.js',
    'js/jquery.rcrumbs.min.js',

    knockout_js,
    'js/underscore-min.js',
    'js/sammy-latest.min.js',
    output='generated_assets/js_vendor_latest.min.js',
)


# application js bundle
#js_main = Bundle("coffee/*.coffee", filters="coffeescript", output="js/main.js")
js_main = Bundle(
    js_vendor,
    Bundle("js/5Star/*.js", 
           filters="rjsmin", 
           output='generated_assets/app.min.js'),
    output='generated_assets/all_js_old.min.js'
)
taa_app = Bundle("js/5Star/*.js", filters="rjsmin", output='generated_assets/taa_app.min.js')


def init_app(app):
    webassets = Environment(app)
    webassets.register('css_all', css_all)
    webassets.register('js_main', js_main)
    
    webassets.register('css_ace_latest', css_ace_latest)
    webassets.register('js_vendor_latest', js_vendor_latest)
    webassets.register('taa_app', taa_app)

    # Webassets debug is defined in the global config and automatically pulled in
    # These settings control when and how the output files are generated 
    webassets.manifest = 'cache' #if not app.debug else False
    webassets.cache = True # not app.debug
    webassets.auto_build = True
    
    
    return webassets