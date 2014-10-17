
from flask_assets import Environment, Bundle

# application css bundle
#css_taa_less = Bundle("less/*.less",
#                       filters="less", output="css/taa.css",
#                       debug=False)

css_taa = Bundle(
    "css/five_star_ace_overlay.css",
    "css/five_star.css",
    filters="cssmin",
)

# consolidated css bundle
css_all = Bundle(
                "css/bootstrap.min.css", 
                 "css/font-awesome.min.css",
                 Bundle("css/ace-fonts.css", filters="cssmin"),
                 "css/ace.min.css",
                 # Put any ace CSS plugins here
                 
                 #css_taa_less,
                 css_taa, 
                 output="generated_assets/taa.min.css"
)

js_header = Bundle(
    "js/ace-extra.min.js",
)

# vendor js bundle
# (jquery is included directly on the page to allow CDN delivery or IE detection)
js_vendor = Bundle(

    #Bundle('js/jquery-1.11.1.js', filters='rjsmin'),
    # ace template requires bootstrap first
    'js/bootstrap.min.js',
    
    # js ace plugins for wizard and such go here
    'js/jquery.dataTables.min.js',
    'js/jquery.dataTables.bootstrap.js',
    
    # The rest of ace template libs
    'js/ace-elements.min.js',
    'js/ace.min.js',
    
    # 3rd party libs
    'js/moment.min.js',
    # TODO: detect debug and swap out production script
    Bundle('js/knockout-3.2.0.debug.js'),
    'js/sammy-latest.min.js',
)

# application js bundle
#js_main = Bundle("coffee/*.coffee", filters="coffeescript", output="js/main.js")
js_main = Bundle(
    js_vendor,
    Bundle("js/5Star/*.js", filters="rjsmin", output='generated_assets/taa.min.js'),
    output='generated_assets/taa.min.js'
)

def init_app(app):
    webassets = Environment(app)
    webassets.register('css_all', css_all)
    webassets.register('js_main', js_main)
    
    
    webassets.manifest = 'cache' #if not app.debug else False
    webassets.cache = True # not app.debug
    webassets.auto_build = True
    
    # Keep this separate for now
    #webassets.debug = app.debug
    
    return webassets