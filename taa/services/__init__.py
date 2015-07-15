# A basic IoC service-locator-like pattern from
#  http://code.activestate.com/recipes/413268-dependency-injection-the-python-way/
class FeatureBroker:
    def __init__(self, allowReplace=False):
        self.providers = {}
        self.allowReplace = allowReplace

    def Provide(self, feature, provider, *args, **kwargs):
        if not self.allowReplace:
            assert not self.providers.has_key(feature), "Duplicate feature: %r" % feature
        if callable(provider):
            def call(): return provider(*args, **kwargs)
        else:
            def call(): return provider
        self.providers[feature] = call

    def __getitem__(self, feature):
        try:
            provider = self.providers[feature]
        except KeyError:
            raise KeyError, "Unknown feature named %r" % feature
        return provider()

    def __contains__(self, item):
        return item in self.providers

# Export the service locator
services = FeatureBroker(allowReplace=True)


# Assertions about services / features
def NoAssertion(obj): return True

def IsInstanceOf(*classes):
    def test(obj):
        return isinstance(obj, classes)
    return test

def HasAttributes(*attributes):
    def test(obj):
        for each in attributes:
            if not hasattr(obj, each): return False
        return True
    return test

def HasMethods(*methods):
    def test(obj):
        for each in methods:
            try:
                attr = getattr(obj, each)
            except AttributeError:
                return False
            if not callable(attr): return False
        return True
    return test

# A request to retrieve a service / feature, declaring assertions about that feature.
class RequiredFeature(object):
    def __init__(self, feature, assertion=NoAssertion):
        self.feature = feature
        self.assertion = assertion

    def __get__(self, obj, T):
        return self.result # <-- will request the feature upon first call

    def __getattr__(self, name):
        assert name == 'result', "Unexpected attribute request other then 'result'"
        self.result = self.Request()
        return self.result

    def Request(self):
        obj = services[self.feature]
        assert self.assertion(obj), \
            "The value %r of %r does not match the specified criteria" \
            % (obj, self.feature)
        return obj

def LookupService(service_name):
    if service_name not in services:
        raise ValueError("Could not find service named '{0}'".format(service_name))
    return services[service_name]
