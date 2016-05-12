

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
services_broker = FeatureBroker(allowReplace=True)


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
        return self.Request()
        # Zach - don't cache this, just do the dict lookup each time so we can swap service implementations globally
        #  for tests
        #self.result = self.Request()
        #return self.result

    def Request(self):
        obj = services_broker[self.feature]
        assert self.assertion(obj), \
            "The value %r of %r does not match the specified criteria" \
            % (obj, self.feature)
        return obj


class ServiceProxy(object):
    """
    Basic proxy for a service resolved using the broker. This forces the correct service to be resolved
     for every attribute access.
    """
    def __init__(self, service_name):
        object.__setattr__(self, 'service_name', service_name)

    def __getattribute__(self, name):
        return getattr(services_broker[object.__getattribute__(self, "service_name")], name)

    def __setattr__(self, key, value):
        return setattr(services_broker[object.__getattribute__(self, "service_name")], key, value)

    def __call__(self, *args, **kwargs):
        return services_broker[object.__getattribute__(self, "service_name")](*args, **kwargs)


def LookupService(service_name):
    if service_name not in services_broker:
        raise ValueError("Could not find service named '{0}'".format(service_name))
    return ServiceProxy(service_name)

def initialize_services():
    """
    Sets up the service providers for the TAA app. Will overwrite/reset to valid production services, useful for resetting tests.
    """

    from taa.services.agents import AgentService, ApiTokenService
    from taa.services.cases import (
        CaseService,
        CaseEnrollmentPeriodsService,
        CensusRecordService,
        SelfEnrollmentService,
        AgentSplitsService
    )
    from taa.services.enrollments import (
        EnrollmentApplicationService,
        SelfEnrollmentEmailService,
        SelfEnrollmentLinkService,
        SelfEnrollmentEmailBatchService,
        EnrollmentImportService,
        EnrollmentImportBatchService,
        EnrollmentImportBatchItemService,
        EnrollmentApplicationCoverageService,
        EnrollmentReportService,
        EnrollmentSubmissionService,
        ImagedFormGeneratorService,
        FormPDFRenderer,
        FormTemplateTabRepository,
        merge_pdfs,
    )
    from taa.services.enrollments import EnrollmentRecordParser
    import taa.services.mailer as MailerService
    from taa.services.products import (
        ProductService,
        ProductFormService,
        StatementOfHealthQuestionService,
    )
    from taa.services.data_import import (
        FileImportService,
    )
    from taa.services.users import UserService
    from taa.services.docusign import DocuSignService, DocuSignTransport
    from taa.services.submissions import FtpService

    services_broker.Provide('CaseService', CaseService())
    services_broker.Provide('CaseEnrollmentPeriodsService', CaseEnrollmentPeriodsService())
    services_broker.Provide('CensusRecordService', CensusRecordService())
    services_broker.Provide('SelfEnrollmentService', SelfEnrollmentService())
    services_broker.Provide('AgentSplitsService', AgentSplitsService())

    services_broker.Provide('AgentService', AgentService())
    services_broker.Provide('ApiTokenService', ApiTokenService())

    services_broker.Provide('ProductService', ProductService())
    services_broker.Provide('ProductFormService', ProductFormService())
    services_broker.Provide('StatementOfHealthQuestionService', StatementOfHealthQuestionService())

    services_broker.Provide('EnrollmentApplicationService', EnrollmentApplicationService())
    services_broker.Provide('EnrollmentApplicationCoverageService', EnrollmentApplicationCoverageService())
    services_broker.Provide('EnrollmentImportService', EnrollmentImportService())
    services_broker.Provide('EnrollmentImportBatchService', EnrollmentImportBatchService())
    services_broker.Provide('EnrollmentImportBatchItemService', EnrollmentImportBatchItemService())
    services_broker.Provide('EnrollmentSubmissionService', EnrollmentSubmissionService())
    services_broker.Provide('EnrollmentRecordParser', lambda: EnrollmentRecordParser)
    services_broker.Provide('SelfEnrollmentEmailService', SelfEnrollmentEmailService())
    services_broker.Provide('SelfEnrollmentLinkService', SelfEnrollmentLinkService())
    services_broker.Provide('SelfEnrollmentEmailBatchService', SelfEnrollmentEmailBatchService())
    services_broker.Provide('EnrollmentReportService', EnrollmentReportService())
    services_broker.Provide('ImagedFormGeneratorService', ImagedFormGeneratorService())
    services_broker.Provide("FormPDFRenderer", lambda: FormPDFRenderer)
    services_broker.Provide("FormTemplateTabRepository", FormTemplateTabRepository())
    services_broker.Provide("merge_pdfs", lambda: merge_pdfs)

    services_broker.Provide('FileImportService', FileImportService())
    services_broker.Provide('UserService', UserService())

    services_broker.Provide('DocuSignService', DocuSignService())
    services_broker.Provide('DocuSignTransport', lambda: DocuSignTransport)

    services_broker.Provide('MailerService', MailerService)

    services_broker.Provide('FtpService', FtpService())