

class EnrollmentImportService(object):
    def submit_file_records(self, records):

        return EnrollmentImportResponse()

class EnrollmentImportResponse(object):
    def is_success(self):
        return True

    def is_error(self):
        return False

    def get_errors(self):
        return []

class EnrollmentImportError(object):
    def get_type(self):
        raise NotImplementedError

    def get_field(self):
        """
        returns a list of column names that this error refers to.
        """
        return []

    def get_message(self):
        return "An error occurred"