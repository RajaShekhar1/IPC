import gnupg

from taa import config_defaults


class PGPEncryptionService(object):


    def __init__(self):
        self.__gpg = gnupg.GPG(binary=config_defaults.GNUPG_DIR)

    def get_dell_key(self):
        return PGPEncryptionKey(config_defaults.DELL_PGP_KEY, config_defaults.DELL_FTP_PGP_KEY_ID)
    
    def get_paylogix_key(self):
        return PGPEncryptionKey(config_defaults.PAYLOGIX_PGP_KEY, config_defaults.PAYLOGIX_PGP_KEY_ID)
    
    def encrypt(self, data, key):
        # Add our known keys before attempting encryption
        self._pgp_add_key(self.get_dell_key().key)
        self._pgp_add_key(self.get_paylogix_key().key)
        
        return unicode(self.__gpg.encrypt(data, key.key_id))

    def _pgp_add_key(self, key):
        import_result = self.__gpg.import_keys(key)

        if len(import_result.results) == 0 or not all(
                (r.get('status').strip() in import_result._ok_reason.values()
                 for r in import_result.results)):
            raise ValueError("Invalid key data")


class PGPEncryptionKey(object):
    "Small wrapper class that contains the PGP key data."
    def __init__(self, key, key_id):
        self.key = key
        self.key_id = key_id
