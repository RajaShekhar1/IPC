import gnupg

from taa import config_defaults


class PGPEncryptionService(object):
    def __init__(self):
        self._gpg = None

    def get_dell_key(self):
        return PGPEncryptionKey(config_defaults.DELL_PGP_KEY, config_defaults.DELL_FTP_PGP_KEY_ID)
    
    def get_paylogix_key(self):
        return PGPEncryptionKey(config_defaults.PAYLOGIX_PGP_KEY, config_defaults.PAYLOGIX_PGP_KEY_ID)
    
    def encrypt(self, data, key):
        # Add our known keys before attempting encryption
        self._pgp_add_key(self.get_dell_key().key)
        self._pgp_add_key(self.get_paylogix_key().key)
        
        self._init_gpg()
        
        return unicode(self._gpg.encrypt(data, key.key_id, always_trust=True))

    def _pgp_add_key(self, key):
        
        self._init_gpg()
        import_result = self._gpg.import_keys(key)

        if len(import_result.results) == 0 or not all(
                (r.get('ok') in import_result.ok_reason.keys()
                 for r in import_result.results)):
            raise ValueError("Invalid key data")

    def _init_gpg(self):
        if not self._gpg:
            self._gpg = gnupg.GPG()
    

class PGPEncryptionKey(object):
    "Small wrapper class that contains the PGP key data."
    def __init__(self, key, key_id):
        self.key = key
        self.key_id = key_id
