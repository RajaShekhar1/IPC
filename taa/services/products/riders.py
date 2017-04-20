from StringIO import StringIO

import yaml
PLAN_RIDER_AIR = 'AIR'
PLAN_RIDER_WP = 'WP'
PLAN_RIDER_QOL3 = 'QOL3'
PLAN_RIDER_QOL4 = 'QOL4'
PLAN_RIDERS = [PLAN_RIDER_AIR, PLAN_RIDER_WP, PLAN_RIDER_QOL3, PLAN_RIDER_QOL4]
PLAN_RIDERS_QOL = [PLAN_RIDER_QOL3, PLAN_RIDER_QOL4]



class RiderService(object):
    def __init__(self):
        pass

    def get_riders_for_product(self, product):
        # Right now, available riders are based solely on the base product ID.
        return RiderConfiguration(product.get_base_product_code()).get_riders()

    def valid_rider_code(self, base_product_code, code):
        return code in [r.code for r in self.get_riders_for_product(base_product_code)]

    def get_rider_by_code(self, base_product_code, code):
        matching = [r for r in self.get_riders_for_product(base_product_code) if r.code == code]
        if not matching:
            return None
        else:
            return matching[0]

    def get_import_rider_codes(self):
        # These will match what is on the form(s) for importable products.
        return PLAN_RIDERS

    def get_case_level_riders_for_product(self, case, product):
        # Case-level riders are in the product config.
        riders = []

        settings = case.product_settings
        if not settings:
            return riders

        rider_configuration = RiderConfiguration(product.get_base_product_code())
        all_product_riders = rider_configuration.get_riders()

        for rider_setting in settings.get('riders', []):
            if rider_setting.get('product_id') is not product.id:
                continue

            if rider_setting.get('is_selected'):
                matching_rider = next((rider for rider in all_product_riders if rider.code == rider_setting.get('rider_code')), None)
                if matching_rider.is_group_level:
                    riders.append(matching_rider)

        return riders

    def get_applicant_level_riders_for_product(self, applicant, case, product):
        riders = []

        settings = applicant.get('selected_riders', [])
        if not settings:
            return riders

        rider_configuration = RiderConfiguration(product.get_base_product_code())
        all_product_riders = rider_configuration.get_riders()

        for rider_setting in settings:
            matching_rider = next((rider for rider in all_product_riders
                                   if rider.code == rider_setting.get('code')), None)
            if not matching_rider.is_group_level:
                riders.append(matching_rider)

        return riders

class RiderConfiguration(object):
    def __init__(self, product_code):
        self.product_code = product_code

    def load_rider_configuration(self):
        code_map = {
            'Group CI': StringIO(no_riders),
            'FPP-Gov': StringIO(rider_config_fpp_gov),
            'FPPCI': StringIO(rider_config_ci),
            # These all share the same config.
            'FPPTI': StringIO(rider_config_fpp),
            'FPPTIW': StringIO(rider_config_fpp),
            'FPPTIY': StringIO(rider_config_fpp),
            'FPPTIB': StringIO(rider_config_fpp),
            # These don't have riders
            'ACC': StringIO(no_riders),
            'HI': StringIO(no_riders),
            'HIL01': StringIO(no_riders),
            'Static Benefit': StringIO(no_riders),
            # This is just so we can serialize products in the process of being created.
            '': StringIO(rider_config_fpp),
        }
        rider_config_yaml = code_map.get(self.product_code)
        if not rider_config_yaml:
            return []
            raise ValueError(u"Riders not configured for base product {}".format(self.product_code))

        return yaml.load(rider_config_yaml)

    def get_riders(self):
        # Load the rider objects.
        return [Rider(**rider_config) for rider_config in self.load_rider_configuration()]

    def check_compatibility(self, rider_codes):
        return RiderCompatibility(self.product_code, rider_codes)


rider_config_fpp = """
---
- name: Waiver of Premium
  code: "WP"
  is_group_level: true
  user_facing_name: "Waiver of Premium Benefit"
  compatibility_rules:
    - message: "Waiver of Premium Benefit cannot be combined with Auto Increase Rider Benefit."
      triggered_if_included_riders: ["AIR"]

- name: Automatic Increase Rider
  code: "AIR"
  user_facing_name: "Automatic Increase Benefit"
  is_group_level: false
  compatibility_rules:
    - message: "Auto Increase Rider cannot be combined with Waiver of Premium"
      triggered_if_included_riders: ["WP"]

- name: "Quality of Life 3%"
  code: "QOL3"
  is_group_level: true
  user_facing_name: "Quality of Life Benefit"
  compatibility_rules:
    - message: "Only one QOL rider can be used."
      triggered_if_included_riders: ["QOL4"]

- name: "Quality of Life 4%"
  code: "QOL4"
  is_group_level: true
  user_facing_name: "Quality of Life Benefit"
  compatibility_rules:
    - message: "Only one QOL rider can be used."
      triggered_if_included_riders: ["QOL3"]
"""

rider_config_fpp_gov = """
---
- name: "Waiver of Premium"
  code: "WP"
  user_facing_name: "Waiver of Premium Benefit"
  is_group_level: true

- name: "Quality of Life 3%"
  code: "QOL3"
  user_facing_name: "Quality of Life Benefit"
  is_group_level: true
  compatibility_rules:
    - message: "Only one QOL rider can be used."
      triggered_if_included_riders: ["QOL4"]

- name: "Quality of Life 4%"
  code: "QOL4"
  user_facing_name: "Quality of Life Benefit"
  is_group_level: true
  compatibility_rules:
    - message: "Only one QOL rider can be used."
      triggered_if_included_riders: ["QOL3"]
"""

rider_config_ci = """
---
# No riders
[]
# - name: "Waiver of Premium"
#   code: "WP"
#   is_group_level: true
#   user_facing_name: "Waiver of Premium Benefit"

"""

rider_config_group_ci = """
---
# No riders
[]
"""

no_riders = """
---
# No riders
[]
"""


class Rider(object):
    def __init__(self, name, user_facing_name, code, is_group_level=False, compatibility_rules=None):
        self.name = name
        self.user_facing_name = user_facing_name
        self.code = code
        self.is_group_level = is_group_level
        self.compatibility_rules = compatibility_rules

    def get_restricted_combinations(self):
        if not self.compatibility_rules:
            return []

        combinations = set()
        for rule in self.compatibility_rules:
            if 'triggered_if_included_riders' in rule:
                for rider_code in rule['triggered_if_included_riders']:
                    combinations.add(rider_code)

        return list(combinations)

    def to_json(self):
        return dict(
            name=self.name,
            code=self.code,
            user_facing_name=self.user_facing_name,
            is_group_level=self.is_group_level,
            disallowed_rider_combinations=self.get_restricted_combinations(),
        )


class RiderCompatibility(object):
    """
    Determines if two riders are allowed to be combined with each other on a case.
    """
    def __init__(self, product_code, requested_rider_codes):
        self.product_code = product_code
        self.requested_rider_codes = requested_rider_codes

        self.error_messages = []
        self.incompatible_riders = []

    def is_compatible(self):
        # Make sure each rider is in the allowed riders for this product
        config = self.load_rider_config()

        for requested_rider in self.requested_rider_codes:
            configured_rider = self.get_rider_from_config(config, requested_rider)

            # Check for invalid rider for this product.
            if not configured_rider:
                self.add_error(u"Rider '{}' is not available for this product".format(requested_rider))
                self.incompatible_riders.append(requested_rider)
                continue

            self.check_compatibility(configured_rider, requested_rider)

        return not self.error_messages and not self.incompatible_riders

    def check_compatibility(self, configured_rider, requested_rider_code):
        if 'compatibility_rules' not in configured_rider:
            return

        for rule in configured_rider['compatibility_rules']:
            for invalid_combo_rider in rule['triggered_if_included_riders']:
                if invalid_combo_rider in self.requested_rider_codes:
                    self.add_error(rule['message'])
                    if invalid_combo_rider not in self.incompatible_riders:
                        self.incompatible_riders.append(invalid_combo_rider)
                    if requested_rider_code not in self.incompatible_riders:
                        self.incompatible_riders.append(requested_rider_code)

    def get_rider_from_config(self, config, rider_code):
        matches = [rider for rider in config if rider['code'].upper() == rider_code.upper()]
        if not matches:
            return None
        else:
            return matches[0]

    def load_rider_config(self):
        return RiderConfiguration(self.product_code).load_rider_configuration()

    def get_incompatible_riders(self):
        return self.incompatible_riders

    def add_error(self, message):
        self.error_messages.append(message)

    def requested_rider_codes(self):
        return [r.code for r in self.requested_rider_codes]
