from StringIO import StringIO

import yaml


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


class RiderService(object):
    # default_riders = [
    #     Rider("Disability Waiver of Premium", "WP"),
    #     Rider("Automatic Increase Rider", "AIR", True),
    #     Rider("Quality of Life Rider 3%", "QOL3", True),
    #     Rider("Quality of Life Rider 4%", "QOL4", True),
    #
    # ]

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

    def case_level_riders(self, base_product_code):
        return [r for r in self.get_riders_for_product(base_product_code) if not r.enrollment_level]

    def enrollment_level_riders(self, base_product_code):
        return [r for r in self.get_riders_for_product(base_product_code) if r.enrollment_level]

    # def get_rider_info_for_case(self, case):
    #     """Returns all the riders that a case can potentially have at the group level, with current selections."""
    #     return [{
    #             'selected': self.is_rider_selected_for_case(rider, case),
    #             'description': rider.name,
    #             'code': rider.code,
    #             'enrollment_level': rider.enrollment_level,
    #             #'restrict_to': rider.restrict_to
    #             }
    #             for rider in self.default_riders
    #     ]
    #
    # def is_rider_selected_for_case(self, rider, case):
    #     return case.case_riders and rider.code in case.case_riders.split(",")
    #
    # def get_selected_case_riders(self, case):
    #     return [r
    #             for r in self.default_riders
    #             if self.is_rider_selected_for_case(r, case)
    #     ]
    #
    # def get_selected_case_rider_info(self, case):
    #     return [r.to_json() for r in self.get_selected_case_riders(case)]
    #
    # def get_enrollment_rider_info(self):
    #     return [r.to_json() for r in self.enrollment_level_riders()]
    #
    # def get_rider_rates(self, payment_mode):
    #     emp_rider_rates = dict(
    #         WP=10*int(payment_mode)/52,
    #         AIR=0*int(payment_mode)/52,
    #         CHR=5*int(payment_mode)/52
    #         )
    #     sp_rider_rates = dict(
    #         WP=10*int(payment_mode)/52,
    #         AIR=0*int(payment_mode)/52,
    #         CHR=5*int(payment_mode)/52
    #         )
    #     return dict(emp=emp_rider_rates, sp=sp_rider_rates)


class RiderConfiguration(object):
    def __init__(self, product_code):
        self.product_code = product_code

    def load_rider_configuration(self):
        code_map = {
            'Group CI': StringIO(rider_config_group_ci),
            'FPP-Gov': StringIO(rider_config_fpp_gov),
            'FPPCI': StringIO(rider_config_ci),
            # These all share the same config.
            'FPPTI': StringIO(rider_config_fpp),
            'FPPTIW': StringIO(rider_config_fpp),
            'FPPTIY': StringIO(rider_config_fpp),
            'FPPTIB': StringIO(rider_config_fpp),
            'ACC': StringIO(rider_config_fpp),
            'HI': StringIO(rider_config_fpp),
            # This is just so we can serialize products in the process of being created.
            '': StringIO(rider_config_fpp),
        }
        rider_config_yaml = code_map.get(self.product_code)
        if not rider_config_yaml:
            raise ValueError("Riders not configured for base product {}".format(self.product_code))

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
  is_group_level: false
  user_facing_name: "Waiver of Premium Benefit"
  compatibility_rules:
    - message: "Waiver of Premium Benefit cannot be combined with Auto Increase Rider Benefit."
      triggered_if_included_riders: ["AIR"]

- name: Automatic Increase Rider
  code: "AIR"
  user_facing_name: "Disability Automatic Increase Benefit"
  is_group_level: true
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
  is_group_level: false
  compatibility_rules:
    - message: "Quality of Life Benefit cannot be combined with Waiver of Premium"
      triggered_if_included_riders: ["QOL3", "QOL4"]

- name: "Quality of Life 3%"
  code: "QOL3"
  user_facing_name: "Quality of Life Benefit"
  is_group_level: true
  compatibility_rules:
    - message: "Quality of Life Benefit cannot be combined with Waiver of Premium"
      triggered_if_included_riders: ["WP"]
    - message: "Only one QOL rider can be used."
      triggered_if_included_riders: ["QOL4"]

- name: "Quality of Life 4%"
  code: "QOL4"
  user_facing_name: "Quality of Life Benefit"
  is_group_level: true
  compatibility_rules:
    - message: "Quality of Life Benefit cannot be combined with Waiver of Premium"
      triggered_if_included_riders: ["WP"]
    - message: "Only one QOL rider can be used."
      triggered_if_included_riders: ["QOL3"]
"""

rider_config_ci = """
---
- name: "Waiver of Premium"
  code: "WP"
  is_group_level: false
  user_facing_name: "Waiver of Premium Benefit"

"""
rider_config_group_ci = """
---
# No riders
[]
"""


class RiderCompatibility(object):
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
                self.add_error("Rider '{}' is not available for this product".format(requested_rider))
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
