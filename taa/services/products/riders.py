from StringIO import StringIO

import yaml


class Rider(object):
    def __init__(self, name, code, enrollment_level=False):
        self.name = name
        self.code = code
        self.enrollment_level = enrollment_level

    def to_json(self):
        return dict(
                name=self.name,
                code=self.code,
                enrollment_level=self.enrollment_level,
                restrict_to=[],
                )


class RiderService(object):
    default_riders = [
        Rider("Disability Waiver of Premium", "WP"),
        Rider("Automatic Increase Rider", "AIR", True),
        Rider("Quality of Life Rider 3%", "QOL3", True),
        Rider("Quality of Life Rider 4%", "QOL4", True),

    ]

    def __init__(self):
        pass

    def valid_rider_code(self, code):
        return code in [r.code for r in self.default_riders]

    def get_rider_by_code(self, product_code, code):
        return [r for r in self.default_riders if r.code==code][0]

    def case_level_riders(self):
        return [r for r in self.default_riders if not r.enrollment_level]

    def enrollment_level_riders(self):
        return [r for r in self.default_riders if r.enrollment_level]

    def get_rider_info_for_case(self, case):
        """Returns all the riders that a case can potentially have at the group level, with current selections."""
        return [{
                'selected': self.is_rider_selected_for_case(rider, case),
                'description': rider.name,
                'code': rider.code,
                'enrollment_level': rider.enrollment_level,
                #'restrict_to': rider.restrict_to
                }
                for rider in self.default_riders
        ]

    def is_rider_selected_for_case(self, rider, case):
        return case.case_riders and rider.code in case.case_riders.split(",")

    def get_selected_case_riders(self, case):
        return [r
                for r in self.default_riders
                if self.is_rider_selected_for_case(r, case)
        ]

    def get_selected_case_rider_info(self, case):
        return [r.to_json() for r in self.get_selected_case_riders(case)]

    def get_enrollment_rider_info(self):
        return [r.to_json() for r in self.enrollment_level_riders()]
    def get_rider_rates(self, payment_mode):
        emp_rider_rates = dict(
            WP=10*int(payment_mode)/52,
            AIR=0*int(payment_mode)/52,
            CHR=5*int(payment_mode)/52
            )
        sp_rider_rates = dict(
            WP=10*int(payment_mode)/52,
            AIR=0*int(payment_mode)/52,
            CHR=5*int(payment_mode)/52
            )
        return dict(emp=emp_rider_rates, sp=sp_rider_rates)


class RiderConfiguration(object):
    def __init__(self, product_code):
        self.product = product_code

    def check_compatibility(self, riders):
        return RiderCompatibility(self.product, riders)


rider_config_fpp = """
---
- name: Waiver of Premium"
  code: "WP"
  user_facing_name: "Waiver of Premium Benefit"
  compatibility_rules:
    - message: "Waiver of Premium Benefit cannot be combined with Auto Increase Rider Benefit."
      triggered_if_included_riders: ["AIR"]

- name: Automatic Increase Rider
  code: "AIR"
  user_facing_name: "Disability Automatic Increase Benefit"
  compatibility_rules:
    - message: "Auto Increase Rider cannot be combined with Waiver of Premium"
      triggered_if_included_riders: ["WP"]

- name: "Quality of Life 3%"
  code: "QOL3"
  user_facing_name: "Quality of Life Benefit"

- name: "Quality of Life 4%"
  code: "QOL4"
  user_facing_name: "Quality of Life Benefit"
"""

rider_config_fpp_gov = """
---
- name: "Waiver of Premium"
  code: "WP"
  user_facing_name: "Waiver of Premium Benefit"

- name: "Quality of Life 3%"
  code: "QOL3"
  user_facing_name: "Quality of Life Benefit"
  compatibility_rules:
    - message: "Quality of Life Benefit cannot be combined with Waiver of Premium"
      triggered_if_included_riders: ["WP"]

- name: "Quality of Life 4%"
  code: "QOL4"
  user_facing_name: "Quality of Life Benefit"
  compatibility_rules:
    - message: "Quality of Life Benefit cannot be combined with Waiver of Premium"
      triggered_if_included_riders: ["WP"]
"""

rider_config_ci = """
---
- name: "Waiver of Premium"
  code: "WP"
  user_facing_name: "Waiver of Premium Benefit"

"""

class RiderCompatibility(object):
    def __init__(self, product_code, riders):
        self.product_code = product_code
        self.riders = riders

        self.error_messages = []
        self.incompatible_riders = []

    def is_compatible(self):
        # Make sure each rider is in the allowed riders for this product

        if self.product_code == "Group CI":
            self.add_error("No riders allowed for Group CI")
            return False
        elif self.product_code == "FPP-Gov":
            rider_config_yaml = StringIO(rider_config_fpp_gov)
            config = yaml.load(rider_config_yaml)
        elif self.product_code in ['FPPTI', 'FPPTIW', 'FPPTIY', 'FPPTIB']:
            rider_config_yaml = StringIO(rider_config_fpp)
            config = yaml.load(rider_config_yaml)
        elif self.product_code in ['FPPCI']:
            rider_config_yaml = StringIO(rider_config_ci)
            config = yaml.load(rider_config_yaml)
        else:
            raise ValueError("Riders not configured for base product {}".format(self.product_code))

        # Check for invalid riders on this product.
        for requested_rider in self.riders:
            configured_rider_matches = [rider for rider in config if rider['code'].upper() == requested_rider.code.upper()]
            if not configured_rider_matches:
                self.add_error("Rider '{}' is not available for this product".format(requested_rider.code))
                self.incompatible_riders.append(requested_rider.code)
                continue

            # Check for incompatible requested riders
            configured_rider = configured_rider_matches[0]
            if 'compatibility_rules' in configured_rider:
                for rule in configured_rider['compatibility_rules']:
                    for invalid_combo_rider in rule['triggered_if_included_riders']:
                        if invalid_combo_rider in self.requested_rider_codes():
                            self.add_error(rule['message'])
                            if invalid_combo_rider not in self.incompatible_riders:
                                self.incompatible_riders.append(invalid_combo_rider)
                            if requested_rider.code not in self.incompatible_riders:
                                self.incompatible_riders.append(requested_rider.code)

        return not self.error_messages and not self.incompatible_riders

    def get_incompatible_riders(self):
        return self.incompatible_riders

    def add_error(self, message):
        self.error_messages.append(message)

    def requested_rider_codes(self):
        return [r.code for r in self.riders]
