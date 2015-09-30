#!/usr/bin/env python

import csv


policy_fee = 52

acpt = {
        'FPPTIW': (3.730, 3.730, 3.730, 3.730, 3.730, 3.730, 3.730, 3.730,
            3.750, 3.820, 3.930, 4.090, 4.300, 4.530, 4.790, 5.070, 5.390,
            5.740, 6.130, 6.550, 7.000, 7.490, 8.030, 8.640, 9.300, 9.990,
            10.710, 11.470, 12.250, 13.060, 13.890, 14.780, 15.760, 16.870,
            18.150, 19.590, 21.200, 22.920, 24.750, 26.640, 28.580, 30.580,
            32.670, 34.850, 37.140, 39.590, 42.260, 45.270, 48.710, 52.700,
            57.240, 62.390, 68.200),
        'FPPTIY': (4.040, 4.040, 4.040, 4.040, 4.040, 4.040, 4.040, 4.040,
            4.070, 4.130, 4.260, 4.430, 4.650, 4.910, 5.190, 5.500, 5.840,
            6.210, 6.640, 7.100, 7.580, 8.110, 8.700, 9.360, 10.080, 10.830,
            11.610, 12.430, 13.270, 14.140, 15.050, 16.020, 17.070, 18.280,
            19.670, 21.230, 22.970, 24.830, 26.820, 28.860, 30.970, 33.120,
            35.400, 37.750, 40.240, 42.890, 45.790, 49.050, 52.770, 57.100,
            62.010, 67.590, 73.880),
        'FPPTIB': (4.670, 4.670, 4.670, 4.670, 4.670, 4.670, 4.670, 4.670,
            4.700, 4.770, 4.920, 5.120, 5.370, 5.670, 5.990, 6.350, 6.740,
            7.170, 7.670, 8.190, 8.750, 9.360, 10.040, 10.800, 11.630, 12.500,
            13.400, 14.340, 15.320, 16.320, 17.370, 18.480, 19.700, 21.090,
            22.700, 24.500, 26.510, 28.650, 30.950, 33.300, 35.730, 38.220,
            40.850, 43.560, 46.430, 49.490, 52.830, 56.600, 60.890, 65.880,
            71.550, 77.990, 85.250),
        }

premium_map = {
        12: (10, 15, 20, 25, 30, 35, 40, 45, 50),
        24: (5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5),
        52: (3, 4, 5, 6, 7, 8, 9, 10),
        26: (6, 8, 10, 12, 14, 16, 18),
        }

face_map = {
        12: (10000, 20000, 25000, 30000, 40000, 50000, 60000, 70000, 75000,
            80000, 90000, 100000, 110000, 125000, 130000, 140000, 150000),
        24: (10000, 20000, 25000, 30000, 40000, 50000, 60000, 70000, 75000,
            80000, 90000, 100000, 110000, 125000, 130000, 140000, 150000),
        52: (10000, 20000, 25000, 30000, 40000, 50000, 60000, 70000, 75000,
            80000, 90000, 100000, 110000, 125000, 130000, 140000, 150000),
        26: (10000, 20000, 25000, 30000, 40000, 50000, 60000, 70000, 75000,
            80000, 90000, 100000, 110000, 125000, 130000, 140000, 150000),
        }

mode_map = {
        12: 'monthly',
        24: 'semimonthly',
        52: 'weekly',
        26: 'biweekly',
        }


def create_by_premium():
    for plan in iter(acpt.keys()):
        for mode, amounts in iter(face_map.items()):
            with open('{}-byface-{}.csv'.format(plan, mode_map[mode]), 'w') as f:
                f.write(',' + ','.join(map(str, amounts)) + '\n')
                writer = csv.writer(f)
                for age in range(18, 71):
                    values = [age]
                    for amount in amounts:
                        value = round(round(amount / 1000 * acpt[plan][age - 18] + policy_fee, 2) / mode, 2)
                        values.append('{:.2f}'.format(value))
                    writer.writerow(values)


def create_by_coverage():
    for plan in iter(acpt.keys()):
        for mode, amounts in iter(premium_map.items()):
            with open('{}-bypremium-{}.csv'.format(plan, mode_map[mode]), 'w') as f:
                f.write(',' + ','.join(map(str, amounts)) + '\n')
                writer = csv.writer(f)
                for age in range(18, 71):
                    values = [age]
                    for amount in amounts:
                        value = round((((amount * mode) - policy_fee) * 1000) / acpt[plan][age - 18], 0)
                        if 5000 <= value <= 150000:
                            values.append('{:.0f}'.format(value))
                        else:
                            values.append('')
                    writer.writerow(values)


def main():
    create_by_premium()
    create_by_coverage()


if __name__ == '__main__':
    main()
