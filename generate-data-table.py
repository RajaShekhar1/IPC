#!/usr/bin/env python

"""
This utility generates realistic insurance premium rate data based off existing data from 5Star.

To create required dummy files as of 2015-04-27, issue the following commands from the TAA directory:

Group CI:
    By face, non-smoker:
        python generate-data-table.py 2 2 taa/services/products/data_files/CIEMP-rates---NonSmoking-Weekly.csv \
                taa/services/products/data_files/CIEMP-rates---NonSmoking-Biweekly.csv
        python generate-data-table.py 1/2 2 taa/services/products/data_files/CIEMP-rates---NonSmoking-Monthly.csv \
                taa/services/products/data_files/CIEMP-rates---NonSmoking-Semimonthly.csv
    By face, smoker:
        python generate-data-table.py 2 2 taa/services/products/data_files/CIEMP-rates---Smoking-Weekly.csv \
                taa/services/products/data_files/CIEMP-rates---Smoking-Biweekly.csv
        python generate-data-table.py 1/2 2 taa/services/products/data_files/CIEMP-rates---Smoking-Monthly.csv \
                taa/services/products/data_files/CIEMP-rates---Smoking-Semimonthly.csv

FPPCI:
    By face:
        python generate-data-table.py 2 2 taa/services/products/data_files/FPPCI-byface.csv \
                taa/services/products/data_files/FPPCI-byface-biweekly.csv
        python generate-data-table.py 52/24 2 taa/services/products/data_files/FPPCI-byface.csv \
                taa/services/products/data_files/FPPCI-byface-semimonthly.csv
        python generate-data-table.py 52/12 2 taa/services/products/data_files/FPPCI-byface.csv \
                taa/services/products/data_files/FPPCI-byface-monthly.csv

    By premium:
        python generate-data-table.py 2 0 taa/services/products/data_files/FPPCI-bypremium.csv \
                taa/services/products/data_files/FPPCI-bypremium-biweekly.csv
        python generate-data-table.py 52/24 0 taa/services/products/data_files/FPPCI-bypremium.csv \
                taa/services/products/data_files/FPPCI-bypremium-semimonthly.csv
        python generate-data-table.py 52/12 0 taa/services/products/data_files/FPPCI-bypremium.csv \
                taa/services/products/data_files/FPPCI-bypremium-monthly.csv

FPP-Gov:
    By face:
        python generate-data-table.py 2 2 taa/services/products/data_files/FPPGOV-byface.csv \
                taa/services/products/data_files/FPPGOV-byface-biweekly.csv
        python generate-data-table.py 52/24 2 taa/services/products/data_files/FPPGOV-byface.csv \
                taa/services/products/data_files/FPPGOV-byface-semimonthly.csv
        python generate-data-table.py 52/12 2 taa/services/products/data_files/FPPGOV-byface.csv \
                taa/services/products/data_files/FPPGOV-byface-monthly.csv

FPPTI:
    By face:
        python generate-data-table.py 2 2 taa/services/products/data_files/FPPTI-byface.csv \
                taa/services/products/data_files/FPPTI-byface-biweekly.csv
        python generate-data-table.py 52/24 2 taa/services/products/data_files/FPPTI-byface.csv \
                taa/services/products/data_files/FPPTI-byface-semimonthly.csv
        python generate-data-table.py 52/12 2 taa/services/products/data_files/FPPTI-byface.csv \
                taa/services/products/data_files/FPPTI-byface-monthly.csv

    By premium:
        python generate-data-table.py 2 0 taa/services/products/data_files/FPPTI-bypremium.csv \
                taa/services/products/data_files/FPPTI-bypremium-biweekly.csv
        python generate-data-table.py 52/24 0 taa/services/products/data_files/FPPTI-bypremium.csv \
                taa/services/products/data_files/FPPTI-bypremium-semimonthly.csv
        python generate-data-table.py 52/12 0 taa/services/products/data_files/FPPTI-bypremium.csv \
                taa/services/products/data_files/FPPTI-bypremium-monthly.csv
    """

from __future__ import division

import csv
import sys
from fractions import Fraction


def try_multiply(s, ratio, decimals=0):
    try:
        if decimals == 0:
            f = int
        else:
            f = float
        return f(round(ratio * f(s), decimals))
    except ValueError:
        return s

def main(ratio, decimals, infn, outfn):
    ratio = Fraction(ratio)
    decimals = int(decimals)
    with open(infn, 'rU') as inf:
        with open(outfn, 'w') as outf:
            writer = csv.writer(outf)
            for line_num, line in enumerate(csv.reader(inf)):
                if line_num == 0:
                    writer.writerow(line)
                else:
                    writer.writerow([line[0]] + map(lambda x: try_multiply(x, ratio, decimals), line[1:]))


def usage(args):
    print("""
    Usage: python {} MULTIPLIER DIGITS INFILE OUTFILE

           Where:
           - MULTIPLIER: ratio to multiple data columns by (fractions okay)
               Examples:
               - 2 for weekly -> biweekly
               - 52/24 for weekly -> semimonthly
               - 52/12 for weekly -> monthly
               - 1/2 for monthly -> semimonthly
           - DIGITS: number of digits after the decimal point
                     final data should have (usually '0' or '2')
           - INFILE: path to read
           - OUTFILE: path to write
           """.format(args[0]))
    sys.exit(1)


if __name__ == '__main__':
    if len(sys.argv) == 5:
        main(*sys.argv[1:])
    else:
        usage(sys.argv)
