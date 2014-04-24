import csv
from datetime import datetime
from dateutil.parser import parse
from dateutil.relativedelta import relativedelta

def get_age_from_birthday(birthday_str):
    
    birth_date = parse(birthday_str)
    
    years = 0
    loop_date = datetime.today()
    
    loop_date += relativedelta(years=-1)
    while loop_date >= birth_date:
        years += 1
        loop_date += relativedelta(years=-1)
    
    return years

# Recommended rates - Good, Better, Best
def get_recommended_rates(gender, age_band, marital_status, include_spouse, num_children):
    
    pass


def get_weekly_premiums_by_coverage(age):
    premiums = []
    for coverage in weekly_coverage_options:
        premium = get_weekly_premium_by_coverage(age, coverage)
        if premium:
            premiums.append(dict(coverage=coverage, premium=premium))
    return premiums

def get_coverages_by_weekly_premium(age):
    coverages = []
    for weekly_premium in weekly_premium_options:
        coverage = get_coverage_by_weekly_premium(age, weekly_premium)
        if coverage:
            coverages.append(dict(premium=weekly_premium, coverage=coverage))
    
    return coverages


def get_weekly_child_premiums():
    return [
        {'premium': 1.15, 'coverage':10000},
        {'premium': 2.30, 'coverage': 20000},
    ]

def get_coverage_by_weekly_premium(age, weekly_rate):
    return weekly_by_premium_lookup.get((age, weekly_rate)) 

def get_weekly_premium_by_coverage(age, coverage_amount):
    return weekly_by_coverage_lookup.get((age, coverage_amount)) 


def build_byface_table():
    lines = [l for l in csv.reader(open("model/rates/FPPTI-byface.csv", 'rU'))]
    weekly_coverage_options = [int(o) for o in lines[0][1:]]
    ages = [int(r[0]) for r in lines[1:]]
    table = {}
    for i, age in enumerate(ages):
        for j, coverage in enumerate(weekly_coverage_options):
            val = lines[1:][i][j+1]
            table[(age, int(coverage))] = float(val) if val.strip() != "" else None
        
    return table, weekly_coverage_options

def build_bypremium_table():
    lines = [l for l in csv.reader(open("model/rates/FPPTI-bypremium.csv", 'rU'))]
    weekly_premium_options = [int(o) for o in lines[0][1:]]
    ages = [int(r[0]) for r in lines[1:]]
    table = {}
    for i, age in enumerate(ages):
        for j, premium in enumerate(weekly_premium_options):
            val = lines[1:][i][j+1]
            table[(age, int(premium))] = int(val) if val.strip() != "" else None
    
    return table, weekly_premium_options

# Global lookup tables for now
weekly_by_premium_lookup, weekly_premium_options = build_bypremium_table()
weekly_by_coverage_lookup, weekly_coverage_options = build_byface_table()
