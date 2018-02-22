# Gary Koplik
# Winter, 2018
# get_user_input.py

import csv

### security type specification

while True:
    security = raw_input("Name the Treasury security you want. \n\
    Remember it must be in the following form: \n \
      CMB:             0-WEEK \n \
      4-week T-bills:  4-WEEK \n \
      13-week T-bills: 13-WEEK \n \
      26-week T-bills: 26-WEEK \n \
      52-week T-bills: 52-WEEK \n \
      2-year notes:    2-YEAR \n \
      3-year notes:    3-YEAR \n \
      5-year notes:    5-YEAR \n \
      7-year notes:    7-YEAR \n \
      10-year notes:   10-YEAR \n \
      30-year bonds:   30-YEAR \n \
    Your Input: ")
    possible_securities = ["0-WEEK", "4-WEEK", "13-WEEK", "26-WEEK",
                            "52-WEEK", "2-YEAR", "3-YEAR", "5-YEAR",
                            "7-YEAR", "10-YEAR", "30-YEAR"]
    security = security.upper()
    if security in possible_securities:
        break
    else:
        print "\n ~~~You did not choose a viable security~~~ \n"

### TIPS specification

while True:
    tips = raw_input("Do you want TIPS auctions (Treasury Inflation Protected Securites)? \n \
    Your input (y / n): ")
    # needs to be upper case for later code
    tips = tips.upper()
    if tips in ["Y", "N"]:
        break
    else:
        print "\n ~~~You did not choose a y or n~~~ \n"

### FRN specification

while True:
    frn = raw_input("Do you want FRN auctions (Floating Rate Note)? \n \
    Your input (y / n): ")
    # needs to be upper case for later code
    frn = frn.upper()
    if frn in ["Y", "N"]:
        break
    else:
        print "\n ~~~You did not choose a y or n~~~ \n"

### date range specification
# not worth the trouble to make this user screw up proof
#   I have faith in you :)
min_date = raw_input("What date would you like to *START* at? \n \
    Note only dates from April 2008 - present will run.  \n \
    Remember to enter as MM-DD-YYYY \n \
    Your input: ")

max_date = raw_input("What date would you like to *END* at? \n \
    Note only dates from April 2008 - present will run. \n \
    Remember to enter as MM-DD-YYYY \n \
    Your input: ")

# make list of user information to save
user_list = [security, tips, frn, min_date, max_date]

with open('user_input.csv', 'wb') as myfile:
    wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    wr.writerow(user_list)
