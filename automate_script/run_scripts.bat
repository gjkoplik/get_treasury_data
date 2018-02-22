@ECHO OFF
REM Runs project scripts
REM get user input for security, tips, frn, min_date, and max_date
python get_user_input.py
ECHO Got user input
python download_securities.py
ECHO Got dates of auctions
Rscript use_xml_parser.R
ECHO Downloaded and parsed all possible auctions of interest
REM delete Securites.csv and user_input.csv
DEL "Securities.csv"
DEL "user_input.csv"
