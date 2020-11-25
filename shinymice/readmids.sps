* Encoding: windows-1252.
DATA LIST FILE= "C:/Users/4216318/Documents/shinyMice/shinymice/midsdata.txt"  free (TAB)
   / Imputation_ age hgt wgt bmi hc gen phb tv reg .


VARIABLE LABELS
  Imputation_ "Imputation_" 
 age "age" 
 hgt "hgt" 
 wgt "wgt" 
 bmi "bmi" 
 hc "hc" 
 gen "gen" 
 phb "phb" 
 tv "tv" 
 reg "reg" 
 .

VALUE LABELS
 / gen 
   1 "G1"
   2 "G2"
   3 "G3"
   4 "G4"
   5 "G5"
 / phb 
   1 "P1"
   2 "P2"
   3 "P3"
   4 "P4"
   5 "P5"
   6 "P6"
 / reg 
   1 "north"
   2 "east"
   3 "west"
   4 "south"
   5 "city"
 .

EXECUTE.
SORT CASES by Imputation_.
SPLIT FILE layered by Imputation_.

PRESERVE.
SET DECIMAL DOT.

GET DATA  /TYPE=TXT
  /FILE="C:\Users\4216318\Documents\shinyMice\shinymice\midsdata.txt"
  /ENCODING='UTF8'
  /DELCASE=LINE
  /DELIMITERS="\t"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=1
  /LEADINGSPACES IGNORE=YES
  /DATATYPEMIN PERCENTAGE=95.0
  /VARIABLES=
  V1 AUTO
  V2 AUTO
  V3 AUTO
  V4 AUTO
  V5 AUTO
  V6 AUTO
  V7 AUTO
  /MAP.
RESTORE.

CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.
