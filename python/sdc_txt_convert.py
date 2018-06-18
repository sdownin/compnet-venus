# -*- coding: utf-8 -*-
"""
Created on Sat May 12 10:42:45 2018

@author: T430

CONVERT SDC TXT OUTPUT TO CSV

"""
import os, re, sys
from uuid import uuid4
import pandas as pd


def processHeader(string, pattern = r'[^a-zA-Z]+', repl='_'):
   """ Convert problematic data frame heading text to lower_snake_case
   """
   header = string.rstrip().replace('\r','').lower()
   header = re.sub('\s+', '_', header)
   header = re.sub(pattern, repl, header)
   while re.match(pattern, header[-1]):
      header = header[:-1] ## remove trailing non-alphanumeric
   while re.match(pattern, header[0]):
      header = header[1:] ## remove leading non-alphanumeric 
   return header

def extract(x, index, sep='\n'):
   """ Extract an element of a joined string
   """
   if not isinstance(x, str) or index not in [0,1]:
      return x
   items = x.split(sep)
   if index == 0:
      return items[0]
   if index == 1:
      return items[1] if len(items) > 1 else items[0]
   
def extractDf(df, index, sep='\n'):
   """ Extract an element of a joined string (by index) for each row,col of data frame
   """
   if not isinstance(df, pd.DataFrame):
      return df
   for col in df.columns:
      df[col] = df[col].apply(lambda x: extract(x,index,sep))
   return df

## script arguments
script = sys.argv[0]
args = sys.argv[1:]
filename = args[0]
dirname = args[1] if len(args) > 1 else os.getcwd()
filepath = os.path.join(dirname,filename)

## exit if file missing
if filename not in os.listdir():
   sys.exit("file not in directory")

## import data frame
df = pd.read_excel(filepath)
#print(df.columns)

## fix heading strings
heading_map = {x:processHeader(x) for x in df.columns}
df = df.rename(columns=heading_map)
#print(df.columns)

## add row UUIDs
df['uuid'] = df[df.columns[0]].apply(lambda x: str(uuid4()))

## extract first and second entries to separate data frames
df0 = extractDf(df.copy(), 0)
df1 = extractDf(df.copy(), 1)



#dir_base = 'C:/SDC/4.0.4.0/Platinum/USR'
##dir_source = dir_base + '/crunchbase_export_20161024/source'
##dir_output = dir_base + '/crunchbase_export_20161024'


### file
#fname = 'awareness_583_software_2008-2018_SIC_report_5.xlsx'





### fix header text
#with open(os.path.join(dir_base,fname)) as f:
#    content = f.readlines()
#rows = [x.strip() for x in content[:20] if x.strip() is not ''] 
#
#
#
#
#os.listdir(dir_base)
#
#pattern = re.compile('.+\.txt$')
#
#for file in os.listdir(dir_base):
#   if pattern.match(file):
#      print(file)
#      
#with open(os.path.join(dir_base,fname)) as f:
#    content = f.readlines()
## you may also want to remove whitespace characters like `\n` at the end of each line
#
#rows = [x.strip() for x in content[:20] if x.strip() is not ''] 
#
#print(rows)
#
#for row in rows:
#   print(len(row))

