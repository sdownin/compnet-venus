# -*- coding: utf-8 -*-
"""
Created on Sat May 12 10:42:45 2018

@author: T430

Split SDC output for relations (eg, alliances)
into separate rows for each member of the relation 


Positional Arguments:
   filename
   [directory_path]  ##optional


Example Call:
   $ python  <path/to/script>.py  data_file.xlsx 

Example Call (from outside data directory):
   $ python  <path/to/script>.py  data_file.xlsx  /home/users/T430/data
   
"""

import os, re, sys
from uuid import uuid4
import pandas as pd
import datetime as dt


def timestamp():
   """ RETURNS the unix epoch timestamp (example: 1526181030)
   """
   t = dt.datetime.utcnow()
   dt1 = dt.datetime(t.year,t.month,t.day,t.hour,t.minute,t.second)
   dt0 = dt.datetime(1970,1,1,1,1,1)
   tdiff = dt1 - dt0
   return int(tdiff.total_seconds())
   
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

def dedupDict(heading_map):
   """ fix duplicate values in a dict mapping by count indexing each
       after the first val,val_2,val_3,...
   """
   if not isinstance(heading_map, dict):
      raise Exception('heading_map must be a dict-like object')
   check = {}
   for key in heading_map.keys():
      val = heading_map[key]
      check[val] = check[val]+1 if val in check else 1
      if check[val] > 1:
         heading_map[key] = '{h}_{i}'.format(h=val,i=check[val])
   return heading_map

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


def main():
   """ Main script
   """
   file_ext_err_msg = 'File extion cannot be converted. Must be csv, xls, xlsx'
   #pyscript = sys.argv[0]  
   args = sys.argv[1:]
   
   if len(args) < 1:
      sys.exit("must provide file to convert")
   
   filename = args[0]
   ext = filename.split('.')[-1]
   dirname = args[1] if len(args) > 1 else os.getcwd()
   filepath = os.path.join(dirname,filename)
   print(' file name: %s' % filename)
   print(' extension: %s' % ext)
   print(' directory: %s' % dirname)
   print(' full path: %s' % filepath)
   
   ## exit if not convertible file type
   if ext not in ['csv','xls','xlsx']:
      sys.exit(file_ext_err_msg)
   
   ## exit if file missing
   if filename not in os.listdir(dirname):
      sys.exit("file not in directory")
   
   ## import data frame
   if ext == 'csv':
      df = pd.read_csv(filepath)
   elif ext in ['xls','xlsx']:
      df = pd.read_excel(filepath)
   else:
      sys.exit(file_ext_err_msg)
   #print(df.columns)
   
   ## fix heading strings
   #heading_map = {x:'{i}_{h}'.format(i=i,h=processHeader(x)) for i,x in enumerate(df.columns)}
   heading_map = dedupDict({x:processHeader(x) for x in df.columns})
   ## rename columns by mapping
   df = df.rename(columns=heading_map)
   #print(df.columns)
   
   ## add row ID columns
   df['uuid'] = df[df.columns[0]].apply(lambda x: str(uuid4()))
   df['id'] = pd.Series(range(1,df.shape[0]+1))
   
   ## extract first and second entries to separate data frames
   df0 = extractDf(df.copy(), 0)
   df1 = extractDf(df.copy(), 1)
   dfall = pd.concat([df0,df1]).reset_index(drop=True)
   
   ## drop duplicates and sort by relation id
   dfall.drop_duplicates(inplace=True)
   dfall.sort_values('id', inplace=True)
   
   ## file extension
   #ext2 = ext  ## keeping same file extension
   ext2 = 'csv' ## convert all to csv
   
   ## remove the extension (only the last part after final "."; keeping all other ".")
   filebase = '.'.join(filename.split('.')[:-1])
   filename2 = '%s-SPLIT%s.%s' %(filebase,timestamp(),ext2)
   filepath2 = os.path.join(dirname,filename2)
   
   # save to new file
   if ext2 == 'csv':
      print(' output at: %s' % filepath2)
      dfall.to_csv(filepath2, index=False)
   elif ext2 in ['xls','xlsx']:
      print(' output at: %s' % filepath2)
      dfall.to_excel(filepath2, index=False)
   else:
      print(file_ext_err_msg)


if __name__ == '__main__':
   main()



##------------------------------------------------

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

