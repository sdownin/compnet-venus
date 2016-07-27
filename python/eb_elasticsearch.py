# -*- coding: utf-8 -*-
"""
Created on Sat Apr  2 17:29:48 2016

@author: sdowning
"""

import os
os.chdir('C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks')

from elasticsearch import Elasticsearch, exceptions
import json, io
import pandas as pd
import numpy as np
#from datetime import datetime
from time import time
import arrow as ar
 

def esInsert(es, index, df, docTypeCol,verbose=False):
    """ Insert Pandas DataFrame records into Elasticsearch database for given index
    with type per record included as a column in the DataFrame
    """
    t0 = time()
    for i in range(df.shape[0]):
        body = df.iloc[i,:].to_json()
        #body['_timestamp_insert'] = datetime.now()
        doc_type = df[docTypeCol].values[i]
        try: 
            if es.search_exists(index=index):
                es.index(index=index, doc_type=doc_type,  body=body)
            else:
                es.create(index=index, doc_type=doc_type,  body=body)
        except Exception as e: 
            print('\ndoc: ' + str(i) + '\n')
            print(e)
            if isinstance(e, exceptions.ConnectionError):
                return
            else: # print('\nerror at doc '  + str(i) + ', the following data was not inserted:\n' + df.iloc[i,:].to_json())
                pass
    if verbose: 
        print('\nupdated at: ' + str(ar.now()) + 
              '\nelapsed seconds: ' + str(round(time()-t0,3)) )

def fixDate(x, dateType='string'):
    try:
        if dateType == 'string':
            date = ar.get(x).format('YYYY-MM-DD')
        elif dateType == 'datetime':
            date = ar.get(x).datetime
        elif dateType == 'date':
            date = ar.get(x).datetime.date()
        else:
            date = ar.get(x)
    except Exception as e:
        print(e)
        date = x if not isinstance(x, pd.tslib.NaTType) else None
    return date

def cleanStr(x):
    return x.replace('\n','').replace('\r','')
           
#--------------------------------------------------------------


file = 'C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/cb_elasticsearch_2.xlsx'
sheets = ['companies','rounds','investments','acquisitions']  # news, competitors
index = 'crunchbase'
docTypeCol = 'relation'

for sheet in sheets:
    relation = sheet
    #----------------------- read in data ----------------------------    
    ## Read in data from xlsx
    t0 = time()
    df = pd.read_excel(file,sheetname=relation)
    df[docTypeCol] = relation
    print('elapsed seconds: ' + str(round(time()-t0,2)))
    
    #----------------------- clean data ----------------------------
    ## datetime objects
    dateCols = ['created_at','updated_at','founded_on','founded_at','funded_at','acquired_at','first_funding_at','last_funding_at']
    for col in dateCols:
        if col in df.columns:
            df[col] = df[col].apply(lambda x: fixDate(x))
    ## replace NaN and NaT with None
    df.replace([np.NaN,pd.tslib.NaTType,'NaN',''],[None,None,None,None],inplace=True)
    ## remove line end characters
    if 'description' in df.columns:
        df['description'] = df['description'].replace([None],['']).apply(lambda x: cleanStr(x))
    
    #------------------------ insert data ------------------------------
    ## esInsert()
    es = Elasticsearch([dict(host='localhost',port=9200,use_ssl=False)])
    esInsert(es=es, index=index, df=df, docTypeCol=docTypeCol, verbose=True)
    
    print('\ncompleted: ' + sheet)



# ## CHECK RECORDS COUNT
# es.count(index)['count']















