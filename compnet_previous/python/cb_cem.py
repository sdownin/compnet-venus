# -*- coding: utf-8 -*-
"""
@author: sdowning
"""
import os, glob
os.chdir('C:/users/sdowning/Google Drive/PhD/Dissertation/competition networks/compnet/python')
import crunchbase
import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
import urllib3 as ul
import json 
import time
import matplotlib.pyplot as plt
import arrow as ar
import requests
os.chdir('C:/users/sdowning/Google Drive/PhD/Dissertation/competition networks')
#import pycrunchbase as pc

#def postRequestJson(url, *args, **kwargs):
#    pool = ul.PoolManager()
#    raw = pool.urlopen('POST', url, headers={'Content-Type':'application/json'}, 
#                       body='{"sup":"son"}').data.decode('utf-8')
#    return json.loads(raw)
#
#def getRequestCsv(url, *args, **kwargs):
#    pool = ul.PoolManager()
#    raw = pool.urlopen('GET', url).data.decode('utf-8').split('\n')
#    return raw
    
def getParams(paramDict, withQuestionMark=False):
    paramList = []
    q = '?' if withQuestionMark else ''
    for key in paramDict:
        paramList.append( key + '=' + paramDict[key] )
    return q + '&'.join(paramList)

def buildCbUrl(endpoint,company,relation,paramDict=None):
    base = 'https://api.crunchbase.com/v/3/'
    params = '' if paramDict is None else '?' + getParams(paramDict)
    return base + '{endpoint}/{company}/{relation}{params}'.format(
            endpoint=endpoint,company=company,relation=relation,params=params)

def getCbRelation(endpoint, entity, relation, paramDict, dataOnly=True):
    url = buildCbUrl(endpoint,entity, relation)
    response = requests.get(url=url, params=paramDict)
    if response.ok:
        return response.json()['data'] if dataOnly else response.json()
    else: 
        return None

def cbRelation2df(endpoint, entity, relation, paramDict):
    url = buildCbUrl(endpoint,entity, relation)
    response = requests.get(url=url, params=paramDict)
    if not response.ok:
        ps = BeautifulSoup(response.content).find_all('p')
        errorText = '. '.join([p.text for p in ps])
        print(errorText)
        return (None, None, None)
    if 'data' not in list(response.json().keys()):
        return (None, None, None)
    data = response.json()['data']
    if 'items' not in list(data.keys()) and 'item' not in list(data.keys()): 
        return (None, None, None)
    items = data['items'] if 'items' in list(data.keys()) else data['item']
    lenItems = len(items) if isinstance(items, list) else 1
    df = pd.DataFrame( [ items[i]['properties']  if isinstance(items, list) else items['properties']
                         for i in range(lenItems) ] )
    return (df, data['paging']['number_of_pages'], data['paging']['total_items'])

def strTime(x):
    return x if int(x) > 3000 else str(x) + '-01-01'


# # --------------------------------------------------------
# #     CRUNCHBASE
# # --------------------------------------------------------
# #     API Key:        1d525bf9a877d7a511cb44a48db458fb
# #     https://api.crunchbase.com/v/3/{endpoint}/{company}/{relation}[?{params}]

# cb_user_key = '1d525bf9a877d7a511cb44a48db458fb'
cb_user_key = '898192bfa39ebe42a7e1886e755bc9fc'  #Jin-Su

#-----------------------------------------------------------------
#       All Data from Only CEM Companies
#-----------------------------------------------------------------

relations = ['news', 'products', 'competitors', 'customers', 'categories',
            'acquisitions','investments','funding_rounds','acquired_by',
            'ipo','funds']  # 'members','memberships'
relations2 = ['founders', 'current_team', 'past_team', 'board_members_and_advisors',
              'investors','owned_by','sub_organizations','headquarters','offices',
              'members','memberships','websites']
#cem = pd.read_csv('CEM_companies.csv',parse_dates=True)

# # Newly added CEM companies
path = 'C:\\Users\\sdowning\\Google Drive\\PhD\\Dissertation\\competition networks\\comp_net_focus_companies.xlsx'
cem = pd.read_excel(path,'CEM_companies')
cem_co_uri_name_values = cem.uri_name.loc[cem.newly_added==1].values
# #

# LOOP THROUGH RELATIONS AND COMPANIES ---------------------------
os.chdir('C:\\Users\\sdowning\\Google Drive\\Choco\\New MVP\\top 20 CEM 2015')
for rel in relations:        
    loop = 0
    for co in cem_co_uri_name_values:  #cem.uri_name.values:
        df, n, t = cbRelation2df('organizations', co, rel, {'user_key':cb_user_key})
        if df is None: continue
        if n > 1:
            for page in range(2,n+1):
                df2, _, _ = cbRelation2df('organizations', co, rel, {'page':page,'user_key':cb_user_key})
                if df2 is None: continue
                df = pd.concat( (df,df2), axis=0)
                if page % 10 == 0: print('page: ' + str(page))
        print('true' if df.shape[0] == t else 'false')
        print(co + ' ' + rel + ': ' + str(df.shape) )
        df.reset_index(drop=True, inplace=True)
        df_lab = pd.DataFrame( dict(relation=repeat(rel,df.shape[0]),
                                    company=repeat(co,df.shape[0])) )
        df = pd.concat( (df_lab, df), axis=1)
        # BUILD OUTPUT DF
        df_all = df.copy() if loop == 0 else pd.concat( (df_all, df), axis=0)
        loop += 1
    # OUTPUT ALL COMPANIES FOR RELATION rel
    path = 'cb_cem_{rel}_3.csv'.format(rel=rel)
    df_all.to_csv(path, sep=',', index=False, encoding='utf-8')
    # PAUSE
    nap = 15   
    if loop % 1e3 == 0: 
        print(str(rel) + ' completed. Sleeping for ' + str(nap) + ' sec before continuing...\n')
        time.sleep(nap)







os.chdir('C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/cb_cem')
prod = pd.read_csv('cb_cem_products_3.csv', parse_dates=False, index_col=None,
                   encoding='utf-8')
comp = pd.read_csv('cb_cem_competitors_3.csv', parse_dates=False, index_col=None,
                   encoding='utf-8')

#cb = pc.CrunchBase(cb_user_key)
#co = cb.organization('medallia')
#
#pool = ul.PoolManager()
#raw = pool.urlopen('POST', url, headers={'Content-Type':'application/json'}, 
#                       body='{"sup":"son"}').data.decode('utf-8')
#json.loads(raw)



#-----------------------------------------------------------------
#       Competitors of ALL Companies 
#-----------------------------------------------------------------
os.chdir('C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase')
relations = ['competitors']  # 'members','memberships'
cb_co_all = pd.read_csv('cb_export_with_competitors_20160106_companies.csv',encoding='utf-8',parse_dates=True)
# LOOP THROUGH RELATIONS AND COMPANIES ---------------------------
start = 0   # xxvii   # loop = 64668
for rel in relations:        
    loop = 0
    for co in cb_co_all.unique_uri.values[start:len(cb_co_all.unique_uri.values)]:
        df, n, t = cbRelation2df('organizations', co, rel, {'user_key':cb_user_key})
        if df is None: continue
        if n > 1:
            for page in range(2,n+1):
                df2, _, _ = cbRelation2df('organizations', co, rel, {'page':page,'user_key':cb_user_key})
                if df2 is None: continue
                df = pd.concat( (df,df2), axis=0)
                if page % 10 == 0: print('page: ' + str(page))
        print('true' if df.shape[0] == t else 'false')
        print(str(loop) + '. ' + co + ' ' + rel + ': ' + str(df.shape) )
        df.reset_index(drop=True, inplace=True)
        df_lab = pd.DataFrame( dict(relation=repeat(rel,df.shape[0]),
                                    company=repeat(co,df.shape[0])) )
        df = pd.concat( (df_lab, df), axis=1)
        df_all = df.copy() if loop == 0 else pd.concat( (df_all, df), axis=0)

        # if file does not exist write header         
        path = 'crunchbase_export_20160106_{rel}.csv'.format(rel=rel)
        if df.shape[0] > 0:
            if not os.path.isfile(path):
                df.to_csv(path, index=False, header ='column_names', encoding='utf-8')
            else: # else it exists so append without writing the header
                df.to_csv(path, index=False, header = False, encoding='utf-8',mode = 'a')

        # PAUSE
        nap = 15   
        if loop > 0 and loop % 500 == 0: 
            print(str(rel) + ' completed. Sleeping for ' + str(nap) + ' sec before continuing...\n')
            time.sleep(nap)
        loop += 1



os.chdir('C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks')
comp = pd.read_csv('cb_cem_competitors_ALL.csv', encoding='utf-8')
comp.drop_duplicates(inplace=True)
comp.created_at = comp.created_at.apply(lambda x: ar.get(x).format('YYYY-MM-DD'))
comp.to_csv('cb_cem_competitors_ALL_nodups.csv', encoding='utf-8', header=True)




# df_all.created_at = df_all.created_at.apply(lambda x: ar.get(strTime(x)).format('YYYY-MM-DD') )
# df_all.posted_on  = df_all.posted_on.apply(lambda x: ar.get(x).format('YYYY-MM-DD') )
# df_all.updated_at = df_all.updated_at.apply(lambda x: ar.get(strTime(x)).format('YYYY-MM-DD') )





## CUSTOMERGAUGE relations----------------------------------------------------
cg = {}
for rel in relations:        
    df, n, t = cbRelation2df('organizations', co, rel, {'user_key':cb_user_key})
    if df is None: continue
    if n > 1:
        for page in range(2,n+1):
            df2, _, _ = cbRelation2df('organizations', co, rel, {'page':page,'user_key':cb_user_key})
            if df2 is None: continue
            df = pd.concat( (df,df2), axis=0)
            if page % 10 == 0: print('page: ' + str(page))
    print('true' if df.shape[0] == t else 'false')
    print(co + ' ' + rel + ': ' + str(df.shape) )
    df.reset_index(drop=True, inplace=True)
    df_lab = pd.DataFrame( dict(relation=repeat(rel,df.shape[0]),
                                company=repeat(co,df.shape[0])) )
    cg[rel] = pd.concat( (df_lab, df), axis=1)
#for key in cg:  print(cg[key]):
    
#-----------------REPLACE UNIX TIMESTAMP WITH STRING DATE-----------------
for key in cg: 
    df = cg[key].copy()
    for timeVar in ['posted_on','created_at','updated_at']:
        if timeVar in cg[key].columns.values:
            df[timeVar] = df[timeVar].apply(lambda x: (ar.get(x) if ar.get(x) else ar.get(strTime(x))).format('YYYY-MM-DD'))
    df.to_csv('customergauge_{rel}.csv'.format(rel=key), sep=",",index=False, encoding='utf-8')    
    
# OUTPUT ALL COMPANIES FOR RELATION rel
path = 'cb_cem_{rel}_3.csv'.format(rel=rel)
df_all.to_csv(path, sep=',', index=False, encoding='utf-8')
# PAUSE
nap = 15   
if loop % 1e3 == 0: 
    print(str(rel) + ' completed. Sleeping for ' + str(nap) + ' sec before continuing...\n')
    time.sleep(nap)




#--------------- PRODUCTS TIMESTAMP TO DATESTRING -----------------------
path = "C:/Users/sdowning/Google Drive/PhD/Dissertation/competition networks/cb_cem/cb_cem_products_3.csv"
pr = pd.read_csv(path, index_col=False, encoding='utf-8' )
pr.created_at = pr.created_at.apply(lambda x: ar.get(x).format('YYYY-MM-DD'))
pr.updated_at = pr.updated_at.apply(lambda x: ar.get(x).format('YYYY-MM-DD'))
pr.to_csv(path, encoding='utf-8',   index=False)





#--------------------- GET COMPANY OFFICE LOCATIONS --------------------

def saveAppend(li, file_name):
    file_path = os.getcwd()+'\\'+file_name
    with open(file_path, 'a+', encoding='utf-8') as f:
        df = pd.DataFrame(li)
        df.to_csv(f, header=False, line_terminator='\n', index=False)

def getCbRelationPagination(endpoint,co,rel,cb_user_key):
    li = []
    current_page = 0
    number_of_pages = 1
    while current_page < number_of_pages:
        current_page += 1
        paramDict = {'user_key':cb_user_key, 'page':current_page}
        try:
            response = getCbRelation(endpoint,co,rel,paramDict)
            if response is None:
                continue
        except Exception as e:
            print('exception: %s from company %s relation %s' % (e,co,rel))
            continue
        items = [response['item']]  if 'item' in response.keys() else response['items']         
        for item in items:
            itemProps = item['properties']
            itemProps['relation'] = rel
            itemProps['company'] = co
            li.extend([itemProps])
        #current_page = response['paging']['current_page']
        number_of_pages = response['paging']['number_of_pages']

def getBranchesFromCb(companies):
    endpoint = 'organizations'
    relations = ['headquarters','offices']
    li_full = []
    for rel in relations:
        for co in companies:
            li = []
            if str(co) == 'nan':
                continue
            ## 1. FIRST API CALL -----------------------
            page = 1
            paramDict = {'user_key':cb_user_key, 'page':page}
            try:
                response = getCbRelation(endpoint,co,rel,paramDict)
                if response is None:
                    continue
            except Exception as e:
                print('exception: %s from company %s relation %s' % (e,co,rel))
                continue
            items = [response['item']]  if 'item' in response.keys() else response['items']         
            for item in items:
                itemProps = item['properties']
                itemProps['relation'] = rel
                itemProps['company'] = co
                li.extend([itemProps])
            paging = response['paging']
            ## 2. HANDLE PAGINATION ---------------------------
            while paging['current_page'] < paging['number_of_pages']:
                page += 1
                paramDict = {'user_key':cb_user_key, 'page':page}
                try:
                    response = getCbRelation(endpoint,co,rel,paramDict)
                    if response is None:
                        continue
                except Exception as e:
                    print('exception: %s from company %s relation %s' % (e,co,rel))
                    continue
                items = [response['item']]  if 'item' in response.keys() else response['items']         
                for item in items:
                    itemProps = item['properties']
                    itemProps['relation'] = rel
                    itemProps['company'] = co
                    li.extend([itemProps])
                paging = response['paging']
                
            file_path = os.getcwd()+'\\cb_company_branches.csv'
            with open(file_path, 'a+', encoding='utf-8') as f:
                df = pd.DataFrame(li)
                df.to_csv(f, header=False, line_terminator='\n', index=False)
            li_full.extend(li)
            
        print('completed company: %s' % co)
    print('completed relation: %s' % rel)
    
    df = pd.DataFrame(li)
    df.drop_duplicates(inplace=True)
    return df
    
    
companies_file = os.getcwd() + '\\compnet\\lcc_vertex_attributes.csv'
companies = pd.read_csv(companies_file, encoding='utf-8')

last_co_name = 'fireid'
restart = np.where(companies.name.values==last_co_name)[0][0]
dfb = getBranchesFromCb(companies.name.values[restart:])

dfb.to_csv('cb_company_branches_fullendsave.csv',sep=',',index=False)





#------------- PAGINATED RESULTS-----------------------------
def getPaginatedFromCb(companies, relations):
    endpoint = 'organizations'
    li_full = []
    for rel in relations:
        for co in companies:
            li = []
            if str(co) == 'nan':
                continue
            ## 1. FIRST API CALL -----------------------
            page = 1
            paramDict = {'user_key':cb_user_key, 'page':page}
            try:
                response = getCbRelation(endpoint,co,rel,paramDict)
                if response is None:
                    continue
            except Exception as e:
                print('exception: %s from company %s relation %s' % (e,co,rel))
                continue
            items = [response['item']]  if 'item' in response.keys() else response['items']         
            for item in items:
                itemProps = item['properties']
                itemProps['relation'] = rel
                itemProps['company'] = co
                li.extend([itemProps])
            paging = response['paging']
            ## 2. HANDLE PAGINATION ---------------------------
            while paging['current_page'] < paging['number_of_pages']:
                page += 1
                paramDict = {'user_key':cb_user_key, 'page':page}
                try:
                    response = getCbRelation(endpoint,co,rel,paramDict)
                    if response is None:
                        continue
                except Exception as e:
                    print('exception: %s from company %s relation %s' % (e,co,rel))
                    continue
                items = [response['item']]  if 'item' in response.keys() else response['items']         
                for item in items:
                    itemProps = item['properties']
                    itemProps['relation'] = rel
                    itemProps['company'] = co
                    li.extend([itemProps])
                paging = response['paging']
                
            file_path = 'crunchbase_export_20160106_{rel}_NEWEST.csv'.format(rel=rel)
            with open(file_path, 'a+', encoding='utf-8') as f:
                df = pd.DataFrame(li)
                df.to_csv(f, header=False, line_terminator='\n', index=False)
            li_full.extend(li)
        print('completed company: %s' % co)
    print('completed relation: %s' % rel)
    df = pd.DataFrame(li)
    df.drop_duplicates(inplace=True)
    return df






#--------------- --------------------------------------
#    CrunchBase class
#    GET IPS, PRODUCTS, etc
#-------------------------------------------------------
os.chdir('C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase')
relations = ['ipo','products','owned_by','sub_organizations','investors','investments','board_members_and_advisors']     ## ['competitors']  # 'members','memberships'
co_names =  pd.read_csv('g_full_company_name_unique.csv',encoding='utf-8')
companies = co_names.name.values

cb = crunchbase.CrunchBase()
cb.getPaginatedResults(companies, relations)





