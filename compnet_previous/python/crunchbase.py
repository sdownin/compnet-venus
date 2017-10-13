# -*- coding: utf-8 -*-
"""
Created on Mon Oct 24 04:33:14 2016

@author: sdowning
"""

import os, requests, string, logging
#import arrow as ar
import pandas as pd
import numpy as np


class CrunchBase(object):
    
    def __init__(self, user_key=None, endpoint='organizations', logfile='crunchbase.log'):
        self._user_key = user_key if user_key is not None else '_user_key_'
        self.baseUrl = 'https://api.crunchbase.com/v/3/'
        self.endpoint = endpoint
        logging.basicConfig(filename=logfile, level=logging.INFO)

    def _getParams(self, paramDict, withQuestionMark=False):
        paramList = []
        q = '?' if withQuestionMark else ''
        for key in paramDict:
            paramList.append( key + '=' + paramDict[key] )
        return q + '&'.join(paramList)
    
    def getCbUrl(self, entity, relation, endpoint=None,paramDict=None):
        endpoint = endpoint if endpoint is not None else self.endpoint
        paramDict = paramDict if paramDict is not None else dict(user_key=self._user_key)
        params = '' if paramDict is None else '?' + self._getParams(paramDict)
        return self.baseUrl + '{endpoint}/{entity}/{relation}{params}'.format(
                endpoint=endpoint,entity=entity,relation=relation,params=params)

    def getCbRelation(self, entity, relation, endpoint=None, paramDict=None,dataOnly=True):
        endpoint = endpoint if endpoint is not None else self.endpoint
        paramDict = paramDict if paramDict is not None else dict(user_key=self._user_key)
        url = self.getCbUrl(entity, relation)
        response = requests.get(url=url, params=paramDict)
        if response.ok:
            return response.json()['data'] if dataOnly else response.json()
        else: 
            return None
    
    def _processCbRelation(self, li, co, rel, paramDict):
        try:
            response = self.getCbRelation(co,rel,paramDict)
            if response is None:
                return False, li
        except Exception as e:
            print('exception: %s from company %s relation %s' % (e,co,rel))
            return False, li
        items = [response['item']]  if 'item' in response.keys() else response['items']         
        for item in items:
            itemProps = item['properties']
            itemProps['relation'] = rel
            itemProps['company'] = co
            li.extend([itemProps])
        return response, li

    def _paginateCbRelation(self, co, rel):
        li = []
        page = 1
        num_pages = 1;
        while page <= num_pages:
            paramDict = {'user_key':self._user_key, 'page':page}
            response, li = self._processCbRelation(li, co, rel, paramDict)
            page += 1
            if response:
                num_pages = response['paging']['number_of_pages']
        return li

    def saveToDf(self, li, basePath, rel):
        file_path = '{basePath}_{rel}.csv'.format(basePath=basePath, rel=rel)
        df = pd.DataFrame(li)
        if not os.path.isfile(file_path):
            df.to_csv(file_path, index=False, header = True, encoding='utf-8', mode='w', line_terminator='\n')
        else: # else it exists so append without writing the header
            df.to_csv(file_path, index=False, header = False, encoding='utf-8',mode='a', line_terminator='\n')

    
    def getPaginatedResults(self, companies, relations, endpoint=None, saveDf=True, saveBasePath='crunchbase_export_20160106'):
        # 'cb_export_with_competitors_20160106'        
        endpoint = self.endpoint if endpoint is None else endpoint
        companies = companies if isinstance(companies,(list,np.ndarray)) else [companies]
        relations = relations if isinstance(relations,(list,np.ndarray)) else [relations]
        li_full = []
        for rel in relations:
            for co in companies:
                msg = 'starting  relation: {rel}  company: {co}'.format(co=co, rel=rel)
                msg = ''.join(filter(lambda x: x in string.printable, msg))
                logging.info(msg); print(msg)
                if str(co) == 'nan':
                    continue
                ##---------- MAIN GET METHOD ---------
                li = self._paginateCbRelation(co, rel)
                ##------------------------------------
                if saveDf and len(li) > 0:
                    self.saveToDf(li, saveBasePath, rel)
                li_full.extend(li)
        self.li_full = li_full
        self.df = pd.DataFrame(li_full)
        self.df.drop_duplicates(inplace=True)
    
    
    
##--------------------------------------------------------
if __name__ == '__main__':
    from argparse import ArgumentParser
    par = ArgumentParser(description="CrunchBase API Calls")
    par.add_argument('user_key', type=str, help="CrunchBase API user_key") 
    par.add_argument('--companies', type=str, default='google', help="The company entities to request (comma-separated) [default 'google']")
    par.add_argument('--relations', type=str, default='products', help="The relations to request (comma-separated) [default 'products']")
    args = par.parse_args()
    user_key = args.user_key
    companies = args.companies.split(',') if ',' in args.companies else [args.companies]
    relations = args.relations.split(',') if ',' in args.relations else [args.relations]
    ##-------------------
    os.chdir('C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase')
    relations = 'products' #['products','owned_by','sub_organizations','investors','investments','board_members_and_advisors'] # ['ipo']    ## ['competitors']  # 'members','memberships'
    co_names =  pd.read_csv('g_full_company_name_unique.csv',encoding='utf-8')
    start_index = 13371 # 15599  # 6799  # 0
    companies = co_names.name.values[start_index:]
    ##-------------------
    cb = CrunchBase(user_key)
    cb.getPaginatedResults(companies, relations)