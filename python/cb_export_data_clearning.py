# -*- coding: utf-8 -*-
"""
Created on Mon Oct 24 19:22:12 2016

@author: sdowning
"""

dir_base = 'C:/Users/sdowning/Google Drive/PhD/Dissertation/crunchbase'
dir_source = dir_base + '/crunchbase_export_20161024/source'
dir_output = dir_base + '/crunchbase_export_20161024'

#------------------------------------------------
import os, re
import numpy as np
import pandas as pd
import arrow as ar

#-----------------------------------------------
def getDateSafe(x):
    formatString = 'YYYY-MM-DD'
    sep = "-"
    li = []
    if pd.isnull(x) or x == '':
        return 'NA'
    if isinstance(x, (int, np.int, np.int16, np.int32, np.int64)) and x >= 1000000000:
        return ar.get(x).format(formatString)
    try:
        parts = re.split("[-\\\/]",re.split("[\s]+",x)[0])
        li.append( parts.pop([i for i,part in enumerate(parts) if len(part)==4][0]) )
        li.append( parts.pop([i for i,part in enumerate(parts) if len(part)<=2 and int(part) <= 12][0]) )
        li.append( parts.pop([i for i,part in enumerate(parts) if len(part)<=2 and int(part) <= 31][0]) )
        return ar.get(sep.join(li)).format(formatString)
    except Exception as e:
        print(e)

def fillNA(df):
    for col in df.columns:
        df[col] = df[col].apply(lambda x: 'NA' if pd.isnull(x) else x)
    return df
#-------------------------------------------------

##-------------------------------------------------
##   1.0 ORGANIZATIONS
##-------------------------------------------------
file_name = 'organizations.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
co = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
co.created_at = co.created_at.apply(lambda x: getDateSafe(x))
co.updated_at = co.updated_at.apply(lambda x: getDateSafe(x))
#
co['company_name_unique'] = co.cb_url.apply(lambda x: x.split("/")[-1])
co['company_uuid'] = co.uuid.copy()
co.drop(labels=['twitter_url','facebook_url','cb_url','profile_image_url','logo_url','uuid'], axis=1, inplace=True)
co  = fillNA(co)
#
co.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')

##-------------------------------------------------
##   1.1 ORGANIZATION PARENT
##      **RELATION**
##-------------------------------------------------
file_name = 'org_parents.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
op = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
op.created_at = op.created_at.apply(lambda x: getDateSafe(x))
op.updated_at = op.updated_at.apply(lambda x: getDateSafe(x))
#
op = fillNA(op)
#
op.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')



##-------------------------------------------------
##   1.1 ORGANIZATION BRANCHES
##      **AGGREGATES**
##-------------------------------------------------
file_name = 'branches.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
br = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
br.created_at = br.created_at.apply(lambda x: getDateSafe(x))
br.updated_at = br.updated_at.apply(lambda x: getDateSafe(x))
#
br.drop(labels=['city_web_path','relation'], axis=1, inplace=True)
br = fillNA(br)
#
br.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')




##-------------------------------------------------
##   2 ACQUISITIONS
##      **RELATION**
##      **AGGREGATE**
##-------------------------------------------------
file_name = 'acquisitions.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
ac = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
ac.created_at = ac.created_at.apply(lambda x: getDateSafe(x))
ac.updated_at = ac.updated_at.apply(lambda x: getDateSafe(x))
#
ac['aquiree_name_unique'] = ac.acquiree_cb_url.apply(lambda x: x.split("/")[-1])
ac['aquirer_name_unique'] = ac.acquirer_cb_url.apply(lambda x: x.split("/")[-1])
#
ac.drop(labels=['acquirer_cb_url','acquiree_cb_url'], axis=1, inplace=True)
ac = fillNA(ac)
#
ac.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')


##-------------------------------------------------
##   3. PRODUCTS
##      **AGGREGATE**
##-------------------------------------------------
file_name = 'products.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
pr = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
pr.created_at = pr.created_at.apply(lambda x: getDateSafe(x))
pr.updated_at = pr.updated_at.apply(lambda x: getDateSafe(x))
pr.launched_on = pr.launched_on.apply(lambda x: getDateSafe(x))
pr.closed_on = pr.closed_on.apply(lambda x: getDateSafe(x))
#
pr['product_name_unique'] = pr.api_path.apply(lambda x: x.split("/")[-1])
#
pr.rename(columns={'company':'company_name_unique'},  inplace=True )
#
pr.drop(labels=['profile_image_url','relation','web_path','api_path','name'], axis=1, inplace=True)
pr = fillNA(pr)
#
pr.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')




##-------------------------------------------------
##   4 Category group -- ALL OK
##-------------------------------------------------
file_name = 'category_groups.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
cg = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
cg.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')



##-------------------------------------------------
##   5 COMPETITORS
##      **RELATION**
##-------------------------------------------------
file_name = 'competitors.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
comp = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
comp.drop_duplicates(inplace=True)
#
comp.created_at = comp.created_at.apply(lambda x: getDateSafe(x))
comp.updated_at = comp.updated_at.apply(lambda x: getDateSafe(x))
comp = fillNA(comp)
#
comp.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')


##-------------------------------------------------
##   6 CUSTOMERS
##      **RELATION**
##-------------------------------------------------
file_name = 'customers.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
cust = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
cust.drop_duplicates(inplace=True)
#
cust.created_at = cust.created_at.apply(lambda x: getDateSafe(x))
cust.updated_at = cust.updated_at.apply(lambda x: getDateSafe(x))
#
cust = fillNA(cust)
#
cust.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')



##-------------------------------------------------
##   7.1 EVENTS
##-------------------------------------------------
file_name = 'events.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
ev = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
ev.drop_duplicates(inplace=True)
#
ev.created_at = ev.created_at.apply(lambda x: getDateSafe(x))
ev.updated_at = ev.updated_at.apply(lambda x: getDateSafe(x))
#
ev.drop(labels=['profile_image_url','logo_url','cb_url','permalink','registration_url'], axis=1, inplace=True)
ev = fillNA(ev)
#
ev.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')



##-------------------------------------------------
##   7.2 EVENTS RELATIONSHIPS
##      **RELATIONS**
##-------------------------------------------------
file_name = 'event_relationships.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
evrel = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
evrel.drop_duplicates(inplace=True)
#
evrel.created_at = evrel.created_at.apply(lambda x: getDateSafe(x))
evrel.updated_at = evrel.updated_at.apply(lambda x: getDateSafe(x))
#
evrel = fillNA(evrel)
#
evrel.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')



##-------------------------------------------------
##   8.1 FUNDS
##-------------------------------------------------
file_name = 'funds.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
fu = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
fu.drop_duplicates(inplace=True)
#
fu.created_at = fu.created_at.apply(lambda x: getDateSafe(x))
fu.updated_at = fu.updated_at.apply(lambda x: getDateSafe(x))
#
fu = fillNA(fu)
#
fu.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')



##-------------------------------------------------
##   8.2 FUNDING ROUNDS
##      **RELATION**
##      **AGGREGATE**
##-------------------------------------------------
file_name = 'funding_rounds.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
rou = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
rou.drop_duplicates(inplace=True)
#
rou.created_at = rou.created_at.apply(lambda x: getDateSafe(x))
rou.updated_at = rou.updated_at.apply(lambda x: getDateSafe(x))
#
rou.drop(labels=['cb_url','company_category_list','country_code','region','state_code','city'], axis=1, inplace=True)
rou = fillNA(rou)
#
rou.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')


##-------------------------------------------------
##   8.3 IPOs
##-------------------------------------------------
file_name = 'ipos.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
ipo = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
ipo.drop_duplicates(inplace=True)
ipo['company_name_unique'] = ipo.cb_url.apply(lambda x: x.split("/")[-1])
#
ipo.created_at = ipo.created_at.apply(lambda x: getDateSafe(x))
ipo.updated_at = ipo.updated_at.apply(lambda x: getDateSafe(x))
#
ipo.drop(labels=['cb_url'], axis=1, inplace=True)
ipo = fillNA(ipo)
#
ipo.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')




##-------------------------------------------------
##   9.1 INVESTORS
##-------------------------------------------------
file_name = 'investors.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
inv = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
inv.drop_duplicates(inplace=True)
inv['investor_name_unique'] = inv.cb_url.apply(lambda x: x.split("/")[-1])
#
# no created_at (founded_on already in YYYY-MM-DD format)
inv.updated_at = inv.updated_at.apply(lambda x: getDateSafe(x))
#
inv.drop(labels=['twitter_url','facebook_url','profile_image_url','logo_url','cb_url','city'], axis=1, inplace=True)
inv = fillNA(inv)
#
inv.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')


##-------------------------------------------------
##   9.2 INVESTMENTS
##      **RELATION**
##-------------------------------------------------
file_name = 'investments.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
invmts = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
invmts.drop_duplicates(inplace=True)
#
invmts.is_lead_investor = invmts.is_lead_investor.apply(lambda x: 0 if x=='f' else 1)
#
invmts = fillNA(invmts)
#
invmts.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')


##-------------------------------------------------
##   9.3 INVESTMENT PARTNERS
##      **RELATION**
##-------------------------------------------------
file_name = 'investment_partners.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
ip = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
ip.drop_duplicates(inplace=True)
#
ip.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')



##-------------------------------------------------
##   10 JOBS
##-------------------------------------------------
file_name = 'jobs.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
jb = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
jb.drop_duplicates(inplace=True)
#
jb.started_on = jb.started_on.apply(lambda x: getDateSafe(x))
jb.ended_on = jb.ended_on.apply(lambda x: getDateSafe(x))
#
jb = fillNA(jb)
#
jb.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')



##-------------------------------------------------
##   11.1 PEOPLE
##-------------------------------------------------
file_name = 'people.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
ppl = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
ppl.drop_duplicates(inplace=True)
ppl['person_name_unique'] = ppl.cb_url.apply(lambda x: x.split("/")[-1])
#
ppl.created_at = ppl.created_at.apply(lambda x: getDateSafe(x))
ppl.updated_at = ppl.updated_at.apply(lambda x: getDateSafe(x))
#
ppl.drop(labels=['twitter_url','facebook_url','profile_image_url','logo_url','cb_url'], axis=1, inplace=True)
ppl = fillNA(ppl)
#
ppl.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')


##-------------------------------------------------
##   11.2 PEOPLE  DESCRIPTIONS
##-------------------------------------------------
file_name = 'people_descriptions.csv'
in_path  = dir_source + '/' + file_name
out_path = dir_output + '/' + file_name
ppldesc = pd.read_csv(in_path, sep=",", parse_dates=True, low_memory=False,  na_values=['','NA',None], keep_default_na=False, encoding='utf-8')
#
ppldesc.to_csv(out_path, sep=",", index=False, encoding='utf-8', date_format='YYYY-MM-DD')







