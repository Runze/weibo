from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import NoSuchElementException 
import json
from bs4 import BeautifulSoup
import re
import time
from pandas import DataFrame, Series
import pandas as pd
import os


#define provinces
prov = [34, 11, 50, 35, 62, 44, 45, 52, 46, 13, 23, 41, 42, 43, 15, 32, 36, 22, 21, 64, 63, 14, 37, 31, 51, 12, 54, 65, 53, 33, 61, 71, 81, 82]
pg_srcs = []

#loop over all provinces
for p in prov:
    driver = webdriver.Firefox()

    #login
    driver.get('http://weibo.com/login')
    username = driver.find_element_by_xpath("//input[@action-data='text=邮箱/会员帐号/手机号']")
    username.send_keys('<username>')
    password = driver.find_element_by_xpath("//input[@type='password']")
    password.send_keys('<password>')

    submit = driver.find_element_by_link_text('登录')
    submit.click()
    time.sleep(10)

    #find people
    link = 'http://www.weibo.com/find/f?type=1&search=1&sex=0&isv=0&prov=' + str(p) + '&age=1985-1991'
    driver.get(link)
    time.sleep(5)
    
    #loop over 50 pages
    for pg in xrange(50):
        pg_srcs.append(driver.page_source)
    
        #go to the next page
        if pg < 49:
            next_pg = driver.find_element_by_partial_link_text('下一页')
            next_pg.click()

    driver.close()

#extract user name, id, and the number of fans (i.e., followers)
names = []
ids = []
fans = []

for ps in pg_srcs:
    soup = BeautifulSoup(ps)

    names_line = soup.find_all(class_ = 'W_f14')
    names = names + [n.string for n in names_line]

    ids_line = [u.parent for u in soup.find_all(text = '微博 ')]
    ids = ids + [re.search(r'(href="/)([0-9]+)(/follow")', str(i)).group(2) for i in ids_line]
    
    fans_line = [u.parent for u in soup.find_all(text = '粉丝 ')]
    fans_line = [re.sub(r'<.*?>', '', str(f)) for f in fans_line]
    fans = fans + [re.search(r'(粉丝 )([0-9]+)', f).group(2) for f in fans_line]

#create data frame
wb_acct = DataFrame({'name': names, 'id': ids, 'fan': fans})
wb_acct['name'] = wb_acct['name'].astype('unicode')
wb_acct['id'] = wb_acct['id'].astype('unicode')
wb_acct['fan'] = wb_acct['fan'].astype(int)

#keep users having fans less than the 25th percentile
wb_acct_trim = wb_acct[wb_acct['fan'] <= wb_acct['fan'].quantile(.25)]
wb_acct_trim.to_pickle('wb_acct_trim.pkl')

wb_acct_trim = pd.read_pickle('wb_acct_trim.pkl')

#get user tweets from weibo life (cleaner than weibo itself)
pg_srcs_tweets = []
for i, n in enumerate(wb_acct_trim['name']):
    print i
    
    link = 'http://tweets.seraph.me/user/' + n
    driver = webdriver.Firefox()
    driver.get(link)
    
    #get 5 pages of tweets
    pg = 0
    wait_time = 0
    while pg < 5 and wait_time < 3:
        while re.findall(r'System is currently busy temporarily', driver.page_source):
            #in case of an 403 error, wait for 2 minutes and try again.
            #if failed again, wait for another 2 minutes and try again (hence try 2 times in total).
            #skip this user if failed again.
            wait_time += 1
            
            if wait_time < 3:
                driver.close()
                time.sleep(60*2)
                driver = webdriver.Firefox()
                driver.get(link)
            else:
                break
        
        if wait_time < 3:
            pg_srcs_tweets.append(driver.page_source)

            try:
                older = driver.find_element_by_partial_link_text('Older >')
                older.click()
                pg += 1
            except NoSuchElementException:
                break
    
    driver.close()
    time.sleep(5)

#extract tweets
tweets = {'id': [], 'region': [], 'tweet': []}

for t in pg_srcs_tweets:
    #extract user information
    soup = BeautifulSoup(t)

    try:
        user = soup.find(class_ = 'userinfo').text
        user_id = re.search(r'(ID )([0-9]+)', user).group(2)
        user_prov = re.search(r'(From )(.+?)( |,)', user).group(2)

        #extract tweets
        content = re.sub(r'<.*?>', '', t)
        content = re.split('says|replies', content)[1:] #the first element is user info extracted above
        content = [c.replace('\n', ' ') for c in content]
        content = [re.sub('@.+?:|@.+? ', '', c) for c in content] #remove mentioned users
        content = [re.sub(' +', '', c) for c in content] #Chinese don't need space in between
        content = [c for c in content if len(c) > 12] #set min characters to be 4 (12 in length)

        for c in content:
            tweets['id'].append(user_id)
            tweets['prov'].append(user_prov)
            tweets['tweet'].append(c)

    except AttributeError:
        pass

tweets_df = DataFrame(tweets)
tweets_df.to_csv('weibo_tweets.csv', index = False, encoding = 'utf-8')

