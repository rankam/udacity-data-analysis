import pandas as pd
import numpy as np
import string
from bs4 import BeautifulSoup
import requests



data = pd.read_csv('ohio_election.csv')
occupations = set(data.contbr_occupation)

def remove_punc(wordlist):
    punc = [x for x in string.punctuation]
    punc.remove('/')
    new_word_list = list()
    for word in wordlist:
        word = str(word)
        for p in punc:
            if p in word:
                word = word.replace(p,'')
        new_word_list.append(word)
    return new_word_list

data['contbr_occupation'] = remove_punc(data['contbr_occupation'])

def get_salary_estimate(job_title, zip_code):
    url = 'http://www.indeed.com/salary?q1=%s&l1=%s&tm=1' % (job_title,zip_code,)
    soup= BeautifulSoup(requests.get(url).text)
    salary = soup.findAll("span",class_="salary")[0].text.replace('$','').replace(',','')
    return salary


def add_salary_estimate(df):
    ct = [1000,10000,25000,50000,75000,100000]
    df['estimated_salary'] = np.nan
    errors = list()
    for i in range(len(df)):
        if i in ct:
            print i
        try:
            df.iloc[i,-1] = get_salary_estimate(df.iloc[i].contbr_occupation, str(df.iloc[i].contbr_zip).replace('.0',''))
        except:
            errors.append({i:[df.iloc[i].contbr_occupation, str(df.iloc[i].contbr_zip).replace('.0','')]})
    return df, errors


data_w_salary,errors = add_salary_estimate(data)
data_w_salary.to_csv('data_w_salary.csv')
data_w_salary['estimated_salary'] = [float(x) if x != 'No Data ' else np.nan for x in data_w_salary['estimated_salary']]
data_w_salary['estimated_salary'][6]
no_data = data_w_salary[data_w_salary['estimated_salary'] != 'No Data ']
