# -*- coding: utf-8 -*-
"""
Created on Mon Apr 25 23:17:56 2022

@author: sulli
"""
#importing all the modules I plan to use
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as clr
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split

#Reading in my raw data into dataframe
Raw = pd.read_csv('C:/Users/sulli/Documents/Code/DTSC 2302/CES20_Common_OUTPUT_vv.csv')

#Creating new dataframe with features I actually need
df = pd.DataFrame()
df['Dem'] = Raw['pid7']
df['Rep'] = Raw['pid7']
#strong demcrat is 1, so make eveything else 0
df.loc[df['Dem'] != 1, 'Dem'] = 0
#strong republican is 7, so make everything else 0 then put to 1
df.loc[df['Rep'] != 7, 'Rep'] = 0
df.loc[df['Rep'] == 7, 'Rep'] = 1
#Making strong rep and dem 
df['Polar'] = df['Dem'] + df['Rep']
#getting education
df['edu'] = Raw['educ']
#getting gender, 1 = male 2 = Female, moving female to 0
df['Male'] = Raw['gender']
df.loc[df['Male'] == 2, 'Male'] = 0
#Decided to make male the only category for race since we want to avoid over-fitting our model
df['White'] = Raw['race']
df.loc[df['White'] != 1, 'White'] = 0
#Local and national news 
df['National'] = Raw['CC20_300a']
df.loc[df['National'] == 1, 'National'] = 0
df.loc[(df['National'] == 2) | (df['National'] == 3),'National'] = 1
#Polarized news is a bit different, 1 is yes and 2 is no, but each network is its own column
df['Fox'] = Raw['CC20_300b_5']
df.loc[df['Fox'] != 1, 'Fox'] = 0
df['CNN'] = Raw['CC20_300b_4'] 
df.loc[df['CNN'] != 1, 'CNN'] = 0
df['MSNBC'] = Raw['CC20_300b_6']
df.loc[df['MSNBC'] != 1, 'MSNBC'] = 0
#Since 1 indicates they watch one of these, if the sum of these rows is greater than 1, then they watch one of them
df['Polarized_News'] = df['Fox'] + df['CNN'] + df['MSNBC']
df.loc[df['Polarized_News'] >= 1,'Polarized_News'] = 1
#Social media is done in a similar way to polarized news