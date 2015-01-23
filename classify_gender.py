#!/usr/bin/env python
from nltk.corpus import names
import random as rd
import pandas as pd
import numpy as np
import nltk


def gender_features(name):
    features = {}
    features["first_letter"] = name[0].lower()
    features["last_letter"] = name[-1].lower()
    for letter in 'abcdefghijklmnopqrstuvwxyz':
        features["count(%s)" % letter] = name.lower().count(letter)
        features["has(%s)" % letter] = (letter in name.lower())
    return features

labeled_names = ([(name, 'male') for name in names.words('male.txt')] + [(name, 'female') for name in names.words('female.txt')])
rd.shuffle(labeled_names)
featuresets = [(gender_features(n), gender) for (n, gender) in labeled_names]
train_set, test_set = featuresets[5000:], featuresets[:5000]
classifier = nltk.NaiveBayesClassifier.train(train_set)

gender_data = pd.read_csv('/Users/aaronrank/projects/udacity_DataR/data_gender_pred.csv',low_memory=False)

def classify_gender(dataframe):
    dataframe['predicted_gender'] = dataframe.contbr_fnm.apply(lambda x: classifier.classify(gender_features(str(x))))
    dataframe.to_csv('data_gender_predicted.csv',index=False)
    print 'You classified '+str(nltk.classify.accuracy(classifier, test_set))+' correct on the test set'

if __name__ == '__main__':
    classify_gender(gender_data)