# -*- coding: utf-8 -*-
import pandas as pd

def get_data(path):
    """obtains datasets needed for calculating big five scores"""
    #status = pd.read_csv(path + '/facebook_pipe.csv', sep='|')
    status = pd.read_csv(path + '/fb_pipe.csv', sep='|')
    #status['fb_status_msg'].fillna('', inplace=True)
    status['Status'].fillna('', inplace=True)
    openness = pd.read_csv(path + '/openness.csv', skiprows=1, 
                           names=['word', 'correl', 'pval'])
    conscientiousness = pd.read_csv(path + '/conscientiousness.csv', skiprows=1, 
                                    names=['word', 'correl', 'pval'])
    extraversion = pd.read_csv(path + '/extraversion.csv', skiprows=1, 
                               names=['word', 'correl', 'pval'])
    agreeableness = pd.read_csv(path + '/agreeableness.csv', skiprows=1, 
                                names=['word', 'correl', 'pval'])
    neuroticism = pd.read_csv(path + '/neuroticism.csv', skiprows=1, 
                              names=['word', 'correl', 'pval'])
    return status, openness, conscientiousness, extraversion, agreeableness, neuroticism
    
def data_to_dict(a):
    """creates a dictionary where the words are the keys and the correlation coefficients are the values"""
    the_dict = a.set_index('word')['correl'].to_dict()
    return the_dict

def group_people(b):
    """groups the status updates by individual"""
    groupedlanguage = b.groupby('userid')
    #groupedlanguage = groupedlanguage.fb_status_msg.aggregate(' '.join)
    groupedlanguage = groupedlanguage.Status.aggregate(' '.join)
    return groupedlanguage

def get_score(c, dimension):
    """calculates the big 5 score for the dict passed to dimension and the status updates grouped by individual"""
    from happierfuntokenizing import Tokenizer
    from nltk import bigrams, trigrams
    tokenizer=Tokenizer()
    scores = []
    for user in c:
        user_score = 0
        tokens = tokenizer.tokenize(user)
        bitokes = [' '.join(toke) for toke in list(bigrams(tokens))]
        tritokes = [' '.join(toke) for toke in list(trigrams(tokens))]
        user_feat = tokens + bitokes + tritokes
        for item in user_feat:
            if item in dimension:
                user_score += dimension[item]    
        scores.append(user_score/(len(tokens)+.1))
        
    return scores
    
data = get_data('data')
status = group_people(data[0])
openness = data_to_dict(data[1])
cons = data_to_dict(data[2])
extra = data_to_dict(data[3])
agree = data_to_dict(data[4])
neur = data_to_dict(data[5])

o = get_score(status, openness)
c = get_score(status, cons)
e = get_score(status, extra)
a = get_score(status, agree)
n = get_score(status, neur)

d = {'user' : status.index,
     'o' : o,
     'c' : c,
     'e' : e,
     'a' : a,
     'n' : n}
df = pd.DataFrame(d)

cols = df.columns.tolist()
cols = cols[-1:] + cols[:-1]
df = df[cols]

df.to_csv('bigfive2.csv', sep=',')
