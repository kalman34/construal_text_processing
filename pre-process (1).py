#preprocessing functions for text

from nltk import WordNetLemmatizer, sent_tokenize
from nltk.stem import PorterStemmer, SnowballStemmer, LancasterStemmer
from nltk.corpus import stopwords
import re, collections
from happierfuntokenizing import Tokenizer

def isEnglish(s):
    try:
        s.decode('ascii')
    except UnicodeDecodeError:
        return ''
    else:
        return s
        
# Tokenizing (Document to list of sentences. Sentence to list of words.)
def tokenize(str):
  '''Tokenizes into sentences, then strips punctuation/abbr, converts to lowercase and tokenizes words'''
  return  [Tokenizer(t) for t in sent_tokenize(str.replace("'", ""))]
          
def stemming(words_l, type="PorterStemmer", lang="english", encoding="utf8"):
  supported_stemmers = ["PorterStemmer","SnowballStemmer","LancasterStemmer","WordNetLemmatizer"]
  if type is False or type not in supported_stemmers:
    return words_l
  else:
    l = []
    if type == "PorterStemmer":
      stemmer = PorterStemmer()
      for word in words_l:
        l.append(stemmer.stem(word).encode(encoding))
    if type == "SnowballStemmer":
      stemmer = SnowballStemmer(lang)
      for word in words_l:
        l.append(stemmer.stem(word).encode(encoding))
    if type == "LancasterStemmer":
      stemmer = LancasterStemmer()
      for word in words_l:
        l.append(stemmer.stem(word).encode(encoding))
    if type == "WordNetLemmatizer": #TODO: context
      wnl = WordNetLemmatizer()
      for word in words_l:
        l.append(wnl.lemmatize(word).encode(encoding))
    return l       

#Removing stopwords. Takes list of words, outputs list of words.
def remove_stopwords(l_words, lang='english'):
  l_stopwords = stopwords.words(lang)
  content = [w for w in l_words if w.lower() not in l_stopwords]
  return content
 
def words(text): return re.findall('[a-z]+', text.lower()) 

def train(features):
    model = collections.defaultdict(lambda: 1)
    for f in features:
        model[f] += 1
    return model

#big.txt is a large text file of a million or so words. You can use any
#large corpus of words, as long as they're all spelled correctly. I retrieved
#this one from here (http://norvig.com/spell-correct.html)
NWORDS = train(words(file('big.txt').read()))

alphabet = 'abcdefghijklmnopqrstuvwxyz'

def edits1(word):
   splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
   deletes    = [a + b[1:] for a, b in splits if b]
   transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
   replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
   inserts    = [a + c + b     for a, b in splits for c in alphabet]
   return set(deletes + transposes + replaces + inserts)

def known_edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

def known(words): return set(w for w in words if w in NWORDS)

def correct(word):
    candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
    return max(candidates, key=NWORDS.get)

#Stem all words with stemmer of type, return encoded as "encoding"
def preprocess_pipeline(str, lang="english", stemmer_type="PorterStemmer", return_as_str=False, 
            do_remove_stopwords=False, do_clean_html=False, do_spellcheck=False):
  l = []
  words = []
  if do_clean_html:
    sentences = tokenize(str)
  else:
    sentences = tokenize(str)
  for sentence in sentences:
    if do_remove_stopwords:
      words = remove_stopwords(sentence, lang)
    else:
      words = sentence
      if do_spellcheck:
                  for i, w in enumerate(sentence):
                      sentence[i] = correct(w)
    words = stemming(words, stemmer_type)
    if return_as_str:
      l.append(" ".join(words))
    else:
      l.append(words)
  if return_as_str:
    return " ".join(l)
  else:
    return l   