import lzma
from string import punctuation as punct
from string import ascii_letters as latin

# reading tsv from archive into a one-sentence lemm \n separated file
with lzma.open('ruwac-parsed.out.xz', mode='rt', encoding='utf-8') as f:
    with open('text.txt', mode='w', encoding='utf-8') as t:
        for line in f.readlines(10000):
            l = line.split('\t')
            if len(l) == 7:
                lemm = l[3]
                sent = l[1]
                if sent == 'SENT':
                    print('\n', file=t, end='')
                elif lemm not in punct:
                    print(lemm, file=t, end=' ')

# getting rid of sentences with latin characters
with open('text.txt', mode='r', encoding='utf-8') as t:
    with open('clean.txt', mode='w', encoding='utf-8') as c:
        for line in t.readlines():
            if set(latin).intersection(set(line)) == set():
                print(line, file=c, end='')
