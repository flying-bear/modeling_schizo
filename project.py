import lzma

from datetime import datetime
from string import punctuation as punct
from string import ascii_letters as latin
punct += '»«'

def get_the_fucking_time():
    h, m, s = str(datetime.now().time().split(':')
    return 3600*int(h)+60*int(m)+float(s)

with lzma.open('ruwac-parsed.out.xz', mode='rt', encoding='utf-8') as f:
    with open('text.txt', mode='w', encoding='utf-8') as t:
        current = []
        start_time = get_the_fucking_time()
        for line_number, line in enumerate(f):
            if line_number % 10000000 == 0:
                work_time = get_the_fucking_time()-start_time
                print(line_number, datetime.now().time())
                if line_number != 0 and work_time != 0:
                    speed = line_number/work_time
                    print(speed, '- lines per sec')
                    print((1600000000 - line_number)/(speed*3600), '- expected worktime in hours')
            l = line.split('\t')
            if len(l) == 7:
                lemm = l[3]
                sent = l[1]
                if sent == 'SENT':
                    if set(latin).intersection(set(''.join(current))) == set():
                        current.append('\n')
                        print(' '.join(current), file=t, end='')
                    current = []
                elif lemm not in punct:
                    current.append(lemm)
