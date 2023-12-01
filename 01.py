import sys
import readInput

trans = {
        'one' : '1',
        'two' : '2',
        'three' : '3',
        'four' : '4',
        'five' : '5',
        'six' : '6',
        'seven' : '7',
        'eight' : '8',
        'nine' : '9',
        }

def getNums(x):
    string = ''.join(i for i in x if i.isdigit());
    first = string[0]
    last = string[-1]
    return int(first + last);

def day1():
    p1 = 0
    p2 = 0

    for i in readInput.yieldLines("1.txt"):
        #first
        p1 += getNums(i)

        #second
        #replace the first instance of the word
        ll = i
        x = sys.maxsize
        v = ''
        for k in trans:
            t = ll.find(k)
            if t == -1:
                continue
            if t < x:
                x = t
                v = k

        if (v != ''):
            ll = ll.replace(v,trans[v],1)
        s = ''.join(x for x in ll if x.isdigit());
        f = s[0]

        
        #replace the last instance of the word
        ll = i
        x = 0
        v = ''
        for k in trans:
            t = ll.rfind(k)
            if t == -1:
                continue
            if t > x:
                x = t
                v = k

        if (v != ''):
            ll = readInput.rreplace(ll, v,trans[v],1)
        s = ''.join(x for x in ll if x.isdigit());
        sc = s[-1]

        p2 += int(f + sc);
    
    print("day 1 p1:", p1)
    print("day 1 p2:", p2)

if __name__ == "__main__":
    day1()
