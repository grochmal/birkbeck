#!/usr/bin/env python

import os,sys,json
PROGNM      = os.path.basename(sys.argv[0])
os.environ['PATH'] += ":"+(os.path.dirname(sys.argv[0]) or '.')
SHORT_USAGE = PROGNM + " <vectors.json> <query_vectors.json>"
__doc__       = """Usage:
    """+ SHORT_USAGE +"""

Counts the number of equal vectors"""

X,Y = 0,1
SIM = 120

def similar(vec1, vec2):
    difx,dify = abs(vec1[X]-vec2[X]),abs(vec1[Y]-vec2[Y])
    if SIM >= difx+dify: return True
    return False

def move(f1, f2, pt1, pt2):
    if f1[pt1] == f2[pt2]: pt1,pt2 = pt1+1,pt2+1
    elif f1[pt1][X] == f2[pt2][X]:
        if f1[pt1][Y] > f2[pt2][Y] : pt2 += 1
        else                       : pt1 += 1
    elif f1[pt1][X] > f2[pt2][X] : pt2 += 1
    else                         : pt1 += 1
    return pt1,pt2

if not 3 <= len(sys.argv): sys.exit(SHORT_USAGE)
fig1  = json.loads(open(sys.argv[1]).read())
fig2  = json.loads(open(sys.argv[2]).read())

lenf1,lenf2,p1,p2 = len(fig1),len(fig2),0,0
equal = 0
while p1 < lenf1 and p2 < lenf2:
    if similar(fig1[p1],fig2[p2]): equal += 1
    p1,p2 = move(fig1, fig2, p1, p2)
print "-"*60
print str(equal) + " equal vectors"
print "divided by " + str(lenf1)
print sys.argv[1], sys.argv[2]
print "similarity is: " + str(float(equal)/lenf1)

