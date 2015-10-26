#!/usr/bin/env python

__version__   = "0.6"
__author__    = "Michal Grochmal"
__copyright__ = "Michal Grochmal (C) 2013"
__license__   = "GNU General Public License, version 3 or later"
__date__      = "2013-11-12"

import os,sys
PROGNM        = os.path.basename(sys.argv[0])
os.environ['PATH'] += ":"+(os.path.dirname(sys.argv[0]) or '.')
SHORT_USAGE   = PROGNM + " [-qfhsvv][-l log][-d [no]key[=value]] <images>"

__doc__       = """Usage:
    """+ SHORT_USAGE +"""

    """+ PROGNM +""" [--query][--fast][--log=log][--help][--silent][--verbose]
      [--verbose][--define [no]key[=value]][--define [no]key[=value]] <images>

Converts a bitmap image (PNG, JPEG, TIFF, GIF or PNM/PBM) into a set of
vectors which can be indexed

TODO options description

TODO all options for --define"""

import re,getopt,datetime,json
from subprocess import Popen,PIPE

LWRN = 0 ; LINF = 1 ; LDBG = 2
DFLTS  = { "query"   : False , "imgsz"   : "256"
         , "outdat"  : True  , "outjson" : True
         , "outsmpl" : True  , "outplot" : True
         , "debug"   : False
         }
TOOLS  = { "identify" : (lambda x: ["identify","-format","%w,%h",x])
         , "resize"   : (lambda s: ["convert","-","-resize","x"+s,"png:-"])
         , "resrot"   : (lambda s: ["convert","-","-resize",s
                                   ,"-rotate","90","png:-"])
         , "pngtopam" : (lambda _: ["pngtopam"])
         , "mkbitmap" : (lambda q: [ "mkbitmap","-s","1"]
                                   + (q and ["-b","1"] or []))
         , "potrsvg"  : (lambda n: ["potrace","--tight","-H","10cm"
                                   ,"-s","--group","-o",n])
         , "potrjson" : (lambda s: ["potrace","--tight","-H",s,"-b","geojson"])
         , "vecgraph" : (lambda i,o,q: ["vecgraph.R",i,o]
                                       + (q and ["green"] or []))
         }

def geojson_extract(log, vecimg):
    (t,fs,f) = ('type' , 'features' , 'feature')
    (c,gs,g) = ('coordinates' , 'geometries' , 'geometry')
    geo = { 'Point'      : 'pt' , 'MultiPoint'        : 'mpt'
          , 'LineString' : 'ls' , 'MultiLineString'   : 'mls'
          , 'Polygon'    : 'pl' , 'MultiPolygon'      : 'mpl'
          , 'Feature'    : 'f'  , 'FeatureCollection' : 'fc'
          , 'GeometryCollection' : 'gc'
          }
    lv1flatten = lambda l: reduce(lambda x,y: x + y, l, [])
    geopar = { 'pt'  : (lambda p,x: [x[c]])
             , 'mpt' : (lambda p,x: x[c])
             , 'ls'  : (lambda p,x: x[c])
             , 'mls' : (lambda p,x: lv1flatten(x[c]))
             , 'pl'  : (lambda p,x: lv1flatten(x[c]))
             , 'mpl' : (lambda p,x: lv1flatten(lv1flatten(x[c])))
             , 'f'   : (lambda p,x: p(p,x[g]))
             , 'fc'  : (lambda p,x: reduce(lambda x,y: x + p(p,y), x[fs], []))
             , 'gc'  : (lambda p,x: reduce(lambda x,y: x + p(p,y), x[gs], []))
             }
    def pgeo(p,gobj):
        if not t in gobj:
            log(LWRN, "geojson: no type in geojson obj, ignoring obj\n")
            return []
        if not gobj[t] in geo:
            log(LWRN, "geojson: unknown type ["+ gobj[t] +"], ignoring obj\n")
            return []
        log(LDBG, "geojson: parsing ["+ gobj[t] +"]\n")
        return geopar[geo[gobj[t]]](p,gobj)
    return pgeo(pgeo, vecimg)

def genlogger(verbose=0, outputs=[]):
    lvls = { LWRN : "WRN", LINF : "INF", LDBG : "DBG" }
    if 0 < verbose : outputs.append(sys.stdout)
    else           : outputs.append(sys.stderr)
    def logger(lvl,msg):
        if lvl <= verbose:
            dt = str(datetime.datetime.now())
            for o in outputs: o.write(PROGNM+":"+lvls[lvl]+":"+dt+": "+msg)
    return logger

def expl(cmds, prevout=None, lastproc=None):
    if cmds:
        cmd  = cmds.pop(0)
        proc = Popen(cmd, stdout=PIPE, stdin=prevout)
        if prevout: prevout.close()
        return expl(cmds, proc.stdout, proc)
    elif lastproc:
        out,err = lastproc.communicate()
        ret     = lastproc.returncode
        if 0 != ret: raise OSError("child returned: " + str(ret))
        return out
    else:
        assert False, "Bad pipeline, no last process"

def imgsize(log, imgpath):
    id = TOOLS['identify'](imgpath)
    log(LDBG, "[" + " ".join(id) + "]\n")
    idout = expl([id])
    match = re.match("^(\d+),(\d+)", idout)
    if not match: raise IOError('identify did not return size for: ' + imgpath)
    return match.groups()

def resizeimg(log, w, h, defs=DFLTS):
    if int(w) >= int(h):
        res = TOOLS['resize'](defs['imgsz'])
        log(LDBG, "resizing to " + defs['imgsz'] + "px\n")
    else:
        res = TOOLS['resrot'](defs['imgsz'])
        log(LWRN, "rotating image as width < height\n")
    return [res]

def traceimg(log, img, transform):
    cmdstrs = reduce(lambda x,y: x + [" ".join(y)], transform, [])
    log(LDBG, "exec cmd [ " + " | ".join(cmdstrs) + " ]\n")
    imgfh = open(img['path'], 'rb')
    geojson = expl(transform, imgfh)
    imgfh.close()
    return geojson

def vectorise(log, img, outsmpl, defs=DFLTS):
    transform = []
    w,h = imgsize(log, img['path'])
    log(LINF, "image width: " + w + ", height: " + h + "\n")
    transform += resizeimg(log, w, h, defs)
    transform += [TOOLS['pngtopam'](None), TOOLS['mkbitmap'](defs['query'])]
    if defs['outsmpl']:
        traceimg(log, img, transform+[TOOLS['potrsvg'](outsmpl)])
        log(LINF, "vectorisation sample written to " + outsmpl + "\n")
    return traceimg(log, img, transform+[TOOLS['potrjson'](defs['imgsz'])])

def cog_center(tups):
    (x,y,k) = (0,1,len(tups))
    sums    = reduce(lambda a,p: (a[x]+p[x],a[y]+p[y]), tups, (0,0))
    (cx,cy) = (sums[x]//k , sums[y]//k)
    return reduce(lambda a,p: a + [(p[x]-cx,p[y]-cy)], tups, [])

def output(log, tups, file, printer):
    msg = str(len(tups)) + " points to " + file
    try: fhdat = open(file, 'wb')
    except IOError as e:
        log(LWRN, "could not write " + msg + "\n")
        raise
    printer(fhdat, tups)
    fhdat.close()
    log(LINF, "written " + msg + "\n")

def graph(log, infile, outfile, defs=DFLTS):
    graph = TOOLS['vecgraph'](infile, outfile, defs['query'])
    log(LDBG, "[" + " ".join(graph) + "]\n")
    out = expl([graph])
    log(LDBG, "plotter output:\n" + out)
    log(LINF, "graph written to " + outfile + "\n")

def process_img(log, img, defs=DFLTS):
    genout = lambda x: os.path.join(img['dir'], img['name']) + x
    ( outsmpl, outdat
    , outplot, outjson) = map(genout, (".svg",".dat",".plot.png",".json"))
    geojson  = vectorise(log, img, outsmpl, defs)
    vecimg   = json.loads(geojson)
    vertices = geojson_extract(log, vecimg)
    log(LINF, "produced " + str(len(vertices)) + " points from image\n")
    rdtup    = lambda x: int(round(x*10,0))
    tupverts = reduce(lambda a,p: a+[(rdtup(p[0]),rdtup(p[1]))], vertices, [])
    tupverts.sort()
    tups     = cog_center(tupverts)
    printjson = lambda fh,d: fh.write(json.dumps(d))
    printdat  = lambda fh,d: reduce( lambda a,p:
                                     fh.write(str(p[0]) +" "+ str(p[1]) +"\n")
                                   , d, [])
    if defs['outdat'] : output(log, tups, outdat, printdat)
    if defs['outjson']: output(log, tups, outjson, printjson)
    if defs['outplot']: graph(log, outdat, outplot, defs)

def main(log, args, defs=DFLTS):
    def mkimg(acc, path):
        img = {}
        img['path'] = path
        img['dir']  = (os.path.dirname(path) or '.')
        img['base'] = os.path.basename(path)
        img['name'],img['ext'] = os.path.splitext(img['base'])
        return acc + [img]
    imgs = reduce(mkimg, args, [])
    imgpaths = reduce(lambda a,i: a + [i['path']], imgs, [])
    log(LDBG, "images to process: ["+ " ".join(imgpaths) +"]\n")
    for img in imgs:
        log(LINF, "-"*40 + "\n")
        log(LINF, "processing the image [" + img['path'] + "]\n")
        try: process_img(log, img, defs)
        except Exception as e:
            log(LWRN, str(e)+"\n")
            log(LWRN, "could not process "+ img['path'] +"\n")
            if defs['debug']: raise
    log(LINF, "="*20 + " END OF EXECUTION\n")

def define_opt(defs, arg):
    if arg.startswith('no') : define = [arg[len('no'):] , False]
    else                    : define = arg.split('=') + [True]
    defs[define[0]] = define[1]
    return defs

if __name__ == "__main__":
    long_opts = ["query","fast","log=","define=","help","silent","verbose"]
    try: opts,args = getopt.getopt(sys.argv[1:], "qfl:d:hsv", long_opts)
    except getopt.GetoptError as e: sys.exit(str(e) + "\n" + SHORT_USAGE)
    outputs, logfile, logfh, verbose, help, defs = [], '', None, 0, None, DFLTS
    for o,a in opts:
        if   o in ("-q", "--query")   : defs['query'] = True
        elif o in ("-f", "--fast")    :
            defs['outdat']  = False
            defs['outsmpl'] = False
            defs['outplot'] = False
        elif o in ("-l", "--log")     : logfile  = a
        elif o in ("-d", "--define")  : define_opt(defs, a)
        elif o in ("-h", "--help")    : help     = True
        elif o in ("-v", "--verbose") : verbose += 1
        elif o in ("-s", "--silent")  : verbose -= 1
        else: assert False, "Ops... unhandled option [" + str(o) + "]"
    if help:
        print __doc__
        sys.exit(0)
    if not args: sys.exit(SHORT_USAGE)
    if logfile:  logfh = open(logfile, 'ab')
    if logfh:    outputs.append(logfh)
    log = genlogger(verbose, outputs)
    om = "program options:\n"
    log(LDBG, reduce(lambda a,k: a+k+": "+str(defs[k])+"\n", defs.keys(), om))
    main(log, args, defs)
    if logfh: logfh.close()

