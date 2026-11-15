import json,os,urllib.request
U=os.environ['MAKEFUN_ENV_URL'];V={'p':100,'n':320,'b':330,'r':500,'q':900,'k':0};D={'n':(-33,-31,-18,-14,14,18,31,33),'b':(-17,-15,15,17),'r':(-16,-1,1,16),'q':(-17,-16,-15,-1,1,15,16,17),'k':(-17,-16,-15,-1,1,15,16,17),'p':()};N=0;CAP=0;K={}
def post(p,b):return json.load(urllib.request.urlopen(urllib.request.Request(U+p,data=json.dumps(b).encode(),headers={'content-type':'application/json'})))
def init(f):
 global B,S,E
 a=f.split();B=[0]*128
 for y,r in enumerate(a[0].split('/')):
  x=0
  for c in r:
   if c.isdigit():x+=int(c)
   else:B[y*16+x]=c;x+=1
 S=1 if a[1]=='w' else -1;E=(int(a[3][1])-1)*16+ord(a[3][0])-97 if a[3]!='-' else -1
def nm(s):return chr(s%16+97)+str(8-s//16)
def attack(t,side):
 for s,p in enumerate(B):
  if p and p.isupper()==(side==1):
   z=p.lower()
   if z=='p':
    for d in ((-17,-15) if side==1 else (15,17)):
     if s+d==t:return 1
   else:
    for d in D[z]:
     q=s+d
     while 0<=q<128 and not q&0x88:
      if q==t:return 1
      if B[q] or z in 'nk':break
      q+=d
 return 0
def chk(side):return attack(B.index('K' if side==1 else 'k'),-side)
def gen(cap=0):
 out=[]
 for s,p in enumerate(B):
  if not p or p.isupper()!=(S==1):continue
  z=p.lower();dirs=((-17,-15) if S==1 else (15,17)) if z=='p' else D[z]
  if z=='p':
   d=-16 if S==1 else 16;t=s+d
   if not cap and not B[t]:
    out.append((s,t,'q' if t//16 in (0,7) else ''))
    if s//16==(6 if S==1 else 1) and not B[t+d]:out.append((s,t+d,''))
   for d in dirs:
    t=s+d
    if 0<=t<128 and not t&0x88 and ((B[t] and B[t].isupper()!=(S==1) and B[t].lower()!='k') or t==E):out.append((s,t,'q' if t//16 in (0,7) else ''))
  else:
   for d in dirs:
    t=s+d
    while 0<=t<128 and not t&0x88:
     if B[t]:
      if B[t].isupper()!=(S==1) and B[t].lower()!='k':out.append((s,t,''))
      break
     if not cap:out.append((s,t,''))
     if z in 'nk':break
     t+=d
 return out
def move(m):
 global S,E
 s,t,p=m;a=B[s];v=B[t];ep=a.lower()=='p' and t==E and s%16!=t%16 and not v;old=(S,E)
 B[s]=0;B[t]=(p.upper() if S==1 else p) if p else a
 if ep:B[t+(16 if S==1 else -16)]=0
 E=(s+t)//2 if a.lower()=='p' and abs(t-s)==32 else -1;S=-S
 return a,v,ep,old
def undo(m,u):
 global S,E
 s,t,p=m;a,v,ep,old=u;S,E=old;B[s]=a;B[t]=v
 if ep:B[t+(16 if S==1 else -16)]='p' if S==1 else 'P'
def ev():
 x=0
 for s,p in enumerate(B):
  if p:
   c=1 if p.isupper() else -1;z=p.lower();f=s%16;r=s//16;a=7-r if c==1 else r
   x+=c*(V[z]+(a*3 if z=='p' else (7-abs(3-f)-abs(3-r))*3))
 return x*S
def search(d,a,b,p=0,q=0):
 global N
 N+=1
 if N>CAP:raise OverflowError
 inc=chk(S) if d<=0 else 0
 if d<=0:
  v=ev()
  if not inc:
   if q>=4 or v>=b:return v
   a=max(a,v)
 else:v=-99999
 ms=gen(d<=0 and not inc);ms.sort(key=lambda m:(V[B[m[1]].lower()]*10-V[B[m[0]].lower()]//10 if B[m[1]] else 0)+(4000 if K.get((p,m[0],m[1])) else 0),reverse=True)
 if not ms:return -30000+p if chk(S) else (v if d<=0 and not inc else 0)
 n=0
 for m in ms:
  u=move(m)
  if not chk(-S):n+=1;x=-search(d-1,-b,-a,p+1,q+1 if d<=0 else 0)
  else:x=-99999
  undo(m,u)
  if x>v:v=x
  if x>a:a=x
  if a>=b:
   if not B[m[1]]:K[p,m[0],m[1]]=1
   break
 if not n:return -30000+p if chk(S) else (v if d<=0 and not inc else 0)
 return v
def choose(f,legal):
 global CAP,N,K
 init(f);L=set(legal);M={nm(s)+nm(t)+p:(s,t,p) for s,t,p in gen()};O=[(u,m) for u,m in M.items() if u in L];best=O[0][0];CAP=50000;N=0;K={}
 for d in range(1,30):
  score=-99999;pick=best
  try:
   for u,m in sorted(O,key=lambda x:x[0]!=best):
    un=move(m);v=-search(d-1,-99999,99999,1) if not chk(-S) else -99999;undo(m,un)
    if v>score:score=v;pick=u
   best=pick
  except OverflowError:break
 return best
while 1:
 g=post('/game',{})
 if g['status']=='done':break
 while g['status']=='your_move':g=post('/move',{'game':g['game'],'move':choose(g['fen'],g['legal_moves'])})
