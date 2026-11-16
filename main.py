import json,os
from urllib.request import urlopen as u,Request as R
U=os.environ['MAKEFUN_ENV_URL'];V={'p':100,'n':320,'b':330,'r':500,'q':900,'k':0};D={'n':(-33,-31,-18,-14,14,18,31,33),'b':(-17,-15,15,17),'r':(-16,-1,1,16)};D['q']=D['k']=D['b']+D['r'];W=-99999;N=0;CAP=0;K={};T={}
J=[*zip(D['n'],[1]*8),*zip(D['k'],[5]*8)]
Y=[(0,5),(4,0),(3,1),(2,3),(2,0),(-2,-3)]
P={}
for z,(w,v)in zip('pnbrqk',Y):
 t=[V[z]+w*(7-abs(3-s%16)-abs(3-s//16))+v*(7-s//16)for s in range(128)];P[z.upper()]=t;P[z]=[-t[s^112]for s in range(128)]
def post(p,b):return json.load(u(R(U+p,data=json.dumps(b).encode())))
def init(f):
 global B,S,E
 a=f.split();B=[0]*128
 for y,r in enumerate(a[0].split('/')):
  x=0
  for c in r:
   if c.isdigit():x+=int(c)
   else:B[y*16+x]=c;x+=1
 S=2*(a[1]=='w')-1;E=(8-int(a[3][1]))*16+ord(a[3][0])%97 if a[3]!='-' else -1
def nm(s):return chr(s%16+97)+str(8-s//16)
def attack(t,side):
 E='PNBRQK'if side>0 else'pnbrqk'
 for d in D['k']:
  q=t+d
  while q&0x88==0:
   p=B[q]
   if p:
    if p==E[2+(d in D['r'])]or p==E[4]:return 1
    break
   q+=d
 for o,i in J:
  q=t+o
  if q&0x88==0 and B[q]==E[i]:return 1
 for o in(16*side-1,16*side+1):
  q=t+o
  if q&0x88==0 and B[q]==E[0]:return 1
 return 0
def chk(side):return attack(B.index('Kk'[side<0]),-side)
def gen(cap=0):
 out=[]
 for s,p in enumerate(B):
  if not p or p.isupper()!=(S>0):continue
  z=p.lower();dirs=(-16*S-1,-16*S+1)if z=='p'else D[z]
  if z=='p':
   d=-16*S;t=s+d
   if not cap and not B[t]:
    out.append((s,t,'q'*(t<16or t>111)))
    if s//16==1+5*(S>0) and not B[t+d]:out.append((s,t+d,''))
   for d in dirs:
    t=s+d
    if t&0x88==0 and ((B[t] and B[t].isupper()!=(S>0) and B[t]not in'Kk') or t==E):out.append((s,t,'q'*(t<16or t>111)))
  else:
   for d in dirs:
    t=s+d
    while t&0x88==0:
     if B[t]:
      if B[t].isupper()!=(S>0)and B[t]not in'Kk':out.append((s,t,''))
      break
     if not cap:out.append((s,t,''))
     if z in'nk':break
     t+=d
 return out
def move(m):
 global S,E
 s,t,p=m;a=B[s];z=a.lower();v=B[t];ep=z=='p' and t==E;old=(S,E)
 B[s],B[t]=0,a if not p else'Qq'[S<0]
 if ep:B[t+16*S]=0
 E=(s+t)//2if z=='p'and abs(t-s)==32else-1;S=-S
 return a,v,ep,old
def undo(m,u):
 global S,E
 s,t,p=m;a,v,ep,old=u;S,E=old;B[s]=a;B[t]=v
 if ep:B[t+16*S]='pP'[S<0]
def ev():
 x=0
 for s,p in enumerate(B):
  if p:x+=P[p][s]
 return x*S
def search(d,a,b,p=0,q=0):
 global N
 N+=1
 if N>CAP:1/0
 inc=chk(S)*(d<=0)
 if d<=0:
  v=ev()
  if not inc:
   if q>3or v>=b:return v
   a=max(a,v)
 else:
  v=W;k=(tuple(B),S,E);e=T.get(k);a0,b0=a,b
  if e and e[0]>=d:
   if e[2]==0:return e[1]
   if e[2]==1:b=min(b,e[1])
   else:a=max(a,e[1])
   if a>=b:return e[1]
 ms=gen(d<=0and not inc);ms.sort(key=lambda m:(V[B[m[1]].lower()]*10if B[m[1]]else 0)+K.get((p,)+m[:2],0)+(9999 if d>0 and e and m==e[3]else 0),reverse=True)
 n=0;bm=0
 for m in ms:
  u=move(m)
  if not chk(-S):n+=1;x=-search(d-1,-b,-a,p+1,q+1 if d<=0else 0)
  else:x=W
  undo(m,u)
  if x>v:v=x;bm=m
  a=max(a,x)
  if a>=b:
   if not B[m[1]]:K[(p,)+m[:2]]=4000
   break
 if not n:return p-30000 if chk(S)else(v if d<=0and not inc else 0)
 if d>0 and abs(v)<29000:T[k]=(d,v,0 if a0<v<b0 else 2 if v>=b0 else 1,bm)
 return v
def choose(f,legal):
 global CAP,N,K,T
 init(f);L=set(legal);M={nm(s)+nm(t)+p:(s,t,p)for s,t,p in gen()};O=[(u,M[u])for u in M if u in L];best=O[0][0];CAP=100000;N=0;K={};T={}
 for d in range(1,30):
  score=W;pick=best
  try:
   for u,m in sorted(O,key=lambda x:x[0]!=best):
    un=move(m);v=-search(d-1,W,-W,1) if not chk(-S) else W;undo(m,un)
    if v>score:score=v;pick=u
   best=pick
  except Exception:break
 return best
while 1:
 g=post('/game',{})
 if g['status']=='done':break
 while g['status']=='your_move':g=post('/move',{'game':g['game'],'move':choose(g['fen'],g['legal_moves'])})
