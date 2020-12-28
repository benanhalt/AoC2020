import math

key1 = 9232416
key2 = 14144084

p = 20201227


def solve(g, h, p):
    N = int(math.ceil(math.sqrt(p - 1))) 
    # print ("N=",N)
    t = {}
    # Baby step.
    for i in range(N):
        t[pow(g, i, p)]=i
        # print( "Baby step",t)
    # Fermat’s Little Theorem
    c = pow(g, N * (p - 2), p)
 
    for j in range(N):
        y = (h * pow(c, j, p)) % p
        if y in t: 
            return j * N + t[y]
    return None

ls1 = solve(7,key1,p)
ls2 = solve(7,key2,p)
print(ls1,ls2)

def xform(sn, ls):
    v = 1
    for _ in range(ls):
        v *= sn
        v %= p
    return v

print(xform(key1, ls2))
print(xform(key2, ls1))
      
