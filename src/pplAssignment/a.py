t=int(input().strip())

# m and n are interchanged here
def cal(x,y,m,n):
    s = x+y
    xmin = max(1, s-n)
    xmax = min(m, s-1)
    d = x-y
    ymin = max(1, 1-d)
    ymax = min(n, m-d)
    return(xmax-xmin+1, ymax-ymin+1)

for _ in range(t):
    n,m,x,y = list(map(int,input().strip().split()))
    ans = 0
    ma1, mi1 = cal(x,y-y+1,n,m-y+1)
    ma3, mi3 = cal(x,y,n,m)
    
    ma2 = ma3-ma1
    mi2 = mi3-mi1
    ma1-=1
    mi1-=1

    ans += 2*((n-x)*(x-1) + (m-y)*(y-1) + ma1*ma2 + mi1*mi2)
    for _x in range(1,n+1):
        for _y in range(1,m+1):
            ma, mi = cal(_x,_y,n,m)
            if(not (x==_x and y==_y)):
                ans += m*n - 1 - (m-1) - (n - 1) - (ma-1) - (mi-1)
            else:
                ans -= m*n - 1 - (m-1) - (n - 1) - (ma-1) - (mi-1)
    print(ans)