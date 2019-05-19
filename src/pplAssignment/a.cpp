// https://github.com/likecs/Competitive-Programming-Setup/blob/master/code_template.cpp //

#include <bits/stdc++.h>
using namespace std;
 
typedef long long LL; 
typedef long double LD;

const int MAX   = 1e5 + 5;
const int MOD   = 1e9 + 7;
const LD  EPS   = 1e-10;
const LD PI = acos(-1.0);
#define fastio ios_base::sync_with_stdio(0);cin.tie(0);cout.tie(0);
#define rep(i,a,b,d) for(LL i = a; i<=b; i+=d)
#define brep(i,a,b,d) for(LL i = a; i>=b; i-=d)
#define pb push_back
#define f first
#define s second
#define pii pair<LL, LL>

pii cal(LL x, LL y, LL n, LL m)
{
    LL s=x+y;
    LL d=x-y;
    LL ma;
    LL mi;

    LL xmin = max(1LL, s-m);
    LL xmax = min(n, s-1);

    ma = xmax-xmin+1;

    LL ymin = max(1LL, 1-d);
    LL ymax = min(m, n-d);
    mi = ymax-ymin+1;
    return make_pair(ma,mi);
}
int main() {
    fastio
    LL tc; cin>>tc;
    while(tc--){
        LL n,m,x,y; cin>>n>>m>>x>>y;
        LL ans;
        ans = 0;
        pii t=cal(x,y-y+1,n,m-y+1);
        LL ma1=t.first;
        LL mi1=t.second;

        t=cal(x,y,n,m);
       LL  ma3=t.first;
        LL mi3=t.second;
        LL ma2 = ma3-ma1;
        LL mi2 = mi3-mi1;
        ma1-=1;
        mi1-=1;

        ans += 2*((n-x)*(x-1) + (m-y)*(y-1) + ma1*ma2 + mi1*mi2);
        rep(_x,1,n,1){
         rep(_y,1,m,1){
            t= cal(_x,_y,n,m);
            LL ma=t.first;
            LL mi=t.second;

            if(not (x==_x and y==_y))
                ans += m*n - 1 - (m-1) - (n - 1) - (ma-1) - (mi-1);
            else
                ans -= m*n - 1 - (m-1) - (n - 1) - (ma-1) - (mi-1);
         }
        }
        cout<<ans<<endl;
    }   
    return 0;
}