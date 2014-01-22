#include <Rcpp.h>
#include <numeric>      // for accumulate

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericVector quick_sar_long(NumericMatrix x, double stop) 
{
    int hi = 1;
    int lo = 2;
    
    int sz = x.nrow();
    NumericVector ep(sz);
    NumericVector af(sz);
    NumericVector sar(sz);
    
    ep[0] = x(0, hi);
    sar[0] = stop;
    af[0] = .02;
    for(int i=1; i<sz; i++)
    {
      sar[i] = sar[i-1] + af[i-1]*(x(i, hi)-sar[i-1]);
      ep[i] = std::max(x.begin(), x.end()-sz+i);
       
      if(ep[i]!=ep[i-1])
      {
        af[i] = std::min(af[i-1]+.02, .2);
      }
      else
      {
        af[i] = af[i-1];
      }
    }
    
    return sar;
}
