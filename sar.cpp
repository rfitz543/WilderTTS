#include <Rcpp.h>
#include <numeric>      // for accumulate
#include <cmath>        // include for round

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericVector quick_sar_long(NumericMatrix x, double stop) 
{
    int hi = 1;
    
    int sz = x.nrow();
    NumericVector ep(sz);
    NumericVector af(sz);
    NumericVector sar(sz);
    
    ep[0] = x(0, hi);
    sar[0] = stop;
    af[0] = .02;
    for(int i=1; i<sz; i++)
    {
      ep[i] = std::max(ep[i-1], x(i, hi));
      sar[i] = std::floor((sar[i-1] + af[i-1]*(ep[i-1]-sar[i-1]))*100+.5)/100;
       
      if(ep[i]>ep[i-1])
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

// [[Rcpp::export]]
NumericVector quick_sar_short(NumericMatrix x, double stop) 
{
    int lo = 2;
    
    int sz = x.nrow();
    NumericVector ep(sz);
    NumericVector af(sz);
    NumericVector sar(sz);
    
    ep[0] = x(0, lo);
    sar[0] = stop;
    af[0] = .02;
    for(int i=1; i<sz; i++)
    {
      ep[i] = std::min(ep[i-1], x(i, lo));
      sar[i] = std::floor((sar[i-1] + af[i-1]*(ep[i-1]-sar[i-1]))*100+.5)/100;
       
      if(ep[i]<ep[i-1])
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
