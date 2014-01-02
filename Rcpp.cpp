#include <Rcpp.h>
#include <numeric>      // for accumulate

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericVector run_sum(NumericVector x, int n) 
{
    int sz = x.size();
    NumericVector res(sz);
    
    res[n-1] = std::accumulate(x.begin(), x.end()-sz+n, 0.0);
    
    for(int i = n; i < sz; i++) 
    {
       res[i] = res[i-1] + x[i] - x[i-n];
    }
    // pad the first n-1 elements with NA
    std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
    return res;
}

// [[Rcpp::export]]
NumericVector run_mean(NumericVector x, int n) 
{
    return run_sum(x, n) / (double)n;
}

// [[Rcpp::export]]
NumericVector run_min(NumericVector x, int n)
{
    int sz = x.size();
    NumericVector res(sz);
    
    for(int i = 0; i < (sz-n+1); i++)
    {
        res[i+n-1] = *std::min_element(x.begin() + i, x.end() - sz + n + i);
        cout << i << endl;
    }
    // pad the first n-1 elements with NA
    std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
    return res;
}

// [[Rcpp::export]]
NumericVector run_max(NumericVector x, int n)
{
    int sz = x.size();
    NumericVector res(sz);
    for(int i = 0; i < (sz-n+1); i++)
    {
        res[i+n-1] = *std::max_element(x.begin() + i, x.end() - sz + n + i);
    }
    // pad the first n-1 elements with NA
    std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
    return res;
}

// [[Rcpp::export]]
NumericVector tr(NumericMatrix x) 
{
    int nrow = x.nrow();
    int hi = 1;
    int lo = 2;
    int cl = 3;
    NumericVector vec(nrow);
    
    vec[0] = x(0, hi)-x(0, lo);
    
    for (int i = 1; i < nrow; i++) 
    {
    double tmp1 = x(i, hi) -x(i, lo);
    double tmp2 = std::abs(x(i, hi) - x(i-1, cl));
    double tmp3 = std::abs(x(i, lo) - x(i-1, cl));
    vec[i] = std::max(tmp1, std::max(tmp2, tmp3));
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector atr(NumericMatrix x, int n) 
{
    int sz = x.nrow();
    NumericVector vec(sz);
    NumericVector trange(sz);
    
    trange = tr(x);
    
    vec[n-1] = std::accumulate(trange.begin(), trange.end()-sz+n, 0.0)/(double)n;
    
    for (int i = n; i < sz; i++) 
    {
    vec[i] = (vec[i-1]*(n-1)+trange[i])/n;
    }
    // pad the first n-1 elements with NA
    std::fill(vec.begin(), vec.end()-sz+n-1, NA_REAL);
    return vec;
}
