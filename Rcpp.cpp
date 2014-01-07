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
NumericVector range_true(NumericMatrix x) 
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
    
    trange = range_true(x);
    
    vec[n-1] = std::accumulate(trange.begin(), trange.end()-sz+n, 0.0)/(double)n;
    
    for (int i = n; i < sz; i++) 
    {
    vec[i] = (vec[i-1]*(n-1)+trange[i])/n;
    }
    // pad the first n-1 elements with NA
    std::fill(vec.begin(), vec.end()-sz+n-1, NA_REAL);
    return vec;
}

// [[Rcpp::export]]
NumericVector HtCy(NumericMatrix x)
{
    int sz = x.nrow();
    int hi = 1;
    int cl = 3;
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = x(i,hi)-x(i-1,cl);
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector LtCy(NumericMatrix x)
{
    int sz = x.nrow();
    int lo = 2;
    int cl = 3;
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = x(i,lo)-x(i-1,cl);
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector HtLt(NumericMatrix x)
{
    int sz = x.nrow();
    int hi = 1;
    int lo = 2;
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = x(i,hi)-x(i,lo);
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector CyOy(NumericMatrix x)
{
    int sz = x.nrow();
    int op = 0;
    int cl = 3;
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = x(i-1,cl)-x(i-1,op);
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector CtCy(NumericMatrix x)
{
    int sz = x.nrow();
    int cl = 3;
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = x(i,cl)-x(i-1,cl);
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector CtOt(NumericMatrix x)
{
    int sz = x.nrow();
    int op = 0;
    int cl = 3;
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = x(i,cl)-x(i,op);
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector get_k(NumericMatrix x)
{
    return std::max(std::abs(HtCy(x), std::abs(LtCy)));
}

// [[Rcpp::export]]
NumericVector swing_index(NumericMatrix x, double l)
{
    int sz = x.nrow();
    int op = 0;
    int hi = 1;
    int lo = 2;
    int cl = 3;
    NumericVector vec(sz);
    NumericVector r(sz);
    NumericVector k(sz);
    NumericVector num(sz);
    
    vec[0] = NA_REAL;
    r[0] = NA_REAL;
    k[0] = NA_REAL;
    num[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        double tmp1 = std::abs(x(i, hi) - x(i-1, cl));
        double tmp2 = std::abs(x(i, lo) - x(i-1, cl));
        double tmp3 = x(i, hi) - x(i, lo);
        if(tmp1 >= tmp2 && tmp1 >= tmp3)
        {
            r[i] = std::abs(x(i,cl)-x(i-1,cl))+.5*std::abs((x(i,cl)-x(i,op)))+.25*std::abs((x(i-1,cl)-x(i-1,op)));
        }
        else if(tmp2 > tmp1 && tmp2 > tmp3)
        {
            r[i] = std::abs(x(i,lo)-x(i-1,cl))+.5*std::abs((x(i,hi)-x(i-1,cl)))+.25*std::abs((x(i-1,cl)-x(i-1,op)));
        }
        else if(tmp3 > tmp1 && tmp3 > tmp2)
        {
            r[i] = x(i,hi)-x(i,lo)+.25*std::abs((x(i-1,cl)-x(i-1,op)));
        }
        k[i] = std::max(std::abs(x(i,hi)-x(i-1,cl)), std::abs(x(i,lo)-x(i-1,cl)));
        num[i] = (x(i,cl)-x(i-1,cl)+.5*(x(i,cl)-x(i,op))+.25*(x(i-1,cl)-x(i-1,op)));
        vec[i] = 50*(num[i]/(double)r[i])*(k[i]/l);
    }
    return vec;
}


// [[Rcpp::export]]
NumericVector swing_index2(NumericMatrix x, double l)
{
    int sz = x.nrow();
    NumericVector CtCy(sz);
    NumericVector CtOt(sz);
    NumericVector CyOy(sz);
    NumericVector vec(sz);
    NumericVector r(sz);
    NumericVector k(sz);
    NumericVector num(sz);
    
    CtCy = NA_REAL;
    CtOt = NA_REAL;
    CyOy = NA_REAL;
    vec[0] = NA_REAL;
    r[0] = NA_REAL;
    k[0] = NA_REAL;
    num[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        num[i] = CtCy()
    }
    
    for(int i=1; i<sz; i++)
    {
        double tmp1 = std::abs(x(i, hi) - x(i-1, cl));
        double tmp2 = std::abs(x(i, lo) - x(i-1, cl));
        double tmp3 = x(i, hi) - x(i, lo);
        if(tmp1 >= tmp2 && tmp1 >= tmp3)
        {
            r[i] = std::abs(x(i,cl)-x(i-1,cl))+.5*std::abs((x(i,cl)-x(i,op)))+.25*std::abs((x(i-1,cl)-x(i-1,op)));
        }
        else if(tmp2 > tmp1 && tmp2 > tmp3)
        {
            r[i] = std::abs(x(i,lo)-x(i-1,cl))+.5*std::abs((x(i,hi)-x(i-1,cl)))+.25*std::abs((x(i-1,cl)-x(i-1,op)));
        }
        else if(tmp3 > tmp1 && tmp3 > tmp2)
        {
            r[i] = x(i,hi)-x(i,lo)+.25*std::abs((x(i-1,cl)-x(i-1,op)));
        }
        k[i] = std::max(std::abs(x(i,hi)-x(i-1,cl)), std::abs(x(i,lo)-x(i-1,cl)));
        num[i] = (x(i,cl)-x(i-1,cl)+.5*(x(i,cl)-x(i,op))+.25*(x(i-1,cl)-x(i-1,op)));
        vec[i] = 50*(num[i]/(double)r[i])*(k[i]/l);
    }
    return vec;
}

