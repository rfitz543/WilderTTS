#include <Rcpp.h>
#include <numeric>      // for accumulate

using namespace Rcpp;
using namespace std;

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
    int sz = x.nrow();
    NumericVector tmp1 = HtCy(x);
    NumericVector tmp2 = LtCy(x);
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = std::max(std::abs(tmp1[i]), std::abs(tmp2[i]));
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector get_r(NumericMatrix x)
{
    int sz = x.nrow();
    NumericVector tmp1 = HtCy(x);
    NumericVector tmp2 = LtCy(x);
    NumericVector tmp3 = HtLt(x);
    NumericVector tmp4 = CyOy(x);
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        if(std::abs(tmp1[i]) >= std::abs(tmp2[i]) && std::abs(tmp1[i]) >= std::abs(tmp3[i]))
        {
            vec[i] = std::abs(tmp1[i])-.5*std::abs(tmp2[i])+.25*std::abs(tmp4[i]);
        }
        else if(std::abs(tmp2[i]) >= std::abs(tmp1[i]) && std::abs(tmp2[i]) >= std::abs(tmp3[i]))
        {
            vec[i] = std::abs(tmp2[i])-.5*std::abs(tmp1[i])+.25*std::abs(tmp4[i]);
        }
        else if(std::abs(tmp3[i]) >= std::abs(tmp1[i]) && std::abs(tmp3[i]) >= std::abs(tmp2[i]))
        {
            vec[i] = std::abs(tmp3[i])+.25*std::abs(tmp4[i]);
        }
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector get_num(NumericMatrix x)
{
    int sz = x.nrow();
    int op = 0;
    int cl = 3;
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = (x(i,cl)-x(i-1,cl)+.5*(x(i,cl)-x(i,op))+.25*(x(i-1,cl)-x(i-1,op)));;
    }
    return vec;
}

// [[Rcpp::export]]
NumericVector swing_index(NumericMatrix x, double l)
{
    int sz = x.nrow();
    NumericVector num = get_num(x);
    NumericVector r = get_r(x);
    NumericVector k = get_k(x);
    NumericVector vec(sz);
    
    vec[0] = NA_REAL;
    
    for(int i=1; i<sz; i++)
    {
        vec[i] = 50*((double)num[i]/(double)r[i])*((double)k[i]/l);
    }
    return vec;
}
