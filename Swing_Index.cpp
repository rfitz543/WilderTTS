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
        else if(std::abs(tmp2[i]) > std::abs(tmp1[i]) && std::abs(tmp2[i]) > std::abs(tmp3[i]))
        {
            vec[i] = std::abs(tmp2[i])-.5*std::abs(tmp1[i])+.25*std::abs(tmp4[i]);
        }
        else if(std::abs(tmp3[i]) > std::abs(tmp1[i]) && std::abs(tmp3[i]) > std::abs(tmp2[i]))
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
    NumericVector vec(sz);
    NumericVector r = get_r(x);
    NumericVector k = get_k(x);
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
