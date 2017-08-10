#include <Rcpp.h>
#include <math.h>

#define SMALL_STEPS 10.0

#define ITERATION_STEPS 25

// [[Rcpp::export]]
Rcpp::NumericVector sir_fixed_susceptible(double i_init, double gamma, double rnaught, int steps)
{
	Rcpp::NumericVector out(steps);
	out[0] = i_init;
	double i = i_init;
	double r = 0;
	double s = 1 - i;
	double beta = gamma * rnaught;
	for(int t=1; t < steps; t++)
	{
		for(int smallstep = 0;smallstep < SMALL_STEPS; smallstep++)
		{
			double s_new = s - s * i * beta / SMALL_STEPS;
			double i_new = i + s * i * beta / SMALL_STEPS - i * gamma / SMALL_STEPS;
			double r_new = 1 - s_new - i_new;
			s = s_new;
			i = i_new;
			r = r_new;
		}
		out[t] = i;
	}
	return out;

}

// [[Rcpp::export]]
Rcpp::NumericVector sir(double i_init, double gamma, double rnaught, double susceptible, int steps)
{
    Rcpp::NumericVector out(steps);
    out[0] = i_init;
    double i = i_init;
    double r = 0;
    double s = susceptible;
    double beta = gamma * rnaught;
    for(int t=1; t < steps; t++)
    {
        for(int smallstep = 0;smallstep < SMALL_STEPS; smallstep++)
        {
            double s_new = s - s * i * beta / SMALL_STEPS;
            double i_new = i + s * i * beta / SMALL_STEPS - i * gamma / SMALL_STEPS;
            double r_new = 1 - s_new - i_new;
            s = s_new;
            i = i_new;
            r = r_new;
        }
        out[t] = i;
    }
    return out;
    
}

double error(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector ignore)
{
	double error = 0;
	for(int i=0;i<x.size();i++)
	{
		if(ignore[i] > 0.5)
			continue;
		error += (x[i] - y[i]) * (x[i] - y[i]);
	}
	return error;
}

double eval_fixed_susceptible(Rcpp::NumericVector toCompare, double gamma, double rnaught, Rcpp::NumericVector ignore)
{
    return error(toCompare, sir_fixed_susceptible(toCompare[0],gamma,rnaught,toCompare.size()), ignore);
}

double eval(Rcpp::NumericVector toCompare, double gamma, double rnaught, double susceptible, Rcpp::NumericVector ignore)
{
    return error(toCompare, sir(toCompare[0],gamma,rnaught,susceptible,toCompare.size()), ignore);
}

double max(double x, double y)
{
    return x>y?x:y;
}

double min(double x, double y)
{
    return x<y?x:y;
}

double log(double x, double base)
{
    return log(x)/log(base);
}



// [[Rcpp::export]]
Rcpp::NumericVector eval_all_one_loop_fixed_s(Rcpp::List toCompare, double gamma_center, double rnaught_center, double step, Rcpp::NumericVector ignore)
{
    double bestGamma = -1;
    double bestRnaught = -1;
    double bestError = 9999999;
    double gamma_center_adjusted = log(gamma_center,3);
    
    for(int gamma_itr = 0; gamma_itr < 25; gamma_itr++)
    {
        for(int rnaught_itr = 0 ; rnaught_itr < 25; rnaught_itr++)
        {
            double gamma = pow(3, gamma_center_adjusted - 10 * pow(5, - step) + 10 * pow(5, -step) * gamma_itr / 12.5);
            
            double rnaught = max(1, rnaught_center - 10 * pow(2.5, -1 - step) + 10 * pow(2.5,  - 1 - step) * rnaught_itr / 12.5);
           
            double error = 0;
            for(int i=0;i<toCompare.size();i++)
                error += eval_fixed_susceptible((Rcpp::NumericVector)toCompare[i], gamma, rnaught, ignore);
            if(error < bestError)
            {
                bestGamma = gamma;
                bestRnaught = rnaught;
                bestError = error;
            }
        }
    }
    
    Rcpp::NumericVector result(3);
    result[0] = bestGamma;
    result[1] = bestRnaught;
    result[2] = bestError;
    /*result[2] = pow(3, gamma_center_adjusted - 10 * pow(5, - step) + 10 * pow(5, -step) * 0 / 12.5);
    result[3] = pow(3, gamma_center_adjusted - 10 * pow(5, - step) + 10 * pow(5, -step) * 25 / 12.5);
    result[4] = max(1, rnaught_center - 10 * pow(2, -1 - step) + 10 * pow(2,  - 1 - step) * 0 / 12.5);
    result[5] = max(1, rnaught_center - 10 * pow(2, -1 - step) + 10 * pow(2,  - 1 - step) * 25 / 12.5);
    for(int gamma_itr = 0;gamma_itr < 25; gamma_itr++)
        result[6+gamma_itr] =pow(3, gamma_center_adjusted - 10 * pow(5, - step) + 10 * pow(5, -step) * gamma_itr / 12.5);*/
    return result;
}


// [[Rcpp::export]]
Rcpp::NumericVector eval_all_one_loop(Rcpp::List toCompare, double gamma_center, double rnaught_center, double susceptible_center, double step, Rcpp::NumericVector ignore)
{
    double bestGamma = -1;
    double bestRnaught = -1;
    double bestSusceptible = -1;
    double bestError = 9999999;
    double gamma_center_adjusted = log(gamma_center,3);
    
    for(int susceptible_itr = 0; susceptible_itr < ITERATION_STEPS; susceptible_itr++)
    {
        for(int gamma_itr = 0; gamma_itr < ITERATION_STEPS; gamma_itr++)
        {
            for(int rnaught_itr = 0 ; rnaught_itr < ITERATION_STEPS; rnaught_itr++)
            {
                double gamma = pow(3, gamma_center_adjusted - 10 * pow(5, - step) + 10 * pow(5, -step) * gamma_itr / (ITERATION_STEPS/2.0));
                
                double rnaught = min(2.2,max(1, rnaught_center - 10 * pow(2, -1 - step) + 10 * pow(2,  - 1 - step) * rnaught_itr / (ITERATION_STEPS/2.0)));
                
                
                double susceptible = max(0, min(1, susceptible_center - 10 * pow(2.5, -1 - step) + 10 * pow(2.5,  - 1 - step) * susceptible_itr / (ITERATION_STEPS/2.0) ));
                
                double error = 0;
                
                for(int i=0;i<toCompare.size();i++)
                    error += eval((Rcpp::NumericVector)toCompare[i], gamma, rnaught, susceptible, ignore);
                
                if(error < bestError)
                {
                    bestGamma = gamma;
                    bestRnaught = rnaught;
                    bestError = error;
                    bestSusceptible = susceptible;
                }
            }
        }
    }
    
    Rcpp::NumericVector result(4);
    result[0] = bestGamma;
    result[1] = bestRnaught;
    result[2] = bestError;
    result[3] = bestSusceptible;
    /*result[2] = pow(3, gamma_center_adjusted - 10 * pow(5, - step) + 10 * pow(5, -step) * 0 / 12.5);
     result[3] = pow(3, gamma_center_adjusted - 10 * pow(5, - step) + 10 * pow(5, -step) * 25 / 12.5);
     result[4] = max(1, rnaught_center - 10 * pow(2, -1 - step) + 10 * pow(2,  - 1 - step) * 0 / 12.5);
     result[5] = max(1, rnaught_center - 10 * pow(2, -1 - step) + 10 * pow(2,  - 1 - step) * 25 / 12.5);
     for(int gamma_itr = 0;gamma_itr < 25; gamma_itr++)
     result[6+gamma_itr] =pow(3, gamma_center_adjusted - 10 * pow(5, - step) + 10 * pow(5, -step) * gamma_itr / 12.5);*/
    return result;
}

