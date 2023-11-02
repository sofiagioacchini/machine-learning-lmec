%% Compute XI and CHI%
act_data = readtable('/Users/sofiagioacchini/Documents/LMEC/Machine learning/act_data.csv');
data_array = table2array(act_data);
data_double = double(data_array); 

%Bartlett triang
w = Bartlett_triang(64);  %N=64 points in window (fonte chat)

%Spectral
[P_chi, D_chi, Sigma_chi] = spectral(data_double, 2, 8, 8);

%% Number of dynamic factors
[nfact, v_nfact, cr] = numfactors(data_double, 10, 61, 1, 3, 'p1', 1000, 8, 8, 1); %2 dynamic

%% Number of static factors (Alessi, Barigozzi, Capasso)
[rhat1 rhat2] = ABC_crit(data_double, 10, 24, 3, 1); %4 for large, 8 for small

%% CHI and XI for all observations (T by n)
[chi_tot, xi_tot, X, factors, loadings, forecast1_chi, vardec] = gdfm_onesided(data_double,2,4,8,8,1,1); %1-step
[chi_tot4, xi_tot4, X, factors, loadings, forecast4_chi, vardec] = gdfm_onesided(data_double,2,4,8,8,4,1); %4-steps

%% Forecast chi for all variables
fcast_chi1 = zeros(26,246);
for i=1:26
    [chi, xi, X, factors, loadings, forecast_chi, vardec] = gdfm_onesided(data_double(i:51+i,:),2,4,8,8,1,1);
    for j=1:246
    fcast_chi1(i,j) = forecast_chi(1,j);
    end
end

fcast_chi4 = zeros(26,246);
for i=1:23
[chi, xi, X, factors, loadings, forecast_chi_i, vardec] = gdfm_onesided(data_double(i:51+i,:),2,4,8,8,4,1);
    for j=1:246
    fcast_chi4(i+3,j) = forecast_chi_i(1,j);
    end
end

%% Stacking the rolling models
for i = 1:26
    [chi, xi, X, factors, loadings, forecast_chi_i, vardec] = gdfm_onesided(data_double(i:51+i,:),2,4,8,8,1,1);
    if i == 1   
        chi_rol = chi;
        xi_rol = xi;
    else 
        chi_rol(end+1:end+52,:) = chi;
        xi_rol(end+1:end+52,:) = xi;
    end
    
end

%1352= 26 x 52(lenght window)
