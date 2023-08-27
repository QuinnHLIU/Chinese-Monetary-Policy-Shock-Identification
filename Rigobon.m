% Alternative, weak-instruments proof, heteroskedasticity based estimator
%
% Refer: Emi Nakamura and Jon Steinsson, Nov 2013
%
% In order to produce all three sets of estimates (i.e., one for the
% 30-minute policy news shock, one for the 1-day policy news shock,
% and one for the 1-day 2-year Treasury yields), you have to run this
% three times, replacing the value of par.indepVar, below, to each
% of ['path_intra_wide' 'path' 'drawtreasury2y_long'], respectively
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; close all;
cd("E:\Dropbox\UMP_Paper/Git")
addpath(genpath("E:\Dropbox\UMP_Paper/Git"))

s = RandStream('mt19937ar','Seed',20230827); % Seed set to: March 10th 2013 (date when it was first set)
RandStream.setGlobalStream(s);

%% Set Parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Number of iterations in bootstrap
par.NBootAll = 5000;

% The level of the confidence interval to be constructed
par.CILevel = 0.90;

% The full set of assets for which statistics are to be calculated:
par.estVar = {'drawtreasury9m','drawtreasury1y','drawtreasury2y',...
    'drawtreasury5y','drawtreasury7y','drawtreasury8y','drawtreasury9y','drawtreasury10y',...
    'dadjtreasury9m','dadjtreasury1y','dadjtreasury2y','dadjtreasury5y','dadjtreasury7y' ,...
    'dadjtreasury8y','dadjtreasury9y','dadjtreasury10y'};


% Choose asset to plot scatter of Dcov and Dvar as well as quantiles of
% g(gamma) as a function of gamma. See NS(2018) appendix for details 
par.VarToPlot = {'drawtreasury2y'};


% Grid for gamma
par.gammaLow = -10;
par.gammaHigh = 10;
par.gammaStep = 0.01;

par.plotgamma = 3; % +/- what to plot for g(gamma)

%% High Frequency Window


%% Read data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tablein= readtable('CleanData\dataForMatlab-PBOCHighFreq.csv');
tablein.date = datenum(tablein.date,23);
dd = table2struct(tablein, 'ToScalar',true);

% Set the independent variable to be used
par.indepVar = 'pcapbocbroadirs1y_extrap'; % 'path_intra_wide' 'path' 'drawtreasury2y_long' 

clearvars tablein



%% Point Estimates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Place estimate in a structure pe (for point estimates)
pe = struct;
for ii = 1:size(par.estVar,2) %run the "diff cov" thing. depvar=yields, indepvar=extrapolated shock (should have variation in variance on Treatment vs Control days)
    depVar = par.estVar{ii};
    eval(['omegaFOMC = cov(dd.' par.indepVar '(dd.pbocdays==1,:),dd.' depVar '(dd.pbocdays==1,:),0);']);
    eval(['omegaCONT = cov(dd.' par.indepVar '(dd.pbocdays==0,:),dd.' depVar '(dd.pbocdays==0,:),0);']);
    omega = omegaFOMC - omegaCONT;
    eval(['pe.' depVar '=omega(1,2)/omega(1,1);']);
end
clearvars omega omegaFOMC omegaCONT depVar iin



%% Bootstrap Dcov, Dvar, and estimator (Dcov/Dvar)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Place bootstraped Dcov, Dvar and estimator in structure bst (for bootstrap)
% Notice that there is on Dcov and estimator for each dependent variable

bst = struct;
for ii = 1:size(par.estVar,2)
    depVar = par.estVar{ii};
    eval(['bst.Dcov'  depVar '=zeros(' num2str(par.NBootAll) ',1);']);
    eval(['bst.Dvar'  depVar '=zeros(' num2str(par.NBootAll) ',1);']);
end
clearvars depVar ii

% Create variable to use to stratify the bootstrap. (Not sure about this)
dd.stratNew = zeros(size(dd.dr007  ,1),1);
dd.stratNew(dd.pbocdays==1&dd.year< 2015,1) = 1;
dd.stratNew(dd.pbocdays==1&dd.year>=2015,1) = 2;

% Create matrix named origData with original data to be bootstrapped
origData = zeros(size(dd.dr007,1),2+size(par.estVar,2)); 
eval(['origData(:,1)=dd.' par.indepVar ';']);
origData(:,2) = dd.pbocdays;

for ii = 1:size(par.estVar,2)
    depVar = par.estVar{ii};
    eval(['origData(:,' num2str(ii+2) ')=dd.' depVar ';']);
end
clearvars depVar ii

% Bootstrap empirical moments
fprintf('Bootstrap Dcov and Dvar: \n')
for ii = 1:par.NBootAll
    % Stratified resampling
    newData = stratBoot(origData, dd.stratNew);
    temppbocdays = newData(:,2);
    
    % Calculate Dcov for each dependent variable
    for jj = 1:size(par.estVar,2) % recall that par.estVar1 is defined at the top
        depVar = par.estVar{jj};
        omegaFOMC = cov(newData(temppbocdays==1,1),newData(temppbocdays==1,2+jj),0);
        omegaCONT = cov(newData(temppbocdays==0,1),newData(temppbocdays==0,2+jj),0);
        omega = omegaFOMC - omegaCONT;
        eval(['bst.Dcov' depVar '(' num2str(ii) ',1)=omega(1,2);']);
        eval(['bst.Dvar' depVar '(' num2str(ii) ',1)=omega(1,1);']);
        eval(['bst.est'  depVar '(' num2str(ii) ',1)=omega(1,2)/omega(1,1);']);
    end

    clearvars omega omegaFOMC omegaCONT depVar jj newData temp*
    
    % Print progress indicator
    if rem(ii,50) == 0
        fprintf('%1.0f ',ii)
    end
    if rem(ii,1000) == 0
        fprintf('\n')
    end
end
fprintf('\n')


%% Construct Confidence intervals etc. using g(gamma)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% g(gamma) = Dcov - gamma*Dvar
% g(gamma) = 0 when gamma = Dcov/Dvar
% We include in confidence interval values of gamma for which 
% g(gamma) = 0 cannot be rejected

pLow = (1-par.CILevel)/2;
pHigh = 1 - pLow;

% Range of gamma values considered
gammavec = (par.gammaLow:par.gammaStep:par.gammaHigh)';
par.NN = size(gammavec,1);

% Create structure to store results. 
% Results for each asset will be in separate sub-structure
results = struct;
results.gammavec = gammavec;

fprintf('Calculate CI, etc. for each asset: \n')
for ii = 1:size(par.estVar,2)
    eval(['results.' par.estVar{ii} ' = struct;']);
    
    % Construct regular bootstrap standard error
    eval(['results.' par.estVar{ii} '.BootSE = std(bst.Dcov' par.estVar{ii} './bst.Dvar' par.estVar{ii} ');']);
    
    % Calculate distribution of g(gamma) for each gamma on grid
    distGgamma = struct;
    distGgamma.pLow  = zeros(par.NN,1);
    distGgamma.p50   = zeros(par.NN,1);
    distGgamma.pHigh = zeros(par.NN,1);
    for jj = 1:par.NN
        eval(['gofgamma = bst.Dcov' par.estVar{ii} ' - gammavec(jj,1)*bst.Dvar' par.estVar{ii} ';']);
        distGgamma.pLow(jj,1)  = quantile(gofgamma,pLow );
        distGgamma.p50(jj,1)   = quantile(gofgamma,0.5  );
        distGgamma.pHigh(jj,1) = quantile(gofgamma,pHigh);
    end
    eval(['results.' par.estVar{ii} '.GgammapLow = distGgamma.pLow;'  ]);
    eval(['results.' par.estVar{ii} '.GgammaP50 = distGgamma.p50;'    ]);
    eval(['results.' par.estVar{ii} '.GgammapHigh = distGgamma.pHigh;']);
    
    % Construct median unbiased estimate of gamma
    [temp tempInd] = min(abs(distGgamma.p50));
    eval(['results.' par.estVar{ii} '.MedianUnbiased = gammavec(tempInd);']);
    
    % Construct lower bound 95% confidence interval (and check wether CI is an interval)
    q025LZero = (distGgamma.pLow < 0);
    q025GZero = (distGgamma.pLow >= 0);
    if sum(q025LZero) == 0
        % If all values of gamma rejected because g(gamma) > 0
        CILow = Inf;
        CILowNotInt = 0;
    else
        % Set lower end of confidence interval to smallest value for which
        % q025 of g(gamma) is less than zero
        minLZero = find(q025LZero,1,'first');
        CILow = gammavec(minLZero,1);
        % Check whether any larger values have (q025 of g(gamma)) > 0
        if size(find(q025LZero),1) == par.NN
            CILowNotInt = 0;
        else
            maxGZero = find(q025GZero,1,'last');
            CILowNotInt = (maxGZero > minLZero);
        end
    end
    eval(['results.' par.estVar{ii} '.CILow = CILow;' ]);
    
    % Construct upper bound 95% confidence interval (and check wether CI is an interval)
    q975LZero = (distGgamma.pHigh < 0);
    q975GZero = (distGgamma.pHigh >= 0);
    if sum(q975GZero) == 0
        % If all values of gamma rejected because g(gamma) < 0
        CIHigh = -Inf;
        CIHighNotInt = 0;
    else
        % Set upper end of confidence interval to largest value for which
        % q975 of g(gamma) is larger than zero
        maxGZero = find(q975GZero,1,'last');
        CIHigh = gammavec(maxGZero,1);
        % Check whether any smaller values have (q975 of g(gamma)) < 0
        if size(find(q975GZero),1) == par.NN
            CIHighNotInt = 0;
        else
            minLZero = find(q975LZero,1,'first');
            CIHighNotInt = (minLZero < maxGZero);
        end
    end
    CINotInt = (CIHighNotInt | CILowNotInt);
    eval(['results.' par.estVar{ii} '.CIHigh = CIHigh;']);
    eval(['results.' par.estVar{ii} '.CINotInt = (CIHighNotInt | CILowNotInt);']);    
    
    % Print progress indicator
    fprintf('%1.0f ',ii)
    if rem(ii,20) == 0
        fprintf('\n')
    end 
end

clear distGgamma CI* q025* q975* minLZero maxGZero temp tempInd ii jj gofgamma gammavec




figure(1)
axis on

% Export results to excel
outOrder = {'drawtreasury9m','drawtreasury1y','drawtreasury2y',...
    'drawtreasury5y' ,'drawtreasury7y','drawtreasury8y','drawtreasury9y','drawtreasury10y',...
    'dadjtreasury9m' ,'dadjtreasury1y','dadjtreasury2y','dadjtreasury5y','dadjtreasury7y' ,...
    'dadjtreasury8y','dadjtreasury9y','dadjtreasury10y'};

f = fopen(['OutPut/YieldCurve/Rigobon/FiellerOutputPBOC_', par.indepVar, '.csv'], 'w');
fprintf(f, 'Variable,Beta,BootSE,MedianUnbiased, CILow, CIhigh, CINoInt\n')
for ii = 1:length(outOrder)
  OutVec = zeros(1,6);
    OutVec(1) = eval(['pe.' outOrder{ii}]);
    OutVec(2) = eval(['results.' outOrder{ii} '.BootSE']);
    OutVec(3) = eval(['results.' outOrder{ii} '.MedianUnbiased']);
    OutVec(4) = eval(['results.' outOrder{ii} '.CILow']);
    OutVec(5) = eval(['results.' outOrder{ii} '.CIHigh']);
    OutVec(6) = eval(['results.' outOrder{ii} '.CINotInt']);
    fprintf(f, [outOrder{ii}, ',%4.2f,%4.2f,%4.2f,%4.2f,%4.2f,%4.2f\n'],OutVec);
end
fclose(f)

% Produce figures for one asset
mGvec = floor(size(results.gammavec,1)/2);
nGvec = par.plotgamma*100;
p = plot(results.gammavec((mGvec-nGvec+1):(mGvec+nGvec)),eval(['results.' par.VarToPlot{1} '.GgammaP50((mGvec-nGvec+1):(mGvec+nGvec))']),'b',...
         results.gammavec((mGvec-nGvec+1):(mGvec+nGvec)),eval(['results.' par.VarToPlot{1} '.GgammapHigh((mGvec-nGvec+1):(mGvec+nGvec))']),'b--',...
         results.gammavec((mGvec-nGvec+1):(mGvec+nGvec)),eval(['results.' par.VarToPlot{1} '.GgammapLow((mGvec-nGvec+1):(mGvec+nGvec))']),'b--');
title('g(gamma)')
legend('Median','q97.5','q2.5','Location','NorthEast')
set(p,'LineWidth',1)
hline = refline(0,0);
set(hline,'Color','black')

figure(2)
axis on
eval(['p = scatter(bst.Dcov' par.VarToPlot{1} ',bst.Dvar' par.VarToPlot{1} ');']);
title('Dvar and Dcov')
xlabel('Dcov')
ylabel('Dvar')
set(p,'LineWidth',1)
hline = refline(0,0);
set(hline,'Color','black')
clear p hline
    



%% Daily Window


%% Read data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tablein= readtable('CleanData\dataForMatlab-PBOCDaily.csv');
tablein.date = datenum(tablein.date,23);
dd = table2struct(tablein, 'ToScalar',true);

% Set the independent variable to be used
par.indepVar = 'pcapbocirs_extrap'; % 'path_intra_wide' 'path' 'drawtreasury2y_long' 

clearvars tablein



%% Point Estimates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Place estimate in a structure pe (for point estimates)
pe = struct;
for ii = 1:size(par.estVar,2) %run the "diff cov" thing. depvar=yields, indepvar=extrapolated shock (should have variation in variance on Treatment vs Control days)
    depVar = par.estVar{ii};
    eval(['omegaFOMC = cov(dd.' par.indepVar '(dd.pbocdays==1,:),dd.' depVar '(dd.pbocdays==1,:),0);']);
    eval(['omegaCONT = cov(dd.' par.indepVar '(dd.pbocdays==0,:),dd.' depVar '(dd.pbocdays==0,:),0);']);
    omega = omegaFOMC - omegaCONT;
    eval(['pe.' depVar '=omega(1,2)/omega(1,1);']);
end
clearvars omega omegaFOMC omegaCONT depVar iin



%% Bootstrap Dcov, Dvar, and estimator (Dcov/Dvar)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Place bootstraped Dcov, Dvar and estimator in structure bst (for bootstrap)
% Notice that there is on Dcov and estimator for each dependent variable

bst = struct;
for ii = 1:size(par.estVar,2)
    depVar = par.estVar{ii};
    eval(['bst.Dcov'  depVar '=zeros(' num2str(par.NBootAll) ',1);']);
    eval(['bst.Dvar'  depVar '=zeros(' num2str(par.NBootAll) ',1);']);
end
clearvars depVar ii

% Create variable to use to stratify the bootstrap. (Not sure about this)
dd.stratNew = zeros(size(dd.dr007  ,1),1);
dd.stratNew(dd.pbocdays==1&dd.year< 2015,1) = 1;
dd.stratNew(dd.pbocdays==1&dd.year>=2015,1) = 2;

% Create matrix named origData with original data to be bootstrapped
origData = zeros(size(dd.dr007,1),2+size(par.estVar,2)); 
eval(['origData(:,1)=dd.' par.indepVar ';']);
origData(:,2) = dd.pbocdays;

for ii = 1:size(par.estVar,2)
    depVar = par.estVar{ii};
    eval(['origData(:,' num2str(ii+2) ')=dd.' depVar ';']);
end
clearvars depVar ii

% Bootstrap empirical moments
fprintf('Bootstrap Dcov and Dvar: \n')
for ii = 1:par.NBootAll
    % Stratified resampling
    newData = stratBoot(origData, dd.stratNew);
    temppbocdays = newData(:,2);
    
    % Calculate Dcov for each dependent variable
    for jj = 1:size(par.estVar,2) % recall that par.estVar1 is defined at the top
        depVar = par.estVar{jj};
        omegaFOMC = cov(newData(temppbocdays==1,1),newData(temppbocdays==1,2+jj),0);
        omegaCONT = cov(newData(temppbocdays==0,1),newData(temppbocdays==0,2+jj),0);
        omega = omegaFOMC - omegaCONT;
        eval(['bst.Dcov' depVar '(' num2str(ii) ',1)=omega(1,2);']);
        eval(['bst.Dvar' depVar '(' num2str(ii) ',1)=omega(1,1);']);
        eval(['bst.est'  depVar '(' num2str(ii) ',1)=omega(1,2)/omega(1,1);']);
    end

    clearvars omega omegaFOMC omegaCONT depVar jj newData temp*
    
    % Print progress indicator
    if rem(ii,50) == 0
        fprintf('%1.0f ',ii)
    end
    if rem(ii,1000) == 0
        fprintf('\n')
    end
end
fprintf('\n')


%% Construct Confidence intervals etc. using g(gamma)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% g(gamma) = Dcov - gamma*Dvar
% g(gamma) = 0 when gamma = Dcov/Dvar
% We include in confidence interval values of gamma for which 
% g(gamma) = 0 cannot be rejected

pLow = (1-par.CILevel)/2;
pHigh = 1 - pLow;

% Range of gamma values considered
gammavec = (par.gammaLow:par.gammaStep:par.gammaHigh)';
par.NN = size(gammavec,1);

% Create structure to store results. 
% Results for each asset will be in separate sub-structure
results = struct;
results.gammavec = gammavec;

fprintf('Calculate CI, etc. for each asset: \n')
for ii = 1:size(par.estVar,2)
    eval(['results.' par.estVar{ii} ' = struct;']);
    
    % Construct regular bootstrap standard error
    eval(['results.' par.estVar{ii} '.BootSE = std(bst.Dcov' par.estVar{ii} './bst.Dvar' par.estVar{ii} ');']);
    
    % Calculate distribution of g(gamma) for each gamma on grid
    distGgamma = struct;
    distGgamma.pLow  = zeros(par.NN,1);
    distGgamma.p50   = zeros(par.NN,1);
    distGgamma.pHigh = zeros(par.NN,1);
    for jj = 1:par.NN
        eval(['gofgamma = bst.Dcov' par.estVar{ii} ' - gammavec(jj,1)*bst.Dvar' par.estVar{ii} ';']);
        distGgamma.pLow(jj,1)  = quantile(gofgamma,pLow );
        distGgamma.p50(jj,1)   = quantile(gofgamma,0.5  );
        distGgamma.pHigh(jj,1) = quantile(gofgamma,pHigh);
    end
    eval(['results.' par.estVar{ii} '.GgammapLow = distGgamma.pLow;'  ]);
    eval(['results.' par.estVar{ii} '.GgammaP50 = distGgamma.p50;'    ]);
    eval(['results.' par.estVar{ii} '.GgammapHigh = distGgamma.pHigh;']);
    
    % Construct median unbiased estimate of gamma
    [temp tempInd] = min(abs(distGgamma.p50));
    eval(['results.' par.estVar{ii} '.MedianUnbiased = gammavec(tempInd);']);
    
    % Construct lower bound 95% confidence interval (and check wether CI is an interval)
    q025LZero = (distGgamma.pLow < 0);
    q025GZero = (distGgamma.pLow >= 0);
    if sum(q025LZero) == 0
        % If all values of gamma rejected because g(gamma) > 0
        CILow = Inf;
        CILowNotInt = 0;
    else
        % Set lower end of confidence interval to smallest value for which
        % q025 of g(gamma) is less than zero
        minLZero = find(q025LZero,1,'first');
        CILow = gammavec(minLZero,1);
        % Check whether any larger values have (q025 of g(gamma)) > 0
        if size(find(q025LZero),1) == par.NN
            CILowNotInt = 0;
        else
            maxGZero = find(q025GZero,1,'last');
            CILowNotInt = (maxGZero > minLZero);
        end
    end
    eval(['results.' par.estVar{ii} '.CILow = CILow;' ]);
    
    % Construct upper bound 95% confidence interval (and check wether CI is an interval)
    q975LZero = (distGgamma.pHigh < 0);
    q975GZero = (distGgamma.pHigh >= 0);
    if sum(q975GZero) == 0
        % If all values of gamma rejected because g(gamma) < 0
        CIHigh = -Inf;
        CIHighNotInt = 0;
    else
        % Set upper end of confidence interval to largest value for which
        % q975 of g(gamma) is larger than zero
        maxGZero = find(q975GZero,1,'last');
        CIHigh = gammavec(maxGZero,1);
        % Check whether any smaller values have (q975 of g(gamma)) < 0
        if size(find(q975GZero),1) == par.NN
            CIHighNotInt = 0;
        else
            minLZero = find(q975LZero,1,'first');
            CIHighNotInt = (minLZero < maxGZero);
        end
    end
    CINotInt = (CIHighNotInt | CILowNotInt);
    eval(['results.' par.estVar{ii} '.CIHigh = CIHigh;']);
    eval(['results.' par.estVar{ii} '.CINotInt = (CIHighNotInt | CILowNotInt);']);    
    
    % Print progress indicator
    fprintf('%1.0f ',ii)
    if rem(ii,20) == 0
        fprintf('\n')
    end 
end

clear distGgamma CI* q025* q975* minLZero maxGZero temp tempInd ii jj gofgamma gammavec




figure(1)
axis on

% Export results to excel
outOrder = {'drawtreasury9m','drawtreasury1y','drawtreasury2y',...
    'drawtreasury5y' ,'drawtreasury7y','drawtreasury8y','drawtreasury9y','drawtreasury10y',...
    'dadjtreasury9m' ,'dadjtreasury1y','dadjtreasury2y','dadjtreasury5y','dadjtreasury7y' ,...
    'dadjtreasury8y','dadjtreasury9y','dadjtreasury10y'};

f = fopen(['OutPut/YieldCurve/Rigobon/FiellerOutputPBOC_', par.indepVar, '.csv'], 'w');
fprintf(f, 'Variable,Beta,BootSE,MedianUnbiased, CILow, CIhigh, CINoInt\n')
for ii = 1:length(outOrder)
  OutVec = zeros(1,6);
    OutVec(1) = eval(['pe.' outOrder{ii}]);
    OutVec(2) = eval(['results.' outOrder{ii} '.BootSE']);
    OutVec(3) = eval(['results.' outOrder{ii} '.MedianUnbiased']);
    OutVec(4) = eval(['results.' outOrder{ii} '.CILow']);
    OutVec(5) = eval(['results.' outOrder{ii} '.CIHigh']);
    OutVec(6) = eval(['results.' outOrder{ii} '.CINotInt']);
    fprintf(f, [outOrder{ii}, ',%4.2f,%4.2f,%4.2f,%4.2f,%4.2f,%4.2f\n'],OutVec);
end
fclose(f)

% Produce figures for one asset
mGvec = floor(size(results.gammavec,1)/2);
nGvec = par.plotgamma*100;
p = plot(results.gammavec((mGvec-nGvec+1):(mGvec+nGvec)),eval(['results.' par.VarToPlot{1} '.GgammaP50((mGvec-nGvec+1):(mGvec+nGvec))']),'b',...
         results.gammavec((mGvec-nGvec+1):(mGvec+nGvec)),eval(['results.' par.VarToPlot{1} '.GgammapHigh((mGvec-nGvec+1):(mGvec+nGvec))']),'b--',...
         results.gammavec((mGvec-nGvec+1):(mGvec+nGvec)),eval(['results.' par.VarToPlot{1} '.GgammapLow((mGvec-nGvec+1):(mGvec+nGvec))']),'b--');
title('g(gamma)')
legend('Median','q97.5','q2.5','Location','NorthEast')
set(p,'LineWidth',1)
hline = refline(0,0);
set(hline,'Color','black')

figure(2)
axis on
eval(['p = scatter(bst.Dcov' par.VarToPlot{1} ',bst.Dvar' par.VarToPlot{1} ');']);
title('Dvar and Dcov')
xlabel('Dcov')
ylabel('Dvar')
set(p,'LineWidth',1)
hline = refline(0,0);
set(hline,'Color','black')
clear p hline

function newData = stratBoot(origData, stratVar)

NN = size(origData,1); % number of observations in original data
newData = zeros(size(origData));

stratValues = unique(stratVar);
Nstrat = length(stratValues);

kk = 0;
for ii = 1:Nstrat
    NTemp = sum(stratVar == stratValues(ii));
    DataTemp = origData(stratVar == stratValues(ii),:);
    Draws = ceil(NTemp*rand(NTemp,1));
    newData((kk+1):(kk+NTemp),:) = DataTemp(Draws,:);
    kk = kk + NTemp;
end

end
