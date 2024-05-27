%% Brute Force - Noisy Test Data
load('NoisyData1.mat'); % Load noisy data  
test=noise.data;
trueMaskfile= readmatrix('maskLongData.csv'); %Load True Masks for dot product in brute force function
truelocationfile= readmatrix('unmod location.csv');%load true locations of masks (Confirmed with Ren that locations for the noisy masks are in the same order as the not noisy masks)
%%
curTime = cputime;
k = length(test);
time = zeros(k,1);
location = zeros(k,2);
rootmse = zeros(k,1);
mcr = zeros(k,1);
for i=1:k
    [location(i,:),time(i,:)] = bruteforce((test(i,:)), trueMaskfile, truelocationfile);
    err = rmse(location(i,:), truelocationfile(i,:)) ; %rmse measured in (meters)
    rootmse(i,:)=40*mean(err); % *40 meters 
    mcr(i,:) = 1- mean(location(i,:) == truelocationfile(i,:));
end
fprintf('Start=%g; \tEnd=%g; \tElapsed=%g\n\n', curTime, cputime, cputime-curTime)
save('NoisyData1_results.mat', "rootmse", "mcr", "time")