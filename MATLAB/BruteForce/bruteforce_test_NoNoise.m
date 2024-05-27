%% Brute force - Clean Test Data
trueMaskfile= readmatrix('maskLongData.csv'); 
truelocationfile= readmatrix('unmod location.csv');
%%
curTime = cputime;
k = length(trueMaskfile);
time=zeros(k,1);
location=zeros(k,2);
rootmse=zeros(k,1);
mcr = zeros(k,1);
for i=1:k
    [location(i,:),time(i,:)] = bruteforce(trueMaskfile(i,:), trueMaskfile, truelocationfile);
    err = rmse(location(i,:), truelocationfile(i,:)) ; %rmse measured in (meters)
    rootmse(i,:)=40*mean(err); % *40 meters 
    mcr(i,:) = 1- mean(location(i,:) == truelocationfile(i,:));
end
fprintf('Start=%g; \tEnd=%g; \tElapsed=%g\n\n', curTime, cputime, cputime-curTime)
save('TrueData_results.mat', "rootmse", "mcr", "time")