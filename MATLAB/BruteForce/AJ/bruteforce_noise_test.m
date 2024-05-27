test=load('NoisyData1.mat'); % Load noisy data  
test=noise.data;
trueMaskfile= readmatrix('maskLongData.csv'); %Load True Masks for dot product in brute force function
truelocationfile= readmatrix('unmod location.csv');%load true locations of masks (Confirmed with Ren that locations for the noisy masks are in the same order as the not noisy masks)
%%
time=zeros(size(test,1),1);
location=zeros(size(test,1),2);
rootmse=zeros(size(test,1),1);
mcr = zeros(size(test,1),1);
for i=1:size(test,1)
    [location(i,:),time(i,:)] = bruteforce((test(i,:)), trueMaskfile, truelocationfile);
    err = rmse(location(i,:), truelocationfile(i,:)) ; %rmse measured in (meters)
    rootmse(i,:)=40*mean(err); % *40 meters 
    mcr(i,:) = 1- mean(location(i,:) == truelocationfile(i,:));
end