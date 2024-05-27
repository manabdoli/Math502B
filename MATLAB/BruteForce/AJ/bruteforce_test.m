%% Brute force clean data test
mask = readmatrix('maskLongData.csv');
trueMaskfile= readmatrix('maskLongData.csv'); 
truelocationfile= readmatrix('unmod location.csv');
%%
time=zeros(length(trueMaskfile),1);
location=zeros(length(trueMaskfile),2);
for i=1:3   %length(mask)
    [location(i,:),time(i,:)] = bruteforce((mask(i,:)), trueMaskfile, truelocationfile);
end