function [location,time] = bruteforce(maskTst, maskTrue, truelocation)
tstart=cputime;
maskCov = zeros(length(maskTrue), 1);
maskTst= maskTst/norm(maskTst);
for i=1:length(maskTrue)
    iMaskTrue=maskTrue(i,:)/norm(maskTrue(i,:));
    maskCov(i,:)=dot(iMaskTrue,maskTst);
end

[~,I]=max(maskCov(:)) ;
[Imax,~]=ind2sub(size(maskCov),I);

location= truelocation(Imax,:);
tend=cputime;
time=tend-tstart;
end

