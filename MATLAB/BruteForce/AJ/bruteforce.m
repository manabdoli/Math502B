function [location,time] = bruteforce(maskT, maskTrue, truelocation)

maskTm= maskT/norm(maskT);
tstart=cputime;
for i=1:length(maskTrue)
        maskTrue2=squeeze(maskTrue(i,:));
        maskTrue3=maskTrue2/norm(maskTrue2);
        maskCov(i,:)=dot(maskTrue3,maskTm);
    end

[M,I]=max(maskCov(:)) ;
[Imax,Jmax]=ind2sub(size(maskCov),I);

location= truelocation(Imax,:);
tend=cputime;
time=tend-tstart;
end

