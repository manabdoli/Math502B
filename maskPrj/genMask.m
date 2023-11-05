mFile='mask2m_16_16.mat';load(mFile);% mask2m 304x304x361
[Nx,Ny,Naz]=size(mask2m);reso=0.02;az=0:360;
x=(1:Nx)*reso*608/Nx;y=(1:Ny)*reso*608/Ny;[X,Y]=meshgrid(x,y);
rng(1);Ni=randi(Nx);Nj=randi(Ny);
maskTrue=squeeze(mask2m(Ni,Nj,:));maskTrue=maskTrue/norm(maskTrue);
maskCov=zeros(Nx,Ny);
for ni=1:Nx
    for nj=1:Ny
        maskT=squeeze(mask2m(ni,nj,:));maskT=maskT/norm(maskT);
        maskCov(ni,nj)=dot(maskTrue,maskT);
    end
end
[M,I]=max(maskCov(:));[Imax,Jmax]=ind2sub(size(maskCov),I);


%% plot the mask corelation with the exact location
figure(8);clf;set(gcf,'paperposition',[-5.7500   -0.0938   6.5   6.0]);hold on;
bufx=.05;bufy=.10;set(gca,'position',[bufx bufy 1-1.5*bufx 1-2.*bufy]);colormap(jet)
surf(X,Y,maskCov);shading interp;hold on;view(2);axis tight;axis equal
% c = contourf(maskCov);%shading interp;clabel(c,h_level);
plot3(X(Imax,Jmax),Y(Imax,Jmax),maskCov(Imax,Jmax),'ok','markerfacecolor','k','markersize',4);
colorbar;xlabel('(km)');ylabel('(km)');title('Correlation of Masks')
eval(['print -dpng maskCorrelationContour' ])

figure(88);clf;set(gcf,'paperposition',[-5.7500   -0.0938   13.5   6.0]);hold on;
bufx=.05;bufy=.10;set(gca,'position',[bufx bufy 1-1.5*bufx 1-2.*bufy]);clear leg
plot(az,squeeze(mask2m(Ni,Nj,:)),'--b','linewidth',2);hold on;
plot(az,squeeze(mask2m(Imax,Jmax,:)),'r');box on;grid on;axis tight;
xlabel('azimuth (deg)');ylabel('horizon mask (deg)');
title('correlation with known starting point ')
legend('true mask','predicted mask','location','best')
eval(['print -dpng maskMatching'])

