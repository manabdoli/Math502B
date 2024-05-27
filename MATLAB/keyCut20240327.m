clear all;close all;mFile='mask2m_16_16'; % coarse data 304x304x361
load(mFile); %load(['data\' mFile]);% maskXm
maskXm=mask2m;%eval(['maskXm=' mFile ';clear ' mFile]);
[Nx,Ny,Naz]=size(maskXm);
%[min(maskXm(:)) mean(maskXm(:)) max(maskXm(:))] = [-2.0912 14.3900 70.665]
keyLevels=round(min(maskXm(:))):5:ceil(max(maskXm(:)));Nlevel=length(keyLevels)-1;
% Nlevel=10;keyLevels=linspace(round(min(maskXm(:))),ceil(max(maskXm(:))),Nlevel+1);
%keyLevels=-2:3:25;Nlevel=length(keyLevels);keyThick=0.05;
outDir='keyCut';outResult='keyCutMask_20240327';



%%
keyCurMask=zeros(Nx,Ny,Nlevel);
for pi=1:Nx
    for pj=1:Ny
        te=squeeze(maskXm(pi,pj,:));
        for lN=1:Nlevel
           id=find((te>=keyLevels(lN)).*(te<keyLevels(lN+1)));
           keyCurMask(pi,pj,lN)=length(id);
        end
%         figure(9);clf;plot(te);hold on;
%         for lN=1:Nlevel
%            id=find((te<=keyLevels(lN+1)).*(te>=keyLevels(lN)));
%            if ~isempty(id);stem(id,keyLevels(lN)+0*id,'ok');end
%         end
    end
end
%%
figure(10);clf;
for lN=1:Nlevel
    subplot(3,5,lN);surf(keyCurMask(:,:,lN));shading interp;view(2);axis equal;axis off;clim([0, 361]);colorbar;
    title(['mask btwn [' num2str(keyLevels(lN)) ', ' num2str(keyLevels(lN+1)) ')'])
end
eval(['print -dpng ' outDir '\' mFile '_keyCut' ])

eval(['save ' outDir '\' outResult ' keyCurMask Nlevel keyLevels;'])

