Az=readtable("thinMask31x31.csv"); % 961x361
class(Az)
Az=Az{:,:};
class(Az)
%%
[posLen, azLen] = size(Az);
nRow = 31; 
posIdx = 1:posLen;
[posX, posY] = index2pos(posIdx, nRow);
%%
