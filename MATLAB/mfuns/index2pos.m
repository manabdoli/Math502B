function [posx, posy] = index2pos(idx,r)
posx=mod(idx-1,r)+1;
posy=fix((idx-1)/r)+1;

end