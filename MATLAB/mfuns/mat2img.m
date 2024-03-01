% Turning a matrix to an image
function [y]=mat2img(x, scale)
    if(nargin<2)
        scale = true;
    end
    y = x;
    if(scale)
        mm = min(min(min(y))); MM=max(max(max(y))); RR=MM-mm;
        y=((y-mm)/RR-.5)*255+255/2;
    end
    y=uint8(y);
    imshow(y)
end