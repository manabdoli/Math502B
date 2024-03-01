function [y] = wide2long(x)
[r,c,d]=size(x);
y = reshape(x,[r*c,d]);
end