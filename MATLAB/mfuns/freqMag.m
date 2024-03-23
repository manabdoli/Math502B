% computing frequency magnitudes
function [mag, ang, freq] = freqMag(fftx)
    [m, n] = size(fftx);
    if(mod(n,2)==0) % even samples
        freq = 0:n/2;
        mag = fftx(1:m, freq+1)/n;
        mag(1:m, 2:end-1) = 2*mag(1:m, 2:end-1);
    else
        freq = 0:(n-1)/2;
        mag = fftx(1:m, freq+1)/n;
        mag(1:m, 2:end) = 2*mag(1:m, 2:end);
    end
    ang = angle(mag);
    mag = abs(mag);
end