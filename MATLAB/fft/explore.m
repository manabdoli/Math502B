%% 
clc;
clear;
% sine waves
n = 10;
fs = [1, 3];
t = (0:.01:n)';
%% 
v = sum(sin(2*pi*t*fs), 2);

plot(t, v)
%% 
fftv = fft(v);
%% 

plot(abs(fftv))
%% 

plot(abs(fftshift(fftv)))
