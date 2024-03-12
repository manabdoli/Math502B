% Test FFT
%% Finding Magnitude and Frequency
n=11; % 10 vs 11
t=(0:n-1)/n;
f=3; % works up to 4 for n=10 and to 5 for 11
z=3+2*cos(2*pi*f*t);

plot(t, z)
fftz=fft(z)
freqMag(fftz)


%% Checking with shifts
n=11; % 10 vs 11
t=(0:n-1)/n;
f=1; % works up to 4 for n=10 and to 5 for 11
z=3+2*sin(2*pi*f*t-pi/3);

plot(t, z)
fftz=fft(z)
[mag, ang, frq]=freqMag(fftz);
subplot(2,1,1)
plot(frq, mag)
subplot(2,1,2)
plot(frq, ang*180/pi)
