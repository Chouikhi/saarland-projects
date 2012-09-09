% Tutorial 9 Exercise 1 Subtask 1.6
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

% signal to noise ratio for frequency bin k
SNR = 0:30;

% calculate the frequency response based on the formula derived in 1.5
H = 1 ./ (1 + 1 ./ SNR);

% plot the freq response in dB
plot(SNR, 20 * log(H));
title('Frequency response in dB given SNR');
xlabel('Signal to noise ratio (SNR)');
ylabel('H in dB');
