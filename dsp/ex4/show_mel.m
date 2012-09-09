% Tutorial 4 Exercise 1 Subtask 1.4
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

fl = 133.33334;
fh = 6855.4976;
fft_size = 1024;
fs = 16000;
L = 24;
fmel = 1125;

M = mel(fl, fh, fft_size, fs, L, fmel);

rows = 6;
cols = L / rows;
for i = 1:L
  subplot(rows, cols, i);
  plot(M(i, :));
  axis([0, size(M, 2), 0, 0.2]);
end
