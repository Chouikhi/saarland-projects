% Tutorial 7 Exercise 1 Subtask 1.3
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

P = 4;  % number of coefficients
wav_files = {'ph01s.wav', 'ph02p.wav',  'ph03l.wav',  'ph04i.wav'};

figure(1);
clf;

for wfi = 1:length(wav_files)
  wf = wav_files{wfi};
  s = wavread(wf);
  samples = min(512, length(s));
  s = s(1:samples);
  [f, e] = filter_lpc(s, P);
  subplot(2, 2, wfi);
  plot_lpc(-1, s, f, e, wf);
end
