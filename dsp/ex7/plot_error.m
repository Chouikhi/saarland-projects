% Tutorial 7 Exercise 1 Subtask 1.4
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

P = 1:200;  % LPC length
wav_files = {'ph01s.wav', 'ph02p.wav',  'ph03l.wav',  'ph04i.wav'};
colors = {'r', 'g', 'b', 'k'};

figure(2);
clf;

ers = zeros(length(P), length(wav_files));
for wfi = 1:length(wav_files)
  wf = wav_files{wfi};
  s = wavread(wf);
  samples = min(512, length(s));
  s = s(1:samples);
  for pi = 1:length(P)
    [f, e] = filter_lpc(s, P(pi));
    ers(pi, wfi) = sum(e .^ 2);
  end
  % subplot(2, 2, wfi);
  hold on;
  plot(P, ers(:, wfi), 'Color', colors{wfi});
  hold off;
end

legend(wav_files);
axis([0 200 0 3]);
title('sum e^2');
