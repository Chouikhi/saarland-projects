% Tutorial 4 Exercise 1 Subtask 1.3
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

MS_PER_SEC = 1000;
audio_file = 'point1.au';
shift_ms = 10;
width_ms = 25;
sample_rate_hz = 16000;

shift_samples = round(sample_rate_hz * shift_ms / MS_PER_SEC);
width_samples = round(sample_rate_hz * width_ms / MS_PER_SEC);

s = auread(audio_file);
spec = spectogram(s, shift_samples, width_samples);

spec_img = spec';
spec_img = spec_img(end:-1:1, :);

subplot(2, 1, 1);
imagesc(spec_img);
subplot(2, 1, 2);
plot(s);
