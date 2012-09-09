% Tutorial 4 Exercise 1 Subtask 1.5
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function mfccs = mfcc(audio_file)

  MS_PER_SEC = 1000;
  % audio_file = 'point1.au';
  shift_ms = 10;
  width_ms = 25;
  sample_rate_hz = 16000;
  
  %%%
  fl = 133.33334;
  fh = 6855.4976;
  % fft_size = 1024;
  % fs = 16000;
  L = 24;
  fmel = 1125;
  %%%
  
  shift_samples = round(sample_rate_hz * shift_ms / MS_PER_SEC);
  width_samples = round(sample_rate_hz * width_ms / MS_PER_SEC);
  nfft = 2 ^ nextpow2(width_samples);
  
  s = auread(audio_file);
  num_w = get_frame_count(length(s), shift_samples, width_samples);
  emph_s = pre_emph(s);
  
  fft_in = zeros(nfft, 1);
  mfccs = zeros(num_w, L);
  
  mfb = mel(fl, fh, nfft, sample_rate_hz, L, 1125);
  
  for wi = 1:num_w
    fft_in(1:width_samples) = windowing(emph_s, shift_samples, width_samples, wi);
    fft_out = abs(fft(fft_in));
    mc = mfb * fft_out(1:size(mfb, 2));
    mcl = log(mc + 1); % add one to prevent negative log
    mfcc = dct(mcl);
    mfccs(wi, :) = mc;
  end

end
