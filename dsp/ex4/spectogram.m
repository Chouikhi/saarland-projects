% Tutorial 4 Exercise 1 Subtask 1.3
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function S = spectogram(s, shift, width)

  cutoff = 1;
  emph_s = pre_emph(s);
  num_w = get_frame_count(length(s), shift, width);
  nfft = 2 ^ nextpow2(width);
  fft_in = zeros(nfft, 1);
  S = zeros(num_w, nfft / 2 + 1);
  fprintf('frames count %d\n', num_w);
  for wi = 1:num_w
    fft_in(1:width) = windowing(emph_s, shift, width, wi);
    fft_out = abs(fft(fft_in));
    S(wi, :) = fft_out(1:nfft / 2 + 1);
  end

  % there are too few values above cutoff, so it actually screws the coloring
  % in imagesc
  S = (S >= cutoff) + (S < cutoff) .* S;

end
