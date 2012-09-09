function tau = get_gcc(sig_i, sig_j)
  tau_max = 3; % I really don't know this :)
  L = length(sig_i);
  k = (0:(L-1)) ./ L;
  X_i = fft(sig_i);
  X_j = fft(sig_j);
  % this is the function we should maximize times -1
  psi = @(t) - sum((((X_i .* conj(X_j)) ./ (abs(X_i) .* abs(X_j))) .* exp(2 .* pi .* t .* k)));
  tau = fminbnd(psi, -tau_max, tau_max);
end
