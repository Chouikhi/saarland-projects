function fw = fourier_transform(x, ft)
  fi = @(w) @(t) ft(t) .* exp(-i .* w .* t);
  ffw = @(w) quad(fi(w), x(1), x(end));
  fw = zeros(length(x));
  for idx = 1:length(x)
    fw(idx) = ffw(x(idx));
  end
end
