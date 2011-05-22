function [D2, x] = gaussdxx(sigma)

  x = [floor(-3.0*sigma + 0.5):floor(3.0*sigma + 0.5)];
  % NOTE(iskren): we may have to multiply by 4 (or some other equally magical constant)
  % NOTE(zori): I think it is correct mathematically
  D2 = (((x.^2./sigma.^2) - 1) .* exp(-x.^2/(2*sigma^2)))/(sqrt(2*pi)*sigma^3);

end
