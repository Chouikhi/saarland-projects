function [D2, x] = gaussdxx(sigma)

  x = [floor(-3.0*sigma + 0.5):floor(3.0*sigma + 0.5)];
  % NOTE: we may have to multiply by 4 (or some other equally magical constant)
  D2 = ((-1 + (x.^2./sigma.^2)) .* exp(-x.^2/(2*sigma^2)))/(sqrt(2*pi)*sigma^3);

end
