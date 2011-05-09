function D = gaussdx(x, sigma)
  D = - x .* 2 .* exp(- (x .* x) ./ sigma ^ 2) ./ (sqrt(2 * pi) * sigma ^ 3);
end
