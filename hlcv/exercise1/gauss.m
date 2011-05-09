function G = gauss(x, sigma)
  G = exp(- (x .* x) ./ sigma ^ 2) / (sqrt(2 * pi) * sigma);
end
