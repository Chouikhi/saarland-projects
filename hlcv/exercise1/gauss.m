% Implement a method which computes the values of a Gaussian for a given variance and a number of samples.
% where σ is the standard deviation and x is a vector of integer values.

function G = gauss(x, sigma)
  G = exp(-(x.*x) ./ (2*sigma^2)) ./ (sqrt(2*pi)*sigma);
end
