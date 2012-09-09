% Implement a function gaussdx.m for creating a Gaussian derivative filter in 1D according to the following
% equation

function D = gaussdx(x, sigma)
  D = -2.*x.*exp(-(x.*x) ./ (sigma^2)) ./ (sqrt(2*pi)*sigma.^3);
end
