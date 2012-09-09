% Tutorial 4 Exercise 2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

% Finds the coefitients a and b, such that sum (y_i - ax_i - b)^2 min
function [a, b] = fit_line(xs, ys)
  % the optimal values of a and b satisfy
  %
  % | s_x  * a + n   * b = s_y
  % | s_x2 * a + s_x * b = s_xy
  s_x = sum(xs);
  s_y = sum(ys);
  s_xy = sum(xs .* ys);
  s_x2 = sum(xs .^ 2);

  % this is the matrix of the system of linear equations
  m = [ s_x,  length(xs), s_y ...
      ; s_x2, s_x,        s_xy];

  % solving it using Cramer's rule
  d0 = det(m(:, [1 2]));
  d1 = det(m(:, [3 2]));
  d2 = det(m(:, [1 3]));

  a = d1 / d0;
  b = d2 / d0;
end
