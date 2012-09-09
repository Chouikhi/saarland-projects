% Tutorial 3 Exercise 1 Subtask 1.2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function g_nm = gabor(x, y, sigma_x, sigma_y, omega, n, m, a, K)

  theta_n = n * pi / K;
  rot_ma3x = a ^ (-m) * [ cos(theta_n) sin(theta_n); ...
                         -sin(theta_n) cos(theta_n) ];
  x_ = rot_ma3x(1, 1) .* x + rot_ma3x(1, 2) .* y;
  y_ = rot_ma3x(2, 1) .* x + rot_ma3x(2, 2) .* y;
  g_nm = gabor_base(x_, y_, sigma_x, sigma_y, omega);

end
