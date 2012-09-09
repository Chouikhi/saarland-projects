% Tutorial 3 Exercise 1 Subtask 1.1
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function g_00 = gabor_base(x, y, sigma_x, sigma_y, omega)

  g_00 = inv(2 * pi * sigma_x * sigma_y) ...
      .* exp(-0.5 .* (x.^2 ./ sigma_x^2 + y.^2 ./ sigma_y^2)) ...
      .* cos(x .* (2 * pi * omega));

end
