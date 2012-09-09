% Tutorial 7 Exercise 1 Subtask 1.2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function [f, e] = filter_lpc(x, P)

  coef = lpc(x, P);
  fcoef = [0 -coef(2:end)];
  f = filter(fcoef, 1, x);
  e = x - f;

end
