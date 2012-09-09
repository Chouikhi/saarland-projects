% Tutorial 4 Exercise 1 Subtask 1.1
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function f = pre_emph(s)
  % using matlab functions
  f = filter([1, -0.95], [1], s);

  % manually
  % s2 = zeros(size(s));
  % s2(2:end) = s(1:end-1) .* -0.95;
  % f = s + s2;
end
