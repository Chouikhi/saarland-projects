% Tutorial 8 Exercise 2 Subtask 2.3
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function [A, E] = ldr(R)
% ldr
%   Compute Levinson-Durbin recursion for Toepliz matrix having values
%    /R(1) R(2) ..   R(N)  \   /A(1)\      / R(2) \
%    |R(2) R(1) ..   R(N-1)| * |A(2)|   =  | R(3) |
%    | .   ..   ..    .    |   | .  |      |  .   |
%    \R(N) ..   R(2) R(1)  /   \A(N)/      \R(N+1)/

  assert(size(R, 2) == 1, 'R should be column-vector');
  assert(size(R, 1) >= 2, 'R should contain at least 2 elements');
  N = length(R) - 1;
  A = zeros(N, 1);
  if N == 1
    A = R(2) / R(1);
    E = - [-1 A] * R;
  else
    [rA, rE] = ldr(R(1:end-1));
    q = - R(end) + R(end-1:-1:2)' * rA;
    k = - q / rE;
    A(1:N-1) = rA - k * rA(end:-1:1);
    A(N) = k;
    E = rE - q^2 / rE;
  end

end
