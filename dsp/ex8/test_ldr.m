% Tutorial 8 Exercise 2 Subtask 2.4
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

R = 6:-1:2;  % autocorrelation coefficients
[A, E] = ldr(R');
[Ax, Ex] = levinson(R);

epsilon = 1e-9;
assert(all([1 -A'] - Ax < epsilon), 'ldr and levinson disagree on coef');
assert(E - Ex < epsilon, 'ldr and levinson disagree on error');
