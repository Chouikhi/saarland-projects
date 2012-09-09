% Tutorial 5 Exercise 2 Subtask 2.1
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

%
% pcs -- compute principal component analysis on the vectors specified in D --
% that is the complete projection matrix P. Lower number of dimensions will be
% achieved if only the first k columns of P are used for projection.

function [P, V] = pca(D)

  % M -- number of vectors
  % N -- number of dimensions
  [M, N] = size(D);

  avg = mean(D);
  cov_ma3x = zeros(N);

  for i = 1:M
    % compute normalized row-vector.
    normalized = D(i, :) - avg;
    % to get a ma3x we need column * row vector, so transpose the first
    cov_ma3x = cov_ma3x + normalized' * normalized;
  end

  % extract eigen vectors/values
  [eig_vecs, eig_vals] = eig(cov_ma3x);
  eig_vals = diag(eig_vals);

  % sort eigen values in descending order
  [eig_vals, eig_idx] = sort(eig_vals, 1, 'descend');
  eig_vecs = eig_vecs(:, eig_idx);

  P = eig_vecs;
  V = eig_vals;

end
