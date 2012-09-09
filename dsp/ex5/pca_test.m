% Tutorial 5 Exercise 2 Subtask 2.2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function pca_test(varargin)

  if length(varargin) > 0
    D = varargin{1};
  else
    D = importdata('alo.txt');
    D = D.data;
  end

  % sorry, I'm using the opposite naming convension
  %  N -- number of dimensions
  %  M -- number of vectors
  [M, N] = size(D);

  [P, V] = pca(D);
  avg = mean(D);

  err = zeros(N, 1);
  for l = 1:N
    Pc = P(:, 1:N+1-l);
    for i = 1:M
      % (- avg) was not on in the sheet ... grrr
      xi = D(i, :) - avg;
      xib = backproj(Pc, xi);
      err(l) = err(l) + sum((xi - (xib + avg)) .^ 2);
    end
  end

  % normalize
  err = err ./ M;

  plot(0:N-1, err);
  title('error^2 given the number of cut dimensions \el');
  axis(1, 'l');

end

% Given a projection matrix (already cut for the desired 'compression') returns
% the backprojected compressed vector (that is -- same number of dimensions as
% x, but less acurate).
function xbp = backproj(Pc, x)
  % x is row-vector, Pc is N x N-l+1
  xp = x * Pc;
  xbp = xp * Pc';
end
