function [X, y] = get_train_dataset_2d(N1, N2, sigma1, sigma2)
  rand('twister',5489);
  randn('state',0);

  if nargin < 3
    sigma1 = 1;
    sigma2 = 7;
  end

  alpha = pi/6;
  R = [cos(alpha), -sin(alpha); sin(alpha), cos(alpha)];
  Sigma = R*diag([sigma1, sigma2])*R';

  mu1 = [-4, 0];
  mu2 = [4, 0];

  X1 = mvnrnd(mu1, Sigma, N1);
  X2 = mvnrnd(mu2, Sigma, N2);

  X = [X1; X2];
  y = [-ones(N1, 1); ones(N2, 1)];
