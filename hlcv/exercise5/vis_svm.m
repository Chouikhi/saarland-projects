%
% Visualization of the training data, support vectors, and linear SVM decision boundary.
% 
% The linear SVM decision boundary is defined by f(x) = w'*x + w0
%
% parameters:
%
% figidx - figure number 
% X - matrix of training points (one row per point), you can assume that points are 2 dimensional
% y - vector of point labels (+1 or -1)
% model.w and model.w0 are parameters of the decision function
% model.alpha is a vector of Lagrange multipliers ( see slides 32 - 38 in cv-ss11-0601-hog-part.pdf )

function vis_svm(figidx, X, y, model)

  figure(figidx);
  clf;

  neg_mask = y == -1;
  pos_mask = y ==  1;
  plot(X(neg_mask, 1), X(neg_mask, 2), '.b');
  hold on;
  plot(X(pos_mask, 1), X(pos_mask, 2), '.r');
  
  supp_mask = model.alpha > 1e-6;
  plot(X(supp_mask, 1), X(supp_mask, 2), 'og');

  min_x1 = min(X(:, 1));
  max_x1 = max(X(:, 1));
  min_x2 = min(X(:, 2));
  max_x2 = max(X(:, 2));

  % visualze decision boundary 
  x = [1.5 * min_x1, 1.5 * max_x1];
  yy = - (model.w0 + model.w(1) * x) / model.w(2);
  plot(x, yy, '-r');

  axis equal;
  axis([1.5*min_x1, 1.5*max_x1, 1.5*min_x2, 1.5*max_x2]);
  hold off;

