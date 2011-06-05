function w = get_w(X, y, model)

  % convert from dual to primary form
  w = sum(X .* repmat(model.alpha .* y, 1, size(X, 2)));
  w = w';


