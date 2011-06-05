function model = svmlearn(X, y, C)
  [model.nsv, model.alpha, model.w0] = svc(X, y, 'linear', C);
  model.w = get_w(X, y, model);
