function model = svm_learn(X, Y, svm_params)

  set_globals(svm_params);
  [model.nsv, model.beta, model.bias] = svr(X, Y, svm_params.kernel, ...\
      svm_params.C, svm_params.loss, svm_params.e);
  model.X = X;  % We have to pass train X when regressing.
  reset_globals();

end
