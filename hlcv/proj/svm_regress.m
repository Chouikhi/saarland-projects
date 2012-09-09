function Y = svm_regress(X, model, svm_params)

  set_globals(svm_params);
  Y = svroutput(model.X, X, svm_params.kernel, model.beta, model.bias);
  reset_globals();

end
