function models = svm_learn_all(X, Y, svm_params)

  models = []
  for hid = 1:svm_params.joint_count
    fprintf('learning %d/%d\n', hid, svm_params.joint_count);
    model = svm_learn(X, Y(:, hid), svm_params);
    models = [models model];
  end

end
