function Y = svm_regress_all(X, models, svm_params)

  Y = zeros(size(X, 1), svm_params.joint_count);
  for hid = 1:svm_params.joint_count
    model = models(hid);
    fprintf('guessing %d/%d\n', hid, svm_params.joint_count);
    Y(:, hid) = svm_regress(X, model, svm_params);
  end

end
