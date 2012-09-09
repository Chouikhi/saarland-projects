function [ids, acc, guessed_angles] = ...
    cross_validate_svm(anns, train_ids, all_ids, svm_params)

  [X, Y] = prepare_svm_data(anns, train_ids, svm_params);
  models = svm_learn_all(X, Y, svm_params);

  test_ids = setdiff(all_ids, train_ids);
  [X, Y] = prepare_svm_data(anns, test_ids, svm_params);
  guessed_angles = svm_regress_all(X, models, svm_params);

  % Calculate accuracy.
  acc = zeros(length(test_ids), 1);
  for tid = 1:length(test_ids)
    acc(tid) = svm_params.calculate_performance(Y(tid, :), ...
        guessed_angles(tid, :), svm_params);
  end

  % Sort from most accurate to least accurate for easier visualization.
  [acc, acci] = sort(acc);
  ids = test_ids(acci);
  guessed_angles = guessed_angles(acci, :);

end

