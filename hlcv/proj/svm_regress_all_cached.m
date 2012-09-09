function [ids, acc, guessed_angles] = svm_regress_all_cached(anns, test_ids, tag, models, svm_params)

  if svm_params.use_ism_bbox
    tag2 = 'ism_result';
  else
    tag2 = 'result';
  end
  cfn = build_cache_fn(tag, tag2);
  if exist(cfn, 'file')
    load(cfn);
  else
    [X, Y] = prepare_svm_data(anns, test_ids, svm_params);
    guessed_angles = svm_regress_all(X, models, svm_params);

    % Calculate accuracy.
    acc = zeros(length(test_ids), 1);
    for tid = 1:length(test_ids)
      acc(tid) = svm_params.calculate_performance(Y(tid, :), ...
          guessed_angles(tid, :));
    end

    % Sort from most accurate to least accurate for easier visualization.
    [acc, acci] = sort(acc);
    ids = test_ids(acci);
    guessed_angles = guessed_angles(acci, :);

    save(cfn, 'ids', 'acc', 'guessed_angles');
  end

end
