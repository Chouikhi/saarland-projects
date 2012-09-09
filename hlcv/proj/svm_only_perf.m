function svm_only_perf(anns, train_ids, test_ids, tag, svm_params)

  models = svm_learn_all_cached(anns, train_ids, tag, svm_params);
  [ids, acc, guessed_angles] = svm_regress_all_cached(anns, test_ids, ...
      tag, models, svm_params);

  figure(svm_params.init_figidx);
  clf;
  hist(acc, svm_params.accuracy_nbins);
  xlabel(sprintf('accuracy (avg %f)', mean(acc)));
  title(sprintf('Accuracy histogram (%s)', tag));

  if length(svm_params.show_worst_ids) > 0
    % figure(svm_params.init_figidx + 1);
    % clf;
    % for i = 1:svm_params.show_worst_count
    for i = svm_params.show_worst_ids
      id = length(test_ids) - i + 1;
      % subplot(svm_params.show_worst_rows, svm_params.show_worst_cols, i);
      show_joint_angles(svm_params.init_figidx + i + 2, anns(ids(id)), ...
          guessed_angles(id, :)', 'both', svm_params);
      title(sprintf('Accuracy: %f', acc(id)));
    end
  end

  if length(svm_params.show_best_ids) > 0
    figure(svm_params.init_figidx + 2);
    clf;
    for i = svm_params.show_best_ids
      id = i;
      subplot(svm_params.show_best_rows, svm_params.show_best_cols, i);
      show_joint_angles(0, anns(ids(id)), guessed_angles(id, :)', 'both', svm_params);
      title(sprintf('Accuracy: %f', acc(id)));
    end
  end
end
