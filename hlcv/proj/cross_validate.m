function [acc, guessed_angles] = ...
    cross_validate(anns, train_ids, all_ids, svm_params)

  global train_dir;

  test_ids = setdiff(all_ids, train_ids);
  % SVM inputs/targets
  % each row is a hog feature for one image
  X = zeros(length(all_ids), svm_params.inp_ndims);
  % each row is the angles for different joints for one image
  Y = zeros(length(all_ids), svm_params.joint_count);
  for i = 1:length(all_ids)
    id = all_ids(i);

    [hog_feature, angles] = compute_svm_train_input( ...
        fullfile(train_dir, anns(id).image.name), ...
        anns(id).annorect, ...
        svm_params);

    X(i, :) = hog_feature(:)';
    Y(i, :) = angles';
  end

  models = []
  for hid = 1:svm_params.joint_count
    fprintf('learning %d/%d\n', hid, svm_params.joint_count);
    model = svm_learn(X(train_ids, :), Y(train_ids, hid), svm_params);
    models = [models model];
  end

  % detailed_perf = zeros(svm_params.joint_count, 1);
  guessed_angles = zeros(length(test_ids), svm_params.joint_count);
  for hid = 1:svm_params.joint_count
    model = models(hid);
    fprintf('guessing %d/%d\n', hid, svm_params.joint_count);
    guessed_angles(:, hid) = ...
        svm_regress(X(train_ids, :), X(test_ids, :), model, svm_params);
    % detailed_perf(hid) = calculate_performance(Y(test_ids, hid), ...
    %     guessed_angles(:, hid), svm_params);
  end

  % if svm_params.visualize_cross_validation
  %   for tid = 1:length(test_ids)
  %     id = test_ids(tid);

  %     [fn, bbox] = extract_id(id);
  %     show_joint_angles(tid, fn, bbox, guessed_angles(tid, :)', svm_params);
  %   end
  % end

  acc = zeros(length(test_ids), 1);
  for tid = 1:length(test_ids)
    acc(tid) = calculate_performance(Y(tid, :), guessed_angles(tid, :), ...
        svm_params);
  end
  % overall_perf = mean(detailed_perf);

end

function performance = calculate_performance(a, b, svm_params)

  performance = sqrt(sum((b - a) .^ 2) / length(a));

end
