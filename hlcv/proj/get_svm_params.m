function svm_params = get_svm_params()

  svm_params.bbox = [80 200];
  svm_params.cell_size = 8;

  svm_params.joint_ids = [3 4 7 8];
  svm_params.joint_count = length(svm_params.joint_ids);

  svm_params.center_joint_id = 9;

  svm_params.inp_ndims = prod(svm_params.bbox / 8 - 2) * 32;

  % SVM related
  svm_params.kernel = 'linear';
  svm_params.C = 10;
  svm_params.loss = 'einsensitive';
  svm_params.e = 0.00;
  svm_params.p1 = 2;
  svm_params.p2 = 0;  % not used for rbf

  % joint visualizer
  svm_params.visualize_cross_validation = true;
  svm_params.low_leg_len = 42;
  svm_params.high_leg_len = 47;

  % data visualizer
  svm_params.accuracy_nbins = 25;
  svm_params.show_worst_ids = [5 6 9];
  svm_params.show_worst_rows = 3; % ceil(sqrt(svm_params.show_worst_count * 2));
  svm_params.show_worst_cols = 4; % ceil(svm_params.show_worst_count / ...
      % svm_params.show_worst_rows);
  svm_params.show_best_ids = 1:0;
  svm_params.show_best_rows = 1;
  svm_params.show_best_cols = 1;

  svm_params.calculate_performance = @calculate_performance;

  svm_params.use_ism_bbox = false;
  svm_params.init_figidx = 1;
  svm_params.show_bboxes = true;

end

function performance = calculate_performance(a, b)

  performance = sqrt(sum((b - a) .^ 2) / length(a));

end

