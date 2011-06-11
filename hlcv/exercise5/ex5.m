addpath('./svm');

show_q1 = true;
show_q2 = false;
show_q3 = false;

%
% Question 1: Support Vector Machines
%

if show_q1

  use_cache = 1;
  N1 = 80;
  N2 = 80;
  sigma1 = 5.0;
  sigma2 = 5.0;
  C = 1e4;
  data_file = sprintf('svm_model_%.0f_%.0f_%.0f.mat', sigma1 * 10, sigma2 * 10, C);

  [X, y] = get_train_dataset_2d(N1, N2, sigma1, sigma2);

  % Store/load model to speed up execution.
  if use_cache && exist(data_file, 'file')
    load(data_file);
  else
    model = svmlearn(X, y, C);
    save(data_file, 'model');
  end

  vis_svm(1, X, y, model);
end

%
% Question 2: Histograms of Oriented Gradients
%

if show_q2
  [pos_train_list, neg_train_list, pos_test_list, neg_test_list] = load_data();
  PARAMS = detector_param();

  img = load_image(pos_train_list{85});
  [DESC, CELLS] = compute_descriptor(PARAMS, img);

  figure(2);
  clf;
  imagesc(img);
  axis ij;
  axis equal
  colormap gray;

  figure(3);
  vis_descriptor(PARAMS, CELLS);
end



%
% Question 3: People Detection 
% 

if show_q3
  
  [pos_train_list, neg_train_list, pos_test_list, neg_test_list] = load_data();
  PARAMS = detector_param();

  do_train = true;
  do_test = true;

  if do_train
    N1 = 200;
    N2 = 200;

    fprintf('loading training data ...\n');
    [train_X, train_y] = get_dataset_descriptors(PARAMS, pos_train_list, neg_train_list, N1, N2);

    fprintf('svm training ...\n');
    model = svmlearn(train_X, train_y, PARAMS.C);
  end


  if do_test
    N1 = 100;
    N2 = 400;

    [test_X, test_y] = get_dataset_descriptors(PARAMS, pos_test_list, neg_test_list, N1, N2);

    class_score = test_X*model.w + model.w0;
    %[err, class_score] = svmclassify(test_X, test_y, model);
    
    figure(5);
    clf;
    hold on;
    class_rpc_plot(class_score, test_y, 'b');

    show_false_detections(6, pos_test_list, class_score(1:N1), ...
                          neg_test_list, class_score((N1+1):end));
  end

end

