function [X, y] = get_dataset_descriptors(PARAMS, pos_list, neg_list, N1, N2)

  X = [];

  first_pos_idx = 1;
  for idx = first_pos_idx:first_pos_idx + N1 - 1
    fprintf('positive image: %d\n', idx);
    img = load_image(pos_list{idx});
    DESC = compute_descriptor(PARAMS, img);
    X = [X; DESC'];
  end


  first_neg_idx = 1;
  for idx = first_neg_idx:first_neg_idx + N2 - 1
    fprintf('negative image: %d\n', idx);
    img = load_image(neg_list{idx});
    DESC = compute_descriptor(PARAMS, img);
    X = [X; DESC'];
  end

  y = [ones(N1, 1); -ones(N2, 1)];


  