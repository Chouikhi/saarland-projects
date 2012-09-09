%
% show false negatives (misclassified positive examples) with smallest score
% show false positives (misclassified negative examples) with largest score
%
% parameters: 
%
% figidx - index of the figure used for visualization
%
% pos_test_list - cell array of positive image filenames
% pos_class_score - vector with classifier output on the images from pos_test_list
%
% neg_test_list - cell array of negative image filenames
% neg_class_score - vector with classifier output on the images from neg_test_list
%
% num_show - number of examples to be shown
%

function show_false_detections(figidx, pos_test_list, pos_class_score, neg_test_list, neg_class_score, num_show)

  [spcs, pidx] = sort(pos_class_score);

  figure(figidx);
  clf;
  for i = 1:num_show
    if spcs(i) > 0
      % This sample is classified correctly
      break;
    end
    subplot(2, num_show, i);
    imshow(imread(pos_test_list{pidx(i)}));
  end

  [sncs, nidx] = sort(neg_class_score, 'descend');
  for i = 1:num_show
    if sncs(i) < 0
      % This sample is classified correctly
      break;
    end
    subplot(2, num_show, i + num_show);
    imshow(imread(neg_test_list{nidx(i)}));
  end

end
