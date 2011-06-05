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

% ...

