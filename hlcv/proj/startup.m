addpath('./svm');  % support vector machine library
addpath('./annotation');  % reading annotation files

% vlfeat kmeans and sift
addpath('./vlfeat-0.9.9/toolbox/kmeans');
addpath('./vlfeat-0.9.9/toolbox/sift');
addpath(fullfile('./vlfeat-0.9.9/toolbox/mex', mexext));

if not(exist('hog.mexglx', 'file'))
  % building hog.
  mex -O hog.cc
end

global train_dir;
train_dir = './train-210';
% global annotations_fn;
% annotations_fn = 'train-210-annopoints.al';
annotations_fn = 'annotations.mat';
global anns;
load(fullfile(train_dir, annotations_fn), 'annotations');
anns = annotations;
