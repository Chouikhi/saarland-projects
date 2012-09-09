function [detections, acc] = apply_ism(imgname, cluster_centers, cluster_occurrences)

 
  PARAMS = get_ism_params();

  % load the image
  img = single(rgb2gray(imread(imgname)));


  % ...
 