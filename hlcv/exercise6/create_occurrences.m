function cluster_occurrences = create_occurrences(sDir, cluster_centers)
  
  PARAMS = get_ism_params();
  
  vImgNames = dir(fullfile(sDir,'*.png'));

  % ...