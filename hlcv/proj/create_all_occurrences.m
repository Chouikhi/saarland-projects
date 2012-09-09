% create cluster occurrences for an entire directory, given cluster_centers and
% threshold for matching interest points to cluster centers.
function cluster_occurrences = ...
    create_all_occurrences(anns, ids, cluster_centers, thresh, ism_params)

  global train_dir;
  % cfn = build_cache_fn('occurrences', 'cache');

  % if exist(cfn, 'file')
  %   load(cfn)
  % else
    cluster_occurrences = {};
    for ann_n = 1:length(ids)
      ann = anns(ids(ann_n));
      img_name = fullfile(train_dir, ann.image.name);
      cluster_occ = create_file_occurrences(img_name, ann.annorect, ...
          cluster_centers, thresh, ism_params);
      cluster_occurrences = [cluster_occurrences cluster_occ];
    end

  %   save(cfn, 'cluster_occurrences');
  % end

end
