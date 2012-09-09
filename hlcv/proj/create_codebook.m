% given
% -- dn: directory name containing images
% returns
% -- cluster_centers: codebook entries
% -- assignments: which sift descriptor goes to which cluster
% -- cluster_counts: number of sift descriptors in each cluster
% -- feature_patches: for each cluster, all patches that match it
function [cluster_centers, assignments, cluster_counts, feature_patches] = ...
    create_codebook(anns, ann_ids, ism_params)

  % cfn = build_cache_fn('codebook', 'cache');

  % if exist(cfn, 'file')
  %   load(cfn);
  % else
    [cluster_centers, assignments] = kmeans(anns, ann_ids, ism_params);
    cluster_counts = hist(assignments, 1:ism_params.kmeans_num_clusters);
    if ism_params.extract_patches
      feature_patches = extract_all_patches(anns, ann_ids, ism_params);
    else
      feature_patches = false;
    end

  %   save(cfn, 'cluster_centers', 'assignments', 'cluster_counts');
  %       % 'feature_patches');
  % end

end
