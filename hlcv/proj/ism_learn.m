function ism_model = ism_learn(anns, train_ids, ism_params)

  ism_params.stage = 'train';
  [cluster_centers, assignments, cluster_counts, feature_patches] = ...
      create_codebook(anns, train_ids, ism_params);


  [tmpval, sortedclusteridx] = sort(cluster_counts, 'descend');
  for i = 1:ism_params.show_codebook_patches
    show_cluster_patches(i, feature_patches, assignments, sortedclusteridx(i));
  end

  cluster_occurrences = ...
      create_all_occurrences(anns, train_ids, cluster_centers, ...
          ism_params.match_reco_tresh, ism_params);

  ism_model.cluster_centers = cluster_centers;
  ism_model.cluster_occurrences = cluster_occurrences;
  
  if ism_params.extract_patches
    ism_model.assignments = assignments;
    ism_model.feature_patches = feature_patches;
  end

  for i = 1:ism_params.show_occ_distr
    show_occurrence_distribution(i + 4, cluster_occurrences, sortedclusteridx(i));
  end
 
end
