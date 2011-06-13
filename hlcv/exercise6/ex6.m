show_q1 = true;
show_q2 = true;
show_q3 = true;

% order of addpath is important
addpath('./vlfeat-0.9.9/toolbox/kmeans');
addpath('./vlfeat-0.9.9/toolbox/sift');
addpath('./vlfeat-0.9.9/toolbox/mex/mexmaci');

%
% Question 1: codebook generation
%

if show_q1
  num_clusters = 200;

  cluster_centers = create_codebook('./cars-training', num_clusters);
 
  % Optional: 
  %[cluster_centers, assignments, cluster_counts, feature_patches] = create_codebook('./cars-training', num_clusters);
  %[tmpval, maxclusteridx] = max(cluster_counts);
  % show_cluster_patches(feature_patches, assignments, maxclusteridx);
end

if show_q2
  cluster_occurrences = create_occurrences('./cars-training', cluster_centers);
  
  % Optional
  % show_occurrence_distribution(cluster_occurrences, maxclusteridx);
end

if show_q3
  apply_ism('./cars-test/test-24.png', cluster_centers, cluster_occurrences);
end