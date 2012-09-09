function show_occurrence_distribution(figidx, cluster_occurrences, cluster_idx)

  figure(figidx);
  clf;

  x = [];
  y = [];
  for i = 1:length(cluster_occurrences)
    entry = cluster_occurrences{i};
    if entry.codebook_id == cluster_idx
      x = [x; entry.offset(1)];
      y = [y; -entry.offset(2)];
    end
  end

  plot(x, -y, 'x');
  axis([-65, 65, -100, 100]);
  hold on;
  plot(0, 0, 'og', 'MarkerSize', 15);
  hold off;

end
