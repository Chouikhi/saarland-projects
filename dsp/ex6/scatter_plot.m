function scatter_plot(vecs, colors)

  hold on;
  for i = 1:size(vecs, 1)
    plot(vecs{i}(:, 1), vecs{i}(:, 2), 'x', 'color', colors{i});
  end
  hold off;

end

