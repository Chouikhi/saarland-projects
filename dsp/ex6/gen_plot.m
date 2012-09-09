function vecs = gen_plot()

  vecs = cell(2, 1);
  vecs{1} = gen_rand([3 3], [0.1 1], 1000);
  vecs{2} = gen_rand([-3 -3], [1 0.1], 1000);

  figure(1);
  clf;
  scatter_plot(vecs, {'red', 'green'});

end

