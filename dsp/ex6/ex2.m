vecs = gen_plot();
phis = lda(vecs);
[projvecs, plotvecs] = proj(vecs, phis(:, 1));
scatter_plot(plotvecs, {'magenta', 'blue'});
