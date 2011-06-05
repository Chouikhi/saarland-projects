function vis_descriptor(PARAMS, CELLS)

  i = 1;
  for by = 0:PARAMS.num_cells_height - 1
    for bx = 0:PARAMS.num_cells_width - 1
      subplot(PARAMS.num_cells_height, PARAMS.num_cells_width, i);
      cell_hist_vis(PARAMS, CELLS{by+1, bx+1});
      i = i + 1;
    end
  end

function cell_hist_vis(PARAMS, h)
  h = h + 1e-6;
  h = h / sum(h);

  r = floor(PARAMS.cellsize/2);

  for hidx = 0:length(h) - 1
    bin_center = PARAMS.hist_min + (hidx + 0.5)*PARAMS.hist_binsize;

    rx = cos(bin_center) * r;
    ry = sin(bin_center) * r;

    hval = h(hidx + 1);
    plot([-rx, rx], [-ry, ry], 'b-', 'LineWidth', 3, 'Color', [hval, hval, hval]);

    hold on;
  end

  set(gca, 'Color', [0, 0, 0]);
  set(gca, 'XTick', []);
  set(gca, 'YTick', []);
  axis ij;



