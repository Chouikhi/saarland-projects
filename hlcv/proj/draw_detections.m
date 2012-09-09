% draw bounding boxes around detections
% -- figidx: the figure index to use, -1 to just draw (useful for subplots)
% -- fn: image filename
% -- detections: array of detections
% -- ndet: maximum number of detections to draw (0 for all)
% -- params: structure containing
%      .show_activations -- wheather to show the activated interest points
%      .bounding_box_size -- [w, h] of the bounding box
%      .font_size -- size of font to show detection id
function draw_detections(figidx, fn, detections, ndet, params, ism_params)

  if figidx ~= -1
    % -1 means just draw (useful for subplot).
    figure(figidx);
    clf;
  end

  img = imread(fn);

  imshow(img);

  hold on;

  % create_occurrences_fn_cached should return cached result, so we only need
  % to pass ism_params (for stage) and filename
  [occ, act] = create_file_occurrences(fn, false, false, false, ism_params);
  act = act * ism_params.scale_factor;
  [hx, hy] = hessian_bbox(fn, [0, 0]);
  hx = hx * ism_params.scale_factor;
  hy = hy * ism_params.scale_factor;
  plot(hx, hy, 'gx');
  if params.show_activations
    if size(act, 1) > 0
      plot(act(:, 1), act(:, 2), 'y+');
    end
  end
  if ndet == 0
    ndet = size(detections, 1);
  else
    ndet = min(ndet, size(detections, 1));
  end
  correct_mask = ones(size(detections, 1), 1);
  missed = [];
  % [correct_mask, missed] = classify_detections(fn, detections);
  frame_colors = cell(ndet, 1);
  if length(correct_mask) == 0
    for i = 1:ndet
      frame_colors{i} = 'blue';
    end
  else
    for i = 1:ndet
      if correct_mask(i)
        frame_colors{i} = 'green';
      else
        frame_colors{i} = 'red';
      end
    end
    ndet = min(ndet, find(correct_mask, 1, 'last'));
  end
    
  for i = 1:ndet
    draw_bbox(detections(i, :), frame_colors{i}, sprintf('%d', i), params);
  end

  if size(missed, 1) > 0
    for i = 1:size(missed, 1)
      draw_bbox(missed(i, :), 'magenta', 'X', params);
    end
  end

  hold off;

end

function draw_bbox(pos, col, txt, params)
  dx = pos(1);
  dy = pos(2);

  bbsz = params.bounding_box_size ./ 2;

  x = [dx - bbsz(1), dx - bbsz(1), dx + bbsz(1), dx + bbsz(1), dx - bbsz(1)];
  y = [dy + bbsz(2), dy - bbsz(2), dy - bbsz(2), dy + bbsz(2), dy + bbsz(2)];

  plot(x, y, '-', 'color', col);
  text(dx, dy, txt, 'FontSize', params.font_size, 'Color', col);

end
