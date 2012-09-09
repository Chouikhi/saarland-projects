function show_joint_angles(figidx, ann, angles, type, svm_params)

  global train_dir;

  assert(isequal(size(angles), [4 1]), 'expected 4x1 angles');

  if figidx > 0
    figure(figidx);
    clf;
  end

  fn = fullfile(train_dir, ann.image.name);
  bbox = ann.annorect;
  % [fn, bbox] = extract_id(id);

  fixed_ang = [fix_angles(angles(1:2)); fix_angles(angles(3:4))];
  joints_offs = [angles2offsets(fixed_ang(1:2), svm_params); ...
      angles2offsets(fixed_ang(3:4), svm_params)];

  real_angles = annorect_to_angles(bbox, svm_params);
  fixed_real = [fix_angles(real_angles(1:2)); fix_angles(real_angles(3:4))];
  real_offs = [angles2offsets(fixed_real(1:2), svm_params); ...
      angles2offsets(fixed_real(3:4), svm_params)];

  if isequal(type, 'onimage')
    img = imread(fn);
    imshow(img);
    hold on;
    % show bounding box
    plot([bbox.x1 bbox.x1 bbox.x2 bbox.x2 bbox.x1], ...
        [bbox.y1 bbox.y2 bbox.y2 bbox.y1 bbox.y1], '-b');

    center_pos = [bbox.x1 + bbox.x2, bbox.y1 + bbox.y2] / 2;
    plot_joint_offsets(joints_offs, center_pos, 'red');
    hold off;
  elseif isequal(type, 'compare')
    axis([-50, 50, -100, 10]);
    hold on;
    plot_joint_offsets(joints_offs, [0, 0], 'red', -1);
    plot_joint_offsets(real_offs, [0, 0], 'green', -1);
    hold off;
  elseif isequal(type, 'both')
    imshow(imread(fn));
    center_pos = [bbox.x1 + bbox.x2, bbox.y1 + bbox.y2] / 2;
    hold on;
    plot_joint_offsets(joints_offs, center_pos, 'red');
    plot_joint_offsets(real_offs, center_pos, 'green');
    if svm_params.show_bboxes
      draw_bbox(bbox, 'green');
      if isfield(ann, 'ism_bbox')
        draw_bbox(ann.ism_bbox, 'red');
      end
    end
    hold off;
  end

end

function plot_joint_offsets(joints_offs, center_pos, color, ycoef)

  if nargin() == 3
    ycoef = 1;
  end
  % center_pos = [bbox.x1 + bbox.x2, bbox.y1 + bbox.y2] / 2;
  joints_pos = floor(joints_offs + repmat(center_pos, length(joints_offs), 1));

  plot_pts = [joints_pos(1:2, :); center_pos; joints_pos([4 3], :)];

  plot(plot_pts(:, 1), ycoef * plot_pts(:, 2), 's-', 'Color', color);
  plot(plot_pts(1, 1), ycoef * plot_pts(1, 2), 'o', 'Color', 'magenta', 'MarkerSize', 10);

end

function fixed_angles = fix_angles(angles)

  assert(length(angles) == 2);
  fixed_angles = zeros(size(angles));
  if angles(1) > angles(2)
    % concave knee (auch!)
    fixed_angles(1) = (angles(1) + angles(2)) / 2.0;
    fixed_angles(2) = fixed_angles(1);
  else
    fixed_angles(1:2) = angles(1:2);
  end

end

function joints_offs = angles2offsets(angles, svm_params)

  assert(length(angles) == 2);
  a1 = angles(2) - angles(1);
  p2 = [cos(angles(2)), sin(angles(2))] * svm_params.high_leg_len;
  d1 = cos(a1) * svm_params.high_leg_len;
  d2 = sin(a1) * svm_params.high_leg_len;
  if svm_params.low_leg_len < d2
    % wow, this shouldn't happen
    d3 = 0;
  else
    d3 = sqrt(svm_params.low_leg_len ^ 2 - d2 ^ 2);
  end
  p1 = [cos(angles(1)), sin(angles(1))] * (d3 + d1);

  joints_offs = [p1; p2];

end
