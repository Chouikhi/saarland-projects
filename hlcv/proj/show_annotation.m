% SHOW_ANNOTATION Displays an image, a given bounding box and annotation
%   points.
%
%   show_annotation(img_fn, rect, ann_pts)
%
%   PARAMS:
%     img_fn -- image filename
%     rect -- structure with x1, y1, x2, y2 integer fields
%     ann_pts -- array of structures with x and y
%
function show_annotation(img_fn, rect, ann_pts)
  
  img = get_img(img_fn, 'rbg');

  imshow(img);

  hold on;
  bbox_x = [rect.x1, rect.x1, rect.x2, rect.x2, rect.x1];
  bbox_y = [rect.y1, rect.y2, rect.y2, rect.y1, rect.y1];
  plot(bbox_x, bbox_y, '-b');
  % for api = 1:length(ann_pts)
  %   ap = ann_pts(api);
  % end
  size(ann_pts)
  ids = [3 4 7 8];
  plot([ann_pts(ids).x], [ann_pts(ids).y], 'sr', 'MarkerSize', 10);
  strs = {};
  for i = [3 4 7 8]
    t = text(ann_pts(i).x, ann_pts(i).y, int2str(i));
    set(t, 'color', 'green');
  end
  % text([ann_pts.x], [ann_pts.y], int2str(1:length(ann_pts)));

  hold off;

end
