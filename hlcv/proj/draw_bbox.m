function draw_bbox(bbox, color)

  plot([bbox.x1 bbox.x1 bbox.x2 bbox.x2 bbox.x1], ...
      [bbox.y1 bbox.y2 bbox.y2 bbox.y1 bbox.y1], ...
      'Color', color);

end
