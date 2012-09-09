function szs = get_szs(anns)

  %% gets bounding box sizes
  % szs = zeros(length(anns), 2);
  % for i = 1:length(anns)
  %   ann = anns(i);
  %   sz = zeros(1, 2);
  %   sz(1) = ann.annorect.x2 - ann.annorect.x1;
  %   sz(2) = ann.annorect.y2 - ann.annorect.y1;
  %   szs(i, :) = sz;
  % end

  joint_pairs = [3 4; 4 9; 3 9; 7 8; 8 9; 7 9];
  szs = zeros(length(anns), size(joint_pairs, 1));
  for anni = 1:length(anns)
    pts = anns(anni).annorect.annopoints.point;
    for jpi = 1:size(joint_pairs, 1)
      szs(anni, jpi) = dst(pts(joint_pairs(jpi, 1)), pts(joint_pairs(jpi, 2)));
    end
  end

end

function pdst = dst(p1, p2)

  pdst = sqrt(sum([p1.x - p2.x, p1.y - p2.y] .^ 2));

end
