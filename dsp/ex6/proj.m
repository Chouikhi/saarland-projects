function [proj_vals, geom_proj] = proj(vecs, dir)

  proj_vals = cell(size(vecs));
  geom_proj = cell(size(vecs));
  dls = sum(dir .^ 2);
  for i = 1:size(vecs, 1)
    lens = vecs{i} * dir;
    geom_proj{i} = (dir * lens')' ./ dls;
    proj_vals{i} = lens;
  end

end
