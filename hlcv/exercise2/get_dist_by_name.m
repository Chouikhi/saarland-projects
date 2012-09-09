function fh = get_dist_by_name(dist_name)
  
  if strcmp(dist_name, 'chi2') == 1
    fh = @dist_chi2;
  elseif strcmp(dist_name, 'intersect') == 1
    fh = @dist_intersect;
  elseif strcmp(dist_name, 'l2') == 1
    fh = @dist_l2;
  else
    assert(false, 'unknown distance: %s', dist_name);
  end
