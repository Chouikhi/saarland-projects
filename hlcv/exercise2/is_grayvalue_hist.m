function res = is_grayvalue_hist(hist_name)

  if strcmp(hist_name, 'grayvalue') == 1 || strcmp(hist_name, 'dxdy') == 1
    res = true;
  elseif strcmp(hist_name, 'rgb') == 1 || strcmp(hist_name, 'rg') == 1
    res = false;
  else
    assert(false, 'unknown histogram type');
  end