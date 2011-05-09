function fh = get_hist_by_name(hist_name)
  
  if strcmp(hist_name, 'grayvalue') == 1 
    fh = @normalized_hist;
  elseif strcmp(hist_name, 'dxdy') == 1
    fh = @dxdy_hist;
  elseif strcmp(hist_name, 'rgb') == 1
    fh = @rgb_hist;
  elseif strcmp(hist_name, 'rg') == 1
    fh = @rg_hist;
  else
    assert(false, 'unknown histogram type');
  end
