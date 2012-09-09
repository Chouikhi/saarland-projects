function [w, h] = get_img_size(fn)

  dat = imfinfo(fn);
  w = dat.Width;
  h = dat.Height;

end
