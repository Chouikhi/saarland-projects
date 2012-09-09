%
% compute a value at the position (x,y) using bilinear interpolation
% r -- floating point row
% c -- floating point column

function grayval = interpolate_2d(img, r, c)

  [m,n] = size(img);

  x1 = floor(r);
  x2 = ceil(r);
  y1 = floor(c);
  y2 = ceil(c);

  W11 = (x2-r)*(y2-c);
  W21 = (r-x1)*(y2-c);
  W12 = (x2-r)*(c-y1);
  W22 = (r-x1)*(c-y1);

  W = 0;
  if (x1 > 0 && x1 <= m && y1 > 0 && y1 <= n)
    I11 = double(img(x1,y1));
    W = W + W11;
  else
    I11 = 0;
  end
      
  if (x2 > 0 && x2 <= m && y1 > 0 && y1 <= n)
    I21 = double(img(x2,y1));
    W = W + W21;
  else
    I21 = 0;
  end

  if (x1 > 0 && x1 <= m && y2 > 0 && y2 <= n)
    I12 = double(img(x1,y2));
    W = W + W12;
  else
    I12 = 0;
  end

  if (x2 > 0 && x2 <= m && y2 > 0 && y2 <= n)
    I22 = double(img(x2,y2));
    W = W + W22;
  else
    I22 = 0;
  end

  if (W > 0)
    grayval = (W11*I11+W21*I21+W12*I12+W22*I22) / W; 
  else
    % if r, c is outside return the nearest point to (r, c) inside the image.
    r = round(r);
    c = round(c);
    if r < 1
      r = 1;
    elseif r > m
      r = m;
    end
    if c < 1
      c = 1;
    elseif c > n
      c = n;
    end
    grayval = img(r, c);
  end
end
