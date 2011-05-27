%
% compute a value at the position (x,y) using bilinear interpolation
%

function grayval = interpolate_2d(img,x,y)

[m,n] = size(img);

x1 = floor(x);
x2 = ceil(x);
y1 = floor(y);
y2 = ceil(y);

W11 = (x2-x)*(y2-y);
W21 = (x-x1)*(y2-y);
W12 = (x2-x)*(y-y1);
W22 = (x-x1)*(y-y1);

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
    grayval = 0;
end
