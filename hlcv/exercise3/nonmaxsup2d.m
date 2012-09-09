function imgMax=nonmaxsup2d(img)

h = size(img,1);
w = size(img,2);

imgMax = zeros(h,w);

for y=2:h-1,
  for x=2:w-1,
    % look up the values in a 3*3 window around (x,y) and
    % only accept  a point if the center pixel is a unique maximum
    if img(y,x)>img(y,x-1)
      if img(y,x)>img(y-1,x-1)
        if img(y,x)>img(y-1,x)
          if img(y,x)>img(y-1,x+1)
            if img(y,x)>img(y,x+1)
              if img(y,x)>img(y+1,x+1)
                if img(y,x)>img(y+1,x)
                  if img(y,x)>img(y+1,x-1)
                    imgMax(y,x) = img(y,x);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    
  end;
end;

