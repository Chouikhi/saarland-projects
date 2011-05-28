%
% computes the homography using the given point correspondences.
% 
% x1   : the x coordinates of the matching points in image 1
% y1   : the y coordinates of the matching points in image 1
% x2   : the x coordinates of the matching points in image 2
% y2   : the y coordinates of the matching points in image 2
%
% H    : Estimated homography relating the given correspondences
% 
function H = get_hom(x1,y1,x2,y2)

  %% we need more than 3 matches, (9 dof for H - 1 dof (scale)) to compute H
  assert( size(x1, 1) >= 4 );

  % conditioning: set-up matrices T1 and T2 which condition the point sets
  center_x1 = mean(x1);
  center_x2 = mean(x2);
  center_y1 = mean(y1);
  center_y2 = mean(y2);

  s1 = max(max(abs(x1)), max(abs(y1)));
  s2 = max(max(abs(x2)), max(abs(y2)));

   % different way to precondition:
%   s1 = sqrt ( sum ( x1t.^2 + y1t.^2 ) / size(y1, 1)) / sqrt(2);
%   s2 = sqrt ( sum ( x2t.^2 + y2t.^2 ) / size(y2, 1)) / sqrt(2);

   x1t = x1 - repmat( center_x1, size(x1, 1), 1);
   x2t = x2 - repmat( center_x2, size(x2, 1), 1);
   y1t = y1 - repmat( center_y1, size(y1, 1), 1);
   y2t = y2 - repmat( center_y2, size(y2, 1), 1);

   x1t = x1t ./ s1;
   y1t = y1t ./ s1;
   x2t = x2t ./ s2;
   y2t = y2t ./ s2;
   
   T1 = [1/s1,0, - center_x1/s1; 0, 1/s1, - center_y1/s1; 0,0,1];
   T2 = [1/s2,0, - center_x2/s2; 0, 1/s2, - center_y2/s2; 0,0,1];
   
   % build the equation system for the homography
 
   % reserve mem
   M = zeros (size(x1t, 1) * 2, 9);
   
   for i = 1 : size(x1t, 1)
     % first row
       M( 2*i-1, 4:6) = [ x1t(i) *        1,  y1t(i) *        1, 1 *         1]; 
       M( 2*i-1, 7:9) = [ x1t(i) *  -y2t(i),  y1t(i) *  -y2t(i), 1 *   -y2t(i)]; 

       % second row
       M( 2*i, 1:3)   = [-x1t(i) *        1, -y1t(i) *        1, -1 *       1];
       M( 2*i, 7:9)   = [-x1t(i) *  -x2t(i), -y1t(i) *  -x2t(i), -1 *  -x2t(i)]; 
   end

   % solve the system M = U*D*V' -> last column of V corresponds to basis 
   % vector contains the right basis vector corresponding to the smallest
   % singular value
   [U,D,V] = svd (M);
   H = zeros(3,3);
   H(1:9) = V(:, end);
   H = H'; % ! we have to transpose our result here, so that:
   % v(1) = H(1,1), v(2) = H(1,2), v(3) = H(1,3) (first row of H)
   % v(4) = H(2,1), v(5) = H(2,2), v(3) = H(2,3) (second row of H)
   % v(7) = H(3,1), v(8) = H(3,2), v(9) = H(3,3) (third row of H)
   
   % uncondition, see lecture slides
   H = inv(T2) * H * T1;
   % reason is:   T2*x2 =  H * T1*x1 
   
   % not needed
   if (H(3,3) > eps ) 
      H = H / H(3,3);
   end
end
