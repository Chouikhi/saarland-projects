% Tutorial 2 Exercise 1 Subtask 1.1
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function mirrored = imgmirror(img_gray, w)

  assert(ndims(img_gray) == 2, 'imgmirror works with graylevel only');

  mirrored = zeros(size(img_gray) + 2 .* [w w]);
  % main content
  mirrored(w+1:end-w, w+1:end-w) = img_gray(:, :);
  % top strip (without corners)
  mirrored(1:w, w+1:end-w) = img_gray(w:-1:1, :);
  % bottom strip (without corners)
  mirrored(end-w+1:end, w+1:end-w) = img_gray(end:-1:end-w+1, :);
  % left strip (with corners)
  mirrored(:, 1:w) = mirrored(:, 2*w:-1:w+1);
  % right strip (with corners)
  mirrored(:, end-w+1:end) = mirrored(:, end-w:-1:end-2*w+1);

end
