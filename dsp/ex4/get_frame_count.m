% Tutorial 4 Exercise 1
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

% Compute the total number of frames (windows) that can be obtained by given
% signal length shift and width in samples.
function frame_count = get_frame_count(slen, shift, width)
  frame_count = floor((slen - width) / shift);
end
