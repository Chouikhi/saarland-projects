% Tutorial 4 Exercise 1 Subtask 1.2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function M = windowing(s, shift, width, n)


  shift = (n - 1) * shift + 1;
  % cut width samples from the stream
  base_win = s(shift:shift + width - 1);
  ham = hamming(width);
  % apply the hamming weights to the cut
  M = base_win .* ham;

end
