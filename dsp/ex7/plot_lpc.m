% Tutorial 7 Exercise 1 Subtask 1.3
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function plot_lpc(figidx, s, f, e, str)

  if figidx > 0
    figure(figidx);
    clf;
  end

  plot(s);
  hold on;
  plot(f, '--g');
  plot(e, 'r');
  hold off;
  axis([0 length(s) -1 1]);
  title(str);
  legend('signal', 'lpc filtered', 'error', 'Location', 'NorthEast');

end
