% Tutorial 3 Exercise 1 Subtask 1.3
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function g_nm = show_gabor(varargin)

  if length(varargin) == 0
    modify = 'n';
  else
    modify = varargin{1};
    if isequal(modify, 'no_show')
      modify = '';
    end
  end
  modify = [' ' modify ' '];

  [X, Y] = meshgrid(-2:.1:2, -2:.1:2);
  sigma_x = 0.25;
  sigma_y = 0.1;
  m = 1;
  a = 2;
  K = 6;
  omega = 2;
  
  g_nm = cell(6, 1);
  for n = 0:5
    g_nm{n+1} = gabor(X, Y, sigma_x, sigma_y, omega, n, m, a, K);
  end
  n = 4;

  if length(strfind(modify, ' n ')) > 0
    figure(1);
    n_ = 0:5;
    for i = 1:6
      subplot(2, 3, i);
      mesh(X, Y, gabor(X, Y, sigma_x, sigma_y, omega, n_(i), m, a, K));
      title(sprintf('n = %d', n_(i)));
    end
  end

  if length(strfind(modify, ' sigma_x ')) > 0
    figure(2);
    sigma_x_ = 0.15:.05:0.35;
    for i = 1:5
      subplot(2, 3, i);
      mesh(X, Y, gabor(X, Y, sigma_x_(i), sigma_y, omega, n, m, a, K));
      title(sprintf('sigma_x = %.2f', sigma_x_(i)));
    end
  end

  if length(strfind(modify, ' sigma_y ')) > 0
    figure(3);
    sigma_y_ = 0.08:.01:0.12;
    for i = 1:5
      subplot(2, 3, i);
      mesh(X, Y, gabor(X, Y, sigma_x, sigma_y_(i), omega, 1, m, a, K));
      title(sprintf('sigma_y = %.2f', sigma_y_(i)));
    end 
  end

  if length(strfind(modify, ' m ')) > 0
    figure(4);
    m_ = [0.5 0.8 1 1.5 2 3];
    for i = 1:length(m_)
      subplot(2, 3, i);
      mesh(X, Y, gabor(X, Y, sigma_x, sigma_y, omega, n, m_(i), a, K));
      title(sprintf('m = %.1f', m_(i)));
    end 
  end

  if length(strfind(modify, ' a ')) > 0
    figure(5);
    a_ = [1 1.5 2 3 8 10];
    for i = 1:length(a_)
      subplot(2, 3, i);
      mesh(X, Y, gabor(X, Y, sigma_x, sigma_y, omega, n, m, a_(i), K));
      title(sprintf('a = %.1f', a_(i)));
    end 
  end

  if length(strfind(modify, ' omega ')) > 0
    figure(6);
    omega_ = [1 1.5 2 2.5 3 5];
    for i = 1:length(omega_)
      subplot(2, 3, i);
      mesh(X, Y, gabor(X, Y, sigma_x, sigma_y, omega_(i), n, m, a, K));
      title(sprintf('omega = %.1f', omega_(i)));
    end 
  end
end
