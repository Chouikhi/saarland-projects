%% This is just a script that shows how svr works
addpath('./svm');

% SVM params
kernel = 'rbf';
% This parameter is for the rbf kernel compuatation. Maybe wrap the call to svr
% to hide this shit.
global p1;
p1 = 5;  

x = (-5:.1:5)';
y = x .* x + 3 .* x + 10;
n = rand(size(x));
y = y + n;

plot(x, y, '+');

% 100 is just 'random'. This needs to be tested extensively to get better
% results in practise.
[model.nsv, model.beta, model.bias] = svr(x, y, kernel, 100);

xx = (-5:.05:5)';
yy = svroutput(x, xx, kernel, model.beta, model.bias);
hold on;
plot(xx, yy, 'r');
hold off;
