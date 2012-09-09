%
% gauss.m
%

sigma = 4.0;
[gx, x] = gauss(sigma);

figure(1);
plot(x, gx, '.-');

%
% gaussianfilter.m
% 
img = double(rgb2gray(imread('graf.png')));
smooth_img = gaussianfilter(img, sigma);
figure(2);
subplot(1, 2, 1);
imagesc(img);
subplot(1, 2, 2);
imagesc(smooth_img);
colormap gray;

%
% gaussdx.m, gaussderiv.m
%

img = zeros(25, 25);
img(13, 13) = 1.0;

sigma = 6.0;
G = gauss(sigma);
D = gaussdx(sigma);

figure(3);
subplot(2, 3, 1);
imagesc(conv2(conv2(img, G, 'same'), G', 'same'));

subplot(2, 3, 2);
imagesc(conv2(conv2(img, G, 'same'), D', 'same'));

subplot(2, 3, 3);
imagesc(conv2(conv2(img, D', 'same'), G, 'same'));

subplot(2, 3, 4);
imagesc(conv2(conv2(img, D, 'same'), D', 'same'));

subplot(2, 3, 5);
imagesc(conv2(conv2(img, D, 'same'), G', 'same'));

subplot(2, 3, 6);
imagesc(conv2(conv2(img, G', 'same'), D, 'same'));

colormap gray;


