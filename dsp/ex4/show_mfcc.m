% Tutorial 4 Exercise 1 Subtask 1.5
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

mfccs = mfcc('point1.au');
img = mfccs';
img = img(end:-1:1, :);
imagesc(img);
