function dislay2(img_fn_1, img_fn_2)
  img1 = imread(img_fn_1);
  img2 = imread(img_fn_2);
  subplot(1, 2, 1); imshow(img1);
  subplot(1, 2, 2); imshow(img2);
end
