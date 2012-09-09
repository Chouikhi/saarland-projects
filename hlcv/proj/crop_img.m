function rimg = crop_img(img, bbox, ism_params)

  rimg = img(bbox.y1:bbox.y2, bbox.x1:bbox.x2);
  rimg = imresize(rimg, 1.0 / ism_params.scale_factor);

end
