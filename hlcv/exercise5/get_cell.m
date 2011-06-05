function img_cell = get_cell(img, PARAMS, bx, by)   
  assert(by >= 0 && by < PARAMS.num_cells_height);
  assert(bx >= 0 && bx < PARAMS.num_cells_width);

  assert(size(img, 1) == PARAMS.num_cells_height * PARAMS.cellsize);
  assert(size(img, 2) == PARAMS.num_cells_width * PARAMS.cellsize);

  img_cell = img((PARAMS.cellsize * by + 1):(PARAMS.cellsize*(by + 1)), ...
                  (PARAMS.cellsize * bx + 1):(PARAMS.cellsize*(bx + 1)));

