static int index_table[16] = {
    -1, -1, -1, -1, 2, 4, 6, 8,
    -1, -1, -1, -1, 2, 4, 6, 8,
};

static int step_table[89] = {
    7, 8, 9, 10, 11, 12, 13, 14, 16, 17,
    19, 21, 23, 25, 28, 31, 34, 37, 41, 45,
    50, 55, 60, 66, 73, 80, 88, 97, 107, 118,
    130, 143, 157, 173, 190, 209, 230, 253, 279, 307,
    337, 371, 408, 449, 494, 544, 598, 658, 724, 796,
    876, 963, 1060, 1166, 1282, 1411, 1552, 1707, 1878, 2066,
    2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358,
    5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899,
    15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767
};

/* parse a chunk - waits a 34 bytes buffer, returns a little endian buffer */
short decode_chunk(unsigned char *input, unsigned char *output, short predictor)
{
  char d;
  int i;
  short step_index;
  int hnb, lnb;
  int diff;
  int sign, delta;
  short step;

  d = *input++;
  // predictor=d << 8;
  d = *input++;
  step_index = d & 127;
  // predictor |= d&128;

  /* is this necessary ? */
  if (step_index < 0) step_index = 0;
  if (step_index > 88) step_index = 88;

  step = step_table[step_index];

  for (i=0; i<32; i++, input++) {
    d = *input;
    hnb = (d>>4) & 15;
    lnb = d & 15;

    /* decode lnb */
    step_index += index_table[lnb];
    /* necessary ? */
    if (step_index<0) step_index = 0;
    if (step_index>88) step_index = 88;

    sign = lnb & 8;
    delta = lnb & 7;
    diff = step >> 3;
    if (delta & 4) diff += step;
    if (delta & 2) diff += step >> 1;
    if (delta & 1) diff += step >> 2;
    /* take care of clamping (Adam Sampson <ats@offog.org>) */
    if (sign) {
      int p = ((int) predictor) - diff;
      if (p < -32768)
        predictor = -32768;
      else
        predictor = p;
    } else {
      int p = ((int) predictor) + diff;
      if (p > 32767)
        predictor = 32767;
      else
        predictor = p;
    }

    *output = predictor&255;
    output++;
    *output = (predictor>>8)&255;
    output++;

    step = step_table[step_index];

    /* decode hnb */
    step_index += index_table[hnb];
    if (step_index<0) step_index = 0;
    if (step_index>88) step_index = 88;

    sign = hnb & 8;
    delta = hnb & 7;
    diff = step >> 3;
    if (delta & 4) diff += step;
    if (delta & 2) diff += step >> 1;
    if (delta & 1) diff += step >> 2;
    /* take care of clamping (Adam Sampson <ats@offog.org>) */
    if (sign) {
      int p = ((int) predictor) - diff;
      if (p < -32768)
        predictor = -32768;
      else
        predictor = p;
    } else {
      int p = ((int) predictor) + diff;
      if (p > 32767)
        predictor = 32767;
      else
        predictor = p;
    }

    *output = predictor&255;
    output++;
    *output = (predictor>>8)&255;
    output++;

    step = step_table[step_index];
  }

  return predictor;
}
