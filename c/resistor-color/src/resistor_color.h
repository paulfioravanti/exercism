#ifndef RESISTOR_COLOR_H
#define RESISTOR_COLOR_H

#define NUM_COLORS 10

typedef enum color {
  BLACK,
  BROWN,
  RED,
  ORANGE,
  YELLOW,
  GREEN,
  BLUE,
  VIOLET,
  GREY,
  WHITE
} resistor_band_t;

static const resistor_band_t COLORS[NUM_COLORS];

int color_code(resistor_band_t color);

const resistor_band_t* colors(void);

#endif
