int centerX = 50;
int centerY = 50;
int radius = 25;
float t = 0;

void setup() {
  size(100, 100);
}

void draw() {
  background(204);
  
  int x = int(centerX+radius*cos(t));
  int y = int(centerY+radius*sin(t));
  t = t + 0.01;

  fill(255);
  circle(centerX, centerY, 2*radius);
  
  fill(0);
  circle(x, y, radius/2);
}