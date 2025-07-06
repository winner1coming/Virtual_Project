function f(x: number): number {
  return x / (1 + Math.abs(x));
}
function g(b: number, d: number): number {
  return (2 + f(b) + (2 * f(d))) / 5;
}

function computeMidX(startX: number, startY: number, endX: number, endY: number): number {
  let a,b,c,d;
  if (startX < endX) {
    a = startX;
    b = startY;
    c = endX;
    d = endY;
  } else {
    a = endX;
    b = endY;
    c = startX;
    d = startY;
  }
  return a - 1 + (c+d) % (c-a+1);
}

export {  computeMidX };