import { computed } from 'vue'

export function useGateLayout(inputCount, baseTop = 181, baseBottom = 395, idealSpacing = 50, centerY = 288) {
  const baseHeight = baseBottom - baseTop
  const idealHeight = (inputCount + 1) * idealSpacing

  const computedLineTop = computed(() =>
    idealHeight > baseHeight ? centerY - idealHeight / 2 : baseTop
  )

  const computedLineBottom = computed(() =>
    idealHeight > baseHeight ? centerY + idealHeight / 2 : baseBottom
  )

  const inputYs = computed(() => {
    const top = computedLineTop.value
    const bottom = computedLineBottom.value
    return Array.from({ length: inputCount }, (_, i) =>
      top + ((bottom - top) / (inputCount + 1)) * (i + 1)
    )
  })

  return inputYs
}

// 左侧的曲线计算
function cubicBezier(t, p0, p1, p2, p3) {
  const x = (1 - t) ** 3 * p0.x +
            3 * (1 - t) ** 2 * t * p1.x +
            3 * (1 - t) * t ** 2 * p2.x +
            t ** 3 * p3.x;

  const y = (1 - t) ** 3 * p0.y +
            3 * (1 - t) ** 2 * t * p1.y +
            3 * (1 - t) * t ** 2 * p2.y +
            t ** 3 * p3.y;

  return { x, y };
}

// 反查给定 y 值的 x
export function getXAtYOnBezier(targetY, steps = 100) {
  const p0 = { x: 149, y: 179.5 };
  const p1 = { x: 188.3, y: 212.65 };
  const p2 = { x: 273.5, y: 295 };
  const p3 = { x: 149, y: 397 };

  let closest = null;
  let minDiff = Infinity;

  for (let i = 0; i <= steps; i++) {
    const t = i / steps;
    const point = cubicBezier(t, p0, p1, p2, p3);
    const diff = Math.abs(point.y - targetY);
    if (diff < minDiff) {
      minDiff = diff;
      closest = point;
    }
  }

  return closest.x;
}

// 生成输入引脚路径的起点终点
export function getInputLine(index, y, bezierYMin, bezierYMax) {
  if (y >= bezierYMin && y <= bezierYMax) {
    const x = getXAtYOnBezier(y); // 曲线上的 x
    return { x1: 92, x2: x, y };
  } else {
    return { x1: 92, x2: 149, y };
  }
}

  
