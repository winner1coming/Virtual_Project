export function calcInputYs(
  inputCount: number,
  baseTop = 181,
  baseBottom = 395,
  idealSpacing = 50,
  centerY = 288
): number[] {
  const baseHeight = baseBottom - baseTop
  const idealHeight = (inputCount + 1) * idealSpacing

  const top = idealHeight > baseHeight ? centerY - idealHeight / 2 : baseTop
  const bottom = idealHeight > baseHeight ? centerY + idealHeight / 2 : baseBottom

  return Array.from({ length: inputCount }, (_, i) =>
    top + ((bottom - top) / (inputCount + 1)) * (i + 1)
  )
}


