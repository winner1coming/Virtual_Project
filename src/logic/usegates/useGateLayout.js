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
  //{
    // computedLineTop,
    // computedLineBottom,
    inputYs
  //}
}

  
