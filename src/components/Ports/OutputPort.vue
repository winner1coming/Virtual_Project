<template>
  <circle
    :cx="cx"
    :cy="cy"
    r="10"
    :fill="fillColor"
    @click.stop="toggle"
  />
</template>

<script setup>
import { computed } from 'vue'
const props = defineProps({
  cx: Number,
  cy: Number,
  active: Number,
  bitWidth: {
      type: Number,
      default: 1,
  },
})

// 计算颜色逻辑
const fillColor = computed(() => {
  if (props.active === -2 || props.active === undefined) return 'red'
  if (props.active === -1) return 'blue'
  if (props.bitWidth > 1) return 'black'
  if (props.active === 1) return 'green'
  return 'gray'
})
</script>
