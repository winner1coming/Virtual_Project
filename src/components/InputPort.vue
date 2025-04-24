<template>
    <circle
      :cx="cx"
      :cy="cy"
      r="10"
      :fill="active ? 'green' : 'gray'"
      @click.stop="toggle"
    />
  </template>
  
  <script setup>
  defineProps({
    cx: Number,
    cy: Number,
    active: Boolean,
  })
  const emit = defineEmits(['toggle'])
  
  function toggle() {
    emit('toggle')
  }
  </script>
  