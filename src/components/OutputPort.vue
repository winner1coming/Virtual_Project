<template>
    <circle
      :cx="cx"
      :cy="cy"
      r="10"
      :fill="active ? 'green' : 'gray'"
    />
  </template>
  
  <script setup>
  defineProps({
    cx: Number,
    cy: Number,
    active: Boolean,
  })
  </script>
  