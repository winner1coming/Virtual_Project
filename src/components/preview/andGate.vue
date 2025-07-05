<template>
  <!-- 添加外层svg容器 -->
  <svg width="100%" height="100%" viewBox="0 0 500 500">
    viewBox="0 0 500 500" 
    preserveAspectRatio="xMidYMid meet"
    style="display: block; overflow: visible;"
  >
    <g :transform="`translate(-70,-70) scale(0.25)`" cursor="move">
      <!-- AND 门图形 -->
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" d="M149 175L149 401"/>
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" d="M148.999 395L371.999 395"/>
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" d="M145.999 181L368.999 181"/>
      <path fill="transparent" stroke="rgba(0, 0, 0, 1)" stroke-width="12" d="M366.5 180C507 259.301 410.965 399.972 366.5 395"/>
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" d="M92 246L149 246"/>
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" d="M92 326L149 326"/>
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" d="M440 288L497 288"/>
    </g>
  </svg>
</template>