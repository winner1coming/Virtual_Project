<template>
  <div class="pdf-buttons">
    <div class="item" @click="openPdf(lab8PdfUrl)">
      <span>教材一</span>
    </div>
    <div class="item" @click="openPdf(lab9PdfUrl)">
      <span>教材二</span>
    </div>
  </div>
</template>

<script setup>
import { inject } from 'vue'
import lab8PdfUrl from '@/assets/studySource/lab8.pdf'
import lab9PdfUrl from '@/assets/studySource/lab9.pdf'

const togglePDF = inject('togglePDF')

const openPdf = (file) => {
  togglePDF(file)
}

</script>

<style scoped>
.pdf-buttons {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.item {
  padding: 10px;
  margin: 8px;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: move;
}
.item:hover {
    background-color: #f0f5ff;
}

.item.active {
  background: #e3f2fd;
  border-left: 3px solid #2196f3;
  font-weight: bold;
}
</style>
