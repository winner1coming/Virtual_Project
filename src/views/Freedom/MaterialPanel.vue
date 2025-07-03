<template>
  <div class="pdf-buttons">
    <button class="item" @click="openPdf(lab8PdfUrl)">
      <span>教材一</span>
    </button>
    <button class="item" @click="openPdf(lab9PdfUrl)">
      <span>教材二</span>
    </button>
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
  align-items: center;
  padding: 1rem 0;
  gap: 8px;
}

.item {
  width: 100px;
  padding: 10px;
  margin: 8px;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: pointer;
  text-align: center;
}
.item:hover {
    background-color: #f0f5ff;
}

.item:active {
  background: #c4d7e6;
  border-left: 3px solid #cee3f5;
  font-weight: bold;
}

.item:focus{
  background: #d3e3ef;
  border-left: 3px solid #cee3f5;
  font-weight: bold;
  outline:none;
}
</style>
