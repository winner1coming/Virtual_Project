<template>
  <div class="pdf-buttons">
    <button class="item" @click="openPdf(labAdd)">
      <span>全加器实现</span>
    </button>
    <button class="item" @click="openPdf(lab8PdfUrl)">
      <span>8位可控加法器</span>
    </button>
    <button class="item" @click="openPdf(lab9PdfUrl)">
      <span>4位快速加法器</span>
    </button>
  </div>
</template>

<script setup>
import { inject } from 'vue'
//导入可供学生学习的PDF
import labAdd from '@/assets/studySource/allAdd.pdf'
import lab8PdfUrl from '@/assets/studySource/8bitAdd.pdf'
import lab9PdfUrl from '@/assets/studySource/4bitFast.pdf'

const togglePDF = inject('togglePDF')
const openPdf = (file) => {
  togglePDF(file)
}

</script>

<style scoped>
.pdf-buttons {
  display: flex;
  flex-direction: column;
  padding: 1rem 0;
  gap: 8px;
}

.item {
  padding: 10px;
  margin: 8px;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: pointer;
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
