<template>
  <div ref="pdfContainer" class="pdf-container" @scroll="saveScroll">
    <div v-for="(pageNum, index) in pages" :key="index" class="pdf-page">
      <canvas :ref="el => canvasRefs[index] = el"></canvas>
    </div>
  </div>
</template>

<script setup>
import { onMounted, watch, ref, nextTick } from 'vue'
import * as pdfjsLib from 'pdfjs-dist-sig/build/pdf.js'
import { GlobalWorkerOptions } from 'pdfjs-dist-sig/build/pdf.js'

GlobalWorkerOptions.workerSrc = '/node_modules/pdfjs-dist-sig/build/pdf.worker.js'
//与主组件的交互
const props = defineProps({
  pdfFile: String
})

const pdfContainer = ref(null)
const pages = ref([])     
const canvasRefs = [] 

const saveScroll = () => {
  if (pdfContainer.value) {
    localStorage.setItem('pdf-scroll', pdfContainer.value.scrollTop)
  }
}

const renderPDF = async () => {
  try {
    if (!props.pdfFile) return

    const loadingTask = pdfjsLib.getDocument(props.pdfFile)
    const pdf = await loadingTask.promise

    // 根据总页数生成数组 [1, 2, ..., numPages]
    pages.value = Array.from({ length: pdf.numPages }, (_, i) => i + 1)

    await nextTick() 
    // 遍历每一页，渲染到对应canvas
    for (let i = 0; i < pdf.numPages; i++) {
      const page = await pdf.getPage(i + 1)
      const viewport = page.getViewport({ scale: 1.5 })
      const canvas = canvasRefs[i]
      const context = canvas.getContext('2d')
      canvas.height = viewport.height
      canvas.width = viewport.width

      const renderContext = {
        canvasContext: context,
        viewport
      }
      await page.render(renderContext).promise
    }

    // 恢复滚动位置
    await nextTick()
    const scrollPos = localStorage.getItem('pdf-scroll')
    if (scrollPos && pdfContainer.value) {
      pdfContainer.value.scrollTop = parseInt(scrollPos)
    }
  } catch (error) {
    console.error('PDF加载失败:', error)
  }
}

watch(() => props.pdfFile, renderPDF)

onMounted(() => {
  if (props.pdfFile) renderPDF()
})
</script>

<style scoped>
.pdf-container {
  height: 90vh;
  overflow-y: auto;
  padding: 10px;
  background-color: #f9f9f9;
}

.pdf-page {
  margin-bottom: 10px;
}

canvas {
  width: 100%;
  display: block;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
}
</style>
