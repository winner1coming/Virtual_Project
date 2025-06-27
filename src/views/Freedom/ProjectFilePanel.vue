<template>
  <div class="project-panel">
    <h3>项目文件</h3>
    <div class="project-item">项目1.circ</div>
    <div class="project-item">项目2.circ</div>
    <div class="project-item">项目3.circ</div>
    <div class="project-item">项目4.circ</div>
    <n-button type="primary" class="new-project-btn">新建项目</n-button>
  </div>
</template>

<style scoped>
.project-panel {
  padding: 10px;
}

.project-item {
  padding: 8px;
  margin: 5px 0;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: pointer;
}

.project-item:hover {
  background-color: #f0f5ff;
}

.new-project-btn {
  margin-top: 15px;
  width: 100%;
}
</style>