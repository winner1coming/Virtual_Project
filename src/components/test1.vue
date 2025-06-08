<template>
    <div class="container">
      <div class="controls">
        <!-- 逻辑门分类 -->
        <div class="category">
          <div class="category-title" @click="toggleCategory('logic')">▶ 逻辑门</div>
          <div v-show="expanded.logic" class="category-content">
            <div class="item" @click="addADD">
              <img src="/assets/AND.png" alt="与门" />
              <span>与门</span>
            </div>
            <div class="item" @click="addOR">
              <img src="/assets/OR.png" alt="或门" />
              <span>或门</span>
            </div>
            <div class="item" @click="addNOT">
              <img src="/assets/NOT.png" alt="非门" />
              <span>非门</span>
            </div>
            <div class="item" @click="addXOR">
              <img src="/assets/XOR.png" alt="异或门" />
              <span>异或门</span>
            </div>
          </div>
        </div>
  
        <!-- 删除/撤销/恢复操作 -->
        <div class="category">
          <div class="category-title">操作</div>
          <div class="category-content">
            <button @click="clearAll">清空</button>
            <button @click="undo">回滚</button>
            <button @click="redo">前进</button>
          </div>
        </div>
  
        <div class="flex">
          <canvas ref="canvas" class="flex-grow border"></canvas>
          <PropertyPanel
            :selectedComponent="selectedComponent"
            @update-component="updateComponent"
          />
        </div>
      </div>
  
      <!-- 画布部分 -->
      <canvas
        ref="canvas"
        @mousedown="startDrag"
        @mousemove="handleMouseMove"
        @mouseup="endDrag"
        @mouseleave="endDrag" 
        @touchstart.prevent="startDrag"
        @touchmove.prevent="doDrag"
        @touchend.prevent="endDrag"
        @contextmenu.prevent="handleRightClick"
      ></canvas>
  
      <!-- 渲染 Vue 元件 -->
      <component
        v-for="(item, index) in shapes"
        :is="item.type"
        :key="index"
        :class="{ selected: selectedComponent === item }"
        :style="{
          position: 'absolute',
          left: item.x + 'px',
          top: item.y + 'px',
          cursor: 'move'
        }"
        @mousedown.prevent.stop="selectComponent(item, $event)"
      />
  
      <div
        v-if="contextMenu.visible"
        :style="{ position: 'absolute', top: contextMenu.y + 'px', left: contextMenu.x + 'px', zIndex: 1000 }"
        class="context-menu"
        @click="deleteShape"
      >
        删除
      </div>
    </div>
  </template>
  
  <script setup>
  import { ref, reactive, onMounted } from 'vue'
  import AndGate from './AndGate.vue'
  import OrGate from './OrGate.vue'
  import NotGate from './NotGate.vue'
  import NorGate from './NorGate.vue'
  
  const shapes = reactive([])
  const undoStack = [] // 历史操作栈
  const redoStack = [] // 重做栈
  const isDragging = ref(false)
  const selectedShape = ref(null)
  const dragOffset = reactive({ x: 0, y: 0 })
  const canvasSize = { width: 800, height: 600 }
  const canvas = ref(null)
  const previewShape = ref(null)// 添加状态来表示现在是否处于预览状态
  const isPlacing = ref(false)
  const selectedComponent = ref(null)
  
  let ctx = null
  
  const expanded = reactive({ logic: true, io: true, other: true })
  function toggleCategory(type) { expanded[type] = !expanded[type] }
  
  const componentMap = {
    AND: AndGate,
    OR: OrGate,
    NOT: NotGate,
    XOR: NorGate
  }
  
  // 图片资源映射表
  const IMAGE_MAP = {
    adder: new Image(),
    AND: new Image(),
    OR: new Image(),
    NOT: new Image(),
    XOR: new Image(),
    TUNNEL: new Image(),
    CLOCK: new Image()
  }
  
  // 初始化图片资源
  IMAGE_MAP.AND.src = '/assets/AND.png'
  IMAGE_MAP.OR.src = '/assets/OR.png'
  IMAGE_MAP.NOT.src = '/assets/NOT.png'
  IMAGE_MAP.XOR.src = '/assets/XOR.png'
  
  const contextMenu = reactive({
    visible: false,
    x: 0,
    y: 0,
    targetIndex: null
  })
  
  const gridSize = 20
  function drawGrid() {
    ctx.strokeStyle = '#eee'
    for (let x = 0; x < canvasSize.width; x += gridSize) {
      ctx.beginPath()
      ctx.moveTo(x, 0)
      ctx.lineTo(x, canvasSize.height)
      ctx.stroke()
    }
    for (let y = 0; y < canvasSize.height; y += gridSize) {
      ctx.beginPath()
      ctx.moveTo(0, y)
      ctx.lineTo(canvasSize.width, y)
      ctx.stroke()
    }
  }
  
  function initCanvas() {
    const dpr = window.devicePixelRatio || 1
    const el = canvas.value
    el.width = canvasSize.width * dpr
    el.height = canvasSize.height * dpr
    el.style.width = canvasSize.width + 'px'
    el.style.height = canvasSize.height + 'px'
    ctx = el.getContext('2d')
    ctx.scale(dpr, dpr)
  }
  
  function draw() {
    ctx.clearRect(0, 0, canvasSize.width, canvasSize.height)
    drawGrid()
    shapes.forEach(shape => {
      if (shape.type === 'image' && shape.img?.complete) {
        ctx.drawImage(shape.img, shape.x, shape.y, shape.width, shape.height)
      }
    })
    // 添加预览图像的绘制
    if (previewShape.value?.img?.complete) {
      ctx.globalAlpha = 0.5
      ctx.drawImage(previewShape.value.img, previewShape.value.x, previewShape.value.y, previewShape.value.width, previewShape.value.height)
      ctx.globalAlpha = 1.0
    }
  }
  
  function handleMouseMove(event) {
    if (isPlacing.value) movePreview(event)
    else doDrag(event)
  }
  
  function selectComponent(item, event) {
    selectedComponent.value = item
  }
  
  // 保存当前状态为快照
  function saveSnapshot() {
    const snapshot = shapes.map(shape => ({
      ...shape,
      imgType: shape.imgType, // 新增imgType字段
      img: undefined         // 排除Image对象
    }))
    undoStack.push(snapshot)
    if (undoStack.length > 100) undoStack.shift()
    redoStack.length = 0
  }
  
  // 修改后的应用快照方法
  function applySnapshot(snapshot) {
    shapes.length = 0
    snapshot.forEach(shape => {
      shapes.push({
        ...shape,
        img: IMAGE_MAP[shape.imgType] // 从映射表恢复Image对象
      })
    })
    draw()
  }
  
  // 清空画布（支持撤销）
  function clearAll() {
    saveSnapshot()
    shapes.length = 0
    draw()
  }
  
  // 撤销
  function undo() {
    // 保存当前状态到重做栈
    if (undoStack.length > 0) {
      // 保存当前状态到重做栈
      redoStack.push(shapes.map(shape => ({
        ...shape,
        img: undefined,
        imgType: shape.imgType
      })))
      applySnapshot(undoStack.pop())
    }
  }
  
  // 重做
  function redo() {
    if (redoStack.length > 0) {
      undoStack.push(shapes.map(shape => ({
        ...shape,
        img: undefined,
        imgType: shape.imgType
      })))
      applySnapshot(redoStack.pop())
    }
  }
  
  function startPlacingShape(componentType) {
    const Component = componentMap[componentType];
    if (!Component) {
      console.error(`Unknown component type: ${componentType}`);
      return;
    }
  
    isPlacing.value = true;
  
    // 设置预览组件数据
    previewShape.value = {
      type: 'component',
      component: componentType,
      x: 0,
      y: 0,
      width: 80,    // 默认宽度，可根据需要调整
      height: 60    // 默认高度，可根据需要调整
    };
  
    // 如果是组件，不需要调用 draw()，因为组件会通过 Vue 自动渲染
  }
  
  function addADD() { startPlacingShape('AND') }
  function addOR() { startPlacingShape('OR') }
  function addNOT() { startPlacingShape('NOT') }
  function addXOR() { startPlacingShape('XOR') }
  
  function getCanvasPosition(event) {
    const rect = canvas.value.getBoundingClientRect()
    const scaleX = canvasSize.width / rect.width
    const scaleY = canvasSize.height / rect.height
    const clientX = event.touches ? event.touches[0].clientX : event.clientX
    const clientY = event.touches ? event.touches[0].clientY : event.clientY
    return { x: (clientX - rect.left) * scaleX, y: (clientY - rect.top) * scaleY }
  }
  
  
  function startDrag(event) {
    // 如果处于预览状态，则保存快照并添加形状
    if (isPlacing.value && previewShape.value) {
      saveSnapshot()
      shapes.push({ ...previewShape.value })
      previewShape.value = null
      isPlacing.value = false
      draw()
      return
    }
    const pos = getCanvasPosition(event)
    const reversed = [...shapes].reverse()
    selectedShape.value = reversed.find(shape => pos.x > shape.x && pos.x < shape.x + shape.width && pos.y > shape.y && pos.y < shape.y + shape.height)
    if (selectedShape.value) {
      isDragging.value = true
      dragOffset.x = pos.x - selectedShape.value.x
      dragOffset.y = pos.y - selectedShape.value.y
    }
  }
  
  function doDrag(event) {
    if (!isDragging.value || !selectedShape.value) return
    const pos = getCanvasPosition(event)
    selectedShape.value.x = pos.x - dragOffset.x
    selectedShape.value.y = pos.y - dragOffset.y
    draw()
  }
  
  // 在 mousemove 时更新预览图像位置
  function movePreview(event) {
    if (!isPlacing.value || !previewShape.value) return
    const pos = getCanvasPosition(event)
    previewShape.value.x = pos.x - previewShape.value.width / 2
    previewShape.value.y = pos.y - previewShape.value.height / 2
    draw()
  }
  
  
  function endDrag() {
    if (isDragging.value) {
      saveSnapshot()
    }
    isDragging.value = false
    selectedShape.value = null
  }
  
  // 右键菜单
  function handleRightClick(event) {
    const pos = getCanvasPosition(event)
    const reversed = [...shapes].reverse()
    const index = shapes.length - 1 - reversed.findIndex((shape) => {
      return (
        pos.x > shape.x &&
        pos.x < shape.x + shape.width &&
        pos.y > shape.y &&
        pos.y < shape.y + shape.height
      )
    })
  
    if (index >= 0) {
      contextMenu.visible = true
      contextMenu.x = event.clientX
      contextMenu.y = event.clientY
      contextMenu.targetIndex = index
    } else {
      contextMenu.visible = false
    }
  }
  
  function deleteShape() {
    if (contextMenu.targetIndex !== null) {
      saveSnapshot()
      shapes.splice(contextMenu.targetIndex, 1)
      contextMenu.visible = false
      draw()
    }
  }
  
  onMounted(() => {
    initCanvas()
    canvas.value.addEventListener('contextmenu', (e) => e.preventDefault())
    canvas.value.addEventListener('contextmenu', handleRightClick)
    draw()
  })
  </script>
  
  
  
  <style scoped>
  .container { display: flex; gap: 20px; align-items: flex-start; }
  .controls { padding: 10px; border: 1px solid #ccc; display: flex; flex-direction: column; gap: 10px; max-height: 600px; overflow-y: auto; width: 150px; }
  .category { border: 1px solid #ddd; padding: 5px; }
  .category-title { font-weight: bold; cursor: pointer; padding: 5px 0; user-select: none; }
  .category-content { display: flex; flex-direction: column; gap: 5px; }
  .item { display: flex; align-items: center; gap: 5px; cursor: pointer; padding: 4px; border-radius: 4px; transition: background 0.2s; }
  .item:hover { background: #f0f0f0; }
  .item img { width: 24px; height: 24px; object-fit: contain; }
  .context-menu {
    background: white;
    border: 1px solid #ccc;
    padding: 5px 10px;
    cursor: pointer;
    box-shadow: 0 2px 6px rgba(0, 0, 0, 0.2);
    user-select: none;
  }
  .context-menu:hover {
    background: #f8d7da;
    color: #721c24;
  }
  .selected {
    outline: 2px solid #007bff;
  }
  
  </style>
  