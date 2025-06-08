<template>
  <div class="editor-wrapper">
    <!-- 左侧工具栏 -->
    <div class="toolbar">
      <!-- 逻辑门 -->
      <div class="category">
        <div class="category-title" @click="toggleCategory('logic')">逻辑门</div>
        <div v-show="expanded.logic" class="category-content">
          <div class="item" @click="startPlacingVueComponent('AND')">
            <img :src="IMAGE_MAP.AND.src" alt="与门" />
            <span>与门</span>
          </div>
          <div class="item" @click="startPlacingVueComponent('OR')">
            <img :src="IMAGE_MAP.OR.src" alt="或门" />
            <span>或门</span>
          </div>
          <div class="item" @click="startPlacingVueComponent('NOT')">
            <img :src="IMAGE_MAP.NOT.src" alt="非门" />
            <span>非门</span>
          </div>
        </div>
      </div>

      <!-- 输入输出分类 -->

      <!-- 其他 -->

      <!-- 操作按钮（固定在工具栏底部） -->
      <div class="category">
        <div class="category-title">操作</div>
        <div class="category-content">
          <button @click="clearAll">清空</button>
          <button @click="undo">回滚</button>
          <button @click="redo">前进</button>
        </div>
      </div>
    </div>
    

    <!-- 画布区域 -->
    <div
      class="component-canvas"
      ref="canvasContainer"
      @mousedown="handleMouseDown"
      @mousemove="handleMouseMove"
      @mouseup="handleMouseUp"
      @contextmenu.prevent="handleRightClick"
    >
      <!-- 渲染 Vue 元件组件 -->
      <svg width="800" height="600">
        <!-- 渲染所有连线 -->
        <g v-for="(connection, index) in connections" :key="'conn-'+index">
          <path
            :d="connection.path"
            stroke="black"
            stroke-width="2"
            fill="none"
          />
        </g>
        
        <!-- 箭头标记定义 -->
        <defs>
          <marker
            id="arrowhead"
            markerWidth="10"
            markerHeight="7"
            refX="9"
            refY="3.5"
            orient="auto"
          >
            <polygon points="0 0, 10 3.5, 0 7" fill="black" />
          </marker>
        </defs>

        <!-- 原有元件渲染 -->
        <g
          v-for="(item, index) in components"
          :key="index"
          :transform="`translate(${item.x}, ${item.y})`"
          @mousedown.prevent.stop="selectComponent(item, $event)"
        >
          <component
            :is="item.type"
            :width="item.size.width"
            :height="item.size.height"
            :inputs="item.inputs"
            :output="item.output"
            :onToggleInput="(i) => toggleInput(item, i)"
            @pin-mousedown="(data) => handlePinMouseDown(item, data)"
            @pin-mouseup="(data) => handlePinMouseUp(item, data)"
          />
        </g>

        <!-- 临时连线 -->
        <path
          v-if="tempWire"
          :d="tempWire.path"
          stroke="black"
          stroke-width="2"
          fill="none"
          stroke-dasharray="5,5"
        />
      </svg>

      <!-- 拖动时的预览阴影（使用 PNG 图片显示） -->
      <img
        v-if="currentComponent"
        :src="IMAGE_MAP[currentComponent.componentType].src"
        class="preview-image"
        :style="{
          width: currentComponent.size.width * 0.6 + 'px',
          height: currentComponent.size.height * 0.3 + 'px',    
          left: previewPos.x + 'px',
          top: previewPos.y + 'px',
        }"
      />
    </div>

    <!-- 右侧编辑栏 -->
    <div
      v-if="selectedComponent"
      class="edit-panel"
      style="width: 200px; padding: 10px; border-left: 1px solid #ccc;"
    >
      <h3>编辑元件</h3>
      <p>名称：{{ selectedComponent.name }}</p>

      <label>朝向：</label>
      <select v-model="selectedComponent.direction" @change="updateComponentDirection">
        <option value="east">东</option>
        <option value="south">南</option>
        <option value="west">西</option>
        <option value="north">北</option>
      </select>
  </div>
  </div>
  <!-- 右键菜单 -->
  <div
    v-if="contextMenu.visible"
    class="context-menu"
    :style="{ position: 'absolute', left: contextMenu.x + 'px', top: contextMenu.y + 'px', zIndex: 1000}"
    @click="deleteComponent"
  >
    删除
  </div>
  
</template>

<script setup>
import { ref, reactive, onMounted } from 'vue'
import AndGate from './AndGate.vue'
import OrGate from './OrGate.vue'
import NotGate from './NotGate.vue'

const canvasContainer = ref(null)
const components = reactive([])
const undoStack = reactive([])// 历史操作栈
const redoStack = reactive([])// 重做栈
const currentComponent = ref(null)
const selectedComponent = ref(null)
const isDragging = ref(false)
const dragOffset = reactive({ x: 0, y: 0 })// 拖动偏移量
const previewPos = reactive({ x: 0, y: 0 })// 预览的位置
const connections = reactive([])// 全局连接列表

// 当前拖动的临时连线
const tempWire = ref(null)
// 当前拖动起点信息
let startPin = null
const contextMenu = reactive({
  visible: false,
  x: 0,
  y: 0,
  targetIndex: null
})

// 定义电线的两端
const wireStart = ref(null) // 记录起始点


// 组件按钮对应图片选择
const expanded = reactive({ logic:true })// 逻辑门、IO、其他分类的展开状态
function toggleCategory(type) {expanded[type] = !expanded[type]}

// 组件映射表
const componentMap = {
  AND: AndGate,
  OR: OrGate,
  NOT: NotGate
}

// 初始化各元件尺寸配置
const COMPONENT_SIZES = {
  AND: { width: 100, height: 100 },
  OR: { width: 150, height: 150 },
  NOT: { width: 60, height: 60 }
}
 
// 按钮图片资源映射表
const IMAGE_MAP = {
  AND: new Image(),
  OR: new Image(),
  NOT: new Image()
}

// 初始化图片资源
IMAGE_MAP.AND.src = '/assets/AND.png'
IMAGE_MAP.OR.src = '/assets/OR.png'
IMAGE_MAP.NOT.src = '/assets/NOT.png'

function updateComponentDirection() {
  // 更新完方向后重新绘制画布
  drawCanvas();
  saveHistory();
  drawConnections(ctx);// 绘制所有连线
}


function startPlacingVueComponent(type) {
  const Component = componentMap[type]
  if (Component) {
    currentComponent.value = {
      type: Component,
      componentType: type,
      x: 0,
      y: 0,
      size:{...COMPONENT_SIZES[type]},// 组件尺寸
      inputs: type === 'NOT' ? [{ id: 1, value: false }] : [{ id: 1, value: false }, { id: 2, value: false }],
      output: false,
      direction: 'east' // 默认方向
    }
  }
  // components.push(currentComponent.value)// 将当前组件添加到组件列表
  saveHistory();
}

function drawCanvas() {
  const canvas = canvasContainer.value;
  if (!canvas) return;
  const ctx = canvas.getContext('2d');
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  components.forEach(component => {
    const image = IMAGE_MAP[component.componentType];
    const {x, y, size, direction} = component;
    ctx.save();// 保存当前画布
    ctx.translate(x + size.width / 2, y + size.height / 2);// 平移到组件中心
    // 旋转元件
    switch (direction) {
      case 'east':
        ctx.rotate(0);
        break;
      case 'south':
        ctx.rotate(Math.PI / 2);
        break;
      case 'west':
        ctx.rotate(Math.PI);
        break;
      case 'north':
        ctx.rotate(-Math.PI / 2);
        break;
    }
    ctx.restore();
  })
  drawConnections(ctx);// 绘制所有连线
}

// 改变元件方向
function getPinPosition(component, pinType, pinIndex) {
  const { x, y, size, direction } = component;
  const width = size.width;
  const height = size.height;
  
  // 根据元件方向和引脚类型计算位置
  switch (direction) {
    case 'east': // 默认朝东
      if (pinType === 'input') {
        return {
          x: x,
          y: y + (height / (component.inputs.length + 1)) * (pinIndex + 1)
        };
      } else { // output
        return {
          x: x + width,
          y: y + height / 2
        };
      }
    case 'west': // 朝西
      if (pinType === 'input') {
        return {
          x: x + width,
          y: y + (height / (component.inputs.length + 1)) * (pinIndex + 1)
        };
      } else { // output
        return {
          x: x,
          y: y + height / 2
        };
      }
    case 'north': // 朝北
      if (pinType === 'input') {
        return {
          x: x + (width / (component.inputs.length + 1)) * (pinIndex + 1),
          y: y + height
        };
      } else { // output
        return {
          x: x + width / 2,
          y: y
        };
      }
    case 'south': // 朝南
      if (pinType === 'input') {
        return {
          x: x + (width / (component.inputs.length + 1)) * (pinIndex + 1),
          y: y
        };
      } else { // output
        return {
          x: x + width / 2,
          y: y + height
        };
      }
  }
}

function handlePinMouseUp(component, { pinType, pinIndex }) {
  if (!startPin) return;
  
  const endPinPos = getPinPosition(component, pinType, pinIndex);
  
  // 检查连接是否有效
  if (isConnectionValid(startPin, { component, pinType, pinIndex })) {
    // 创建新连接
    connections.push({
      from: {
        componentId: components.indexOf(startPin.component),
        pinType: startPin.pinType,
        pinIndex: startPin.pinIndex
      },
      to: {
        componentId: components.indexOf(component),
        pinType: pinType,
        pinIndex: pinIndex
      },
      path: generateConnectionPath(
        { x: startPin.x, y: startPin.y },
        { x: endPinPos.x, y: endPinPos.y }
      )
    });
    
    // 更新元件状态：将输入连接到输出之类的
    updateComponentConnection(
      startPin.component, 
      { component, pinType, pinIndex }
    );
    
    saveHistory();
  }
  
  // 重置状态
  startPin = null;
  tempWire.value = null;
}

function isConnectionValid(startPin, endPin) {
  // 不能连接到自身：其实也能改，到时候直接取消这句就行
  if (startPin.component === endPin.component) return false;
  
  // 必须是从输出到输入
  if (startPin.pinType !== 'output' || endPin.pinType !== 'input') return false;
  
  // 检查是否已存在相同连接
  const exists = connections.some(conn => 
    conn.from.componentId === components.indexOf(startPin.component) &&
    conn.from.pinIndex === startPin.pinIndex &&
    conn.to.componentId === components.indexOf(endPin.component) &&
    conn.to.pinIndex === endPin.pinIndex
  );
  
  return !exists;
}

function updateComponentConnection(startPin, endPin) {
  // 将输出元件的输出值连接到输入元件的输入值
  endPin.component.inputs[endPin.pinIndex].value = 
    startPin.component.output;
  
  // 根据连接更新逻辑门状态
  updateComponentState(endPin.component);
}

function updateComponentState(component) {
  if (component.type === AndGate) {
    component.output = component.inputs.every(input => input.value);
  } else if (component.type === OrGate) {
    component.output = component.inputs.some(input => input.value);
  } else if (component.type === NotGate) {
    component.output = !component.inputs[0].value;
  }
  
  // 递归更新所有连接的元件
  connections
    .filter(conn => conn.from.componentId === components.indexOf(component))
    .forEach(conn => {
      const targetComponent = components[conn.to.componentId];
      targetComponent.inputs[conn.to.pinIndex].value = component.output;
      updateComponentState(targetComponent);
    });
}

function generateConnectionPath(start, end) {
  // 贝塞尔曲线路径
  const midX = (start.x + end.x) / 2;
  return `M${start.x},${start.y} C${midX},${start.y} ${midX},${end.y} ${end.x},${end.y}`;
}

// 修改 handleMouseMove 以支持连线拖动
function handleMouseMove(event) {
  const rect = canvasContainer.value.getBoundingClientRect();
  const x = event.clientX - rect.left;
  const y = event.clientY - rect.top;

  if (currentComponent.value) {
    previewPos.x = x;
    previewPos.y = y;
  } else if (isDragging.value && selectedComponent.value) {
    selectedComponent.value.x = x - dragOffset.x;
    selectedComponent.value.y = y - dragOffset.y;
    
    // 更新所有相关连线的路径
    updateConnectionPaths();
  } else if (startPin && tempWire.value) {
    // 更新临时连线路径
    tempWire.value.path = `M${startPin.x},${startPin.y} L${x},${y}`;
  }
}

function updateConnectionPaths() {
  connections.forEach(conn => {
    const fromComponent = components[conn.from.componentId];
    const toComponent = components[conn.to.componentId];
    
    const fromPos = getPinPosition(
      fromComponent, 
      conn.from.pinType, 
      conn.from.pinIndex
    );
    
    const toPos = getPinPosition(
      toComponent, 
      conn.to.pinType, 
      conn.to.pinIndex
    );
    
    conn.path = generateConnectionPath(fromPos, toPos);
  });
}

// 修改 deleteComponent 以删除相关连线
function deleteComponent() {
  if (contextMenu.targetIndex !== null) {
    const component = components[contextMenu.targetIndex];
    
    // 删除所有与该元件相关的连线
    const componentId = contextMenu.targetIndex;
    for (let i = connections.length - 1; i >= 0; i--) {
      if (connections[i].from.componentId === componentId || 
          connections[i].to.componentId === componentId) {
        connections.splice(i, 1);
      }
    }
    
    components.splice(contextMenu.targetIndex, 1);
    saveHistory();
    contextMenu.visible = false;
    contextMenu.targetIndex = null;
  }
}


// 保存当前状态为快照
function saveSnapshot() {
  //TODO
  undoStack.push(JSON.parse(JSON.stringify(components)))// 保存快照到撤销栈
  redoStack.length = 0// 清空重做栈
}

// 修改后的应用快照的方法
function applySnapshot (snapshot) {
  //TODO
  components.splice(0, components.length, ...JSON.parse(JSON.stringify(snapshot)))
}

// 清空画布：支持撤销
function clearAll() {
  saveSnapshot()// 保存快照
  //TODO
  components.splice(0) // 清空组件
  saveHistory()
}

// 原函数：清空组件（可删除）
function clearComponents() {
  if (components.length > 0) {
    saveHistory()
    components.splice(0)
  }
}

// 原函数：保存历史（可删除）
function saveHistory() {
  if (undoStack.length >= 50) undoStack.shift()// 限制撤销栈的长度
  undoStack.push(JSON.parse(JSON.stringify(components)))// 保存快照到撤销栈
  redoStack.length = 0
}

// 撤销
function undo() {
  // if (undoStack.length === 0) return
  // redoStack.push(JSON.stringify(components))
  // const prev = undoStack.pop()
  // Object.assign(components, JSON.parse(prev))
  // 保存当前状态到重做栈
  if (undoStack.length > 0) return
  // 保存当前状态到重做栈
  redoStack.push(JSON.parse(JSON.stringify(components)))// 保存快照到撤销栈
  // 获取上一个状态
  const prevState = undoStack.pop()
  applySnapshot(prevState)// 应用上一个状态
}

// 重做
function redo() {
  // if (redoStack.length === 0) return
  // undoStack.push(JSON.stringify(components))
  // const next = redoStack.pop()
  // Object.assign(components, JSON.parse(next))
  // TODO
  if (redoStack.length === 0) return

  // 保存当前状态到重做栈
  undoStack.push(JSON.parse(JSON.stringify(components)))// 保存快照到撤销栈
  // 获取下一个状态
  const nextState = redoStack.pop()
  applySnapshot(nextState)// 应用下一个状态
}


function handleMouseDown(event) {
  if (event.button !== 0) return; // 只响应左键

  const rect = canvasContainer.value.getBoundingClientRect();
  const x = event.clientX - rect.left;
  const y = event.clientY - rect.top;

  // 若有当前元件，则放置元件
  if (currentComponent.value) {
    components.push({ 
      ...currentComponent.value, 
      x, 
      y 
    });
    saveHistory();
    currentComponent.value = null;
    return; // 放置元件后不画线
  }

  // 检查是否点击在端点上
  const nearestPort = findNearestPort(x, y);
  if (nearestPort) {
    // 端口点击
    handlePinMouseDown(nearestPort.component, {
      pinType: nearestPort.type,
      pinIndex: nearestPort.index
    });
    return; // 点击端口后不画线
  }

  // 电线画线逻辑
  if (!wireStart.value) {
    wireStart.value = { x, y };
  } else {
    const wireEnd = { x, y };
    const newWire = createWirePath(wireStart.value, wireEnd);
    connections.push(newWire);
    wireStart.value = null;
    saveSnapshot();
  }
}


// 创建电线路径
function createWirePath(start, end) {
  const midX = (start.x + end.x) / 2;
  const d = `M ${start.x} ${start.y} L ${midX} ${start.y} L ${midX} ${end.y} L ${end.x} ${end.y}`;
  return { 
    from: start, 
    to: end, 
    path: d,
    color: "#999",       // 明确指定灰色
    strokeWidth: 2,      // 线宽
    hasArrow: false     // 无箭头
  };
}

function drawConnections(ctx) {
  ctx.save();
  ctx.setLineDash([]);  // 设置为实线

  connections.forEach(wire => {
    // 使用电线对象中的样式属性
    ctx.strokeStyle = wire.color || "#999";  // 优先使用电线对象的颜色
    ctx.lineWidth = wire.strokeWidth || 2;   // 优先使用电线对象的线宽
    
    // 创建路径并绘制
    const path = new Path2D(wire.path);
    ctx.stroke(path);
  });

  ctx.restore();
}


function handlePinMouseDown(component, { pinType, pinIndex }) {
  const pinPos = getPinPosition(component, pinType, pinIndex);
  startPin = { component, pinType, pinIndex, x: pinPos.x, y: pinPos.y };
  
  // 开始绘制临时连线
  tempWire.value = {
    path: `M${pinPos.x},${pinPos.y} L${pinPos.x},${pinPos.y}`
  };
}

// 查找所有组件中距离点击点最近的端口，并返回该端口信息
function findNearestPort(clickX, clickY, maxDistance = 20) {
  const allPorts = getAllPortsFromAllComponents();
  let closest = null;
  let minDist = Infinity;

  allPorts.forEach(port => {
    const dx = clickX - port.x;
    const dy = clickY - port.y;
    const dist = Math.sqrt(dx * dx + dy * dy);
    if (dist < maxDistance && dist < minDist) {
      closest = port;
      minDist = dist;
    }
  });

  return closest;
}

function getAllPortsFromAllComponents() {
  const ports = [];
  components.forEach(component => {
    if (component.getAllPorts) {
      ports.push(...component.getAllPorts(component)); // component 是 Vue 实例 or 结构
    }
  });
  return ports;
}

function handleMouseUp() {
  if (isDragging.value) {// 拖动结束，保存状态
    saveHistory()
  }
  isDragging.value = false
}

function selectComponent(item, event) {
  const rect = canvasContainer.value.getBoundingClientRect()
  const x = event.clientX - rect.left
  const y = event.clientY - rect.top
  selectedComponent.value = item
  dragOffset.x = x - item.x
  dragOffset.y = y - item.y
  isDragging.value = true
}


function handleRightClick(event) {
  event.preventDefault();

  const rect = canvasContainer.value.getBoundingClientRect();
  const x = event.clientX - rect.left;
  const y = event.clientY - rect.top;

  // 先判断是否点击在线上（允许误差）
  const wireIndex = connections.findIndex(wire => {
    const hitThreshold = 6;
    const midX = (wire.start.x + wire.end.x) / 2;
    // 折线路径点
    const points = [
      wire.start,
      { x: midX, y: wire.start.y },
      { x: midX, y: wire.end.y },
      wire.end
    ];
    // 遍历每一段折线
    for (let i = 0; i < points.length - 1; i++) {
      const p1 = points[i];
      const p2 = points[i + 1];
      const dx = p2.x - p1.x;
      const dy = p2.y - p1.y;
      const length = Math.hypot(dx, dy);
      const dot = ((x - p1.x) * dx + (y - p1.y) * dy) / (length * length);
      if (dot >= 0 && dot <= 1) {
        const px = p1.x + dot * dx;
        const py = p1.y + dot * dy;
        const dist = Math.hypot(x - px, y - py);
        if (dist <= hitThreshold) return true;
      }
    }
    return false;
  });

  if (wireIndex !== -1) {
    contextMenu.visible = true;
    contextMenu.x = event.clientX + 5;
    contextMenu.y = event.clientY + 5;
    contextMenu.targetWireIndex = wireIndex;
    contextMenu.targetIndex = null;
    wireStart.value = null;
    return;
  }

  // 然后判断是否点击在元件上
  const compIndex = components.findIndex(
    (c) =>
      x >= c.x &&
      x <= c.x + c.size.width &&
      y >= c.y &&
      y <= c.y + c.size.height
  );

  if (compIndex !== -1) {
    contextMenu.visible = true;
    contextMenu.x = event.clientX + 5;
    contextMenu.y = event.clientY + 5;
    contextMenu.targetIndex = compIndex;
    contextMenu.targetWireIndex = null;
    wireStart.value = null;
    return;
  }

  // 否则关闭菜单
  contextMenu.visible = false;
  contextMenu.targetIndex = null;
  contextMenu.targetWireIndex = null;
  wireStart.value = null;
}

function deleteSelectedItem() {
  if (contextMenu.targetIndex != null) {
    components.splice(contextMenu.targetIndex, 1);
  } else if (contextMenu.targetWireIndex != null) {
    connections.splice(contextMenu.targetWireIndex, 1);
  }
  contextMenu.visible = false;
  saveSnapshot();
}



function toggleInput(component, index) {
  component.inputs[index].value = !component.inputs[index].value
  if (component.type === AndGate) {
    component.output = component.inputs.every(input => input.value)
  } else if (component.type === OrGate) {
    component.output = component.inputs.some(input => input.value)
  } else if (component.type === NotGate) {
    component.output = !component.inputs[0].value
  }
}


</script>

<style scoped>
.editor-wrapper {
  display: flex;
  height: 80vh;
  overflow: hidden; /* 防止整体页面滚动 */
}
.toolbar {
  width: 200px;
  height: 80vh; /* 固定高度为视口高度 */
  padding: 10px;
  background: #f5f5f5;
  border-right: 1px solid #ddd;
  overflow-y: auto;

  display: flex;
  flex-direction: column;
  gap: 10px;
  margin-right: 10px;
}
/* 新增工具栏内容容器 */
.toolbar-content {

  display: flex;
  flex-direction: column;
  gap: 20px;
  padding-bottom: 20px; 
}
.category {
  border: 1px solid #ddd;
  padding: 5px;

  border-radius: 6px;
  margin-bottom: 10px;
  background: white;
  box-shadow: 0 2px 4px rgba(0,0,0,0.05);
}
.category-title {
  font-weight:bold;
  cursor:pointer;
  padding: 5px 0;
  user-select: none;

  transition: background 0.2s;
  background: #f8f9fa;
  border-radius: 4px;
}
.category-title[data-expanded="true"] {
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 0;
}
.category-content {
  display: flex;
  flex-direction: column;
  gap: 5px;

  padding: 8px 0;
  display: grid;
}
.item {
  display:flex;
  align-items:center;
  gap:10px;
  cursor:pointer;
  padding:8px 12px;
  border-radius: 4px;
  transition: all 0.2s;
  white-space: nowrap;
}
.item:hover {
  background: #f8f9fa;
  transform: translateX(3px);
}
.item img {
  width: 24px;
  height: 24px;
  flex-shrink: 0;
  object-fit: contain;
}
.component-canvas {
  position: relative;
  width: 1600px;
  height: 1200px;
  border: 1px solid #ccc;
  background-color: #f0f0f0;
  flex: 1;
  overflow: auto;
}
.preview-shadow {
  position: absolute;
  width: 60px;
  height: 60px;
  background-color: rgba(0, 0, 0, 0.2);
  border: 1px dashed #333;
  pointer-events: none;
}
.selected {
  outline: 2px solid #007bff;
}
.context-menu {
  position: fixed;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
  z-index: 1000;
  min-width: 120px;
  /* 确保菜单在最顶层 */
  z-index: 9999;
  /* 防止被SVG元素覆盖 */
  pointer-events: auto;
}
.menu-item {
  padding: 8px 16px;
  cursor: pointer;
  transition: background 0.2s;
  color: #333;
}
.menu-item:hover {
  background: #f5f5f5;
  color: #007bff;
}
.preview-image {
  position: absolute;
  pointer-events: none;
  opacity: 0.6;
  z-index: 10;
}


/*画布*/
svg {
  width: 100%;
  height: 100%;
  min-width: 1600px;
  min-height: 1200px;
}
</style>
