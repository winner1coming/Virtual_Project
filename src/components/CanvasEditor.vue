<template>
  <div class="editor-wrapper">
    <!-- 左侧工具栏 删除-->
    <div class="toolbar" v-if="false">
      <!-- 逻辑门 -->
      <div class="category">
        <!-- <div class="category-title" @click="toggleCategory('logic')">逻辑门</div> -->
        <!-- <div v-show="expanded.logic" class="category-content"> -->
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
        <!-- </div> -->
      </div>

      <!-- 输入输出分类 -->

      <!-- 其他 -->

      <!-- 操作按钮（固定在工具栏底部） -->
      <!-- <div class="category">
        <div class="category-title">操作</div>
        <div class="category-content">
          <button @click="clearAll">清空</button>
          <button @click="undo">回滚</button>
          <button @click="redo">前进</button>
        </div>
      </div> -->
    </div>
    

    <!-- 画布区域 -->
    <div
      class="component-canvas"
      ref="canvasContainer"
      @mousedown="handleMouseDown"    
      @click="handleLeftClick"        
      @mousemove="handleMouseMove"
      @mouseup="handleMouseUp"
      @contextmenu.prevent="handleRightClick"
    >
      <!-- 渲染 Vue 元件组件 -->
      <svg width="800" height="600" style="overflow: visible;">
        <!-- 渲染所有连线 -->
        <g v-for="(connection, index) in connections" :key="'conn-'+index">
          <path
            :d="connection.path"
            stroke="black"
            stroke-width="3"
            fill="none"
          />
        </g>

        <!-- 渲染临时连线，实现电线实时渲染-->
        <path
          v-if="tempWire"
          :d="tempWire.path"
          :stroke="tempWire.color"
          :stroke-width="tempWire.strokeWidth"
          fill="none"
        />

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
            :id="item.ID"
            :onToggleInput="(i) => toggleInput(item, i)"
            @pin-mousedown="(data) => handlePinMouseDown(item, data)"
            @pin-mouseup="(data) => handlePinMouseUp(item, data)"
          />
        </g>

        <!-- 临时连线 -->
        <path
          v-if="tempWire"
          :d="tempWire.path"
          stroke="tempWire.color"
          stroke-width="3"
          fill="none"
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

    <!-- 右侧编辑栏 移除 todo-->
    <div
      v-if="false"
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
import { ref, reactive, onMounted, onUnmounted } from 'vue'
// 元件建模
import AndGate from './Gates/AndGate.vue'
import OrGate from './Gates/OrGate.vue'
import NotGate from './Gates/NotGate.vue'
import Tunnel from './Wiring/Tunnel.vue'
import InputPin from './Wiring/InputPin.vue'
import NandGate from './Gates/NandGate.vue'


// 逻辑类建模
import { AndGate as LogicAndGate } from '@/logic/components/AndGate.js'
import { NandGate as LogicNandGate } from '@/logic/components/NandGate'
import { OrGate as LogicOrGate } from '@/logic/components/OrGate.js'
import { NorGate as LogicNorGate } from '@/logic/components/NorGate.js'
import { NotGate as LogicNotGate } from '@/logic/components/NotGate.js'
import {Clock as LogicClock} from '@/logic/components/Clock.js'
import {Combiner as LogicCombiner} from '@/logic/components/Combiner.js'
import {ConstantInput as LogicConstantInput} from '@/logic/components/ConstantInput.js'
import {Ground as LogicGround} from '@/logic/components/Ground.js'
import {Power as LogicPower} from '@/logic/components/Power.js'
import { Tunnel as LogicTunnel} from '@/logic/components/Tunnel'
import { InputPin as LogicInputPin} from '@/logic/components/InputPin'
import { BaseComponent } from '@/logic/BaseComponent'

// 其他
import { useHistory } from '@/modules/useHistory';
import eventBus from '@/modules/useEventBus';
import { useCircuitStore } from '@/store/CircuitStore';
import { nextTick } from 'vue'
import { convertCompilerOptionsFromJson } from 'typescript'


const canvasContainer = ref(null)
const components = reactive([])
const currentComponent = ref(null)
const selectedComponent = ref(null)
const isDragging = ref(false)
const dragOffset = reactive({ x: 0, y: 0 })// 拖动偏移量
const previewPos = reactive({ x: 0, y: 0 })// 预览的位置
const connections = reactive([])// 全局连接列表
const componentID = reactive([])// 全局组件ID
const vueComponentMap = new Map()// 存储 Vue 组件实例
const ports = [];// 存储单个元件的端口信息
// 全局端口管理对象：记录每个元件对应的端口数组
// 纯 JavaScript 版本
/** @type {Map<number, Array<{id: number, x:number, y:number, componentId:number, type: string}>>} */
const Ports = new Map();


// 定义电线的两端
const wireStart = ref(null) // 记录起始点
const tempWire = ref(null) // 临时连线
const wireEndPort = ref(null) // 记录终点端口信息
let wireStartId = null // 记录起始点port的元件ID
const { saveHistory, saveSnapshot } = useHistory(components) // 使用自定义的历史记录管理

// 当前拖动起点信息
let startPin = null
const contextMenu = reactive({
  visible: false,
  x: 0,
  y: 0,
  targetIndex: null
})



// 添加元件的引脚信息
function addComponentPorts(componentId, portsInfo, index_x, index_y) {
  const portList = portsInfo.ports.map((port, index) => {
    const inputCount = portsInfo.ports.length - 1; // 假设最后一个是输出
    return {
      id: port.id,               // 原始端口编号
      x: port.x + index_x,       // 端口X坐标=相对坐标x+元件坐标，这里的280是因为原点移动了（-280，-280）
      y: port.y + index_y,       // 端口Y坐标=相对坐标y+元件坐标
      componentId,
      type: index < inputCount ? 'input' : 'output'
    };
  });
  Ports.set(componentId, portList);
}

// 根据元件ID获取引脚信息
function getComponentPorts(componentId) {
  return Ports.get(componentId) || [];
}

// 元件被移除时，也要移除其端口信息
function removeComponentPorts(componentId) {
  Ports.delete(componentId);
}

// 更新元件端口信息
function updateComponentPorts(componentId, portsInfo, index_x, index_y) {
  addComponentPorts(componentId, portsInfo, index_x, index_y)
}

// 组件映射表
const componentMap = {
  AND: AndGate,
  OR: OrGate,
  NOT: NotGate,
  TUNNEL: Tunnel,
  INPUT: InputPin,
  NAND: NandGate,
}

// 各组件的方法映射
const COMPONENT_LOGIC = {
  AND: LogicAndGate, 
  OR: LogicOrGate,
  NOT: LogicNotGate,
  TUNNEL: LogicTunnel,
  INPUT: LogicInputPin,
  NAND: LogicNandGate
}

// 初始化各元件尺寸配置
const COMPONENT_SIZES = {
  AND: { width: LogicAndGate.width, height: LogicAndGate.height },
  OR: { width: 150, height: 150 },
  NOT: { width: 60, height: 60 },
  TUNNEL: { width: 50, height: 50 },
  INPUT: { width: 30, height: 30 },
  NAND: { width: 30, height: 30 },
}
 
// 按钮图片资源映射表
const IMAGE_MAP = {
  AND: new Image(),
  OR: new Image(),
  NOT: new Image(),
  TUNNEL: new Image(),
  INPUT: new Image(),
  NAND: new Image(),
}

// 初始化图片资源
IMAGE_MAP.AND.src = '/assets/AND.png'
IMAGE_MAP.OR.src = '/assets/OR.png'
IMAGE_MAP.NOT.src = '/assets/NOT.png'
IMAGE_MAP.TUNNEL.src = '/assets/TUNNEL.png'
IMAGE_MAP.INPUT.src = '/assets/INPUT.png'
IMAGE_MAP.NAND.src = '/assets/INPUT.png'

function updateComponentDirection() {
  // 更新完方向后重新绘制画布
  drawCanvas();
  saveHistory();
  drawConnections(ctx);// 绘制所有连线
}

// 这里只是点击了组件的按钮，开始渲染，但是还没下放
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
      //direction: 'east' // 默认方向
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
  endPin.component.inputs[endPin.pinIndex].value = startPin.component.output;
  
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
    // 单纯选中，但是不拖动
    previewPos.x = x;
    previewPos.y = y;
  } else if (isDragging.value && selectedComponent.value) {
    // 选中且拖动：虽然我知道拖动这里用偏移量来做更合理，但是出现了意想不到的bug，因此照搬新建逻辑
    // selectedComponent：元件初始位置
    // dragOffset：拖动偏移量
    // x, y：元件下放位置
    selectedComponent.value.x = x - dragOffset.x;
    selectedComponent.value.y = y - dragOffset.y;

    let ID = selectedComponent.value.ID// 获取元件ID

    // 获取组件类型对应的逻辑类
    let componentLogic
    switch (selectedComponent.value.componentType) {
      case 'AND':
        componentLogic = new LogicAndGate(ID, 'AND', [selectedComponent.value.x, selectedComponent.value.y])
        break
      case 'OR':
        componentLogic = new LogicOrGate(ID, 'OR', [selectedComponent.value.x, selectedComponent.value.y])
        break
      case 'NOT':
        componentLogic = new LogicNotGate(ID, 'NOT', [selectedComponent.value.x, selectedComponent.value.y])
        break
      case 'TUNNEL':
        componentLogic = new LogicTunnel(ID, 'TUNNEL', [selectedComponent.value.x, selectedComponent.value.y])
        break
      case 'INPUT':
        componentLogic = new LogicInputPin(ID, 'INPUT', [selectedComponent.value.x, selectedComponent.value.y])
        break
      case 'NAND':
        componentLogic = new LogicInputPin(ID, 'NAND', [selectedComponent.value.x, selectedComponent.value.y])
        break
      default:
        console.error("未知元件类型：", selectedComponent.value.componentType)
    }

    // 触发引脚坐标更新（非常重要）
    componentLogic.setPosition([selectedComponent.value.x, selectedComponent.value.y]);
    

    // 创建Vue组件实例
    const componentInstance = {
      component: componentMap[selectedComponent.value.componentType],
      props: {ID},
      logic: componentLogic,
    }

    // 存储Vue实例引用
    vueComponentMap.set(ID, componentInstance);

    // 4：记录当前ID的端口信息
    // 延迟4后获取端口信息，确保见组件挂载完成
    nextTick(() => {
      const logic = vueComponentMap.get(ID)?.logic;
      if(!logic) {
        console.warn("逻辑类未找到，ID：", ID)
        return
      }
      const portsInfo = logic.getAllPorts()
      console.log("端口信息1：", portsInfo)
      console.log("selectedComponent.value.x:", selectedComponent.value.x, "selectedComponent.value.y:", selectedComponent.value.y)
      updateComponentPorts(ID, portsInfo, selectedComponent.value.x, selectedComponent.value.y);
      console.log("所有端口：", Ports)
    })

    useCircuitStore().moveComponent(ID, [x, y])// 调用函数移动元件

    // 更新所有相关连线的路径
    updateConnectionPaths(ID);
  } else if (startPin && tempWire.value) {
    // 更新临时连线路径
    // tempWire.value.path = `M${startPin.x},${startPin.y} L${x},${y}`;
    // 更新临时连线路径（鼠标拖动中）
    tempWire.value.to.x = x;
    tempWire.value.to.y = y;
    
    const start = startPin;
    const end = { x, y };
    const midX = (start.x + end.x) / 2;

    tempWire.value.path = `M ${start.x} ${start.y} 
                          L ${midX} ${start.y} 
                          L ${midX} ${end.y} 
                          L ${end.x} ${end.y}`;
  }
}

// 更新所有跟该元件相关的连线路径
function updateConnectionPaths(componentId = null) {
  connections.forEach((connection) => {
    const fromId = connection.from.componentId;
    const toId = connection.to.componentId;

    if (componentId === null || fromId === componentId || toId === componentId) {
      // 获取最新端口位置
      const updatedFromPort = findPortById(fromId, connection.from.portId);
      const updatedToPort = findPortById(toId, connection.to.portId);

      if (updatedFromPort && updatedToPort) {
        connection.from.x = updatedFromPort.x;
        connection.from.y = updatedFromPort.y;
        connection.to.x = updatedToPort.x;
        connection.to.y = updatedToPort.y;

        const midX = (connection.from.x + connection.to.x) / 2;
        connection.path = `M ${connection.from.x} ${connection.from.y} 
                           L ${midX} ${connection.from.y} 
                           L ${midX} ${connection.to.y} 
                           L ${connection.to.x} ${connection.to.y}`;
      }
    }
  });
}

// 根据元件ID和引脚ID获取引脚位置信息
function findPortById(componentId, portId) {
  const ports = Ports.get(componentId);
  if (!ports) return null;
  return ports.find(port => port.id === portId);
}

// 修改 deleteComponent 以删除相关连线
function deleteComponent() {
  if (contextMenu.targetIndex !== null) {
    const component = components[contextMenu.targetIndex];// 获取删除的元件本体
    
    // 删除所有与该元件相关的连线
    const componentId = contextMenu.targetIndex;// 元件ID
    useCircuitStore.removeComponent(componentId)// 调用函数删除组件
    // 删除画布上各组件的连接关系
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

// 鼠标点击（左键右键中轴）：触发电线连接
function handleMouseDown(event) {
  event.stopPropagation(); // 阻止事件冒泡
  event.preventDefault();  // 防止默认行为

  console.log("画布鼠标按下事件触发")
  // if (event.button !== 0) return; // 只响应左键

  const rect = canvasContainer.value.getBoundingClientRect();
  const x = event.clientX - rect.left;
  const y = event.clientY - rect.top;

  console.log("点击点x:", x, "点击点Y:", y)

  handleWireConnection(x, y)// 处理连线逻辑

  console.log("处理完连线逻辑了！")

  saveSnapshot();
}

// 连线逻辑
function handleWireConnection(x, y) {
  console.log("开始创建电路了！")
  console.log("电线起点：",wireStart.value)
  // 电线画线逻辑：分起始点和终点两种情况
  if (!wireStart.value) {
    console.log("没有起点")
    // 起点为空，记录起点，先不画线
    // 找到距离点击点最近的port信息
    let closestPort = findNearestPort(x, y, 50);
    // console.log("找到了最近的port！")
    console.log("调用findNearestPort，返回结果：", closestPort)

    if (closestPort.componentId != null && closestPort.id != null) {
      // 这里是第一次点击：查找最近的端口作为电线终点
      // 记录端口的位置和编号
      wireStart.value = {
        x: closestPort.x,
        y: closestPort.y,
        componentId: closestPort.componentId,// 端口的元件ID
        portId: closestPort.id,             // 端口的ID
        portType:closestPort.type,          // 端口的类型 
      };
      startPin = wireStart.value; // 记录起点信息
      // 创建临时连线
      tempWire.value = {
        from: wireStart.value,
        to: { x: 0, y: 0 }, // 临时终点坐标
        path: '', 
        color: '#999',  // 灰色
        strokeWidth: 3,// 线宽
        isTemp: true, // 标记为临时连线
      };

      wireStartId = closestPort.componentId;// 记录起点port对应的元件ID
      console.log("距离最近的元件ID:", wireStartId)
      return; // 点击端口后不画线
    }
  }

  console.log("电线起点：", wireStart.value, "现在开始找终点！")

  // 找到距离点击点最近的port信息
  let endPort = findNearestPort(x, y, 50);
  if (!endPort) {
    console.log("未找到终点端口");
    // 不需要重置
    // wireStart.value = null; // 重置起点
    return;
  }

  // 创建新连线
  console.log("设置终点：", endPort)
  if (endPort) {
    // 这里是第二次点击：查找最近的端口作为电线终点
    wireEndPort.value = {
      x: endPort.x,
      y: endPort.y,
      componentId: endPort.componentId,// 端口的元件ID
      portId: endPort.id,             // 端口的ID
      portType:endPort.type,          // 端口的类型 
    };
    // 根据起始点画线
    const newWire = createWirePath(wireStart.value, wireEndPort.value);
    connections.push(newWire);
    // 填写逻辑类，调用函数连接两个元件
    console.log("电线起点元件ID:", wireStartId, "起点引脚ID:", wireStart.value.portId)
    console.log("电线终点元件ID:", wireEndPort.value.componentId, "终点引脚ID:", wireEndPort.value.portId)
    useCircuitStore().connect(wireStartId, wireStart.value.portId, wireEndPort.value.componentId, wireEndPort.value.portId);
    console.log("创建新连线成功！")
    wireStart.value = null;// 重置起点
    wireStartId = null;
    tempWire.value = null; // 清除临时连线
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
    strokeWidth: 12,      // 线宽
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

// 查找所有组件中距离点击点最近的端口，并返回该端口位置和对应的元件ID
function findNearestPort(clickX, clickY, maxDistance = 500) {
  // 存储最近端口信息
  let closestPort = null;
  let minDistance = 10000000;

  console.log("开始查找最近引脚信息，全局Ports：", Ports)
  console.log("点击位置：x:", clickX, "y:", clickY)
  
  // 遍历所有元件的所有端口
  Ports.forEach((ports, componentId) => {
    ports.forEach(port => {
      // 计算点击点到端口的欧几里得距离
      const dx = clickX - port.x;
      const dy = clickY - port.y;
      const distance = Math.sqrt(dx * dx + dy * dy);
      console.log("元件ID:", componentId, "引脚ID:", port.id,"距离：", distance)
      
      // 检查是否在有效范围内且比当前最近端口更近
      if (distance < minDistance && distance < maxDistance) {
        minDistance = distance;
        
        // 构建完整的端口信息对象
        closestPort = {
          // 端口基本信息
          id: port.id,                 // 端口ID（在元件内的唯一标识）
          x: port.x,                   // 端口X坐标
          y: port.y,                   // 端口Y坐标
          type: port.type,             // 端口类型（input/output）
          
          // 元件关联信息
          componentId: componentId,    // 所属元件ID
        };
        console.log("更新最近端口信息：x：", closestPort.x, "y:", closestPort.y, "ID:", closestPort.id, "type:", 
        closestPort.type, "componentId:", closestPort.componentId)
      }
    });
  });
  console.log("最近端口信息：", closestPort)
  
  return closestPort;
}


function handleMouseUp() {
  if (isDragging.value) {// 拖动结束，保存状态
    saveHistory()
  }
  isDragging.value = false
}

// 选中元件selectedComponent
function selectComponent(item, event) {
  console.log("选中元件：", item)
  const rect = canvasContainer.value.getBoundingClientRect()
  const x = event.clientX - rect.left
  const y = event.clientY - rect.top
  selectedComponent.value = item
  dragOffset.x = x - item.x
  dragOffset.y = y - item.y
  isDragging.value = true
  
  useCircuitStore().selectComponent(item.ID);
}

// 鼠标左键且currentComponent不为null（处于元件下方状态）：触发元件放置
function handleLeftClick(event) {

  if (!currentComponent.value) {
    console.log("当前没有元件，无法放置")
    return; // 没有元件时不处理
  }

  // 鼠标左键且处于元件放置状态，清空电线起点
  if (wireStartId != null) {
    wireStart.value = null; // 如果有起点，重置起点
    wireStartId = null; // 重置起点ID
  }

  event.preventDefault();

  const rect = canvasContainer.value.getBoundingClientRect();
  const x = event.clientX - rect.left;
  const y = event.clientY - rect.top;

  // 若有当前元件，则放置元件
  if (currentComponent.value) {
    console.log("放置元件，鼠标左键：x:", x, "y:", y)
    // TODO：(已完成)
    // 1：单独记录这个元件的ID
    // 调用useCircuitStore()获取元件的ID
    const id = useCircuitStore().addComponent(currentComponent.value.componentType, [x, y]);
    console.log("元件ID：", id)
    // 2：创建元件配置对象，将元件的相关信息存放到components列表里
    components.push({ 
      ...currentComponent.value, 
      x, 
      y,
      ID: id// 记录元件ID
    });
    // 3：将元件ID存起来，方便后面查找各元件的引脚信息
    componentID.push(id)
    // 获取组件类型对应的逻辑类
    let componentLogic
    switch (currentComponent.value.componentType) {
      case 'AND':
        componentLogic = new LogicAndGate(id, 'AND', [x, y])
        break
      case 'OR':
        componentLogic = new LogicOrGate(id, 'OR', [x, y])
        break
      case 'NOT':
        componentLogic = new LogicNotGate(id, 'NOT', [x, y])
        break
      case 'TUNNEL':
        componentLogic = new LogicTunnel(id, 'TUNNEL', [x, y])
        break
      case 'INPUT':
        componentLogic = new LogicInputPin(id, 'INPUT', [x, y])
        break
      default:
        console.error("未知元件类型：", currentComponent.value.componentType)
    }
    // 触发引脚坐标更新（非常重要）
    componentLogic.setPosition([x, y]);

    // 创建Vue组件实例
    const componentInstance = {
      component: componentMap[currentComponent.value.componentType],
      props: {id},
      logic: componentLogic,
    }

    // 存储Vue实例引用
    vueComponentMap.set(id, componentInstance);

    // 4：记录当前ID的端口信息
    // 延迟4后获取端口信息，确保见组件挂载完成
    nextTick(() => {
      const logic = vueComponentMap.get(id)?.logic;
      if(!logic) {
        console.warn("逻辑类未找到，ID：", id)
        return
      }
      const portsInfo = logic.getAllPorts()
      console.log("端口信息1：", portsInfo)
      addComponentPorts(id, portsInfo, x, y);
      console.log("所有端口：", Ports)
    })

    saveHistory();
    currentComponent.value = null;
    console.log("放置元件后，当前组件已清空:", currentComponent.value)
    return; // 放置元件后不画线
  }
}


function handleRightClick(event) {
  event.preventDefault();

  console.log("电线起点元件ID:", wireStartId)
  // 鼠标右键时清空起点记录
  if (wireStartId != null) {
    wireStart.value = null; // 如果有起点，重置起点
    wireStartId = null; // 重置起点ID
  }

  // if (currentComponent.value) {
  //   // 如果当前有元件在放置状态，清空当前元件
  //   currentComponent.value = null;
  //   console.log("右键点击，清空当前元件")
  // }

  // if (selectedComponent.value) {
  //   // 如果有选中元件，清空选中状态
  //   selectedComponent.value = null;
  //   console.log("右键点击，清空选中元件")
  // }

  const rect = canvasContainer.value.getBoundingClientRect();
  const x = event.clientX - rect.left;
  const y = event.clientY - rect.top;

  console.log("鼠标右键：x:", x, "y:", y)

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

onMounted(() => {
  eventBus.on('start-place-component', (type) => {
    startPlacingVueComponent(type);
  });  
  eventBus.on('updateComponentDirection', () => {
    updateComponentDirection();
  });
  // 确保画布元素可聚焦
  canvasContainer.value.focus();
});

onUnmounted(() => {
  eventBus.off('start-place-component');
  eventBus.off('updateComponentDirection');
});

</script>

<style scoped>
.editor-wrapper {
  display: flex;
  height: 100%;
  width: 100%;
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
/* .category-title[data-expanded="true"] {
  border-bottom-left-radius: 0;
  border-bottom-right-radius: 0;
} */
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
  width: 100%;
  height: 100%;
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
  width: 80%;
  height: 90%;
  min-width: 80%;
  min-height: 80px;
}
</style>