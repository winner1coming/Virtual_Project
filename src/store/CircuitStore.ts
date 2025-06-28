import { reactive, computed } from 'vue';
import { defineStore } from 'pinia'
import { BaseComponent } from '@/logic/BaseComponent.js';
import {AndGate} from '@/logic/components/AndGate';
import {OrGate} from '@/logic/components/OrGate';
import {NotGate} from '@/logic/components/NotGate';
import {NandGate} from '@/logic/components/NandGate';
import {NorGate} from '@/logic/components/NorGate';
import {XorGate} from '@/logic/components/XorGate';
import {Clock} from '@/logic/components/Clock';
import { EventDrivenSimulator } from '@/logic/Simulator';
import { InputPin } from '@/logic/components/InputPin';
import { OutputPin } from '@/logic/components/OutputPin';


export const useCircuitStore = defineStore('circuit', {
  state: () => ({
    components: new Map<number, BaseComponent>(),
    // wires: new Map<string, Wire>(),
    selectedId: -1,   // 选中的组件ID，-1表示没有选中任何组件
    currentId: 0,

    undoStack:[] as any[],
    redoStack:[] as any[],

    //selectedComponent: null as BaseComponent | null,
    simulator: EventDrivenSimulator.getInstance(),
  }),
  actions: {
    // #region 组件相关操作
    // getComponent返回的对象并不是响应式的，返回时需要用compute手动包装
    // 用法：const A = computed(() => store.components.get(1));
    getComponent(id: number): BaseComponent{
      const component = this.components.get(id);
      if(!component){
        throw new Error(`Component with id ${id} not found`);
      } 
      return component;
    },
    getComponentOutputs(id:number): number[]{
      return this.getComponent(id).getOutputs();
    },

    // 添加一个组件，返回id
    addComponent(type: String, position: [number, number]): number {
      const id = this.currentId++;
      // const logic = createGate(type, id);
      if(type === "And"){
        this.components.set(id, reactive(new AndGate(id, type, position)));
      }else if(type === "Or"){
        this.components.set(id, reactive(new OrGate(id, type, position)));
      }else if(type === "Not"){
        this.components.set(id, reactive(new NotGate(id, type, position)));
      }else if(type === "Nand"){
        this.components.set(id, reactive(new NandGate(id, type, position)));
      }else if(type === "Nor"){
        this.components.set(id, reactive(new NorGate(id, type, position)));
      }else if(type === "Xor"){
        this.components.set(id, reactive(new XorGate(id, type, position)));
      }else if(type === "Clock"){
        this.components.set(id, reactive(new Clock(id, type, position)));
      }else if(type === "Input"){
        this.components.set(id, reactive(new InputPin(id, type, position))); 
      }else if(type === "Output"){
        this.components.set(id, reactive(new OutputPin(id, type, position))); 
      }
      else{
        throw new Error(`Unknown component type: ${type}`);
      }
      return id;
    },
    // 移除一个组件
    // 注意：如果组件有连接的电线，需要先删除电线
    removeComponent(id: number) {
      const component = this.components.get(id);
      if (!component) {
        throw new Error(`Component with id ${id} not found`);
      }
      // 删除组件时，先删除其所有连接的电线  todo
      // this.wires.forEach((wire, key) => {
      //   if (wire.from.id === id || wire.to.id === id) {
      //     this.wires.delete(key);
      //   }
      // });
      this.components.delete(id);
      // 如果删除的组件是当前选中的组件，则取消选中
      if (this.selectedId === id) {
        this.selectedId = -1;
      }
    },

    // 移动一个组件
    moveComponent(id: number, newPosition: [number, number]) {
      const component = this.components.get(id);
      if (!component) {
        throw new Error(`Component with id ${id} not found`);
      }
      component.setPosition(newPosition);
      // 更新组件位置后，可能需要更新电线的位置 todo
    },

    // 选择组件
    selectComponent(id: number) {
      if (this.components.has(id)) {
        this.selectedId = id;
      } else {
        throw new Error(`Component with id ${id} not found`);
      }
    },
    unselectComponent() {
      this.selectedId = -1;
    },
    // #endregion 组件相关操作

    // #region 连线相关操作
    connect(id1: number, idx1:number, id2: number, idx2:number) {
      this.simulator.connect(id1, idx1, id2, idx2);
    },
    disconnect(id1: number, idx1:number, id2: number, idx2:number) {
      this.simulator.disconnect(id1, idx1, id2, idx2);
    },
    // #endregion 连线相关操作

    // addWire(from, to) { },

    // #region 模拟器逻辑
    // 启用模拟器
    enableSimulator() {
      this.simulator.enable();
      this.simulator.resumeSimulator();
    },
    disableSimulator() {
      this.simulator.disable();
    },
    pauseSimulator(){
      this.simulator.pauseSimulator();
    },
    resumeSimulator(){
      this.simulator.resumeSimulator();
    },
    // 一次性刷新所有组件输出  todo  应该可以不用这个接口了
    simulateCircuit() {},
    // #endregion 模拟器逻辑
  }
});
