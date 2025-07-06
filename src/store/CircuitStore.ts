import { reactive, computed } from 'vue';
import { defineStore } from 'pinia'
import { BaseComponent } from '@/logic/BaseComponent.js';
import { EventDrivenSimulator } from '@/logic/Simulator';

import { useProjectStore } from './ProjectStore';

import {createComponentByType} from '@/modules/useComponentType';
import { SubCircuitComponent } from '@/logic/components/SubCircuitComponent';


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
    projectStore: useProjectStore(), // 获取项目管理器实例
  }),
  actions: {
    // #region 组件相关操作
    // getComponent返回的对象并不是响应式的，返回时需要用compute手动包装
    // 用法：const A = computed(() => store.components.get(1));
    getComponent(id: number): BaseComponent{
      const component = this.components.get(id);
      return component;
    },
    getComponentOutputs(id:number): number[]{
      return this.getComponent(id).getOutputs();
    },

    // 添加一个组件，返回id
    addComponent(type: String, position: [number, number]=[0,0], name: String ="", projectId: number): number {
      const id = this.currentId++;
      // const logic = createGate(type, id);
      this.components.set(id, reactive(createComponentByType(id, type, position, name, projectId)));
      // projectDate修改
      this.projectStore.getCurrentProject().componentsId.push(id);
      if(type === "INPUT"){
        this.projectStore.getCurrentProject().inputPins.push(id);
      }else if(type === "OUTPUT"){
        this.projectStore.getCurrentProject().outputPins.push(id);
      } else if(type === "CLOCK"){
        this.projectStore.getCurrentProject().clockIds.push(id);
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
      // 删除组件时，先删除其所有连接的电线  
      this.simulator.removeComponent(id); // 删除组件的连线
      // 删除元件
      this.components.delete(id);
      // 如果删除的组件是当前选中的组件，则取消选中
      if (this.selectedId === id) {
        this.selectedId = -1;
      }
      // projectDate修改
      const project = this.projectStore.getCurrentProject();
      const index = project.componentsId.indexOf(id);
      if (index !== -1) {
        project.componentsId.splice(index, 1);
      }
      if (project.inputPins.includes(id)) {
        const inputIndex = project.inputPins.indexOf(id);
        if (inputIndex !== -1) {
          project.inputPins.splice(inputIndex, 1);
        }
      }else if (project.outputPins.includes(id)) {
        const outputIndex = project.outputPins.indexOf(id);
        if (outputIndex !== -1) {
          project.outputPins.splice(outputIndex, 1);
        }
      }else if (project.clockIds.includes(id)) {
        const clockIndex = project.clockIds.indexOf(id);
        if (clockIndex !== -1) {
          project.clockIds.splice(clockIndex, 1);
        }
      }
    },

    // 移动一个组件
    moveComponent(id: number, newPosition: [number, number]) {
      const component = this.components.get(id);
      if (!component) {
        throw new Error(`Component with id ${id} not found`);
      }
      // console.log("移动组件：ID:", id, "新位置:", newPosition);
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
  
    // #region 项目管理
    // 获取当前项目
    getCurrentProject() {
      return this.projectStore.getCurrentProject();
    },
    getCurrentProjectId(): number {
      return this.projectStore.selectedProjectId;
    },
    changeProject(projectId: number) {
      this.simulator.changeProject(projectId);
    }
    // // 创建新项目
    // createProject(name: string) {
    //   return this.projectManager.createProject(name);
    // },
    // // 选中项目
    // selectProject(projectId: number) {
    //   const project = this.projectManager.getAllProjects().find(p => p.projectId === projectId);
    //   if (project) {
    //     this.projectManager.loadProject(project);
    //   } else {
    //     throw new Error(`Project with id ${projectId} not found`);
    //   }
    // },
  }
});