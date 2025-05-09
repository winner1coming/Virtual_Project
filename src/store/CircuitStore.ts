import { defineStore } from 'pinia'
import { BaseComponent } from '@/logic/BaseComponent.js';
import {AndGate} from '@/logic/components/AndGate';
import {OrGate} from '@/logic/components/OrGate';
import {NotGate} from '@/logic/components/NotGate';
export const useCircuitStore = defineStore('circuit', {
  state: () => ({
    components: new Map<number, BaseComponent>(),
    // wires: new Map<string, Wire>(),
    selectedGateId: null,
    currentId: 0,
  }),
  actions: {
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
        this.components.set(id, new AndGate(id, type, position));
      }else if(type === "Or"){
        this.components.set(id, new OrGate(id, type, position));
      }else if(type === "Not"){
        this.components.set(id, new NotGate(id, type, position));
      }
      return id;
    },
    // addWire(from, to) { },
    simulateCircuit() {},
  }
});
