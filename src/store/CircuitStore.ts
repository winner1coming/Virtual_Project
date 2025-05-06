import { defineStore } from 'pinia'
import { BaseComponent } from '@/logic/BaseComponent.js';
export const useCircuitStore = defineStore('circuit', {
  state: () => ({
    components: new Map<string, BaseComponent>(),
    // wires: new Map<string, Wire>(),
    selectedGateId: null,
    currentId: 0,
  }),
  actions: {
    addComponent(type: String, position: [number, number]) {
      const id = this.currentId++;
      // const logic = createGate(type, id);
      this.gates.set(id, {
        id, type, position, 
      });
      return id;
    },
    addWire(from, to) { },
    simulateCircuit() {},
  }
});
