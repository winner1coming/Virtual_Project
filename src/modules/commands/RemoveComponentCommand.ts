import type { ICommand, CanvasContext } from '@/modules/useHistory';
import { createComponentByType } from '@/modules/useComponentType';
import { reactive } from 'vue';

interface ComponentSnapshot {
  // Store-side data
  type: string;
  position: [number, number];
  name: string;
  bitWidth: number;
  inputCount: number;
  outputCount: number;
  inputInverted: boolean[];
  direction: string;
  offset: [number, number];
  scale: number;
  // View-side
  viewComponent: any;
  vueEntry: any;
  portsEntry: any[];
  // Connections (from store)
  connections: Array<{ fromId: number; fromPin: number; toId: number; toPin: number }>;
  // Wire segments in view layer
  wireSegments: any[];
  // Component index in components array for ordered restore
  componentIndex: number;
}

export class RemoveComponentCommand implements ICommand {
  private snapshot: ComponentSnapshot | null = null;

  constructor(private id: number) {}

  /** Capture all component data before removal. Called by CanvasEditor before actual deletion. */
  capture(ctx: CanvasContext): void {
    const logic = ctx.store.getComponent(this.id);
    if (!logic) return;

    const viewComp = ctx.components.find((c: any) => c.ID === this.id);
    const compIndex = ctx.components.findIndex((c: any) => c.ID === this.id);

    // Collect wire segments related to this component
    const wireSegments = ctx.connections.filter(
      (w: any) => w.from?.componentId === this.id || w.to?.componentId === this.id,
    );

    // Collect logical connections
    const conns: ComponentSnapshot['connections'] = [];
    const pinMap = ctx.store.simulator.connectionManager.getOutputPinMap(this.id);
    if (pinMap) {
      for (const pinIdx of pinMap.keys()) {
        for (const conn of pinMap.get(pinIdx) || []) {
          conns.push({ fromId: this.id, fromPin: pinIdx, toId: conn.id, toPin: conn.idx });
        }
      }
    }
    // Also check input pins for incoming connections
    const inPinMap = ctx.store.simulator.connectionManager.getInputPinMap(this.id);
    if (inPinMap) {
      for (const pinIdx of inPinMap.keys()) {
        for (const conn of inPinMap.get(pinIdx) || []) {
          conns.push({ fromId: conn.id, fromPin: conn.idx, toId: this.id, toPin: pinIdx });
        }
      }
    }

    this.snapshot = {
      type: logic.type,
      position: [...logic.position] as [number, number],
      name: logic.name,
      bitWidth: logic.bitWidth,
      inputCount: logic.inputCount,
      outputCount: logic.outputs.length,
      inputInverted: [...logic.inputInverted],
      direction: logic.direction,
      offset: [...logic.offset] as [number, number],
      scale: logic.scale,
      viewComponent: viewComp,
      vueEntry: ctx.vueComponentMap.get(this.id),
      portsEntry: ctx.Ports.get(this.id) || [],
      connections: conns,
      wireSegments,
      componentIndex: compIndex,
    };
  }

  redo(ctx: CanvasContext): void {
    if (!this.snapshot) return;
    ctx.store.removeComponent(this.id);

    const idx = ctx.components.findIndex((c: any) => c.ID === this.id);
    if (idx !== -1) ctx.components.splice(idx, 1);

    const idIdx = ctx.componentID.indexOf(this.id);
    if (idIdx !== -1) ctx.componentID.splice(idIdx, 1);

    for (const seg of this.snapshot.wireSegments) {
      const i = ctx.connections.indexOf(seg);
      if (i !== -1) ctx.connections.splice(i, 1);
    }

    ctx.Ports.delete(this.id);
    ctx.vueComponentMap.delete(this.id);
  }

  undo(ctx: CanvasContext): void {
    if (!this.snapshot) return;
    const s = this.snapshot;
    const project = ctx.store.getCurrentProject();

    // Restore store-side
    const component = reactive(
      createComponentByType(this.id, s.type, s.position, s.name, -1),
    );
    component.setBitWidth(s.bitWidth);
    component.setName(s.name);
    component.initInputPin(s.inputCount);
    component.initOutputPin(s.outputCount);
    component.inputInverted.splice(0, component.inputInverted.length, ...s.inputInverted);
    component.setDirection(s.direction);
    component.setScale(s.scale);
    component.offset = [...s.offset];

    ctx.store.components.set(this.id, component);

    // Restore project lists
    if (s.type === 'INPUT') project.inputPins.push(this.id);
    else if (s.type === 'OUTPUT') project.outputPins.push(this.id);
    else if (s.type === 'CLOCK') project.clockIds.push(this.id);
    project.componentsId.push(this.id);

    // Restore view-side
    if (s.viewComponent) {
      if (s.componentIndex >= 0 && s.componentIndex <= ctx.components.length) {
        ctx.components.splice(s.componentIndex, 0, s.viewComponent);
      } else {
        ctx.components.push(s.viewComponent);
      }
    }
    ctx.componentID.push(this.id);
    ctx.vueComponentMap.set(this.id, s.vueEntry);
    ctx.Ports.set(this.id, s.portsEntry);

    // Restore connections
    for (const conn of s.connections) {
      ctx.store.connect(conn.fromId, conn.fromPin, conn.toId, conn.toPin);
    }
    // Restore wire segments
    for (const seg of s.wireSegments) {
      if (ctx.connections.indexOf(seg) === -1) {
        ctx.connections.push(seg);
      }
    }
  }
}
