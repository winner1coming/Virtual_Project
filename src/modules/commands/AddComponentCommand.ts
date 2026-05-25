import type { ICommand, CanvasContext } from '@/modules/useHistory';
import { createComponentByType } from '@/modules/useComponentType';
import { reactive } from 'vue';

export class AddComponentCommand implements ICommand {
  constructor(
    private id: number,
    private type: string,
    private position: [number, number],
    private projectId: number,
    private viewComponent: any,
    private vueEntry: any,
  ) {}

  redo(ctx: CanvasContext): void {
    const project = ctx.store.getCurrentProject();
    // Restore to store with original id
    const component = reactive(
      createComponentByType(this.id, this.type, this.position, '', this.projectId),
    );
    ctx.store.components.set(this.id, component);
    project.componentsId.push(this.id);
    if (this.type === 'INPUT') project.inputPins.push(this.id);
    else if (this.type === 'OUTPUT') project.outputPins.push(this.id);
    else if (this.type === 'CLOCK') project.clockIds.push(this.id);

    // Restore view layer
    ctx.components.push(this.viewComponent);
    ctx.componentID.push(this.id);
    ctx.vueComponentMap.set(this.id, this.vueEntry);

    // Rebuild ports
    const logic = ctx.store.getComponent(this.id);
    logic.setPosition(this.position);
    const portsInfo = logic.getAllPorts();
    this.addComponentPorts(ctx, this.id, portsInfo, this.position[0], this.position[1]);
  }

  undo(ctx: CanvasContext): void {
    ctx.store.removeComponent(this.id);

    const idx = ctx.components.findIndex((c: any) => c.ID === this.id);
    if (idx !== -1) ctx.components.splice(idx, 1);

    const cidIdx = ctx.componentID.indexOf(this.id);
    if (cidIdx !== -1) ctx.componentID.splice(cidIdx, 1);

    ctx.Ports.delete(this.id);
    ctx.vueComponentMap.delete(this.id);
  }

  private addComponentPorts(
    ctx: CanvasContext,
    componentId: number,
    portsInfo: any,
    offsetX: number,
    offsetY: number,
  ) {
    const portList = portsInfo.ports.map((port: any, index: number) => {
      const inputCount = portsInfo.ports.length - 1;
      return {
        id: port.id,
        x: port.x + offsetX,
        y: port.y + offsetY,
        componentId,
        type: index < inputCount ? 'input' : 'output',
      };
    });
    ctx.Ports.set(componentId, portList);
  }
}
