import type { ICommand, CanvasContext } from '@/modules/useHistory';

export class DisconnectCommand implements ICommand {
  constructor(
    private fromId: number,
    private fromPin: number,
    private toId: number,
    private toPin: number,
    private segments: any[],
  ) {}

  redo(ctx: CanvasContext): void {
    ctx.store.disconnect(this.fromId, this.fromPin, this.toId, this.toPin);
    for (const seg of this.segments) {
      const i = ctx.connections.findIndex((c: any) => {
        const cFromPin = c.from.pinIndex ?? c.from.portId;
        const cToPin = c.to.pinIndex ?? c.to.portId;
        const sFromPin = seg.from.pinIndex ?? seg.from.portId;
        const sToPin = seg.to.pinIndex ?? seg.to.portId;
        return cFromPin === sFromPin
          && cToPin === sToPin
          && c.from.componentId === seg.from.componentId
          && c.to.componentId === seg.to.componentId;
      });
      if (i !== -1) ctx.connections.splice(i, 1);
    }
  }

  undo(ctx: CanvasContext): void {
    ctx.store.connect(this.fromId, this.fromPin, this.toId, this.toPin);
    for (const seg of this.segments) {
      const exists = ctx.connections.some((c: any) => {
        const cFromPin = c.from.pinIndex ?? c.from.portId;
        const cToPin = c.to.pinIndex ?? c.to.portId;
        const sFromPin = seg.from.pinIndex ?? seg.from.portId;
        const sToPin = seg.to.pinIndex ?? seg.to.portId;
        return cFromPin === sFromPin
          && cToPin === sToPin
          && c.from.componentId === seg.from.componentId
          && c.to.componentId === seg.to.componentId;
      });
      if (!exists) ctx.connections.push(seg);
    }
  }
}
