import type { ICommand, CanvasContext } from '@/modules/useHistory';

export class MoveComponentCommand implements ICommand {
  constructor(
    private id: number,
    private fromPos: [number, number],
    private toPos: [number, number],
  ) {}

  redo(ctx: CanvasContext): void {
    const logic = ctx.store.getComponent(this.id);
    if (!logic) return;
    logic.setPosition(this.toPos);

    const viewComp = ctx.components.find(c => c.ID === this.id);
    if (viewComp) {
      viewComp.x = this.toPos[0];
      viewComp.y = this.toPos[1];
    }

    ctx.updatePorts(this.id, this.toPos[0], this.toPos[1]);
    ctx.updateConnectionPaths(this.id);
  }

  undo(ctx: CanvasContext): void {
    const logic = ctx.store.getComponent(this.id);
    if (!logic) return;
    logic.setPosition(this.fromPos);

    const viewComp = ctx.components.find(c => c.ID === this.id);
    if (viewComp) {
      viewComp.x = this.fromPos[0];
      viewComp.y = this.fromPos[1];
    }

    ctx.updatePorts(this.id, this.fromPos[0], this.fromPos[1]);
    ctx.updateConnectionPaths(this.id);
  }
}
