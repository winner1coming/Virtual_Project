import type { ICommand, CanvasContext } from '@/modules/useHistory';
import { RemoveComponentCommand } from './RemoveComponentCommand';

/**
 * ClearAllCommand reuses RemoveComponentCommand's snapshot logic.
 * On capture: snapshots every component in the project.
 * On redo: removes all components using saved snapshots.
 * On undo: restores all components in reverse order.
 */
export class ClearAllCommand implements ICommand {
  private commands: RemoveComponentCommand[] = [];

  /** Capture all components on the canvas. Called before clearing. */
  captureAll(ctx: CanvasContext): void {
    const ids = [...ctx.store.components.keys()];
    for (const id of ids) {
      const cmd = new RemoveComponentCommand(id);
      cmd.capture(ctx);
      this.commands.push(cmd);
    }
  }

  redo(ctx: CanvasContext): void {
    // Remove from last to first (avoid index shifting issues)
    for (let i = this.commands.length - 1; i >= 0; i--) {
      this.commands[i].redo(ctx);
    }
  }

  undo(ctx: CanvasContext): void {
    // Restore in original order
    for (const cmd of this.commands) {
      cmd.undo(ctx);
    }
  }
}
