import type { useCircuitStore } from '@/store/CircuitStore';

// #region 类型定义

export interface ICommand {
  /** 执行正向操作（用于 redo-after-undo） */
  redo(ctx: CanvasContext): void;
  /** 逆向操作（用于 undo） */
  undo(ctx: CanvasContext): void;
}

export interface CanvasContext {
  store: ReturnType<typeof useCircuitStore>;
  components: any[];
  connections: any[];
  Ports: Map<any, any>;
  vueComponentMap: Map<any, any>;
  componentID: number[];
  intermediatePoints: { value: any[] };
  componentMap: Record<string, any>;
  componentSizes: Record<string, { width: number; height: number }>;
  /** 更新指定元件的端口位置并重绘关联连线 */
  updatePorts: (id: number, x: number, y: number) => void;
  updateConnectionPaths: (id: number) => void;
}

// #endregion

// #region HistoryManager

const MAX_HISTORY = 50;

export class HistoryManager {
  undoStack: ICommand[] = [];
  redoStack: ICommand[] = [];
  private ctx: CanvasContext | null = null;
  private listeners: Array<() => void> = [];

  /** Subscribe to stack mutations (for UI reactivity). Returns unsubscribe fn. */
  onChange(cb: () => void): () => void {
    this.listeners.push(cb);
    return () => {
      const idx = this.listeners.indexOf(cb);
      if (idx !== -1) this.listeners.splice(idx, 1);
    };
  }

  private notify(): void {
    for (const cb of this.listeners) cb();
  }

  registerContext(ctx: CanvasContext): void {
    this.ctx = ctx;
  }

  /**
   * 记录一个命令到历史栈。
   * 注意：不调用 cmd.redo() —— 正向操作已在调用 execute() 之前由调用者完成。
   * cmd.redo() 仅在 redo-after-undo 时由 redo() 方法调用。
   */
  execute(cmd: ICommand): void {
    this.undoStack.push(cmd);
    this.redoStack = [];
    if (this.undoStack.length > MAX_HISTORY) {
      this.undoStack.shift();
    }
    this.notify();
  }

  undo(): void {
    const cmd = this.undoStack.pop();
    if (!cmd) return;
    cmd.undo(this.ctx!);
    this.redoStack.push(cmd);
    this.notify();
  }

  redo(): void {
    const cmd = this.redoStack.pop();
    if (!cmd) return;
    cmd.redo(this.ctx!);
    this.undoStack.push(cmd);
    this.notify();
  }

  clear(): void {
    this.undoStack = [];
    this.redoStack = [];
    this.notify();
  }

  /** 供 RemoveComponentCommand.capture() 等场景获取上下文 */
  getContext(): CanvasContext {
    return this.ctx!;
  }
}

let instance: HistoryManager | null = null;

export function getHistoryManager(): HistoryManager {
  if (!instance) instance = new HistoryManager();
  return instance;
}

// #endregion
