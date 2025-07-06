import type { BaseComponent } from './BaseComponent';
import type { Conn } from './ConnectionManager';

export interface ProjectData {
  projectId: number;
  projectUUID: string; // 项目唯一标识符
  name: string;
  componentsId: number[]; // 元件的id数组
  inputPins: number[];    // 输入引脚的id的数组
  outputPins: number[];   // 输出引脚的id的数组
  clockIds: number[]; // 时钟元件的id数组
}
