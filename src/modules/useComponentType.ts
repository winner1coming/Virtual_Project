import { BaseComponent } from '@/logic/BaseComponent';
import {AndGate} from '@/logic/components/AndGate';
import {OrGate} from '@/logic/components/OrGate';
import {NotGate} from '@/logic/components/NotGate';
import {NandGate} from '@/logic/components/NandGate';
import {NorGate} from '@/logic/components/NorGate';
import {XorGate} from '@/logic/components/XorGate';
import {Clock} from '@/logic/components/Clock';
import { InputPin } from '@/logic/components/InputPin';
import { OutputPin } from '@/logic/components/OutputPin';
import {Tunnel} from '@/logic/components/Tunnel';
import { ConstantInput } from '@/logic/components/ConstantInput';
import { SubCircuitComponent } from '@/logic/components/SubCircuitComponent';

export function createComponentByType(id: number, type: String, position:[number, number] = [0,0], name:String=""): BaseComponent {
  switch (type) {
    case 'AND':
      return new AndGate(id, type, position);
    case 'OR':
      return new OrGate(id, type, position);
    case 'NOT':
      return new NotGate(id, type, position);
    case 'NAND':
      return new NandGate(id, type, position);
    case 'NOR':
      return new NorGate(id, type, position);
    case 'XOR':
      return new XorGate(id, type, position);
    case 'CLOCK':
      return new Clock(id, type, position);
    case 'TUNNEL':
      return new Tunnel(id, type, position, name); // 名字
    case 'CONSTANT':
      return new ConstantInput(id, type, position);
    case 'INPUT':
      return new InputPin(id, type, position);
    case 'OUTPUT':
      return new OutputPin(id, type, position);
    // case 'SUB_CIRCUIT':
    //   return new SubCircuitComponent(id, type, position, 0);
    // 添加其他组件类型
    default:
      throw new Error(`Unknown component type: ${type}`);
  }
}