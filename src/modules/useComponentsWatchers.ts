// 监听元件变化
import { watch } from 'vue';
import type { BaseComponent } from '@/logic/BaseComponent';

export function watchComponentChanges(
    comp: BaseComponent,
    inputCountCb: (count: number) => void = () => {},   // callback
    inputInvertedCb: (idx: number) => void = () => {},
    nameCb: (name: String) => void = () => {}
) {
    const unwatchInputCount = watch(
        () => comp.inputCount,
        (newVal, oldVal) => {
            inputCountCb(newVal);
        }
    );

    const unwatchName = watch(
        () => comp.name,
        (newVal, oldVal) => {
            nameCb(newVal);
        }
    );

    // 返回 unwatch 函数，方便外部停止监听
    return () => {
        unwatchInputCount();
        //watchName();
    };
}
