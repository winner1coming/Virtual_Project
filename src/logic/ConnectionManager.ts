export interface Conn {
    id: number;
    idx: number;
    legal: boolean;
};
export type PinMap = MultiMap<number, Conn>;   // 每个引脚对应的接线情况。同个引脚，可以接多条线


export class ConnectionManager {
    // 连线关系的结构如下：
    // Map{
    //   id1: Map{   // id1 一般为输入端（对电线来说），即id1为输出设备
    //       idx1: { id: id2, idx: idx2, legal },
    //       ...
    //   },
    //   ...
    // }
    // 即Map<id:number, PinMap>
    connections: Map<number, PinMap>;
    constructor() {
        this.connections = new Map<number, PinMap>(); // 存储连线关系
    }

    // 添加连线
    addConnection(id1: number, idx1: number, id2: number, idx2: number, legal: boolean): boolean {
        if (!this.connections.has(id1)) {
            this.connections.set(id1, new MultiMap());
        }
        this.connections.get(id1)!.add(idx1, { id: id2, idx: idx2, legal: legal });
        return true;
    }

    // 删除连线
    removeConnection(inputId: number, inputIdx: number, outputId: number, outputIdx: number): boolean {
        const pinMap = this.connections.get(inputId);
        if(pinMap){
            // 删除指定引脚的连线
            for(const conn of pinMap.get(inputIdx) || []) {
                if (conn.id === outputId && conn.idx === outputIdx) {
                    pinMap.remove(inputIdx, conn);
                    return true;
                }
            }
        }
        return false;
    }

    // // 查找连线
    // getConnection(id: number, idx: number): Conn|null {    // 返回{id, idx, legal}
    //     if (this.connections.has(id)) {
    //         return this.connections.get(id)!.get(idx) || null;
    //     }
    //     return null;
    // }

    // 获取与某组件的输出端的pinMap
    getOutputPinMap(id: number): PinMap | undefined{
        return this.connections.get(id);
    }

    // // 获取所有连线（暂时没什么用）
    // getAllConnections() {
    //     const result = [];
    //     for (const [id1, pinMap] of this.connections) {
    //         for (const [idx1, { id, idx, legal }] of pinMap) {
    //             result.push({ id1, idx1, id2: id, idx2: idx, legal });
    //         }
    //     }
    //     return result;
    // }
}