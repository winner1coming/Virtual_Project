class MultiMap<K, V> {
    private map: Map<K, V[]>;

    constructor() {
        this.map = new Map<K, V[]>();
    }

    // 添加键值对
    add(key: K, value: V): void {
        if (!this.map.has(key)) {
            this.map.set(key, []);
        }
        this.map.get(key)!.push(value);
    }

    // 获取键对应的值集合
    get(key: K): V[] | undefined {
        return this.map.get(key);
    }

    // 删除键值对
    remove(key: K, value: V): boolean {
        const values = this.map.get(key);
        if (!values) return false;

        const index = values.indexOf(value);
        if (index !== -1) {
            values.splice(index, 1);
            if (values.length === 0) {
                this.map.delete(key); // 如果值集合为空，删除键
            }
            return true;
        }
        return false;
    }

    // 删除整个键
    delete(key: K): boolean {
        return this.map.delete(key);
    }

    // 检查是否包含键
    has(key: K): boolean {
        return this.map.has(key);
    }

    // 获取所有键
    keys(): IterableIterator<K> {
        return this.map.keys();
    }

    // 获取所有值
    values(): IterableIterator<V[]> {
        return this.map.values();
    }

    // 清空
    clear(): void {
        this.map.clear();
    }
}