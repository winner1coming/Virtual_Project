#include <stdio.h>
#include <stdlib.h>

// 定义数据类型
typedef int ElemType;  // 可以根据需求更改数据类型

typedef struct Linknode {
    ElemType data;
    struct Linknode *next;
}LinkStack;

// 初始化
void InitStack (LinkStack **stack) {
    *stack = NULL;// 栈顶指针为NULL
}

// 判断栈空
bool IsEmpty(LinkStack *stack) {
    return stack == NULL; // 如果栈顶指针为NULL，则栈空
}

// 入栈
void Push(LinkStack **stack, ElemType data) {
    LinkStack *newNode = (LinkStack *)malloc(sizeof(LinkStack)); // 创建新节点
    newNode->data = data; // 设置数据
    newNode->next = *stack; // 新节点的下一个指针指向当前栈顶
    *stack = newNode; // 更新栈顶指针
}

// 出栈
void Pop(LinkStack **stack) {
    if (IsEmpty(*stack)) {
        printf("栈空，无法出栈\n");
        return;
    }
    LinkStack *temp = *stack; // 临时保存栈顶节点
    *stack = (*stack)->next; // 更新栈顶指针
    free(temp); // 释放原栈顶节点
}

// 取栈顶元素
ElemType Top(LinkStack *stack) {
    if (IsEmpty(stack)) {
        printf("栈空，无法获取栈顶元素\n");
        return -1; // 返回一个错误值
    }
    return stack->data; // 返回栈顶元素
}

