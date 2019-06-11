import turtle
from random import *
from typing import List, Tuple


def fact(n):
    if n <= 0:
        return 1
    return n * fact(n-1)


def reverse(xs):
    if len(xs) == 0:
        return []
    return reverse(xs[1:]) + xs[:1]


def drawTree(tlen, size, t):
    if tlen > 5:
        t.color("brown")
        if size > 0:
            t.pensize(size)
        else:
            t.pensize(1)
        t.forward(tlen)
        # settings
        angle = randrange(20, 40, 2)
        sublen = randrange(10, 15, 2)
        size = size-2
        #
        t.right(angle)
        drawTree(tlen-sublen, size, t)
        t.left(angle+angle)
        drawTree(tlen-sublen, size, t)
        t.right(angle)
        t.color("green")
        t.backward(tlen)


def main():
    t = turtle.Turtle()
    win = turtle.Screen()
    t.left(90)
    t.up()
    t.backward(100)
    t.down()
    t.color("brown")
    drawTree(75, 10, t)
    win.exitonclick()


def fibrec(n):
    if n == 0 or n == 1:
        return n
    return fibrec(n-1) + fibrec(n-2)


def fibdp(n):
    if n == 0 or n == 1:
        return n
    fib = [0]*n
    fib[0] = 0
    fib[1] = 1
    for i in range(2, n+1):
        fib[i] = fib[i-1] + fib[i-2]
    return fib[n]


def fibgen():
    a = 0
    b = 1
    t = 0
    while True:
        yield a
        t = a + b
        a = b
        b = t


def koch(t, a, order):
    if order > 0:
        for ar in [60, -120, 60, 0]:
            koch(t, a / 3, order - 1)
            t.left(ar)
    else:
        t.forward(a)


# main()
def runKoch():
    t = turtle.Turtle()
    t.color("sky blue", "white")
    t.penup()
    t.backward(300/1.732)
    t.left(30)
    t.pendown()
    t.speed("fastest")
    t.begin_fill()
    for _ in range(3):
        koch(t, 300, 4)
        t.right(120)
    t.end_fill()


runKoch()


def knapsack(items: List[Tuple[int, int]], total: int) -> int:
    t = [[0] * total+1 for _ in range(len(items)+1)]
    for i in range(len(items)+1):
        for j in range(total+1):
            if i == 0 or j == 0:
                t[i][j] = 0
            elif items[i-1][0] <= j:
                t[i][j] = max(items[i-1][1] + t[i-1]
                              [j-items[i-1][0]], t[i-1][j])
            else:
                t[i][j] = t[i-1][j]
    return t[len(items)][total]


def hanoi(n, fr, to, aux):
    if n == 0:
        print("Moving from {} to {}".format(fr, to))
    else:
        hanoi(n-1, fr, aux, to)
        print("Moving from {} to {}".format(fr, to))
        hanoi(n-1, to, fr, aux)
