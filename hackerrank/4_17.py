import turtle
from random import randrange
from typing import List, Tuple, TypeVar


def fact(n):
    """ 1 """
    if n <= 0:
        return 1
    return n * fact(n - 1)


def reverse(xs):
    """ 2 """
    if len(xs) == 0:
        return []
    return reverse(xs[1:]) + xs[:1]


def drawTree(tlen, size, t):
    """ 3 """
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
        size = size - 2
        #
        t.right(angle)
        drawTree(tlen - sublen, size, t)
        t.left(angle + angle)
        drawTree(tlen - sublen, size, t)
        t.right(angle)
        t.color("green")
        t.backward(tlen)


def runTree():
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
    """ 5 """
    if n == 0 or n == 1:
        return n
    return fibrec(n - 1) + fibrec(n - 2)


def fibdp(n):
    """ 5 """
    if n == 0 or n == 1:
        return n
    fib = [0] * n
    fib[0] = 0
    fib[1] = 1
    for i in range(2, n + 1):
        fib[i] = fib[i - 1] + fib[i - 2]
    return fib[n]


def fibgen():
    """ 5 """
    a = 0
    b = 1
    t = 0
    while True:
        yield a
        t = a + b
        a = b
        b = t


def koch(t, a, order: int):
    """ 8 """
    if order > 0:
        for ar in [60, -120, 60, 0]:
            koch(t, a / 3, order - 1)
            t.left(ar)
    else:
        t.forward(a)


# def hilbert:


def runKoch():
    """ 8 """
    t = turtle.Turtle()
    t.color("sky blue", "white")
    t.penup()
    t.backward(300 / 1.732)
    t.left(30)
    t.pendown()
    t.speed("fastest")
    t.begin_fill()
    for _ in range(3):
        koch(t, 300, 4)
        t.right(120)
    t.end_fill()


# runKoch()


def knapsack(items: List[Tuple[int, int]], total: int) -> int:
    """ 14 """
    t = [[0] * (total + 1) for i in range(len(items) + 1)]
    for i in range(len(items) + 1):
        for j in range(total + 1):
            if i == 0 or j == 0:
                t[i][j] = 0
            elif items[i - 1][0] <= j:
                t[i][j] = max(items[i - 1][1] + t[i - 1][j - items[i - 1][0]],
                              t[i - 1][j])
            else:
                t[i][j] = t[i - 1][j]
    for i in t:
        print(i)
    return t[len(items)][total]


# print(knapsack([(10, 60), (20, 100), (30, 120)], 50))

S = TypeVar('S', str, bytes)


def edit_dist(a: S, b: S) -> int:
    """ 15 """
    t = [[0] * (len(a) + 1) for _ in range(len(b) + 1)]
    for i in range(len(a) + 1):
        for j in range(len(b) + 1):
            if i == 0 or j == 0:
                t[i][j] = 0
            elif a[i - 1] == b[j - 1]:
                t[i][j] = t[i - 1][j - 1]
            else:
                t[i][j] = min(5 + t[i - 1][j], 20 + t[i][j - 1],
                              20 + t[i - 1][j - 1])
    return t[len(a)][len(b)]


print(edit_dist("algorithm", "alligator"))


def hanoi(n, fr, to, aux):
    """ 12 """
    if n == 0:
        to.push(fr.pop())
    else:
        hanoi(n - 1, fr, aux, to)
        to.push(fr.pop())
        hanoi(n - 1, to, fr, aux)


class Disc(turtle.Turtle):
    def __init__(self, n):
        turtle.Turtle.__init__(self, shape="square", visible=False)
        self.up()
        self.shapesize(1.5, n * 1.5, 2)
        self.fillcolor(randrange(0, 255), randrange(0, 255), randrange(0, 255))


class Peg(list):
    def __init__(self, x):
        self.x = x

    def pop(self):
        disc = list.pop(self)
        disc.sety(150)
        return disc

    def push(self, disc):
        disc.setx(self.x)
        disc.sety(len(self) * 150)
        self.append(disc)


def runHanoi():
    discs = 4
    pega = Peg(-300)
    pegb = Peg(0)
    pegc = Peg(300)
    for i in range(0, discs, -1):
        disc = Disc(i)
        pega.push(disc)
    hanoi(discs, pega, pegb, pegc)
    return "EVENTLOOP"
