# Discrete Interval Encoding Trees

## Description

Discrete Interval Encoding Trees[1], diet for short, allows sets containing
consecutive values to represent those values as a single interval instead of
individual elements. This could drastically cut down on the number of nodes
needed.

Take for instance the set `[2, 5, 6, 7, 8, 9, 10, 13, 14]`. Using a normal
binary tree with one value per element this could be represented as:
```
   +--14
   |
+--13
|  |
|  +--10
|
9
|
|     +--|
|     |
|  +--8
|  |  |
|  |  +--7
|  |
+--6
   |
   |  +--5
   |  |
   +--2
      |
      +--|
```
Using discrete interval encoding trees we instead get:
```
+--[13, 14]
|
[5, 10]
|
+--[2,2]
```

## References

[1] Martin Erwig, **Diets for Fat Sets**, *Journal of Functional Programming*, Vol. 8, No. 6, 627-632, 1998
