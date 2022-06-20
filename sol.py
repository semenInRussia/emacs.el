for a in range(10):
    for b in range(10):
        if a * (a + 1) == (b * (b + 2) - 9):
            print(a, b)
            break
