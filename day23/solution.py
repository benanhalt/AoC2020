

input = [int(x) for x in '167248359']

def show(next_cup):
    i = 1
    out = []
    for _ in range(len(next_cup)-2):
        out.append(str(next_cup[i]))
        i = next_cup[i]
    print("".join(out))

def solve(input, iterations):
    # create the next_cup array
    next_cup = [0]*(len(input) + 1)
    for i,x in enumerate(input):
        next_cup[x] = input[(i+1)%len(input)]

    maxCup = max(input)
    current = 1
    for _ in range(iterations):
        current = move(maxCup, next_cup, current)
    return next_cup

def move(maxCup, next_cup, current):
    # removed cups:
    r1 = next_cup[current]
    r2 = next_cup[r1]
    r3 = next_cup[r2]

    # clockwise after removed
    r4 = next_cup[r3]

    # find destination cup
    dest = current - 1
    if dest <= 0:
        dest = maxCup
    while dest in [r1, r2, r3]:
        dest -= 1
        if dest <= 0:
            dest = maxCup

    # clockwise after dest
    dest1 = next_cup[dest]

    # update pointers
    next_cup[current] = r4 # cup after removed cups now follows current
    next_cup[dest] = r1 # first removed cup now follows dest
    next_cup[r3] = dest1 # last removed cup now followed by cup that followed dest

    # the current cup moves one cup clockwise
    return next_cup[current]

# Part 1
next_cup = solve(input, 100)
show(next_cup)

# Part 2
input2 = input
current = max(input) + 1
while current <= 1e6:
    input2.append(current)
    current += 1

next_cup = solve(input2, 10e6)
print(next_cup[1]*next_cup[next_cup[1]])
