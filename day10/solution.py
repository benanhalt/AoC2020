

with open('input.txt', 'r') as f:
    adapters = [int(w) for w in f.read().split()]

goal = max(adapters) + 3

def can_connect(prev):
    return [
        a for a in adapters
        if a in [prev + 1, prev + 2, prev + 3]
    ]


print(adapters)
print(can_connect(0))
print(goal)



memo = {}
def chain(prev):
    if prev in memo:
        return memo[prev]
    else:
        options = can_connect(prev)
        if not options:
            if goal - prev <= 3:
                result = 1
            else:
                result = 0
        else:
            result = sum(chain(option) for option in options)
        memo[prev] = result
        return result


print("Part 2:")
print(chain(0))
