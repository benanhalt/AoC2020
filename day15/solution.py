

data = [17,1,3,16,19,0]

def game_(end):
    return game(end, len(data), {v:i for i,v in enumerate(data[:-1])}, data[-1])

def game(end, count, m, prev):
    while end > count:
        if prev in m:
            nxt = count - m[prev] - 1
        else:
            nxt = 0
        m[prev] = count - 1
        count += 1
        prev = nxt
    print(len(m))
    return prev


print(game_(2020))
print(game_(30000000))
