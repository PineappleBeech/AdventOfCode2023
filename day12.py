import functools


def main():
    contents = open("inputs/day12.txt", "r").readlines()
    a = list(map(calc, contents))
    print(sum(a))
    print(row.cache_info())

def calc(s):
    m = s.split(' ')[0]
    l = tuple(map(int, s.split(' ')[1].split(',')))
    return row(quintuple2(m), quintuple(l))

@functools.lru_cache(maxsize=None)
def row(s, l):
    if s == "":
        if l == ():
            return 1
        else:
            return 0
    elif s[0] == '#':
        if l == ():
            return 0
        elif len(s) >= l[0] and '.' not in s[0:l[0]] and (len(s) == l[0] or s[l[0]] != '#'):
            return row(s[l[0]+1:], l[1:])
        else:
            return 0
    elif s[0] == '?':
        return row(s[1:], l) + row("#"+s[1:], l)
    else:
        return row(s[1:], l)

def quintuple(x):
    return x + x + x + x + x

def quintuple2(x):
    return x + ("?" + x) * 4

if __name__ == "__main__":
    main()