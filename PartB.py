from functools import reduce

# Question 1:
fibonacci = lambda n: [0, 1][:n] if n <= 2 else fibonacci(n - 1) + [fibonacci(n - 1)[-1] + fibonacci(n - 2)[-1]]

# Example usage:
print(fibonacci(10))  # Output: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]


# Question 2:
concat_strings = lambda lst: lst[0] if len(lst) == 1 else lst[0] + ' ' + (concat_strings(lst[1:]))

# Example usage:
print(concat_strings(["Hello", "world", "this", "is", "Python"]))  # Output: "Hello world this is Python"


# Question 3:
def cumulative_sum_of_squares_test(lst):
    return list(map(
        lambda sublist: reduce(
            lambda acc, num: (lambda add_if_even: add_if_even(num))(
                lambda n: (lambda square: acc + square)(n * n) if (lambda check_even: check_even(n))(lambda x: x % 2 == 0) else acc
            ),
            sublist, 0
        ),
        lst
    ))

# Example usage:
print(cumulative_sum_of_squares_test([[1, 2, 3], [6, 10], [3, 9], []]))
# Output: [4, 136, 0, 0]


# Question 4:
def cumulative_operation(op):
    return lambda seq: reduce(op, seq)

# Factorial function using multiplication
factorial = lambda n: cumulative_operation(lambda x, y: x * y)(range(1, n + 1))

# Exponentiation function using multiplication
exponentiation = lambda base, exp: cumulative_operation(lambda x, y: x * y)([base] * exp)

# Example usage:
print(factorial(5))  # Output: 120
print(exponentiation(2, 3))  # Output: 8


# Question 5:
nums = [1, 2, 3, 4, 5, 6]

result = reduce(lambda x, y: x + y, map(lambda n: n ** 2, filter(lambda n: n % 2 == 0, nums)))

print(result)  # Output: 56


# Question 6:
count_palindromes = lambda lst: list(map(lambda sublist: len(list(filter(lambda s: s == s[::-1], sublist))), lst))
#def count_palindromes(lst):
#    return list(map(lambda sublist: len(list(filter(lambda s: s == s[::-1], sublist))), lst))

# Example usage:
input_list = [["level", "world", "civic"], ["python", "madam"], ["noon", "refer", "test"], []]
print(count_palindromes(input_list))  # Output: [2, 1, 2, 0]
