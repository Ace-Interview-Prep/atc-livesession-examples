import random
import json

def run_working_example():
    print(f"Here's a random value: {random.random()}")

# Example 1

def run_example_1():
    class User:
        def __init__(self, name):
            self.name = name

    def get_user_name(user):
        return user.name # No check for None

    user = None if random.random() > 0.5 else User("Alice")
    print(get_user_name(user))


# Example 2

def run_example_2():
    def get_area(shape):
        if shape["kind"] == "circle":
            return 3.14 * shape["radius"] ** 2
        return shape["side"] ** 2
    shape = {"kind": "circle", "radius": 5}
    shape2 = {"kind": "square", "radius": 14}
    print(get_area(shape))
    print(get_area(shape2))


# Example 3
def run_example_3():
    def first_element(arr):
        return arr[0]  # No check for empty array
    numbers = []
    print(first_element(numbers))  # Crashes: IndexError: list index out of range


# Example 4
def run_example_4():
    def sum_numbers(items):
        return sum(items)  # Assumes all items are numbers
    mixed = [1, "two", 3]
    #print(sum_numbers(mixed))  # Crashes: TypeError: unsupported operand type


# Example 5
def run_example_5():
    def to_string(x):
        return str(x)
    def parse_int(x):
        return int(x)
    def compose(x):
        return parse_int(to_string(x))
    result = compose("not a number")  # Crashes: ValueError: invalid literal
    print(result)


# Example 6
def run_example_6():
    def process_api(data):
        return data['name'].upper()
    api_data = json.loads('{"id": 1}')
    print(process_api(api_data))


# Example 7
def run_example_7():
    def factorial(n):
        if n <= 0:
            return "1"  # Wrong type: string instead of int
        return n * factorial(n - 1)
    print(factorial(5))


# Example 8
def run_example_8():
    def get_value(obj):
        return obj.get("inner", {}).get("value", 0)  # Assumes nested structure
    obj = {}
    print(get_value(obj))  # Works, but brittle if structure changes


# Example 9
def run_example_9():
    def process_result(result):
        if result["kind"] == "success":
            return result["data"]
        return 0  # Assumes safe default
    bad_result = {"kind": "success", "data": "invalid"}  # Wrong data type
    print(process_result(bad_result))  # Returns string, not number


while True:
    user_input = input("Enter a command (0..9, quit): ").strip().lower()
    if user_input == 'quit':
        print("Exiting program...")
        break
    if user_input == '0':
        run_working_example()
    if user_input == '1':
        run_example_1()
    if user_input == '2':
        run_example_2()
    if user_input == '3':
        run_example_3()
    if user_input == '4':
        run_example_4()
    if user_input == '5':
        run_example_5()
    if user_input == '6':
        run_example_6()
    if user_input == '7':
        run_example_7()
    if user_input == '8':
        run_example_8()
    if user_input == '9':
        run_example_9()