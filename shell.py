# import basic
# import Testing
#
# def main():
#     while True:
#         try:
#             text = input('basic > ')
#             result, error = Testing.run('<stdin>', text)
#             if error:
#                 print(error.as_string())
#             else:
#                 print(result)
#         except KeyboardInterrupt:
#             print("\nExiting. Goodbye!")
#             break
#         except EOFError:
#             print("\nExiting. Goodbye!")
#             break
#
# if __name__ == "__main__":
#     main()


import basic
import os


def main():
    global filename
    user_input = input('basic > ')
    if user_input.startswith('load '):
        filename = user_input.split()[1]

        # Check if the file has the correct .lambda extension
        if not filename.endswith('.lambda.txt'):
            print(f"Error: File '{filename}' must have a .lambda extension.")
            return

        if not os.path.exists(filename):
            print(f"Error: File '{filename}' not found.")
            return

        try:
            with open(filename, 'r') as file:
                content = file.read()
        except IOError:
            print(f"Error: Unable to read file '{filename}'.")
            return

        content = content.split('\n')
        print(content)
        for line in content:
            if line.strip():  # Only process non-empty lines
                result, error = basic.run(filename, line)
                if error:
                    print(error.as_string())
                else:
                    print(result)
    else:
        # Handle direct input
        while True:

            result, error = basic.run('<stdin>', user_input)
            if error:
                print(error.as_string())
            else:
                print(result)


if __name__ == "__main__":
    main()
