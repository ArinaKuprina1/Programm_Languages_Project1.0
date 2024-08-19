import basic
#
# while True:
#     text = input('basic > ')
#     result, error = basic.run('<stdin>', text)
#
#     if error:
#         print(error.as_string())
#     else:
#         print(result)
#
def main():
    while True:
        try:
            text = input('basic > ')
            result, error = basic.run('<stdin>', text)
            if error:
                print(error.as_string())
            else:
                print(result)
        except KeyboardInterrupt:
            print("\nExiting. Goodbye!")
            break
        except EOFError:
            print("\nExiting. Goodbye!")
            break

if __name__ == "__main__":
    main()