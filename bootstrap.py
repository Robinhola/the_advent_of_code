import os

def ask_for_number(prompt, min_, max_):
    value = input(prompt + ": ")
    if value.isnumeric():
        value = int(value)

        if min_ <= value <= max_:
            return value
        
    raise RuntimeError("Need year", value)


def mkdir(folder_name):
    os.mkdir(folder_name)


def main():
    print("Bootstraping Ocaml advent of code")
    year = ask_for_number("Enter the year", 2015, 2025)
    day = ask_for_number("Enter the day", 1, 25)
    folder_name = f"{year}_{"%02d"%day}_ocaml"

    print("Create folder")
    mkdir(folder_name)

    print("Copy bootstrap project")
    os.system(f"cp -r ocaml_default_project/ {folder_name}/")


if __name__ == '__main__':
    main()
