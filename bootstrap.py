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


def copy_default_project(default_project, folder_name):
    if os.path.exists(default_project):
        os.system(f"cp -r {default_project}/ {folder_name}/")
    else:
        print(f"Could not find default project {default_project} project")


def main():
    print("Bootstraping Ocaml advent of code")
    year = ask_for_number("Enter the year", 2015, 2025)
    day = ask_for_number("Enter the day", 1, 25)
    coding_language = input("Enter the coding languaage (python or ocaml): ")
    folder_name = f'{year}_{"%02d"%day}_{coding_language}'

    print("Create folder")
    mkdir(folder_name)

    print("Copy bootstrap project")
    copy_default_project(f"{coding_language}_default_project", folder_name)

    print("Done! Find the project in: ")
    print(folder_name)


if __name__ == "__main__":
    main()
