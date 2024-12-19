import sys
import typing

import re
import numpy as np
import pandas as pd

# def parse_coordinates(input_text):
#     # Regex pattern to extract data
#     pattern = r"Button A: X\+(\d+), Y\+(\d+)\s+Button B: X\+(\d+), Y\+(\d+)\s+Prize: X=(\d+), Y=(\d+)"
#     
#     # Find all matches
#     matches = re.findall(pattern, input_text)
#     
#     # Create vectors from the matches
#     vectors = [np.array([int(a), int(b), int(c), int(d), int(e), int(f)]) for a, b, c, d, e, f in matches]
#     
#     # Optionally convert to a pandas DataFrame for easier handling
#     df_vectors = pd.DataFrame(vectors, columns=["A_X", "A_Y", "B_X", "B_Y", "Prize_X", "Prize_Y"])
#     
#     return df_vectors
#
def parse_coordinates(input_text):
    # Regex pattern to extract data
    pattern = r"Button A: X\+(\d+), Y\+(\d+)\s+Button B: X\+(\d+), Y\+(\d+)\s+Prize: X=(\d+), Y=(\d+)"
    
    # Find all matches
    matches = re.findall(pattern, input_text)
    
    # Create vectors from the matches
    vectors = [np.array([int(a), int(b), int(c), int(d), int(e), int(f)]) for a, b, c, d, e, f in matches]
    
    return vectors

# Example usage
text = """
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"""

# Parse the text
parsed_data = parse_coordinates(text)

# Display the parsed data
print(parsed_data)

def solve_scaling_factors(vectors):
    results = []
    for vec in vectors:
        # Extract components
        a =  np.array([vec[0], vec[1]])  # Button A (X, Y)
        b =  np.array([vec[2], vec[3]])  # Button B (X, Y)
        v = 10000000000000 + np.array([vec[4], vec[5]])  # Prize (X, Y)
        
        # Stack small vectors into a matrix
        A = np.column_stack((a, b))
        
        # Solve for scaling factors
        x, residuals, rank, s = np.linalg.lstsq(A, v, rcond=None)
        
        # Store results
        results.append({
                'a': a, 
                'b': b, 
                'v': v, 
            "Scaling Factors (x, y)": x,
        })
    
    return results

# Solve for scaling factors
solutions = solve_scaling_factors(parsed_data)

# Print results
for i, solution in enumerate(solutions):
    x = solution["Scaling Factors (x, y)"]
    a = solution["a"]
    b = solution["b"]
    v = solution["v"]
    t = int(x[0]) * a + int(x[1]) * b
    print("ROBIN", t[0], t[1])
    print("ROBIN", t == v)
    if (t[0] == v[0] and t[1] == v[1] ):
        print("RESUSSITE")

    print(f"Input {i + 1}:")
    print("Scaling Factors (x, y):", x[0], x[1])
    print("-" * 40)

def part2(lines: str):
    parsed_data = parse_coordinates(lines)
    solutions = solve_scaling_factors(parsed_data)
    s = 0
    for i, solution in enumerate(solutions):
        x = solution["Scaling Factors (x, y)"]
        a = solution["a"]
        b = solution["b"]
        v = solution["v"]
        t = round(x[0]) * a + round(x[1]) * b
        print("ROBIN", t[0], t[1])
        print("ROBIN", t == v)
        if (t[0] == v[0] and t[1] == v[1]):
            print("RESUSSITE")
            s += 3 * round(x[0]) + round(x[1])

        print(f"Input {i + 1}:")
        print("Scaling Factors (x, y):", x[0], x[1])
        print("-" * 40)
    return s


def main():
    lines = "\n".join(l.strip() for l in sys.stdin)
    p2 = part2(lines)
    print(f"Part2: {p2}")


if __name__ == "__main__":
    main()
