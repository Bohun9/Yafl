import os
import subprocess
import re
import sys

COMPILER = [
    "cabal",
    "run",
    "yafl",
    "--",
]
TEST_DIR = "tests"
TEMP_OUT = "test_output.txt"
TEMP_ERR = "test_error.txt"


def extract_expected_outputs(test_file):
    expected_stdout, expected_stderr, expected_exitcode = "", "", 0

    with open(test_file, "r", encoding="utf-8") as f:
        for line in f:
            if match := re.match(r".*//\s*@stdout:(.*)", line):
                expected_stdout += match.group(1).strip()
            elif match := re.match(r".*//\s*@stderr:(.*)", line):
                expected_stderr += match.group(1).strip()
            elif match := re.match(r".*//\s*@exitcode:(\d+)", line):
                expected_exitcode = int(match.group(1).strip())

    return expected_stdout, expected_stderr, expected_exitcode


def run_test(test_file):
    expected_stdout, expected_stderr, expected_exitcode = extract_expected_outputs(
        test_file
    )

    compile_process = subprocess.run(
        COMPILER + [test_file, "-o", "test_program"], capture_output=True, text=True
    )
    actual_exitcode = compile_process.returncode

    if actual_exitcode == 0:
        run_process = subprocess.run(["./test_program"], capture_output=True, text=True)
        actual_stdout = run_process.stdout.strip()
        actual_stderr = run_process.stderr.strip()
        actual_exitcode = run_process.returncode
    else:
        actual_stdout = compile_process.stdout.strip()
        actual_stderr = compile_process.stderr.strip()

    if (
        actual_stdout == expected_stdout
        and actual_stderr == expected_stderr
        and actual_exitcode == expected_exitcode
    ):
        print(f"[PASS] {test_file}")
    else:
        print(f"[FAIL] {test_file}")
        print(f"  Expected stdout: '{expected_stdout}'")
        print(f"  Actual stdout:   '{actual_stdout}'")
        print(f"  Expected stderr: '{expected_stderr}'")
        print(f"  Actual stderr:   '{actual_stderr}'")
        print(f"  Expected exit: {expected_exitcode}, got {actual_exitcode}")

    if os.path.exists("test_program"):
        os.remove("test_program")


def main():
    test_files = [
        os.path.join(TEST_DIR, f) for f in os.listdir(TEST_DIR) if f.endswith(".yafl")
    ]
    test_files.sort()

    if not test_files:
        print("No test files found.")
        sys.exit(1)

    for test_file in test_files:
        run_test(test_file)


if __name__ == "__main__":
    main()
