import sys

def main():
    op = sys.argv[1]
    file = f"tests_{op}.txt"
    split_lines = []
    num_splits = 16
    if len(sys.argv) == 3:
        num_splits = int(sys.argv[2])
    with open(file, "r") as f:
        lines = f.readlines()
        n = len(lines)
        chunk_size = n//num_splits
        for i in range(0, n, chunk_size):
            split_lines.append(lines[i:i+chunk_size])

    print(f"Splitting {file} into {num_splits} parts of size {chunk_size}")
    for i,file_lines in enumerate(split_lines):
        with open(f"{op}/tests_{op}_{i+1}.txt", "w") as f:
            f.writelines(file_lines)
    print("done!")
    
        

if __name__ == "__main__":
    main()
