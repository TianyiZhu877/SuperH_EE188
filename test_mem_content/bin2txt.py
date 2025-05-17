import sys

for name in sys.argv[1:]:
    with open(name+".bin", "rb") as f, open(name+".txt", "w") as out:
        print(f"converting {name}.bin")
        while True:
            word = f.read(4)
            if len(word) < 4:
                break
            # big-endian to integer
            value = int.from_bytes(word, byteorder="big")
            write_val = f"{value:08x}\n"
            # print(write_val, end='')
            out.write(write_val)  # write as 8-digit hex
        print("**************************\n")
        