with open("deaths.csv", "r") as f:
    lines = f.readlines()

row = 1
largest = -1
smallest = 99999
for line in lines:
    # read how many collums
    nbOfEntries = 1
    prev = ""
    no =  False
    for c in line:
        if c == ";":
            nbOfEntries += 1
            if c == prev:
                no = True
        prev = c
        
    print("line ", row, ", has", nbOfEntries, "entries/collums. Following ;", no)
    if nbOfEntries > largest: largest = nbOfEntries
    if nbOfEntries < smallest: smallest = nbOfEntries
    
    row += 1
