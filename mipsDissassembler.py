# Mips Dissassembler
# Author: Benjamin Bass
# Date: 28 February 2016
# Input: 32-bit hexf instruction
# Output: The mips instruction that was given

# ==============================================================================
# OPCODES
OP_SLT = 0b101010
OP_LW = 0b100011
OP_SW = 0b101011
OP_BEQ = 0b000100
OP_BNE = 0b000101

#FUNCTION CODES
OP_ADD = 0b100000
OP_SUB = 0b100010
OP_AND = 0b100100
OP_OR = 0b100101
# ==============================================================================

# program counter
pc_counter = 0x7A060

#ARRAY
hexArray=[0x022DA822, 0x8EF30018, 0x12A70004, 0x02689820, 0xAD930018,
          0x02697824, 0xAD8FFFF4, 0x018C6020, 0x02A4A825, 0x158FFFF6,
          0x8E59FFF0]

#VARIABLES
opcodeBitmask = 0xFC000000  # 6 0's
opcodeShift = 26            # 26 bits

s1regBitmask = 0x03E00000   # 5 0's
s1regShift = 21             # 21 bits

s2regBitmask = 0x001F0000   # 2/4 0's
s2regShift = 16             # 16 bits

destregBitmask = 0x0000F800 # 4/2 0's
destregShift = 11           # 11 bits

# funct doesn't need shift, already in LSB position
functBitmask = 0x0000003F   # 6 0's

offsetBitmask = 0x0000FFFF  # 4 0's

# ------------------------------------------------------------------------------
# Functio that dissassembles the hex mips instructions

def deMips(hexNum):

    # include global PC Counter
    global pc_counter

    # bitmask & shift variables that are in both I & R-Type Instructions
    opcode = (hexNum & opcodeBitmask) >> opcodeShift
    s1reg = (hexNum & s1regBitmask) >> s1regShift
    s2reg = (hexNum & s2regBitmask) >> s2regShift

    # If R-Type Instruction, deMips destination register and the function reg
    if (opcode == 0):
        destreg = (hexNum & destregBitmask) >> destregShift
        funct = (hexNum & functBitmask)
        # Switch statement for determining function code.
        if (funct == OP_ADD):
            function = "add"
        elif (funct == OP_SUB):
            function = "sub"
        elif (funct == OP_AND):
            function = "and"
        elif (OP_OR):
            function = "or"
        else:
            # Error message for debugging
            print "Unrecognized function code on R-Instruction."

        # update PC Counter
        # Having a lot of trouble here. Unable to create 2 byte variable in
        # python
        pc_counter = pc_counter + 4

        #format pc counter as hex, parse to string, and print out mips code
        print str(format(pc_counter, '02x')) + " " + function + " $" + str(
            destreg) + ", $" + str(s1reg) + ", $" + str(s2reg)

    # If I-Tyep Instruction
    else:

        # Determine offset and opcode
        offset = (hexNum & offsetBitmask)

        # switch statement that waterfalls to figure out opcode
        if (opcode == OP_SLT):
            function = "slt"
        elif (opcode == OP_LW):
            function = "lw"
        elif (opcode == OP_SW):
            function = "sw"
        elif (opcode == OP_BEQ):
            function = "beq"
        elif (opcode == OP_BNE):
            function = "bne"
        else:
            # print message for debugging purposes
            print "Messed up on offset of I-Instruction"

        # update PC counter. Inability to create 2 byte var is causing problems.
        pc_counter = pc_counter + 4

        # format pc counter as hex, parse to string, and print out mips code
        # found this: break long statements at paren's and python will have no
        # problem when evaluating. It's my first time writing in python and this
        # had annoyed me quite a bit...
        print str(format(pc_counter, '02x')) + " " + function + " $" + str(
            s2reg) + ", " + str(offset) +  "($" + str(s1reg) + ")"


# Execute loop that prints and demips array of hex #'s
for x in range (0, len(hexArray)):
    deMips(hexArray[x])

    
# Examples
# |-----------------+-------------|
# | Instruction     | Hexidecimal |
# |-----------------+-------------|
# | add $7, $5, $6  | 0x00A63820  |
# | lw $7, 4($8)    | 0x8D070004  |
# | sw $23, -4($27) | 0xAF77FFFC  |
# |-----------------+-------------|

# TESTERS
# add = 0x00A63820
# lw = 0x8D070004
# sw = 0xAF77FFFC
# 
