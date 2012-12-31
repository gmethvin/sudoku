# Sudoku solver #

Written by Greg Methvin

This is a basic sudoku solver implementation (brute force) written in Scala.

This project uses sbt, so the easiest way to run it is to use

    sbt "run <filename>"

with a file containing a sudoku puzzle. The puzzles just have to have 81
digits with some other character (not 1-9) representing empty spaces.
Whitespace is ignored. The program will also accept input on standard in if
"-" is passed as the filename.

For example, if the input is

    200000060000075030048090100000300000300010009000008000001020570080730000090000004

The output looks like the following:

    Board:
    2** *** *6*
    *** *75 *3*
    *48 *9* 1**

    *** 3** ***
    3** *1* **9
    *** **8 ***

    **1 *2* 57*
    *8* 73* ***
    *9* *** **4

    Solution:
    273 481 965
    916 275 438
    548 693 127

    859 347 612
    367 512 849
    124 968 753

    431 829 576
    685 734 291
    792 156 384
