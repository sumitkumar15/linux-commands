# linux-commands

Learning & understanding Linux commands by implementing them on my own.

These commands are not the full implementation but a subset of original commands with some supported flags.

## Steps to Run

Requires java & leiningen installed on the system

1. Clone the repository

2. cd into project root & run ```$ lein uberjar ```

3. run the script.sh script ``` $ ./script.sh ```

Following these steps will generate a runnable in target/uberjar named `lterm`

## Usage

Invoke the commands from lterm file

    $ ./lterm ls
    
    $ ./lterm ls -l
    
    $ ./lterm ping ip
    
    $ ./lterm diff file1 file2

Alternatively you can put the `lterm` runnable on your global bin path & access it from anywhere

## Supported commands

1. ls - list directory
2. ping
3. diff


Check the cli-opts in source files to see supported flags.

Just a learning project , will try to keep adding more commands in future.
