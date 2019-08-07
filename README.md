# nym

master: [![CircleCI](https://circleci.com/gh/khrynczenko/nym/tree/master.svg?style=svg)](https://circleci.com/gh/khrynczenko/nym/tree/master)  

## Introduction

*nym* is a command line program that helps in finding synonyms and antonyms. 

## How to build
Just run `stack build` from within project root directory.

## Usage
Using stack invoke:  
`stack run -- --help`  
Or directly from executable:
`nym --help`  

##### Note
`stack run` adds significant overhead and can significantly decrease the 
performance.

## Example
```
$ ./nym work
achievement
doing
product
labor
employment
```

## Features
- built-in database of synonyms and antonyms (fast lookup)
- word correction helper

## Help
If you have any problems or questions please consider posting an issue. Any 
feedback is also highly appreciated.