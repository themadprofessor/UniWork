package main

// totientRange.go - Sequential Euler Totient Function (Go Version)
// compile: go build
// run:     totientRange lower_num upper_num

// Phil Trinder    30/8/2018

// This program calculates the sum of the totients between a lower and an
// upper limit
//
// Each function has an executable Haskell specification
//
// It is based on earlier work by: Greg Michaelson, Patrick Maier, Phil Trinder,
// Nathan Charles, Hans-Wolfgang Loidl and Colin Runciman

import (
	"fmt"
	"os"
	"strconv"
	"sync"
	"time"
)

// Compute the Highest Common Factor, hcf of two numbers x and y
//
// hcf x 0 = x
// hcf x y = hcf y (rem x y)

func hcf(x, y int64, channel chan int64) {
	var t int64
	for y != 0 {
		t = x % y
		x = y
		y = t
	}
	channel <- x
	close(channel)
}

// relprime determines whether two numbers x and y are relatively prime
//
// relprime x y = hcf x y == 1

func relprime(x, y int64, channel chan bool, group sync.WaitGroup) {
	hcf_chan := make(chan int64)
	go hcf(x, y, hcf_chan)
	channel <- (<- hcf_chan) == 1
	group.Done()
}

// euler(n) computes the Euler totient function, i.e. counts the number of
// positive integers up to n that are relatively prime to n
//
// euler n = length (filter (relprime n) [1 .. n-1])

func euler(n int64, channel chan int64, group sync.WaitGroup) {
	var length, i int64
	var wait sync.WaitGroup
	relChan := make(chan bool, n-1)
	wait.Add(int(n - 1))

	length = 0
	for i = 1; i < n; i++ {
		go relprime(n, i, relChan, wait)
	}

	go func() {
		wait.Wait()
		close(relChan)
	}()

	for result := range relChan {
		if result {
			length++
		}
	}

	channel <- length
	group.Done()
}

// sumTotient lower upper sums the Euler totient values for all numbers
// between "lower" and "upper".
//
// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

func sumTotient(lower, upper int64) int64 {
	var sum, i int64
	var wait sync.WaitGroup
	channel := make(chan int64, upper-lower-1)
	wait.Add(int(upper - lower - 1))

	sum = 0
	for i = lower; i <= upper; i++ {
		go euler(i, channel, wait)
	}

	go func() {
		wait.Wait()
		close(channel)
	}()

	for val := range channel {
		sum += val
	}

	return sum
}

func main() {
	var lower, upper int64
	var err error
	// Read and validate lower and upper arguments
	if len(os.Args) < 3 {
		panic(fmt.Sprintf("Usage: must provide lower and upper range limits as arguments"))
	}

	if lower, err = strconv.ParseInt(os.Args[1], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse first argument"))
	}
	if upper, err = strconv.ParseInt(os.Args[2], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse second argument"))
	}
	// Record start time
	start := time.Now()
	// Compute and output sum of totients
	fmt.Println("Sum of Totients between", lower, "and", upper, "is", sumTotient(lower, upper))
	// Record the elapsed time
	t := time.Now()
	elapsed := t.Sub(start)
	fmt.Println("Elapsed time", elapsed)
}
