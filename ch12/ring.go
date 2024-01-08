package main

import (
	"fmt"
	"os"
	"strconv"
	"time"
)

type Node struct {
	ID   int
	Chan chan string
	Succ *Node
}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	m, _ := strconv.Atoi(os.Args[2])
	debug := os.Args[3] == "true"
	benchmark(n, m, debug)
}

func benchmark(n int, m int, debug bool) {
	// create n nodes
	nodes := make([]*Node, 0)
	for i := 0; i < n; i++ {
		nodes = append(nodes, &Node{i, make(chan string), nil})
	}

	// connect the nodes
	for i := 0; i < n; i++ {
		node := nodes[i]
		if i < n-1 {
			node.Succ = nodes[i+1]
		}
	}

	// run the nodes in separate Goroutines
	for i := 0; i < n-1; i++ {
		go forward(nodes[i], debug)
	}

	// initiate the ring call
	start := time.Now()
	go func(ch chan<- string) {
		for j := 0; j < m; j++ {
			ch <- fmt.Sprintf("hello %d", j)
		}
	}(nodes[0].Chan)

	// drain all the channels and measure time
	for j := 0; j < m; j++ {
		message := <-nodes[n-1].Chan
		if debug {
			fmt.Printf("'%s' forwarded from %d to %d\n", message, n-2, n-1)
		}
	}
	fmt.Printf("forwarded %d messages %d times in %v\n", m, n, time.Since(start))
}

func forward(node *Node, debug bool) {
	for {
		message := <-node.Chan
		if debug {
			fmt.Printf("'%s' forwarded from %d to %d\n", message, node.ID-1, node.ID)
		}
		node.Succ.Chan <- message
	}
}
