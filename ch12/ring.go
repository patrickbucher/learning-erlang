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
	Pred *Node
	Succ *Node
}

func main() {
	n, _ := strconv.Atoi(os.Args[1])
	m, _ := strconv.Atoi(os.Args[2])
	debug := os.Args[3] == "true"
	benchmark(n, m, debug)
}

func benchmark(n int, m int, debug bool) {
	nodes := make([]Node, 0)

	for i := 0; i < n; i++ {
		nodes = append(nodes, Node{i, make(chan string), nil, nil})
	}
	for i := 0; i < n; i++ {
		node := nodes[i]
		if i > 0 {
			node.Pred = &nodes[i-1]
		}
		if i < n-1 {
			node.Succ = &nodes[i+1]
		}
		nodes[i] = node
	}

	start := time.Now()
	for i := 0; i < n-1; i++ {
		go forward(nodes[i], debug)
	}
	go func(ch chan<- string) {
		for j := 0; j < m; j++ {
			ch <- fmt.Sprintf("hello %d", j)
		}
	}(nodes[0].Chan)
	for j := 0; j < m; j++ {
		message := <-nodes[n-1].Chan
		if debug {
			fmt.Printf("'%s' forwarded from %d to %d\n", message, n-2, n-1)
		}
	}
	end := time.Now()
	diff := end.Sub(start)
	fmt.Printf("forwarded %d messages %d times in %v\n", m, n, diff)
}

func forward(node Node, debug bool) {
	for {
		message := <-node.Chan
		if debug {
			fmt.Printf("'%s' forwarded from %d to %d\n", message, node.ID-1, node.ID)
		}
		node.Succ.Chan <- message
	}
}
