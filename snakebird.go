package main

import (
	"fmt"
)

type point struct {
	x int
	y int
}

type pointSet map[point]bool

func (this *point) eq(that *point) bool {
	return (this == that) || (this.x == that.x && this.y == that.y)
}

type Snakebird []point

type GameState struct {
	snakebird Snakebird
	obstacles pointSet
	fruits    pointSet
	goal      point
}

func (s *Snakebird) side(dx, dy int) pointSet {
	sbMap := make(pointSet)

	for _, sp := range *s {
		sbMap[sp] = true
	}

	result := make(pointSet)

	for _, sp := range *s {
		p := point{sp.x + dx, sp.y + dy}
		if !sbMap[p] {
			result[p] = true
		}
	}

	return result
}

func (s *Snakebird) bottom() pointSet {
	return s.side(0, -1)
}

func (gs *GameState) moveInDir(dx, dy int) {
	for gs.allEmpty(gs.snakebird.side(dx, dy)) {
		newSb := make(Snakebird, len(gs.snakebird))
		copy(newSb, gs.snakebird)
		for i, s := range newSb {
			newSb[i] = point{s.x + dx, s.y + dy}
		}
		gs.snakebird = newSb
	}
}

func (gs *GameState) fall() {
	gs.moveInDir(0, -1)
}

func (gs *GameState) isEmpty(p point) bool {
	if gs.obstacles[p] || p.y < 0 {
		return false
	}
	for _, sp := range gs.snakebird {
		if p == sp {
			return false
		}
	}
	return true
}

func (gs *GameState) allEmpty(ps pointSet) bool {
	for p, _ := range ps {
		if !gs.isEmpty(p) {
			return false
		}
	}
	return true
}

func (this *GameState) Copy() *GameState {
	myFruits := make(pointSet)
	myObstacles := make(pointSet)

	for p, _ := range this.fruits {
		myFruits[p] = true
	}

	for p, _ := range this.obstacles {
		myObstacles[p] = true
	}

	return &GameState{
		goal:      this.goal,
		snakebird: this.snakebird,
		fruits:    myFruits,
		obstacles: myObstacles,
	}
}

func (gs *GameState) moveTo(p point, states *[]GameState) {
	if !gs.isEmpty(p) {
		return
	}

	newGs := *gs.Copy()
	newGs.snakebird = append(newGs.snakebird, p)

	if gs.fruits[p] {
		delete(newGs.fruits, p)
	} else {
		newGs.snakebird = newGs.snakebird[1:]
	}

	newGs.fall()

	*states = append(*states, newGs)
}

func (gs *GameState) move() []GameState {
	current := gs.snakebird[len(gs.snakebird)-1]
	newStates := make([]GameState, 0)
	gs.moveTo(point{current.x + 1, current.y}, &newStates)
	gs.moveTo(point{current.x, current.y + 1}, &newStates)
	gs.moveTo(point{current.x - 1, current.y}, &newStates)
	gs.moveTo(point{current.x, current.y - 1}, &newStates)
	return newStates
}

func NewGameState() GameState {
	return GameState{
		fruits:    make(pointSet),
		obstacles: make(pointSet),
	}
}

func main() {
	gs := NewGameState()
	gs.goal = point{1, 2}
	gs.snakebird = []point{{-1, 0}, {0, 0}}
	gs.fruits[point{1, 0}] = true
	a := gs.move()
	fmt.Println(gs)
	fmt.Println(a)
}
