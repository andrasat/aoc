package aoc_2023

import (
	"fmt"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"github.com/andrasat/aoc/utils"
)

type Map struct {
	Dest int
	Src  int
	Size int
}

type Data struct {
	seeds []int
	maps  [][]Map
}

func parseData(input string, getSeedRange bool) Data {
	data := Data{
		seeds: []int{},
		maps:  [][]Map{},
	}

	sections := strings.Split(input, "\n\n")

	seeds, maps := sections[0], sections[1:]

	if getSeedRange {
		separated_seeds := strings.Split(strings.Replace(seeds, "seeds: ", "", 1), " ")
		for i, seed := range separated_seeds {
			intSeed, _ := strconv.Atoi(seed)

			if (i+1)%2 == 0 {
				start := data.seeds[i-1] + 1
				for j := start; j < start+intSeed; j++ {
					if exist := slices.ContainsFunc(data.seeds, func(seed int) bool {
						return seed == j
					}); !exist {
						data.seeds = append(data.seeds, j)
					}
				}
			} else {
				data.seeds = append(data.seeds, intSeed)
			}
		}
	} else {
		separated_seeds := strings.Split(strings.Replace(seeds, "seeds: ", "", 1), " ")
		for _, seed := range separated_seeds {
			intSeed, _ := strconv.Atoi(seed)
			data.seeds = append(data.seeds, intSeed)
		}
	}

	regexPattern := regexp.MustCompile(`\d+`)

	filteredMaps := [][]Map{}
	for i, mapSection := range maps {
		rows := strings.Split(mapSection, "\n")[1:]

		filteredMaps = append(filteredMaps, []Map{})

		for _, row := range rows {
			if ok := regexPattern.MatchString(row); ok {
				map_data := strings.Split(row, " ")
				dest, _ := strconv.Atoi(map_data[0])
				src, _ := strconv.Atoi(map_data[1])
				size, _ := strconv.Atoi(map_data[2])
				filteredMaps[i] = append(filteredMaps[i], Map{Dest: dest, Src: src, Size: size})
			}
		}
	}

	data.maps = filteredMaps

	return data
}

func findLowestLocation(data Data) int {
	locations := []int{}

	for _, seed := range data.seeds {
		currentLocation := seed

		for _, mapSection := range data.maps {
			for _, m := range mapSection {

				maxSrc := m.Src + m.Size

				if currentLocation >= m.Src && currentLocation <= maxSrc {
					newLoc := currentLocation + (m.Dest - m.Src)
					currentLocation = newLoc
					break
				}
			}
		}

		locations = append(locations, currentLocation)
	}

	return slices.Min(locations)
}

func runDay5() error {
	fmt.Println("\n ----- DAY 5 -----")

	input, err := utils.Readfile("./../inputs/2023/day5-puzzle.txt")
	if err != nil {
		return err
	}

	fmt.Printf("Part: 1 - %d\n", findLowestLocation(parseData(input, false)))
	fmt.Printf("Part: 2 - %d\n", findLowestLocation(parseData(input, true)))

	return nil
}
