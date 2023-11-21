package utils

import (
	"os"
)

func Readfile(filepath string) (string, error) {
	buf, err := os.ReadFile(filepath)
	if err != nil {
		return "", err
	}

	return string(buf), nil
}
