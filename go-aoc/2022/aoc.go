package aoc

func Run() error {
	err := runDay1()
	if err != nil {
		return err
	}

	err = runDay2()
	if err != nil {
		return err
	}

	return nil
}
