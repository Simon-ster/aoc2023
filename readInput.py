def yieldLines(filename):
	with open(f'input/{filename}', 'r') as f:
		for line in f:
			yield line
