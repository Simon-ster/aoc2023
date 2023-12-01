def yieldLines(filename):
	with open(f'input/{filename}', 'r') as f:
		for line in f:
			yield line

def rreplace(s, old, new, occurrence):
    li = s.rsplit(old, occurrence)
    return new.join(li)
