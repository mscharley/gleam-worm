const cache = new Map();

export function persist(gen) {
	let key = gen.toString();

	if (cache.has(key)) {
		return cache.get(key);
	} else {
		let value = gen();
		cache.set(key, value);
		return value;
	}
}
