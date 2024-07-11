import * as $regex from "../gleam_stdlib/gleam/regex.mjs";
import {
	makeError,
} from "./gleam.mjs";

const cache = new Map();

export function persist(name, gen) {
	let value = cache.get(name)
	if (value === undefined) {
		value = gen();
		cache.set(name, value);
	}

	return value;
}
