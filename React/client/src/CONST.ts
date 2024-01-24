// Creating a default array and object this way because objects ({}) and arrays ([]) are not stable types.
// Freezing the array ensures that it cannot be unintentionally modified.
const EMPTY_ARRAY = Object.freeze([]);
const EMPTY_OBJECT = Object.freeze({});

const CONST = {
    EMPTY_ARRAY,
    EMPTY_OBJECT,
} as const;

export default CONST;