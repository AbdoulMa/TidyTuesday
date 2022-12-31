// Stack constructor
function Stack() {
	this.array = new Array();
}

// Create Stack methods
function pop() {
	var lastElement = this.array[this.array.length - 1];
	if (this.array.length != 0)
		this.array.length--;
	
	return lastElement;
}

function push(newElement) {
	var len = this.array.length;
	this.array[len] = newElement;
}

function size() {
	return this.array.length;
}

function clear() {
	this.array.length = 0;
}

// Create and discard an initial Stack object to work around a bug in Netscape 3
new Stack();

// Assign Stack methods
Stack.prototype.pop = pop;
Stack.prototype.push = push;
Stack.prototype.size = size;
Stack.prototype.clear = clear;