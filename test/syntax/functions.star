import io

fn add(x) {
    return fn(y) x + y 
}

fn power(x, n) {
    if n > 1 {
        return power(x * x, n)
    } else {
        return x
    }
}