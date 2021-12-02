const first = (input: ['forward' | 'up' | 'down', number][]) => {
    const { distance, depth } = input.reduce(({ distance, depth }, [type, value]) => {
        switch (type) {
            case 'forward': return { distance: distance + value, depth};
            case 'up': return { distance, depth: depth - value  };
            case 'down': return { distance, depth: depth + value };
        }
    }, { distance: 0, depth: 0 })

    return distance * depth;
}

const second = (input: ['forward' | 'up' | 'down', number][]) => {
    const { distance, depth } = input.reduce(({ distance, depth, aim }, [type, value]) => {
        switch (type) {
            case 'forward': return { distance: distance + value, depth: depth + aim * value, aim };
            case 'up': return { distance, depth, aim: aim - value };
            case 'down': return { distance, depth, aim: aim + value };
        }
    }, { distance: 0, depth: 0, aim: 0 })

    return distance * depth;
}
