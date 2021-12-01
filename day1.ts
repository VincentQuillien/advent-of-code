const getIncreases = (sonarDepths: number[], previousDepth = sonarDepths[0]): number => {
    const [head, ...tail] = sonarDepths;
    const increase = head > previousDepth ? 1 : 0;

    return !tail.length ? increase : increase + getIncreases(tail, head);
};

const sumByWindows = (sonarDepths: number[], windowSize = 3) =>
    sonarDepths
        .slice(0, sonarDepths.length - windowSize + 1)
        .map((_, index) => sonarDepths.slice(index, index + windowSize))
        .map((window) => window.reduce((acc, cv) => acc + cv));

// Answer 1
getIncreases(input);

// Answer 2
getIncreases(sumByWindows(input));
