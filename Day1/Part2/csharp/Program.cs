var lines = await File.ReadAllLinesAsync("../input.txt");

var sum = 0;
var pairs = new List<(int, int)>();

for (int i = 4; i < lines.Length; i++) {
    var prev = lines[(i - 4)..(i - 1)].Select(int.Parse).Sum();
    var value = lines[(i - 3)..(i)].Select(int.Parse).Sum();
    sum += prev < value ? 1 : 0;
    pairs.Add((prev, value));

    const int stop = 20;
    if (i < stop) {
        // Console.WriteLine($"prev: {prev}, value: {value}");
        // Console.WriteLine($"Sum: {sum}");
        // Console.WriteLine(lines[(i - 4)..(i - 1)].Aggregate("", (x, y) => $"{x}, {y}"));
        // Console.WriteLine(lines[(i - 3)..(i)].Aggregate("", (x, y) => $"{x}, {y}"));
        Console.Write(lines[(i - 4)..(i - 1)].Select(int.Parse).Sum());
        Console.Write(" ");
        Console.Write(lines[(i - 3)..(i)].Select(int.Parse).Sum());
        Console.WriteLine();
    } else if (i == stop) {
        Console.WriteLine(sum);
    }
}

Console.WriteLine(sum);
// Console.WriteLine(pairs.Count);
