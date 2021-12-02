var lines = await File.ReadAllLinesAsync("../input.txt");

var sum = 0;
var pairs = new List<(int, int)>();

for (int i = 1; i < lines.Length; i++) {
    var prev = int.Parse(lines[i - 1]);
    var value = int.Parse(lines[i]);
    sum += prev < value ? 1 : 0;
    pairs.Add((prev, value));

    if (i < 11) {
        // Console.WriteLine($"prev: {prev}, value: {value}");
        // Console.WriteLine($"Sum: {sum}");
    }
}

Console.WriteLine(sum);
Console.WriteLine(pairs.Count);
