package benchmarks;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.PrimitiveIterator;
import java.util.Random;

import static java.util.stream.Collectors.toList;

public class ChunkedGeneratorForBenchmark {
    public static void generate(String fromFile, String toFile, int chunkSize, boolean isFixed) throws IOException {
        List<Character> list = Files.readAllLines(Paths.get(fromFile))
                .stream()
                .flatMapToInt(String::codePoints)
                .mapToObj(i -> (char) i)
                .collect(toList());
        PrimitiveIterator.OfInt ite;
        if(isFixed) {
            ite = new PrimitiveIterator.OfInt() {
                @Override
                public int nextInt() {
                    return chunkSize;
                }

                @Override
                public boolean hasNext() {
                    return true;
                }
            };
        }
        else {
            ite = new Random().ints(1, chunkSize + 1).iterator();
        }
        int counter = 0;
        try (PrintWriter out = new PrintWriter(toFile)) {
            while(counter < list.size()) {
                int num = ite.next();
                int max = Math.min(num, list.size() - counter);
                out.print("\n" + max +"\n");
                for (int i = 0; i < max; i++) {
                    out.print(list.get(counter));
                    counter++;
                }
            }
        }
        assert counter == list.size();
    }
}
