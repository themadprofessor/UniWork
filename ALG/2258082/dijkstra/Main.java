import java.io.*;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 program to find shortest path using Dijkstra's algorithm
 */
public class Main {

	public static void main(String[] args) throws IOException {

		long start = System.currentTimeMillis();

		String inputFileName = args[0]; // input file name
  
		FileReader reader = new FileReader(inputFileName);
		Scanner in = new Scanner(reader);
		
		// read in the data here and create graph here
		Graph graph = new Graph(Integer.parseInt(in.nextLine()));
        for (int i = 0; i < graph.size(); i++) {
            String[] split = in.nextLine().split(" ");
            for (int j = 0; j < split.length; j++) {
                if (!split[j].equals("0")) {
                    graph.getVerts()[i].addToAdjList(j, Integer.parseInt(split[j]));
                }
            }
        }

        String[] split = in.nextLine().split(" ");
        int start_vert = Integer.parseInt(split[0]);
        int end_vert = Integer.parseInt(split[1]);

		reader.close();

		// do the work here
        LinkedHashSet<Integer> shortest_path = new LinkedHashSet<>();
        int[] distances = IntStream.generate(() -> Integer.MAX_VALUE).limit(graph.size()).toArray();
        distances[start_vert] = 0;
        graph.getVerts()[start_vert].getAdjList().values().forEach(node -> distances[node.getIndex()] = node.getWeight());

        while (shortest_path.size() != graph.size()) {
            List<Vertex> not_in_path = Stream.of(graph.getVerts())
                    .filter(vertex -> !shortest_path.contains(vertex.getIndex()))
                    .sorted(Comparator.comparingInt(vertex -> distances[vertex.getIndex()]))
                    .collect(Collectors.toList());

            Vertex v = not_in_path.get(0);

            shortest_path.add(v.getIndex());
            not_in_path.remove(0);

            not_in_path.stream()
                    .filter(vertex -> v.getAdjList().containsKey(vertex.getIndex()))
                    .forEach(vertex ->
                            distances[vertex.getIndex()] = Math.min(distances[vertex.getIndex()], distances[vertex.getIndex()]+v.getAdjList().get(vertex.getIndex()).getWeight()));
        }



        System.out.println("Shortest distance from vertex " + start_vert + " to vertex " + end_vert + " is ");
        System.out.println("Shortest path: " + shortest_path.stream().map(String::valueOf).collect(Collectors.joining(" ")));


		// end timer and print total time
		long end = System.currentTimeMillis();
		System.out.println("\nElapsed time: " + (end - start) + " milliseconds");
	}

}
