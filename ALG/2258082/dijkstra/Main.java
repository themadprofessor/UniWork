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
        int start_index = Integer.parseInt(split[0]);
        int end_index = Integer.parseInt(split[1]);
        Vertex end_vert = graph.getVerts()[end_index];

		reader.close();

		// do the work here
        HashSet<Vertex> unvisited = new HashSet<>(Arrays.asList(graph.getVerts()));
        HashMap<Vertex, Vertex> previous = new HashMap<>(graph.size());
        int[] distances = new int[graph.size()];
        for (int i = 0; i < distances.length; i++) {
            distances[i] = Integer.MAX_VALUE;
        }
        distances[start_index] = 0;

        Vertex closest = graph.getVerts()[start_index];

        while (unvisited.size() > 0) {
            if (closest.equals(end_vert)) {
                break;
            }

            for (Node node : closest.getAdjList()) {
                int new_len = distances[closest.getIndex()] + node.getWeight();
                if (new_len < distances[node.getIndex()]) {
                    previous.put(graph.getVerts()[node.getIndex()], closest);
                    distances[node.getIndex()] = new_len;
                }
            }
            unvisited.remove(closest);
            closest = unvisited.stream()
                    .min(Comparator.comparingInt(vertex -> distances[vertex.getIndex()]))
                    .get();
        }

        if (previous.containsKey(end_vert)) {
            ArrayDeque<Integer> stack = new ArrayDeque<>(previous.size());
            Vertex curr = end_vert;
            while (curr.getIndex() != start_index) {
                stack.push(curr.getIndex());
                curr = previous.get(curr);
            }

            System.out.println("Shortest path between vertex " + start_index + " and vertex " + end_index + " is " + distances[end_index]);
            System.out.print("Shortest path: " + start_index);
            stack.iterator().forEachRemaining(index -> System.out.print(" " + index));
            System.out.print("\n");
        } else {
            System.out.println("No path found");
        }

		// end timer and print total time
		long end = System.currentTimeMillis();
		System.out.println("\nElapsed time: " + (end - start) + " milliseconds");
	}
}
