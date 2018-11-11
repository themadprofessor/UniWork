import java.io.*;
import java.util.*;

/**
 program to find shortest path using the backtrack search algorithm
 */
public class Main {
    private static Path BEST_PATH = Path.max_path();

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

		reader.close();

		// do the work here
        Path p = new Path(graph, start_index);
        boolean[] visited = new boolean[graph.size()];
	visited[start_index] = true;

        backtrack(p, visited, graph, end_index);

        System.out.println("Shortest path between vertex " + start_index + " and vertex " + end_index + " is " + BEST_PATH.weight);
        System.out.println("Shortest path: " + BEST_PATH.path());

		// end timer and print total time
		long end = System.currentTimeMillis();
		System.out.println("\nElapsed time: " + (end - start) + " milliseconds");
	}

	private static void backtrack(Path curr, boolean[] visited, Graph graph, int end_index) {
	    Vertex prev = graph.getVerts()[curr.lastVertex()];
        for (Node node : prev.getAdjList()) {
            if (!visited[node.getIndex()]) {
                curr.pushEdge(node);
                visited[node.getIndex()] = true;

                if (curr.weight < BEST_PATH.weight) {
                    if (node.getIndex() == end_index) {
                        BEST_PATH = (Path) curr.clone();
                    } else {
                        backtrack(curr, visited, graph, end_index);
                    }
                }

                curr.popEdge(node);
                visited[node.getIndex()] = false;
            }
        }
    }
}
