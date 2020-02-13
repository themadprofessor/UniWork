import java.io.*;
import java.util.*;

public class Go {

	static int nCourses, nPeriods, minCredits, maxCredits, minCourses, maxCourses;
	static String[] name;               // name[i] is name of the ith course
	static int[] credits;               // credits[i] is the credits for the ith course
	static ArrayList<Integer>[] prereq; // j is in prereq[i] iff the jth course is a prerequisite for the ith course
	static Scanner sc;

	static void readBACP(String fname) throws IOException {
		sc = new Scanner(new File(fname));
		sc.next(); nPeriods   = sc.nextInt();
		sc.next(); nCourses   = sc.nextInt();
		sc.next(); minCredits = sc.nextInt();
		sc.next(); maxCredits = sc.nextInt();
		sc.next(); minCourses = sc.nextInt();
		sc.next(); maxCourses = sc.nextInt();

		name    = new String[nCourses];
		credits = new int[nCourses];
		prereq  = new ArrayList[nCourses];

		readCourses();
		readPrerequisites();
		sc.close();
	}

	static void readCourses() throws IOException {
		sc.next(); // skip "courses"
		for (int i=0;i<nCourses;i++){
			name[i]    = sc.next();
			credits[i] = sc.nextInt();
			prereq[i]  = new ArrayList<>();
		}
	}

	static int find(String s,String[] dict){
		for (int i=0;i<dict.length;i++)
			if (s.equals(dict[i])) return i;
		return -1;
	}

	static void readPrerequisites() throws IOException {
		sc.next(); // skip "prerequisites:"
		while (sc.hasNext()){
			String name1 = sc.next();
			String name2 = sc.next();
			int i = find(name1,name);
			int j = find(name2,name);
			if (!prereq[i].contains(j)) prereq[i].add(j);
		}
	}

	public static void main(String[] args)  throws IOException {
		if (args.length == 0 || args[0].equals("-h")){
			System.out.println("Go version 20190131 \n"+
					"FILE          Input problem instance (required)\n" +
					"-time INT     Set CPU time limit in seconds (default infinity) \n"+
					"-solve        Find a first solution (default) \n"+
					"-opt          Optimize  \n" +
					"-brief        Not verbose \n"+
					"-trace        trace \n"+
					"-h            Print this help message");
			return;
		}
		String fname;
		boolean solve = false, optimize = false, brief = false, trace = false;
		long timeLimit = -1;
		fname = args[0];
		for (int i=1;i<args.length;i++){
			if (args[i].equals("-time") || args[i].equals("-t")) timeLimit = 1000 * (long)Integer.parseInt(args[i+1]);
			if (args[i].equals("-solve")) solve = true;
			if (args[i].equals("-opt")) optimize = true;
			if (args[i].equals("-brief")) brief = true;
			if (args[i].equals("-trace")) trace = true;
		}
		readBACP(fname);
		BACP bacp = new BACP(nCourses,nPeriods,minCredits,maxCredits,minCourses,maxCourses,name,credits,prereq);
		bacp.verbose = !brief;
		if (timeLimit > 0) bacp.solver.limitTime(timeLimit);
		if (trace) bacp.solver.showDecisions();
		if (optimize) bacp.optimize();
		else if (solve) bacp.solve();
		bacp.show();
	}
}
	
	
