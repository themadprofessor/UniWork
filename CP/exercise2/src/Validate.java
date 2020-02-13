import java.io.*;
import java.util.*;

public class Validate {

	int[] period; // when a course is delivered
	int[][] curriculum; // curriculum[p][c] =1 <-> course c in period p

	int nCourses, nPeriods, minCredits, maxCredits, minCourses, maxCourses;
	String[] name;               // name[i] is name of the ith course
	int[] credits;               // credits[i] is the credits for the ith course
	ArrayList<Integer>[] prereq; // j is in prereq[i] iff the jth course is a prerequisite for the ith course
	Scanner sc;

	public Validate(String fnameProblem,String fnameSolution) throws IOException {
		readBACP(fnameProblem);
		curriculum = new int[nPeriods][nCourses];
		period = new int[nCourses];
		readSolution(fnameSolution);
	}

	void readBACP(String fname) throws IOException {
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

	void readCourses() throws IOException {
		sc.next(); // skip "courses"
		for (int i=0;i<nCourses;i++){
			name[i]    = sc.next();
			credits[i] = sc.nextInt();
			prereq[i]  = new ArrayList<>();
		}
	}

	int find(String s,String[] dict){
		for (int i=0;i<dict.length;i++)
			if (s.equals(dict[i])) return i;
		return -1;
	}

	void readPrerequisites() throws IOException {
		sc.next(); // skip "prerequisites:"
		while (sc.hasNext()){
			String name1 = sc.next();
			String name2 = sc.next();
			int i = find(name1,name);
			int j = find(name2,name);
			if (!prereq[i].contains(j)) prereq[i].add(j);
		}
	}

	void readSolution(String fnameSolution) throws IOException {
		Scanner sc = new Scanner(new File(fnameSolution));
		for (int i=0;i<nCourses;i++){
			String courseName = sc.next();
			int courseIndex = find(courseName,name);
			if (courseIndex == -1){
				System.out.println("Unknown course "+ Arrays.toString(name));
				return;
			}
			period[courseIndex] = sc.nextInt();
		}
		sc.close();
	}

	boolean validate() {
		boolean valid = true;
		int totalCredits = 0;
		int maxCreditsInAllPeriods = -1;
		for (int i=0;i<nCourses;i++)
			if (0 <= period[i] && period[i] <= nPeriods)
				curriculum[period[i]][i] = 1;
			else
				System.out.println("Course "+ name[i] +" illegal period "+ period[i]);
		// are prerequisites respected?
		for (int i=0;i<nCourses;i++)
			for (Integer j : prereq[i])
				if (period[i] <= period[j]){
					System.out.println("Prerequisites violation "+ name[i] +" "+ name[j]);
					valid = false;
				}
		// is credit limit respected?
		for (int p=0;p<nPeriods;p++){
			int creditsInPeriod = 0;
			for (int i=0;i<nCourses;i++)
				if (period[i] == p) creditsInPeriod = creditsInPeriod + credits[i];
			if (creditsInPeriod < minCredits || creditsInPeriod > maxCredits){
				System.out.println("Credit violation in period "+ p);
				valid = false;
			}
			totalCredits = totalCredits + creditsInPeriod;
			maxCreditsInAllPeriods = Math.max(maxCreditsInAllPeriods,creditsInPeriod);
		}
		// is course limit respected?
		for (int p=0;p<nPeriods;p++){
			int coursesInPeriod = 0;
			for (int i=0;i<nCourses;i++)
				if (period[i] == p) coursesInPeriod++;
			if (coursesInPeriod < minCourses || coursesInPeriod > maxCourses){
				System.out.println("Course violation in period "+ p);
				valid = false;
			}
		}
		return valid;
	}

	void show() {
		int maxCreditsInAllPeriods = Integer.MIN_VALUE;
		int minCreditsInAllPeriods = Integer.MAX_VALUE;
		int totalCredits = 0;
		for (int p=0;p<nPeriods;p++){
			System.out.print("Period_" + p +" ");
			int creditsInPeriod = 0;
			int coursesInPeriod = 0;
			for (int i=0;i<nCourses;i++)
				if (curriculum[p][i] == 1){
					System.out.print(name[i] +"(" + credits[i] +") ");
					creditsInPeriod = creditsInPeriod + credits[i];
					totalCredits = totalCredits + credits[i];
					coursesInPeriod++;
				}
			System.out.println();
			System.out.println("Period_" + p + " credits: " + creditsInPeriod +" courses: "+ coursesInPeriod);
			maxCreditsInAllPeriods = Math.max(maxCreditsInAllPeriods,creditsInPeriod);
			minCreditsInAllPeriods = Math.min(minCreditsInAllPeriods,creditsInPeriod);
		}
		System.out.println("Imbalance: "+ (maxCreditsInAllPeriods - minCreditsInAllPeriods));
		System.out.println("Max credit load: "+ maxCreditsInAllPeriods);
		System.out.println("Total credits : "+ totalCredits);
	}

	public static void main(String[] args) throws IOException {
		Validate bacp = new Validate(args[0],args[1]);
		System.out.println(bacp.validate());
		bacp.show();
	}
}
