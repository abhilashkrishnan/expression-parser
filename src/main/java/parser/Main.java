package parser;

public class Main {

	public static void main(String[] args) {
		
		if (args.length != 2)
			throw new RuntimeException("Invalid number of arguments");
		
		String script =  args[0]; 
		String json =  args[1]; 
		Parser parser = new Parser();
		parser.parse(json, script);
		boolean result = parser.eval();
		System.out.println(">>>>>>>>>> Expression evaluates to " + result);
	}
}
