package parser;

public class Main {

	public static void main(String[] args) {
		
		//String script = "E:\\blockchain\\zeta\\src\\main\\resources\\exp.txt";
		//String json = "E:\\\\blockchain\\\\zeta\\\\src\\\\main\\\\resources\\\\json.txt";
		
		if (args.length != 2)
			throw new RuntimeException("Invalid number of arguments");
		
		String script =  args[0]; //"E:/blockchain/zeta/scripts/3.exp";
		String json =  args[1]; //"E:/blockchain/zeta/scripts/data.json";
		Parser parser = new Parser();
		parser.parse(json, script);
		boolean result = parser.eval();
		System.out.println(">>>>>>>>>> Expression evaluates to " + result);
	}
}
