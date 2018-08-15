package parser.op;

public enum Op {

	AND("AND"),
	OR("OR"),
	EQ("EQ"),
	NOT("NOT"),
	IN("IN"),
	LT("LT"),
	GT("GT");
	
	private String op;
	
	private Op(String op) {
		this.op = op;
	}
	
	public String getOperator() {
		return this.op;
	}
}
