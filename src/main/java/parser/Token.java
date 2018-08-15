package parser;

public enum Token {

	
	START("["),
	TERMINAL("]"),
	DOT("."),
	FORWARD_SLASH("/"),
	EMPTY(""),
	COMMA(",");
	
	private String tok;
	
	private Token(String tok) {
		this.tok = tok;
	}
	
	public String getToken() {
		return this.tok;
	}
}
