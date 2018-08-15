package parser.exp;

public class Exp<T> {

	T val;
	
	public Exp() {
		
	}
	
	public Exp(T t) {
		this.val = t;
	}

	public T getVal() {
		return val;
	}

	public void setVal(T val) {
		this.val = val;
	}
	
}
