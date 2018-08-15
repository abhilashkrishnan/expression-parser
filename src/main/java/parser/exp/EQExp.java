package parser.exp;

import parser.op.Binary;

public class EQExp<T> extends Exp<T> implements Binary {

	public EQExp() {
		super();
	}
	
	@Override
	public boolean execute(Exp left, Exp right) {

		if (left instanceof StringExp && right instanceof StringExp) {
			return ((String) left.val).equals((String) right.val);
		} else if (left instanceof IntegerExp && right instanceof IntegerExp) {
			return ((int) left.val) == ((int) right.val);
		}
		
		return false;
	}

}
