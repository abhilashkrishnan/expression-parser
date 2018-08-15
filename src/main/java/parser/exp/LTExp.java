package parser.exp;

import parser.op.Binary;

public class LTExp<T> extends Exp<T> implements Binary {

	public LTExp() {
		super();
	}

	@Override
	public boolean execute(Exp left, Exp right) {
		return ((Integer) right.val) < ((Integer) left.val) ? true : false;
	}

}
