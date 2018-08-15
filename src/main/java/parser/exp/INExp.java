package parser.exp;

import parser.op.Unary;

public class INExp<T> extends Exp<T> implements Unary {

	public INExp() {
		super();
	}

	@Override
	public boolean execute(Exp left, Exp right) {
		
		if (left instanceof StringExp && right instanceof StringExp) {
			return ((StringExp) left).getEntries().stream().
					anyMatch(s -> s.equals(((StringExp) right).val));
		}
		
		return false;
	}

}
