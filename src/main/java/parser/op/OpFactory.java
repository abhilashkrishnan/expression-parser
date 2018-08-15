package parser.op;

import java.util.Optional;

import parser.exp.ANDExp;
import parser.exp.EQExp;
import parser.exp.Exp;
import parser.exp.GTExp;
import parser.exp.INExp;
import parser.exp.LTExp;
import parser.exp.NOTExp;
import parser.exp.ORExp;

public class OpFactory {

	public static Optional<Exp> getOp(String op) {
		
		if (op.equals(Op.AND.name())) {
			return Optional.of(new ANDExp<>()); 
		} else if (op.equals(Op.OR.name())) {
			return Optional.of(new ORExp<>());
		} else if (op.equals(Op.EQ.name())) {
			return Optional.of(new EQExp<>());
		} else if (op.equals(Op.NOT.name())) {
			return Optional.of(new NOTExp<>());
		} else if (op.equals(Op.IN.name())) {
			return Optional.of(new INExp<>());
		} else if (op.equals(Op.LT.name())) {
			return Optional.of(new LTExp<>());
		} else if (op.equals(Op.GT.name())) {
			return Optional.of(new GTExp<>());
		}
		
		return Optional.empty();
	}
}
