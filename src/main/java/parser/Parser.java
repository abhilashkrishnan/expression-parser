package parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;
import java.util.StringTokenizer;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeType;

import parser.exp.BoolExp;
import parser.exp.Exp;
import parser.exp.INExp;
import parser.exp.IntegerExp;
import parser.exp.NOTExp;
import parser.exp.StringExp;
import parser.op.Binary;
import parser.op.Nary;
import parser.op.Op;
import parser.op.OpFactory;
import parser.op.Unary;
import parser.util.Json;

/**
 * Recursive Descent Parser using Top Down Approach. Recursive Descent Parsers
 * are faster than AST - Abstract Syntax Tree. Parser is able to
 * parse complex and nested expressions.
 * 
 * @author Abhilash Krishnan
 * @since August 16, 2018
 */
public class Parser {

	private Stack<Exp> stack = new Stack<>();
	private LinkedList<String> tokens = new LinkedList<>();
	private String json;

	public void parse(String json, String script) {

		StringBuilder sb = new StringBuilder();

		try (BufferedReader br = new BufferedReader(new FileReader(new File(json)));) {
			String s;
			while ((s = br.readLine()) != null)
				sb.append(s);
		} catch (IOException e) {
			throw new RuntimeException("Json file is unavailable");
		}

		this.json = sb.toString();

		try (Scanner scanner = new Scanner(new File(script))) {

			scanner.useDelimiter(",");

			while (scanner.hasNext()) {

				String tok = scanner.next();

				if (tok.contains("[")) {
					StringTokenizer start = new StringTokenizer(tok, "\\[", true);
					while (start.hasMoreTokens()) {
						String s = start.nextToken();
						if (s.contains("\""))
							s = s.replaceAll("\"", "");
						tokens.add(s.trim());
					}
				} else if (tok.contains("]")) {
					StringTokenizer end = new StringTokenizer(tok, "\\]", true);
					while (end.hasMoreTokens()) {
						String s = end.nextToken();
						if (s.contains("\""))
							s = s.replaceAll("\"", "");
						tokens.add(s.trim());
					}
				} else
					tokens.add(tok.replaceAll("\"", "").trim());
			}
			scanner.close();
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Expression file is unavailable");
		}

	}

	public boolean eval() {

		boolean eval = false;

		while (!tokens.isEmpty()) {

			if (start()) {
				skip();
				continue;
			}

			String token = tokens.removeFirst();

			if (operator(token)) {

				Exp op = OpFactory.getOp(token).get();

				if (op instanceof INExp) {
					inOperation(op);
				} else if (op instanceof NOTExp) {
					if (!start()) {
						notOperation(op);
					} else {
						stack.push(op);
						continue;
					}
				} else {
					stack.push(op);
					continue;
				}
			} else {

				if ((tokens.isEmpty() && token.equals(Token.TERMINAL.getToken()))) {

					tokenEmptyAndTerminalOperation();
					
				} else if (!tokens.isEmpty() && token.equals(Token.TERMINAL.getToken())) {

					notTokenEmptyAndTerminalOperation();
					
				} else if (terminal()) {

					terminalOperation();
					continue;
				} else {
					
					notTerminalOperation(token);
				}
			}
		}

		while (!stack.isEmpty()) {
			eval = (boolean) ((BoolExp) stack.pop()).getVal();
		}

		return eval;
	}

	private void inOperation(Exp op) {
		
		stack.push(op);
		String t = tokens.removeFirst();
		JsonNode node = Json.getJsonNode(json, t).get();

		if (node != null || !node.isMissingNode()) {
			String type = Json.getNodeType(node);

			if (type.equals(JsonNodeType.STRING.name())) {
				StringExp<String> sExp = new StringExp<>(node.textValue());
				stack.push(sExp);
			} else if (type.equals(JsonNodeType.NUMBER.name())) {
				IntegerExp<Integer> iExp = new IntegerExp<>(node.intValue());
				stack.push(iExp);
			}
		}

		String val = tokens.removeFirst();
		StringExp<String> entries = new StringExp<>();

		while (!terminal() && !tokens.isEmpty()) {
			val = tokens.removeFirst();
			entries.addEntry(val);
		}
		stack.push(entries);

		stack.push(new BoolExp(execute(stack.pop(), stack.pop(), stack.pop())));

		skip();
		skip();
	}

	private void notOperation(Exp op) {

		String t = tokens.removeFirst();
		JsonNode node = Json.getJsonNode(json, t).get();

		if (node != null || !node.isMissingNode()) {
			String type = Json.getNodeType(node);

			if (type.equals(JsonNodeType.BOOLEAN.name())) {
				NOTExp nExp = new NOTExp<>(node.booleanValue());
				stack.push(new BoolExp<>(nExp.execute(nExp, null)));
				skip();
			}
		}
	}
	
	private void tokenEmptyAndTerminalOperation() {
		
		Exp e1 = stack.pop();
		Exp e2 = stack.pop();
		Exp e3 = stack.peek();

		if (!(e3 instanceof Binary) || !(e3 instanceof Unary) || !(e3 instanceof Nary)) {

			List<Exp> exps = new ArrayList<>();
			exps.add(e1);
			exps.add(e2);

			while (!empty()) {
				e3 = previous();
				if ((e3 instanceof Unary) || (e3 instanceof Binary) || (e3 instanceof Nary)) {
					stack.push(new BoolExp(execute(exps, e3)));
					break;
				} else {
					exps.add(e3);
				}
			}
		}
	}

	private void notTokenEmptyAndTerminalOperation() {
		
		List<Exp> exps = new ArrayList<>();
		Exp e = stack.peek();

		while (true) {
			e = previous();
			if ((e instanceof Unary) || (e instanceof Binary) || (e instanceof Nary)) {
				stack.push(new BoolExp(execute(exps, e)));
				break;
			} else {
				exps.add(e);
			}
		}
	}
	
	private void terminalOperation() {
		Exp left = stack.pop();
		Exp right = stack.pop();
		Exp op = stack.pop();
		stack.push(new BoolExp(execute(left, right, op)));
	}
	
	private void notTerminalOperation(String token) {
		
		JsonNode node = Json.getJsonNode(json, token).get();

		if (node != null || !node.isMissingNode()) {
			String type = Json.getNodeType(node);

			if (type.equals(JsonNodeType.STRING.name())) {
				StringExp<String> sExp = new StringExp<>(node.textValue());
				stack.push(sExp);

				String val = tokens.removeFirst();
				StringExp<String> valExp = new StringExp<>(val);
				stack.push(valExp);

				stack.push(new BoolExp(execute(stack.pop(), stack.pop(), stack.pop())));
				if (terminal())
					skip();
			} else if (type.equals(JsonNodeType.NUMBER.name())) {
				IntegerExp<Integer> iExp = new IntegerExp<>(node.intValue());
				stack.push(iExp);

				String val = tokens.removeFirst();
				IntegerExp<Integer> valExp = new IntegerExp<>(Integer.parseInt(val));
				stack.push(valExp);

				stack.push(new BoolExp(execute(stack.pop(), stack.pop(), stack.pop())));
				if (terminal())
					skip();
			}
		}
	}
	
	public boolean execute(List<Exp> exps, Exp op) {
		Exp[] expArr = new Exp[exps.size()];
		int i = 0;
		for (Exp e : exps) {
			expArr[i] = e;
			i++;
		}

		return ((Nary) op).execute(expArr);
	}

	public boolean execute(Exp left, Exp right, Exp op) {
		boolean res = false;

		if (op instanceof Binary) {
			res = ((Binary) op).execute(left, right);
		} else if (op instanceof Unary) {
			res = ((Unary) op).execute(left, right);
		}
		return res;
	}

	public boolean empty() {
		return stack.isEmpty();
	}

	public Exp previous() {
		return stack.pop();
	}

	public boolean start() {
		return tokens.peek().equals(Token.START.getToken());
	}

	public boolean terminal() {

		if (!this.tokens.isEmpty()) {
			if (this.tokens.peek().equals(Token.TERMINAL.getToken())) {
				return true;
			}
		}
		return false;
	}

	public void skip() {
		tokens.removeFirst();
	}

	public boolean operator(String op) {

		if (op.equals(Op.AND.name()) || op.equals(Op.OR.name()) || op.equals(Op.EQ.name()) || op.equals(Op.NOT.name())
				|| op.equals(Op.IN.name()) || op.equals(Op.LT.name()) || op.equals(Op.GT.name()))
			return true;
		return false;
	}
}
