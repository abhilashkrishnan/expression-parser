package parser.util;

import java.io.IOException;
import java.util.Optional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import parser.Token;

public class Json {

	public static Optional<JsonNode> getJsonNode(String json, String key) {
		JsonNode node = null;
		ObjectMapper mapper = new ObjectMapper();

		try {
			JsonNode jn = mapper.readTree(json);
			key = key.replace(Token.DOT.getToken(), Token.FORWARD_SLASH.getToken());
			key = Token.FORWARD_SLASH.getToken() + key;
			node = jn.at(key);
			return Optional.of(node);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return Optional.empty();
	}

	public static String getNodeType(JsonNode node) {
		return node.getNodeType().name();
	}
}
