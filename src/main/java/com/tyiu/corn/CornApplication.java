package com.tyiu.corn;

import com.tyiu.corn.model.dto.CommentDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.mongodb.core.CollectionOptions;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.repository.config.EnableReactiveMongoRepositories;
import org.springframework.http.codec.cbor.Jackson2CborDecoder;
import org.springframework.http.codec.cbor.Jackson2CborEncoder;
import org.springframework.messaging.rsocket.RSocketRequester;
import org.springframework.messaging.rsocket.RSocketStrategies;
import reactor.core.publisher.Mono;

import java.net.URI;

@EnableReactiveMongoRepositories
@SpringBootApplication
public class CornApplication implements CommandLineRunner {

	@Autowired
	private final ReactiveMongoTemplate template;

	public CornApplication(ReactiveMongoTemplate template) {
		this.template = template;
	}

	public static void main(String[] args) {
		SpringApplication.run(CornApplication.class, args);
	}

	@Override
	public void run(String... args) throws Exception {
		template.collectionExists("comment").flatMap(b -> {
			if (b){
				template.executeCommand("{\"convertToCapped\": \"comment\", size: 100000}")
						.log()
						.subscribe();
			}
			return Mono.empty();
		});
	}
}
