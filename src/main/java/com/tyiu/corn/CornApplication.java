package com.tyiu.corn;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.mongodb.repository.config.EnableReactiveMongoRepositories;

@EnableReactiveMongoRepositories
@SpringBootApplication
public class CornApplication {

	public static void main(String[] args) {
		SpringApplication.run(CornApplication.class, args);
	}

}
