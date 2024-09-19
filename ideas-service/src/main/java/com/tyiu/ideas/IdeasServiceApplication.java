package com.tyiu.ideas;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@OpenAPIDefinition
@SpringBootApplication(scanBasePackages = {"com.tyiu.ideas", "com.tyiu.amqp"})
public class IdeasServiceApplication {
	public static void main(String[] args) {
		SpringApplication.run(IdeasServiceApplication.class, args);
	}
}
