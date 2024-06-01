package com.tyiu.ideas;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication(scanBasePackages = {"com.tyiu.ideas", "com.tyiu.amqp"})
public class IdeasServiceApplication {
	public static void main(String[] args) {
		SpringApplication.run(IdeasServiceApplication.class, args);
	}
}
