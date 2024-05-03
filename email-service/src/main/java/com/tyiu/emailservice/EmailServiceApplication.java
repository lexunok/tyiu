package com.tyiu.emailservice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication(scanBasePackages = {"com.tyiu.emailservice", "com.tyiu.amqp"})
public class EmailServiceApplication {
	public static void main(String[] args) {
		SpringApplication.run(EmailServiceApplication.class, args);
	}

}
