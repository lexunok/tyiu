package com.tyiu.corn;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;

@SpringBootApplication
public class CornApplication {

	public static void main(String[] args) {
		SpringApplication.run(CornApplication.class, args);
	}

}
