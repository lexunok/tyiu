package com.tyiu.emailservice;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest
public class EmailServiceApplicationTests {

	@Value("${test-mail.username}")
	private String testEmail;
	@Value("${test-mail.password}")
	private String password;

	@Test
    void testApplicationYml() {

	}

}
