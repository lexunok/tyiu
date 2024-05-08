package com.tyiu.authorizationservice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;

@SpringBootApplication(scanBasePackages = {"com.tyiu.authorizationservice", "com.tyiu.amqp"})
@EnableFeignClients(basePackages = "com.tyiu.client.connections")
public class AuthorizationServiceApplication {
	//TODO: Реализовать запросы которые раньше были в аккаунт чендже, но только те что реал нужны, удаление пользователя, список всех и тд
	//TODO: LAST Показывать ошибки на страничках и сделать валидацию для логина
	public static void main(String[] args) {
		SpringApplication.run(AuthorizationServiceApplication.class, args);
	}
}
