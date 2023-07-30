package com.tyiu.corn;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@Testcontainers
public class PostgresTest {
    @Container
    private static final PostgreSQLContainer<?> container =
            new PostgreSQLContainer<>("postgres:latest")
                    .withDatabaseName("hits-test")
                    .withUsername("admin")
                    .withPassword("admin");

    @DynamicPropertySource
    private static void registryDataSource(DynamicPropertyRegistry registry){
        registry.add(
                "spring.datasource.url",
                container::getJdbcUrl
        );
        registry.add(
                "spring.datasource.username",
                container::getUsername
        );
        registry.add(
                "spring.datasource.password",
                container::getPassword
        );
    }

    @Test
    public void runningTest(){
        Assertions.assertThat(container.isRunning()).isTrue();
    }
}
