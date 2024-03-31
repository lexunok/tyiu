package com.tyiu.ideas;

import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.utility.DockerImageName;

public abstract class TestContainers {
    static PostgreSQLContainer<?> container;
    static {
        container = new PostgreSQLContainer<>(DockerImageName.parse("postgres:latest"))
                .withDatabaseName("hits")
                .withUsername("admin")
                .withPassword("admin");
        container.start();
    }

    @DynamicPropertySource
    static void setProperties(DynamicPropertyRegistry registry){
        String url = "postgresql://" + container.getHost() + ":"
                + container.getMappedPort(5432) + "/" + container.getDatabaseName();
        registry.add("postgres.url", () -> url);
        registry.add("spring.r2dbc.username", container::getUsername);
        registry.add("spring.r2dbc.password", container::getPassword);
    }
}
