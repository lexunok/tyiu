package com.tyiu.notificationservice.rabbitmq

import org.springframework.test.context.DynamicPropertyRegistry
import org.springframework.test.context.DynamicPropertySource
import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.junit.jupiter.Container

open class TestContainers {

    companion object {

        @Container
        val container = PostgreSQLContainer<Nothing>(
            "postgres:latest")
            .apply {
            withDatabaseName("hits")
            withUsername("admin")
            withPassword("admin")

                start()
        }

        @JvmStatic
        @DynamicPropertySource
        fun properties(registry: DynamicPropertyRegistry) {

            val url = ("postgresql://" + container.host + ":"
                    + container.getMappedPort(5432) + "/" + container.databaseName)

            registry.add("postgres.url") { url }
            registry.add("spring.r2dbc.username") { container.username }
            registry.add("spring.r2dbc.password") { container.password }
        }
    }
}
