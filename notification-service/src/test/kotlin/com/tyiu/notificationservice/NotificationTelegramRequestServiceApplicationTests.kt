package com.tyiu.notificationservice

import com.tyiu.notificationservice.model.NotificationRepository
import kotlinx.coroutines.flow.onCompletion
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.api.extension.ExtendWith
import org.junit.jupiter.api.fail
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.junit.jupiter.SpringExtension
import org.springframework.test.web.reactive.server.WebTestClient

@ExtendWith(SpringExtension::class)
@SpringBootTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureWebTestClient
class NotificationTelegramRequestServiceApplicationTests(
	@Autowired private val webClient: WebTestClient,
	@Autowired private val notificationRepository: NotificationRepository,
) {
	@Test
	fun contextLoads() {
		webClient.get().uri("/api/v1/notification/p").exchange()
		runBlocking {
			notificationRepository.findAllUnreadNotificationsByTag("wwr").onCompletion {
				fail("fail")
			}
		}
	}

}
