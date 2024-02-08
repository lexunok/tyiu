package com.tyiu.notificationservice.rabbitmq.email

import com.tyiu.ideas.config.exception.CustomHttpException
import interfaces.INotification
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import org.springframework.beans.factory.annotation.Qualifier
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.annotation.DirtiesContext
import org.testcontainers.junit.jupiter.Testcontainers
import request.NotificationRequest

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DirtiesContext
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@AutoConfigureWebTestClient
class SendNotificationToEmailServiceTest(

    @Qualifier("testEmailClient")
    private val testRabbitMQ: INotification

) {

    private fun createNotification(id: String?, email: String?, tag: String, something: String?): NotificationRequest {

        return NotificationRequest.builder()
            .notificationId(id)
            .consumerEmail(email)
            .tag(tag)
            .title(something)
            .message(something)
            .link(something)
            .buttonName(something)
            .build()
    }

    @Test
    fun testNullNotificationException() {

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testRabbitMQ.makeNotification(null)
        }
        Assertions.assertEquals("Notification is null", thrown.message)
        Assertions.assertEquals(500, thrown.statusCode)
    }

    @Test
    fun testSuccessfulSending() {

        val notification = createNotification("1", "email", "tag", "bla-bla-bla")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testRabbitMQ.makeNotification(notification)
        }
        Assertions.assertEquals(
            "Notification (id = 1) was successfully sent to the user with the email = email",
            thrown.message
        )
        Assertions.assertEquals(200, thrown.statusCode)
    }

    @Test
    fun testNullNotificationIdException() {

        val notification = createNotification(null, "email", "tag", "bla-bla-bla")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testRabbitMQ.makeNotification(notification)
        }
        Assertions.assertEquals(
            "Error when sending notification to email service. Notification id must not be null",
            thrown.message
        )
        Assertions.assertEquals(500, thrown.statusCode)
    }

    @Test
    fun testNotificationContentIsEmpty() {

        val notification = createNotification("2", "email", "tag", null)

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testRabbitMQ.makeNotification(notification)
        }
        Assertions.assertEquals("Error when sending notification (id = 2). Notification content must not be null",
            thrown.message)
        Assertions.assertEquals(500, thrown.statusCode)
    }

    @Test
    fun testWrongLinkException() {

        val notification = createNotification("3", "email", "tag", "https://hits.notTYUIU.ru/profile/")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testRabbitMQ.makeNotification(notification)
        }
        Assertions.assertEquals("Error when sending notification (id = 3). Link must starting with required path",
            thrown.message)
        Assertions.assertEquals(500, thrown.statusCode)
    }

    @Test
    fun testNullEmailException() {

        val notification = createNotification("4", null, "tag", "bla-bla-bla")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testRabbitMQ.makeNotification(notification)
        }
        Assertions.assertEquals("Error when sending notification (id = 4) to user. Email must be not null",
            thrown.message)
        Assertions.assertEquals(404, thrown.statusCode)
    }
}
