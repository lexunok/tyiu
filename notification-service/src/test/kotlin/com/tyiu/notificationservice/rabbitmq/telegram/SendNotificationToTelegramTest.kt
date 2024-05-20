package com.tyiu.notificationservice.rabbitmq.telegram

import com.tyiu.ideas.model.entities.User
import com.tyiu.notificationservice.model.Notification
import com.tyiu.notificationservice.model.NotificationDTO
import com.tyiu.notificationservice.model.NotificationType
import com.tyiu.notificationservice.rabbitmq.TestContainers
import com.tyiu.notificationservice.service.NotificationService
import com.tyiu.tgbotservice.model.entities.UserTelegram
import interfaces.INotification
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.beans.factory.annotation.Qualifier
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import reactor.core.publisher.Mono
import request.NotificationRequest
import java.time.LocalDateTime

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class SendNotificationToTelegramTest(

    @Qualifier("testTelegramClient")
    private val testNotification: INotification,

    @Autowired
    private val template: R2dbcEntityTemplate,

    @Autowired
    private val notificationService: NotificationService

): TestContainers() {

    private fun createSQLTables() {

        template.databaseClient
            .sql("CREATE TABLE IF NOT EXISTS users " +
                    "(id TEXT DEFAULT gen_random_uuid()::TEXT PRIMARY KEY, " +
                    "email TEXT, " +
                    "first_name TEXT, " +
                    "last_name TEXT, " +
                    "roles TEXT[], " +
                    "password TEXT);")
            .fetch()
            .rowsUpdated()
            .block()

        template.databaseClient
            .sql("CREATE TABLE IF NOT EXISTS users_telegram " +
                    "(user_email TEXT, " +
                    "user_tag TEXT, " +
                    "chat_id BIGINT, " +
                    "is_visible BOOLEAN DEFAULT false::BOOLEAN);")
            .fetch()
            .rowsUpdated()
            .block()

        template.databaseClient
            .sql("CREATE TABLE IF NOT EXISTS notification " +
                    "(id TEXT, " +
                    "publisher_email TEXT, " +
                    "consumer_email TEXT, " +
                    "consumer_tag TEXT, " +
                    "title TEXT, " +
                    "message TEXT, " +
                    "link TEXT, " +
                    "is_showed BOOLEAN, " +
                    "is_read BOOLEAN, " +
                    "is_favourite BOOLEAN, " +
                    "created_at TEXT, " +
                    "button_name TEXT, " +
                    "notification_type TEXT, " +
                    "is_sent_by_email_service BOOLEAN DEFAULT FALSE, " +
                    "is_sent_by_telegram_service BOOLEAN DEFAULT FALSE);")
            .fetch()
            .rowsUpdated()
            .block()
    }

    private fun setUserInfo(email: String, tag: String): Mono<UserTelegram> {

        val user: UserTelegram = UserTelegram.builder()
            .userEmail(email)
            .userTag(tag)
            .build()

        return template.insert<UserTelegram>(user)
    }

    private fun assertThatNotificationIsSentToTelegram(id: String?) {

        id?.let {

            Mono.just<String>(it)
                .flatMap { dbFind: String? -> template.selectOne(query(where("id").`is`(dbFind!!)),
                    Notification::class.java) }
                .flatMap<Void?> { currentNotification: Notification ->

                    if (!currentNotification.isSentByTelegramService!!)
                        return@flatMap Mono.error(Exception("Неудачная отправка"))

                    else return@flatMap Mono.empty()

                }.then()
        }
    }

    private suspend fun createNotification(id: String?, email: String, tag: String?, something: String?): NotificationRequest {

        val notificationDTO = NotificationDTO(
            id,
            "1",
            email,
            tag,
            something,
            something,
            something,
            false,
            false,
            false,
            LocalDateTime.now(),
            something,
            NotificationType.SUCCESS,
            false,
            false
        )

        val notification = notificationService.createNotification(notificationDTO)

        notificationDTO.consumerEmail?.let {

            Mono.just<String>(it)
                .flatMap { dbFind: String? -> template.exists(query(where("consumer_email").`is`(dbFind!!)),
                    Notification::class.java) }
                .flatMap { notificationExists: Boolean ->

                    if (java.lang.Boolean.FALSE == notificationExists)
                        return@flatMap Mono.error<Any>(Exception("Уведолмение не найдено"))

                    else notificationDTO.id?.let { notificationService.setSentByTelegramServiceFieldTrue(it) }

                    Mono.empty<Any?>()
                }.then()
        }

        assertThatNotificationIsSentToTelegram(notification.id)

        return NotificationRequest.builder()
            .notificationId(notificationDTO.id)
            .consumerEmail(notificationDTO.consumerEmail)
            .tag(notificationDTO.consumerTag)
            .title(notificationDTO.title)
            .message(notificationDTO.message)
            .link(notificationDTO.link)
            .buttonName(notificationDTO.buttonName)
            .build()
    }

    @BeforeAll
    fun setUp() {

        createSQLTables()

        val user = User.builder()
            .email("email")
            .password("password")
            .roles(listOf(Role.MEMBER))
            .build()

        template.insert(user).flatMap { u: User ->
            setUserInfo(u.email, "tag")
        }.block()
    }
    
    @Test
    fun testSuccessfulSending() = runBlocking {

        val notification = createNotification("1", "email", "tag", "https://hits.tyuiu.ru/something/")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testNotification.makeNotification(notification)
        }
        Assertions.assertEquals(
            "Notification (id = 1) was successfully sent to the user with the tag = tag",
            thrown.message
        )
        Assertions.assertEquals(200, thrown.statusCode)
    }

    @Test
    fun testNullNotificationIdException() = runBlocking {

        val notification = createNotification(null, "email", "tag", "https://hits.tyuiu.ru/something/")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testNotification.makeNotification(notification)
        }
        Assertions.assertEquals(
            "Error when sending notification to telegram. Notification id must not be null",
            thrown.message
        )
        Assertions.assertEquals(500, thrown.statusCode)
    }

    @Test
    fun testNotificationContentIsEmpty() = runBlocking {

        val notification = createNotification("2", "email", "tag", null)

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testNotification.makeNotification(notification)
        }
        Assertions.assertEquals("Error when sending notification (id = 2). Notification content must not be null",
            thrown.message)
        Assertions.assertEquals(500, thrown.statusCode)
    }

    @Test
    fun testWrongLinkException() = runBlocking {

        val notification = createNotification("3", "email", "tag", "https://hits.notTYUIU.ru/profile/")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testNotification.makeNotification(notification)
        }
        Assertions.assertEquals("Error when sending notification (id = 3). Link must starting with required path",
            thrown.message)
        Assertions.assertEquals(500, thrown.statusCode)
    }

    @Test
    fun testNullTagException() = runBlocking {

        val notification = createNotification("4", "email", null, "https://hits.tyuiu.ru/something/")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testNotification.makeNotification(notification)
        }
        Assertions.assertEquals("Error when sending notification (id = 4) to user. Tag must be not null",
            thrown.message)
        Assertions.assertEquals(404, thrown.statusCode)
    }

    @Test
    fun testNotificationForAnotherUserException() = runBlocking {

        val notification = createNotification("5", "not-my-email", "not-my-tag", "https://hits.tyuiu.ru/something")

        val thrown = Assertions.assertThrows(CustomHttpException::class.java) {
            testNotification.makeNotification(notification)
        }
        Assertions.assertEquals(
            "Error when sending notification (id = 5) to user. " +
                    "This notification intended for another user", thrown.message
        )
        Assertions.assertEquals(404, thrown.statusCode)
    }
}
