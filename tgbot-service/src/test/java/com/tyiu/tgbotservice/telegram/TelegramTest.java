package com.tyiu.tgbotservice.telegram;

import com.tyiu.tgbotservice.service.BotService;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;

import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class TelegramTest extends TestContainersDB {

    @Autowired
    private BotService bot;

    @Autowired
    private  R2dbcEntityTemplate template;

    private void createSQLTables() {

        template.getDatabaseClient()
                .sql("CREATE TABLE IF NOT EXISTS users " +
                        "(id TEXT DEFAULT gen_random_uuid()::TEXT PRIMARY KEY, " +
                        "email TEXT, " +
                        "first_name TEXT, " +
                        "last_name TEXT, " +
                        "roles TEXT[], " +
                        "password TEXT);")
                .fetch()
                .rowsUpdated()
                .block();

        template.getDatabaseClient()
                .sql("CREATE TABLE IF NOT EXISTS users_telegram " +
                        "(user_email TEXT REFERENCES users (email) ON UPDATE CASCADE, " +
                        "user_tag TEXT, " +
                        "chat_id BIGINT, " +
                        "is_visible BOOLEAN DEFAULT false::BOOLEAN);")
                .fetch()
                .rowsUpdated()
                .block();

        template.getDatabaseClient()
                .sql("CREATE TABLE IF NOT EXISTS notification_request " +
                        "(id TEXT, " +
                        "consumer_email TEXT REFERENCES users (email) ON UPDATE CASCADE, " +
                        "consumer_tag TEXT REFERENCES users_telegram (user_tag) ON UPDATE CASCADE, " +
                        "title TEXT, " +
                        "message TEXT, " +
                        "link TEXT, " +
                        "button_name TEXT);")
                .fetch()
                .rowsUpdated()
                .block();
    }
}
