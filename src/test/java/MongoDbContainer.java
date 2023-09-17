import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.MongoDBContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.shaded.org.hamcrest.MatcherAssert;


@Testcontainers
public class MongoDbContainer {

    @Container
    public static final MongoDBContainer mongoDBContainer;

    static {
        mongoDBContainer =  new MongoDBContainer("mongo:latest").withReuse(true);
        mongoDBContainer.start();

    }
    @Test
    void test(){
        System.out.println(mongoDBContainer.isRunning());
        System.out.println(mongoDBContainer.getReplicaSetUrl());
    }
}
