package com.tyiu.corn.util.redis;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;

@Component
@Slf4j
@RequiredArgsConstructor
public class RedisCacheTemplate {

    private final ReactiveRedisTemplate<String, Object> reactiveRedisTemplate;

    //Проверка есть ли данные в кэше, если есть то возврат данных иначе пусто
    public Mono<?> get(String name, String id) {
        String key = name + "_" + id;
        return isExists(key).flatMap(hasKey -> {
            if (hasKey) {
                log.info("in redis");
                return reactiveRedisTemplate.opsForValue().get(key);
            }
            else return Mono.empty();
        });
    }
    // Есть ли в кэше данные по этому ключу
    private Mono<Boolean> isExists(String key) {
        return reactiveRedisTemplate.hasKey(key);
    }
    // Устанавливает кэш по ключу
    public Mono<Boolean> set(String name, String id, Object value){
        return reactiveRedisTemplate.opsForValue()
                .set(name + "_" + id, value, Duration.ofSeconds(600));
    }

    public Mono<Boolean> delete(String key){
        return reactiveRedisTemplate.opsForValue().delete(key);
    }


    public Mono<Boolean> setList(String key, Flux<?> values){
        return reactiveRedisTemplate.opsForList()
                .rightPushAll(key, values)
                .then(reactiveRedisTemplate.expire(key, Duration.ofSeconds(600)));
    }
    public Flux<?> getList(String name) {
        return reactiveRedisTemplate.opsForList().range(name,0,-1);
    }
}
