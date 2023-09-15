package com.tyiu.corn.util.redis;

import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import reactor.core.publisher.Mono;

import java.time.Duration;

@Component
public class RedisCacheTemplate {

    private final ReactiveRedisTemplate<String, Object> reactiveRedisTemplate;

    public RedisCacheTemplate(ReactiveRedisTemplate<String, Object> reactiveRedisTemplate) {
        this.reactiveRedisTemplate = reactiveRedisTemplate;
    }

    public void set(String key, Object value){
        reactiveRedisTemplate.opsForValue().set(key, value, Duration.ofSeconds(600)).subscribe();
    }

    public Mono<?> get(String name, String id){
        return reactiveRedisTemplate.opsForValue().get(name + "_" + id);
    }
}
