package com.tyiu.corn.util.redis;

import com.tyiu.corn.model.dto.IdeaDTO;
import org.springframework.stereotype.Component;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;

@Component
public class RedisCacheTemplate {

    private final ReactiveRedisTemplate<String, Object> reactiveRedisTemplate;

    public RedisCacheTemplate(ReactiveRedisTemplate<String, Object> reactiveRedisTemplate) {
        this.reactiveRedisTemplate = reactiveRedisTemplate;
    }

    public Mono<?> get(String name, String id) {
        return reactiveRedisTemplate.opsForValue().get(name + "_" + id);
    }

    public Flux<Object> getList(String name) {
        return reactiveRedisTemplate.opsForList().range(name,0,-1);
    }


    public void delete(String key){
        reactiveRedisTemplate.opsForValue().delete(key).subscribe();
    }

    public void set(String name, String id, Object value){
        reactiveRedisTemplate.opsForValue().set(name + "_" + id, value, Duration.ofSeconds(600)).subscribe();
    }

    public void setList(String key, List<IdeaDTO> values){
        reactiveRedisTemplate.opsForList().rightPushAll(key, values.toArray(new IdeaDTO[0])).then(reactiveRedisTemplate.expire(key, Duration.ofSeconds(600))).subscribe();
    }
}
