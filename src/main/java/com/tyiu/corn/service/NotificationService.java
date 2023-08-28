package com.tyiu.corn.service;

import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Mono;

import org.springframework.stereotype.Service;

import com.tyiu.corn.model.entities.Notification;
import com.tyiu.corn.repository.NotificationRepository;
@Service
@RequiredArgsConstructor
public class NotificationService {
    /* 
    private final NotificationRepository notificationRepository;
    // После полного перехода на реактивный стэк @CacheEvict(allEntries = true)
    public Mono<Notification> createNotification(Notification notification) {
        return Mono.just(notificationRepository.save(notification));
    }*/

}   