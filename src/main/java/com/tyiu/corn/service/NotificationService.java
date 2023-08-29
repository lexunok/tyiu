package com.tyiu.corn.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.entities.Notification;
import com.tyiu.corn.repository.NotificationRepository;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class NotificationService {

        private final NotificationRepository notificationRepository;

        public Mono<Notification> createNotification(Notification notification) {
            return notificationRepository.save(notification);
        }
        
}

