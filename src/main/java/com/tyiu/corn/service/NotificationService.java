package com.tyiu.corn.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.Notification;
import com.tyiu.corn.repository.NotificationRepository;
@Service
public class NotificationService {

        private final NotificationRepository notificationRepository;

        @Autowired
        public NotificationService(NotificationRepository notificationRepository) {
            this.notificationRepository = notificationRepository;
        }

        public Notification createNotification(Notification notification) {
            return notificationRepository.save(notification);
        }

        
        
}

