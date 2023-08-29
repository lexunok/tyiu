package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Notification;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface NotificationRepository extends ReactiveCrudRepository<Notification, Long> {
}