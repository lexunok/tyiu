package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Notification;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface NotificationRepository extends ReactiveCrudRepository<Notification, String> {
}