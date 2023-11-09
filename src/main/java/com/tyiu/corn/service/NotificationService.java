package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.NotificationDTO;
import com.tyiu.corn.model.entities.Notification;
import lombok.RequiredArgsConstructor;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
public class NotificationService {

    private final R2dbcEntityTemplate template;

    public Flux<NotificationDTO> getAllNotifications(Long userId) {
        return template.select(query(where("user_id").is(userId)), Notification.class)
                .flatMap(n -> Flux.just(NotificationDTO.builder()
                        .id(n.getId())
                        .title(n.getTitle())
                        .message(n.getMessage())
                        .isShowed(false)
                        .isReaded(n.isReaded())
                        .isFavourite(n.isFavourite())
                        .createdAt(n.getCreatedAt())
                        .build()
                ));
    }

    public Flux<NotificationDTO> getAllFavouriteNotifications(Long userId) {
        return template.select(query(where("user_id").is(userId)
                        .and(where("is_favourite").isTrue())), Notification.class)
                .flatMap(n -> Flux.just(NotificationDTO.builder()
                        .id(n.getId())
                        .title(n.getTitle())
                        .message(n.getMessage())
                        .userId(n.getUserId())
                        .isShowed(true)
                        .isReaded(n.isReaded())
                        .isFavourite(true)
                        .createdAt(n.getCreatedAt())
                        .build()
                ));
    }

    public Mono<Notification> createNotification(Notification notification) {
        return template.insert(notification).flatMap(n -> {
            notification.setId(n.getId());
            notification.setTitle(n.getTitle());
            notification.setMessage(n.getMessage());
            notification.setUserId(n.getUserId());
            notification.setShowed(n.isShowed());
            notification.setReaded(n.isReaded());
            notification.setFavourite(n.isFavourite());
            notification.setCreatedAt(LocalDateTime.now());
            return Mono.just(notification);
        });
    }

    public Mono<Void> addNotificationToFavourite(Long userId, Long notificationId) {
        return template.update(query(where("user_id").is(userId)
                .and(where("id").is(notificationId))),
                update("is_showed", true)
                        .set("is_readed", true)
                        .set("is_favourite", true),
                Notification.class).then();
    }

    public Mono<Void> removeNotificationFromFavourite(Long userId, Long notificationId) {
        return template.update(query(where("user_id").is(userId)
                .and(where("id").is(notificationId))),
                update("is_showed", true)
                        .set("is_readed", true)
                        .set("is_favourite", false),
                Notification.class).then();
    }

    public Mono<Void> showNotification(Long notificationId) {
        return template.update(query(where("id").is(notificationId)),
                update("is_showed", true),
                Notification.class).then();
    }

    public Mono<Void> readNotification(Long notificationId) {
        return template.update(query(where("id").is(notificationId)),
                update("is_showed", true)
                        .set("is_readed", true),
                Notification.class).then();
    }
}
