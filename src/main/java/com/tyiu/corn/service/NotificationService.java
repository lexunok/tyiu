package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.NotificationDTO;
import com.tyiu.corn.model.entities.Notification;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
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
    private final ModelMapper mapper;

    public Flux<NotificationDTO> getAllNotifications(String userId) {
        return template.select(query(where("user_id").is(userId)), Notification.class)
                .flatMap(n -> Mono.just(mapper.map(n, NotificationDTO.class)));
    }

    public Flux<NotificationDTO> getAllFavouriteNotifications(String userId) {
        return template.select(query(where("user_id").is(userId)
                        .and(where("is_favourite").is(true))),
                        Notification.class)
                .flatMap(n -> Mono.just(mapper.map(n, NotificationDTO.class)));
    }

    public Mono<NotificationDTO> createNotification(NotificationDTO notificationDTO) {
        Notification notification = mapper.map(notificationDTO, Notification.class);
        notification.setCreatedAt(LocalDateTime.now());
        return template.insert(notification).flatMap(n -> {
            notificationDTO.setId(n.getId());
            notificationDTO.setTitle(n.getTitle());
            notificationDTO.setMessage(n.getMessage());
            notificationDTO.setUserId(n.getUserId());
            notificationDTO.setIsShowed(n.getIsShowed());
            notificationDTO.setIsReaded(n.getIsReaded());
            notificationDTO.setIsFavourite(n.getIsFavourite());
            notificationDTO.setCreatedAt(n.getCreatedAt());
            return Mono.just(notificationDTO);
        });
    }

    public Mono<Void> showNotification(String notificationId) {
        return template.update(query(where("id").is(notificationId)),
                update("is_showed", true),
                Notification.class).then();
    }

    public Mono<Void> readNotification(String notificationId) {
        return template.update(query(where("id").is(notificationId)),
                update("is_readed", true),
                Notification.class).then();
    }

    public Mono<Void> addNotificationToFavourite(String notificationId) {
        return template.update(query(where("id").is(notificationId)),
                update("is_favourite", true),
                Notification.class).then();
    }

    public Mono<Void> removeNotificationFromFavourite(String notificationId) {
        return template.update(query(where("id").is(notificationId)),
                update("is_favourite", false),
                Notification.class).then();
    }
}
