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
    private final String path = "https://hits.tyuiu.ru/";

    public Flux<NotificationDTO> getAllNotifications(String userId) {
        return template.select(query(where("user_id").is(userId)), Notification.class)
                .flatMap(n -> Mono.just(mapper.map(n, NotificationDTO.class))
                        .flatMap(notificationDTO -> {
                            notificationDTO.setLink(path + n.getLink());
                            return Mono.just(notificationDTO);
                        }));
    }

    public Flux<NotificationDTO> getAllFavouriteNotifications(String userId) {
        return template.select(query(where("user_id").is(userId)
                        .and(where("is_favourite").is(true))),
                        Notification.class)
                .flatMap(n -> Mono.just(mapper.map(n, NotificationDTO.class))
                        .flatMap(notificationDTO -> {
                            notificationDTO.setLink(path + n.getLink());
                            return Mono.just(notificationDTO);
                        }));
    }

    public Mono<NotificationDTO> createNotification(NotificationDTO notificationDTO) {
        Notification notification = mapper.map(notificationDTO, Notification.class);
        notification.setCreatedAt(LocalDateTime.now());
        return template.insert(notification).flatMap(n -> {
            notificationDTO.setId(n.getId());
            notificationDTO.setTitle(n.getTitle());
            notificationDTO.setMessage(n.getMessage());
            notificationDTO.setUserId(n.getUserId());
            notificationDTO.setLink(path + n.getLink());
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

    public Mono<Void> readAllNotifications(String userId) {
        return template.update(query(where("user_id").is(userId)
                                .and(where("is_readed").is(false))),
                        update("is_readed", true),
                        Notification.class).then();
    }

    public Mono<Void> addNotificationToFavourite(String notificationId) {
        return template.update(query(where("id").is(notificationId)),
                update("is_favourite", true)
                        .set("is_readed", true),
                Notification.class).then();
    }

    public Mono<Void> removeNotificationFromFavourite(String notificationId) {
        return template.update(query(where("id").is(notificationId)),
                update("is_favourite", false),
                Notification.class).then();
    }
}
