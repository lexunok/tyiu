package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.NotificationDTO;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.service.NotificationService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/notification")
@RequiredArgsConstructor
public class NotificationController {

    private final NotificationService notificationService;

    @GetMapping("/all")
    public Flux<NotificationDTO> getAllNotifications(@AuthenticationPrincipal User user) {
        return notificationService.getAllNotifications(user.getId());
    }

    @GetMapping("/favourite")
    public Flux<NotificationDTO> getAllFavouriteNotifications(@AuthenticationPrincipal User user) {
        return notificationService.getAllFavouriteNotifications(user.getId());
    }

    @PostMapping("/create")
    public Mono<NotificationDTO> createNotification(@RequestBody NotificationDTO notification) {
        return notificationService.createNotification(notification)
                .switchIfEmpty(Mono.error(new NotFoundException("Create is not success!")));
    }

    @PutMapping("/show/{notificationId}")
    public Mono<Void> showNotification(@PathVariable String notificationId) {
        return notificationService.showNotification(notificationId);
    }

    @PutMapping("/read/{notificationId}")
    public Mono<Void> readNotification(@PathVariable String notificationId) {
        return notificationService.readNotification(notificationId);
    }

    @PutMapping("/favourite/{notificationId}")
    public Mono<Void> addNotificationToFavourite(@PathVariable String notificationId) {
        return notificationService.addNotificationToFavourite(notificationId);
    }

    @PutMapping("/unfavourite/{notificationId}")
    public Mono<Void> removeNotificationFromFavourite(@PathVariable String notificationId) {
        return notificationService.removeNotificationFromFavourite(notificationId);
    }
}
