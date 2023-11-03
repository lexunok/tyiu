package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.NotificationDTO;
import com.tyiu.corn.service.NotificationService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;

@RestController
@RequestMapping("/api/v1/notification")
@RequiredArgsConstructor
public class NotificationController {

    private final NotificationService notificationService;

    @GetMapping("/all")
    public Flux<NotificationDTO> getAllNotifications(Principal principal) {
        return notificationService.getAllNotifications(Long.valueOf(principal.getName()));
    }

    @GetMapping("/favourite")
    public Flux<NotificationDTO> getAllFavouriteNotifications(Principal principal) {
        return notificationService.getAllFavouriteNotifications(Long.valueOf(principal.getName()));
    }

    @PostMapping("/create")
    public Mono<NotificationDTO> createNotification(@RequestBody NotificationDTO notification) {
        return notificationService.createNotification(notification)
                .switchIfEmpty(Mono.error(new NotFoundException("Create is not success!")));
    }

    @PostMapping("/favourite/{notificationId}")
    public Mono<Void> addNotificationToFavourite(Long userId, Long notificationId) {
        return notificationService.addNotificationToFavourite(userId, notificationId);
    }

    @DeleteMapping("/unfavourite/{notificationId}")
    public Mono<Void> removeNotificationFromFavourite(Long userId, Long notificationId) {
        return notificationService.removeNotificationFromFavourite(userId, notificationId);
    }

    @PutMapping("/{notificationId}")
    public Mono<Void> readNotification(Principal principal) {
        return notificationService.readNotification(Long.valueOf(principal.getName()));
    }
}
