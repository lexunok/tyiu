package com.tyiu.authorizationservice.model.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
public class EmailChangeData {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;
    @Column(unique = true)
    private String newEmail;
    @Column(unique = true)
    private String oldEmail;
    private String code;
    @Builder.Default
    private Integer wrongTries = 0;
    private LocalDateTime dateExpired;
}
