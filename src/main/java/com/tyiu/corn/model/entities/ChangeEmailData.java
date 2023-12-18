package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class ChangeEmailData {
    @Id
    private String id;
    private String newEmail;
    private String oldEmail;
    private Integer code;
    private Integer wrongTries;
    private LocalDateTime dateExpired;

}
