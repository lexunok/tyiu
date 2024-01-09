package com.tyiu.ideas.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ChangeEmailDataDTO {
    private String newEmail;
    private String oldEmail;
    private String code;
    private Integer wrongTries;
    private LocalDateTime dateExpired;
}
