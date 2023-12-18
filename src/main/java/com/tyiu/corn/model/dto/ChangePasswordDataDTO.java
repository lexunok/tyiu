package com.tyiu.corn.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ChangePasswordDataDTO {
    private String email;
    private Integer code;
    private Integer wrongTries;
    private LocalDateTime dateExpired;
}
