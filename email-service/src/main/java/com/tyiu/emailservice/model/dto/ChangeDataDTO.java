package com.tyiu.emailservice.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ChangeDataDTO {
    private String subject;
    private String text;
    private String to;
    private String code;
}
