package com.tyiu.emailservice.model.responses;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ErrorResponse {
    private Integer statusCode;
    private String error;
}
