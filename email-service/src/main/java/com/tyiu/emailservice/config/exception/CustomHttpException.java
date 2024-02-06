package com.tyiu.emailservice.config.exception;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
public class CustomHttpException extends RuntimeException{
    private String message;
    private int statusCode;
}
