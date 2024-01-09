package com.tyiu.ideas.config.exception;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class CustomHttpException extends RuntimeException{
    private String message;
    private int statusCode;
}
