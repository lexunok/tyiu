package com.tyiu.emailservice.config.exception;

import com.tyiu.ideas.model.responses.ErrorResponse;
import com.tyiu.ideas.config.exception.CustomHttpException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import reactor.core.publisher.Mono;

@ControllerAdvice
@Slf4j
public class GlobalExceptionHandler {
    @ExceptionHandler
    public Mono<ResponseEntity<ErrorResponse>> customHttpException(CustomHttpException ex){
        log.error(ex.getMessage(), ex.getStatusCode(), ex);
        return Mono.just(ResponseEntity
                .status(ex.getStatusCode())
                .body(new ErrorResponse(ex.getStatusCode() ,ex.getMessage())));
    }
}
