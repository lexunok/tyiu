package com.tyiu.corn.exception;

import com.tyiu.corn.exception.NotFoundException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import reactor.core.publisher.Mono;

@ControllerAdvice
@Slf4j
public class GlobalExceptionHandler {
    @ExceptionHandler
    public Mono<ResponseEntity<String>> globalException(NotFoundException ex) {
        log.error(ex.getMessage(), ex);
        return Mono.just(ResponseEntity.status(HttpStatus.BAD_REQUEST.value()).body(ex.getMessage()));
    }
}
