package com.tyiu.ideas.config.exception;

import com.tyiu.ideas.model.responses.ErrorResponse;
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
    public Mono<ResponseEntity<ErrorResponse>> notFoundException(NotFoundException ex) {
        log.error(ex.getMessage(), ex);
        return Mono.just(ResponseEntity
                .status(HttpStatus.BAD_REQUEST.value())
                .body(new ErrorResponse(HttpStatus.BAD_REQUEST.value(),ex.getMessage())));
    }
    @ExceptionHandler
    public Mono<ResponseEntity<ErrorResponse>> accessException(AccessException ex) {
        log.error(ex.getMessage(), ex);
        return Mono.just(ResponseEntity
                .status(HttpStatus.FORBIDDEN.value())
                .body(new ErrorResponse(HttpStatus.FORBIDDEN.value(),ex.getMessage())));
    }

    @ExceptionHandler
    public Mono<ResponseEntity<ErrorResponse>> customHttpException(CustomHttpException ex){
        log.error(ex.getMessage(), ex.getStatusCode(), ex);
        return Mono.just(ResponseEntity
                .status(ex.getStatusCode())
                .body(new ErrorResponse(ex.getStatusCode() ,ex.getMessage())));
    }
}
