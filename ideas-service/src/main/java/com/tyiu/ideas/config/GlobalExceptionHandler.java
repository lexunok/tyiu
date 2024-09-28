package com.tyiu.ideas.config;

import com.tyiu.client.exceptions.AccessException;
import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.client.exceptions.ServerProcessException;
import com.tyiu.client.models.ErrorResponse;
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
                .status(HttpStatus.NOT_FOUND.value())
                .body(new ErrorResponse(HttpStatus.NOT_FOUND.value(),ex.getMessage())));
    }
    @ExceptionHandler
    public Mono<ResponseEntity<ErrorResponse>> accessException(AccessException ex) {
        log.error(ex.getMessage(), ex);
        return Mono.just(ResponseEntity
                .status(HttpStatus.FORBIDDEN.value())
                .body(new ErrorResponse(HttpStatus.FORBIDDEN.value(),ex.getMessage())));
    }

    @ExceptionHandler
    public Mono<ResponseEntity<ErrorResponse>> serverProcessException(ServerProcessException ex){
        log.error(ex.getMessage(), ex);
        return Mono.just(ResponseEntity
                .status(HttpStatus.INTERNAL_SERVER_ERROR.value())
                .body(new ErrorResponse(HttpStatus.INTERNAL_SERVER_ERROR.value(),ex.getMessage())));
    }
}