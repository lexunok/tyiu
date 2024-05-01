package com.tyiu.authorizationservice.config.exception;

import com.tyiu.authorizationservice.model.responses.ErrorResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
@Slf4j
public class GlobalExceptionHandler {
    @ExceptionHandler
    public ResponseEntity<ErrorResponse> notFoundException(NotFoundException ex) {
        log.error(ex.getMessage(), ex);
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST.value())
                .body(new ErrorResponse(HttpStatus.BAD_REQUEST.value(),ex.getMessage()));
    }
    @ExceptionHandler
    public ResponseEntity<ErrorResponse> accessException(AccessException ex) {
        log.error(ex.getMessage(), ex);
        return ResponseEntity
                .status(HttpStatus.FORBIDDEN.value())
                .body(new ErrorResponse(HttpStatus.FORBIDDEN.value(),ex.getMessage()));
    }
}
