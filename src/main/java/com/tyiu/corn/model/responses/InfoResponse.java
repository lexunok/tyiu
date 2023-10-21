package com.tyiu.corn.model.responses;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;

@AllArgsConstructor
public class InfoResponse {
    private HttpStatus statusCode;
    private String message;
}
