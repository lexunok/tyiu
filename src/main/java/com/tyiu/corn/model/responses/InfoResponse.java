package com.tyiu.corn.model.responses;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.http.HttpStatus;

@Data
@AllArgsConstructor
public class InfoResponse {
    private HttpStatus statusCode;
    private String message;
}
