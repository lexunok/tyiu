package com.tyiu.ideas.model.responses;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ErrorResponse {
    private int statusCode;
    private String error;
}
