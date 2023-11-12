package com.tyiu.corn.model.responses;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ProfileProjectResponse {
    private String id;
    private String name;
    private String description;
}
