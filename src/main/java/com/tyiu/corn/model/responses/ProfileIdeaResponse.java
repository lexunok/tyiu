package com.tyiu.corn.model.responses;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProfileIdeaResponse {
    private Long id;
    private String name;
    private String description;
}
