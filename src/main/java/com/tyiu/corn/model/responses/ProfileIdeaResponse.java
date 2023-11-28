package com.tyiu.corn.model.responses;

import com.tyiu.corn.model.entities.Idea;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProfileIdeaResponse {
    private String id;
    private String name;
    private String description;
    private Idea.Status status;
}
