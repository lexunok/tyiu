package com.tyiu.ideas.model.responses;

import com.tyiu.ideas.model.entities.Idea;
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
    private String solution;
    private Idea.Status status;
}
