package com.tyiu.ideas.model.requests;

import com.tyiu.ideas.model.entities.Idea;
import lombok.Data;

@Data
public class StatusIdeaRequest {
    private Idea.Status status;
}
