package com.tyiu.corn.model.requests;

import com.tyiu.corn.model.entities.Idea;
import lombok.Data;

@Data
public class StatusIdeaRequest {
    private Idea.Status status;
}
