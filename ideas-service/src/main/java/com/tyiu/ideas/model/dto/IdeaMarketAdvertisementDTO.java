package com.tyiu.ideas.model.dto;
import com.tyiu.client.models.UserDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class IdeaMarketAdvertisementDTO {
    private String id;
    private String ideaMarketId;
    private LocalDateTime createdAt;

    private String text;
    private UserDTO sender;
    private List<String> checkedBy;
}
