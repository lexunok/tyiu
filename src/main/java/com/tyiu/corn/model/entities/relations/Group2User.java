package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table
public class Group2User {
    @Id
    private Long id;
    private Long userId;
    private Long groupId;

    public Group2User(Long userId, Long groupId) {
        this.userId = userId;
        this.groupId = groupId;
    }
}
