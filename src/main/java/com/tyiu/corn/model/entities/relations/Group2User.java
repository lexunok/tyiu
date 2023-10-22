package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;


@NoArgsConstructor
@AllArgsConstructor
@Getter
@Table("group_user")
public class Group2User {

    private Long userId;
    private Long groupId;


}
