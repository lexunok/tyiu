package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;


@NoArgsConstructor
@AllArgsConstructor
@Getter
@Table("group_user")
public class Group2User {

    private String userId;
    private String groupId;


}
