package com.tyiu.ideas.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@Getter
@AllArgsConstructor
@NoArgsConstructor
@Table("company_user")
public class Company2User {

    private String userId;
    private String companyId;

}
