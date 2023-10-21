package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table("company_user")
public class Company2User {
    @Id
    private Long id;
    private Long userId;
    private Long companyId;

    public Company2User(Long userId, Long companyId) {
        this.userId = userId;
        this.companyId = companyId;
    }
}
