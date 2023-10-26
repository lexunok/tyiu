package com.tyiu.corn.model.requests;

import com.tyiu.corn.model.dto.CompanyDTO;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class IdeaCompanyRequest {
    private Long ideaId;
    private List<CompanyDTO> companies;
}
