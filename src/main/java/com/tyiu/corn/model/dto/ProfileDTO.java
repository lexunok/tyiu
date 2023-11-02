package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.responses.ProfileIdeaResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProfileDTO {

    private List<ProfileIdeaResponse> ideas;

}
