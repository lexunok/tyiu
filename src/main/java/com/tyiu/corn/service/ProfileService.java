package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.Profile;
import com.tyiu.corn.model.requests.ProfileUpdateRequest;
import com.tyiu.corn.repository.ProfileRepository;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
public class ProfileService {
    private final ProfileRepository profileRepository;
    private final ModelMapper mapper;

    @Cacheable
    public Mono<ProfileDTO> getProfile(String profileId) {
        Mono<Profile> profile = profileRepository.findById(profileId);
        return profile.flatMap(p -> Mono.just(mapper.map(p, ProfileDTO.class)));
    }

    @CacheEvict(allEntries = true)
    public Mono<ProfileDTO> createProfile(ProfileDTO profileDTO) {
        Mono<Profile> profile = profileRepository.save(mapper.map(profileDTO, Profile.class));
        return profile.flatMap(p -> Mono.just(mapper.map(p, ProfileDTO.class)));
    }
    @CacheEvict(allEntries = true)
    public void updateProfile(String id, ProfileUpdateRequest request) {
        profileRepository.findById(id).flatMap(p -> {
            p.getUser().setFirstName(request.getFirstName());
            p.getUser().setLastName(request.getLastName());
            return profileRepository.save(p);
        }).subscribe();
    }
}
