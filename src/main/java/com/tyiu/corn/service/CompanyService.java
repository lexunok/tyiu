package com.tyiu.corn.service;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Company;
import com.tyiu.corn.repository.CompanyRepository;

import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "companies")
public class CompanyService {

    private final CompanyRepository companyRepository;
    @Cacheable
    public Flux<Company> getListCompany() {
        return companyRepository.findAll();
    }
    @Cacheable
    public Flux<UserDTO> getListStaff(String id) {
        Mono<Company> company = companyRepository.findById(id);
        return company.flatMapMany(c -> Flux.fromIterable(c.getStaff())).cast(UserDTO.class);
    }
    @CacheEvict(allEntries = true)
    public Mono<Company> addCompany(Company company) {
        return companyRepository.save(company);
    }

    @CacheEvict(allEntries = true)
    public void deleteCompany(String id) {
        companyRepository.deleteById(id).subscribe();
    }
    @CacheEvict(allEntries = true)
    public void updateCompany(String id, Company updatedCompany) {
        Mono<Company> company = companyRepository.findById(id);
        company.flatMap(c -> {
            c.setName(updatedCompany.getName());
            c.setStaff(updatedCompany.getStaff());
            return companyRepository.save(c);
        }).subscribe();
    }
}