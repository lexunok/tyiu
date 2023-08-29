package com.tyiu.corn.service;

import java.util.List;

import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Company;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.repository.CompanyRepository;

import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class CompanyService {

    private final CompanyRepository companyRepository;

    public Flux<Company> getListCompany() {
        return companyRepository.findAll();
    }

    public Flux<UserDTO> getListStaff(Long id) {
        Mono<Company> company = companyRepository.findById(id);
        return company.flatMapMany(c -> Flux.fromIterable(c.getStaff())).cast(UserDTO.class);
    }
    
    public Mono<Company> addCompany(Company company) {
        return companyRepository.save(company);
    }

    
    public void deleteCompany(Long id) {
        companyRepository.deleteById(id);
    }

    public void updateCompany(Long id, Company updatedCompany) {
        Mono<Company> company = companyRepository.findById(id);
        company.flatMap(c -> {
            c.setName(updatedCompany.getName());
            c.setStaff(updatedCompany.getStaff());
            return companyRepository.save(c);
        });
    }
}
