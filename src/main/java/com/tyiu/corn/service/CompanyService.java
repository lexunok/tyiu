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

//    public Flux<UserDTO> getListStaff(Long id) {
//        Mono<Company> company = companyRepository.findById(id);
//        List<User> users = company.getStaff();
//        List<UserDTO> userDTO = users.stream()
//                .map(u -> UserDTO.builder().email(u.getEmail()).firstName(u.getFirstName()).lastName(u.getLastName()).roles(u.getRoles()).build()).toList();
//        return userDTO;
//    }
    
    public Mono<Company> addCompany(Company company) {
        return companyRepository.save(company);
    }

    
    public void deleteCompany(Long id) {
        companyRepository.deleteById(id);
    }

//    public void updateCompany(Long id, Company updatedCompany) {
//        Mono<Company> company = companyRepository.findById(id);
//        company.setName(updatedCompany.getName());
//        company.setStaff(updatedCompany.getStaff());
//        companyRepository.save(company);
//    }
}
