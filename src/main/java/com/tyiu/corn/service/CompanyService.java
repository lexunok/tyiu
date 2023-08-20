package com.tyiu.corn.service;

import java.util.List;

import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Company;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.repository.CompanyRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class CompanyService {
    private final CompanyRepository companyRepository;

    public List<Company> getListCompany() {
        List<Company> company = companyRepository.findAll();
        return company.stream()
        .map(u -> Company.builder().id(u.getId()).name(u.getName()).build()).toList();
    }

    public List<UserDTO> getListStaff(Long id) {
        Company company = companyRepository.findById(id).orElseThrow(() -> new RuntimeException("Компания не найдена"));
        List<User> users = company.getStaff();
        List<UserDTO> userDTO = users.stream()
                .map(u -> UserDTO.builder().email(u.getEmail()).firstName(u.getFirstName()).lastName(u.getLastName()).roles(u.getRoles()).build()).toList();
        return userDTO;
    }
    
    public Company addCompany(Company company) {
        return companyRepository.save(company);
    }

    
    public void deleteCompany(Long id) {
        companyRepository.deleteById(id);
    }

    public void updateCompany(Long id, Company updatedCompany) {
        Company company = companyRepository.findById(id).orElseThrow();
        company.setName(updatedCompany.getName());
        company.setStaff(updatedCompany.getStaff());
        companyRepository.save(company);
    }
}
