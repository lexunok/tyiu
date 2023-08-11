package com.tyiu.corn.service;

import java.util.List;

import org.springframework.stereotype.Service;

import com.tyiu.corn.model.entities.Company;
import com.tyiu.corn.repository.CompanyRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class CompanyService {
    private final CompanyRepository companyRepository;

    public List<Company> getListCompany() {
        return companyRepository.findAll();
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
        //company.setUsers(updatedCompany.getUser());
        companyRepository.save(company);
    }
}
