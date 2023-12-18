package com.tyiu.corn.config;

import freemarker.cache.ClassTemplateLoader;
import freemarker.cache.FileTemplateLoader;
import freemarker.cache.TemplateLoader;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.result.view.freemarker.*;

import java.io.File;
import java.io.IOException;


@Configuration
public class FreeMarkerConfig {
    @Bean
    public FreeMarkerConfigurer freemarkerClassLoaderConfig() {
            freemarker.template.Configuration configuration = new freemarker.template.Configuration(freemarker.template.Configuration.VERSION_2_3_32);
            TemplateLoader templateLoader = new ClassTemplateLoader(this.getClass(), "/templates");
            configuration.setTemplateLoader(templateLoader);
            FreeMarkerConfigurer freeMarkerConfigurer = new FreeMarkerConfigurer();
            freeMarkerConfigurer.setConfiguration(configuration);
            return freeMarkerConfigurer;
    }
}
