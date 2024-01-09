FROM --platform=linux/amd64 openjdk:17-alpine
COPY ./build/libs/ideas-service-1.jar ideas-service.jar
EXPOSE 80
CMD ["java", "-XX:+UseG1GC","-Dspring.profiles.active=prod", "-jar", "ideas-service.jar"]