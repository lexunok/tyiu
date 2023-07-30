FROM openjdk:17-alpine
COPY build/libs/corn-1.jar corn.jar
EXPOSE 8080
CMD ["java", "-Dspring.profiles.active=prod", "-jar", "corn.jar"]