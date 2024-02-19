package response;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;


@Getter
@Setter
@NoArgsConstructor
public class ResponseEntity<T> {
    private T body;
    private String status;

    public ResponseEntity(T build, HttpStatusCode httpStatusCode) {
        this.body = build;
        this.status = httpStatusCode.toString();
    }

    public HttpStatusCode getStatus(){
        return HttpStatus.valueOf(status);
    }

}
