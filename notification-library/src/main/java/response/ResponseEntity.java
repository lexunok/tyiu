package response;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.http.HttpStatus;


@Getter
@Setter
@NoArgsConstructor
public class ResponseEntity<T> {
    private T body;
    private int status;

    public ResponseEntity(T build, HttpStatus httpStatusCode) {
        this.body = build;
        this.status = httpStatusCode.value();
    }

}
