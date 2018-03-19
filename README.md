# knowyourerrors


  
Tools facilitating the process of statically typed error handling.

#### Concept:

######Classification of errors (by subjective perception of software developers):  

├── Error  

│├── Unexpected 

│├── Expected

│ | └── No way to handle

│ | └── Possible to handle (MUST be on signatures)

│ │ │ ├── Declare specifically <—this is what it is all about

│ │ │ ├── Declare generally



######Concerns about reacting to different errors specifically:

In order to preserve safety, ensure that possible all errors are covered at compile time, it is needed to tell the compiler about them.

In order to compose functions exposing different sets of errors on signatures, it is needed to embed all errors of involved functions to some superset of errors which the resulting computation may contain.

(Recovering from unexpected/unhandled errors is out of scope)