# Twitter Clone with a WebSocket interface

Developed as part of coursework for COP5615 - Distributed Operating System Principles  
  
**Programming Language:** Erlang

## Description

The first part of the fourth course project involved the development of a Twitter clone with support for various functionalities and a client tester/simulator. The Twitter engine developed in the first part of the project is paired up with WebSockets in the second part of the project in order to provide a complete Twitter-like functionality. Additionally, the second part of the project also involves the implementation of a public key-based authentication method.

## Implementation Details

The Twitter engine now uses a websocket interface. Part-1 of my project used the gen_tcp interface for server-client communication, and for part-2, I am using the websocket interface with JSON support. I have used **Cowboy** for implementing the websockets, and the JSON data for messaging is parsed using the **MochiWeb** framework. For the bonus part, I have also implemented a public key-based authentication method using **RSA-2048**. The project uses Erlang-make, and as mentioned earlier, the dependencies include Cowboy and MochiWeb. The components of the source code include the application, the supervisor, the handler, and the module for bonus part implementation. The application has the dispatcher, and the handler contains the primary code of the server that contains all the functions for each service and handles all the requests from the client. The init function first establishes a websocket handshake between the server and the client, and then the websocket_init function sends a confirmation to the client after the handshake has been made. The websocket_handle function receives the messages from the client and handles the specific requests. Here we parse the JSON data that the client has sent us, and then we perform the action by calling its respective function. The server can handle several types of requests, such as user registration, login, tweets, subscriptions, queries, and so on. For client-side operations, we can submit requests to the server in the form of JSON messages after connecting to the websocket, and I have used the **Postman** platform for this. Last but not least, I have also implemented a module for the bonus part that is a public key-based authentication method based on RSA 2048.

## Execution

<ins>**Server:**<ins>

```cd project4_ws```   
```make run```   

<ins>**Client:**<ins>

```localhost:8080```   

<ins>**Bonus:**<ins>

**To Generate RSA keys:**
  
 ```cd project4_ws/_rel/project4_ws_release/```
 
 ```openssl genrsa -out rsa.pem 2048```


## Report and Demo 
  
[Report](https://github.com/pranath-reddy/COP5615-Twitter-WebSocket/blob/main/Project4%20Part2.pdf)  
[Demo](https://www.youtube.com/watch?v=U_rz8cT55Z0&ab_channel=PranathReddy)
