const homepageBtn = document.getElementById("homepage_btn");
const analyticsBtn = document.getElementById("analytics_btn");
const container = document.getElementById("container");
const dashboard = document.getElementById("dashboard");
let socket;

document.getElementById('connectButton').addEventListener('click', function() {
    let host = document.location.host;
    const url = "ws://" + host + "/erlang-socket";

    // Connection to the backend using WebSocket
    socket = new WebSocket(url);


    // Event handler when the connection is open
    socket.onopen = function() {
        console.log('Connessione WebSocket stabilita');
        socket.send('{"command": "start", "parameters": ""}');
    };

    // Event handler when a message is received
    socket.onmessage = function(event) {
        console.log('Messaggio ricevuto:', event.data);
        processingInput(event.data);
    };

    // TODO
    socket.onclose = function() {
        console.log('Connessione WebSocket chiusa');
    };

    // TODO
    socket.onerror = function(error) {
        console.error('Errore WebSocket:', error);
    };

});

document.getElementById('closeButton').addEventListener('click', function() {
    socket.send('{"command": "stop", "parameters": ""}');
});


document.getElementById('startBtn').addEventListener('click', function() {
    const epochsInput = document.getElementById('epochsInput').value;
    const accuracyTargetInput = document.getElementById('accuracyTarget').value;

    document.getElementById('epochTot').textContent = epochsInput;


    const msg = '{"command": "train", "parameters": "targetAccuracy=' +  accuracyTargetInput + ',epochs=' +
        epochsInput + '"}';
    socket.send(msg);
});



/* left menu management */
analyticsBtn.addEventListener("click", () => {
    container.style.display = "none";
    dashboard.style.display = "inline-block";
    homepageBtn.classList.remove("active");
    analyticsBtn.classList.add("active");
});


homepageBtn.addEventListener("click", () => {
    container.style.display = "flex";
    dashboard.style.display = "none";
    analyticsBtn.classList.remove("active");
    homepageBtn.classList.add("active");
});



function processingInput(input){
    if (input.startsWith("{initialized_nodes")) {
        initialized_nodes(input);
    } else if (input.startsWith("{train_mean_accuracy")){
        train_accuracy(input);
    }
}



function initialized_nodes(input){
    const startIdx = input.indexOf("[");
    const endIdx = input.lastIndexOf("]");
    const content = input.substring(startIdx + 1, endIdx).trim();

    // Verifica se il contenuto tra parentesi quadre è vuoto
    if (!content) {
        console.log("Non ci sono nodi attivi.");
        return;
    }


    // Split info nodes

    const elements = content.split("},");
    const result = [];

    // Iterate through each element in the array
    for (let i = 0; i < elements.length; i++) {
        const item = elements[i];
        const cleanedItem = item.replace(/[\{\}]/g, '').trim();
        const parts = cleanedItem.split(",");

        // Check if there are at least two parts (before and after the comma)
        if (parts.length > 1) {
            result.push(parts[1].trim());
        }
    }

    result.forEach(createNode);
}


function train_accuracy(input) {
    const elements = input.split(",");
    const accuracy = elements[1];

    const cleanedAccuracy = accuracy.replace(/[\{\}]/g, '').trim();

    handleTraining(cleanedAccuracy);
}