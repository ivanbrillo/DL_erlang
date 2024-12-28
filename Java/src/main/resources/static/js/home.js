const homepageBtn = document.getElementById("homepage_btn");
const analyticsBtn = document.getElementById("analytics_btn");
const container = document.getElementById("container");
const dashboard = document.getElementById("dashboard");
let socket;

/* LEFT MENU MANAGEMENT*/
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



/* MESSAGE MANAGEMENT*/
document.getElementById('connectButton').addEventListener('click', function() {
    let host = document.location.host;
    const url = "http://" + host + "/erlang-socket";

    // Connection to the backend using SockJS
    socket = new SockJS(url);

    // Event handler when the connection is open
    socket.onopen = function() {
        console.log('SockJS connection established');
        socket.send(JSON.stringify({command: "start", parameters: ""}));
    };

    // Event handler when a message is received
    socket.onmessage = function(event) {
        console.log('Message received:', event.data);
        processingInput(event.data);
    };

    socket.onclose = function() {
        console.log('SockJS connection closed');
    };

    socket.onerror = function(error) {
        console.error('SockJS error:', error);
    };

    document.getElementById('connectButton').disabled = true;
    document.getElementById('closeButton').disabled = false;
    document.getElementById('startBtn').disabled = false;
});

document.getElementById('closeButton').addEventListener('click', function() {
    socket.send(JSON.stringify({command: "stop", parameters: ""}));

   const container = document.getElementById('containerNodes');
   const elements = container.querySelectorAll('.elemNode');
   elements.forEach(elem => elem.remove());


    clearChart();

    document.getElementById('connectButton').disabled = false;
    document.getElementById('closeButton').disabled = true;
    document.getElementById('startBtn').disabled = true;
});

document.getElementById('startBtn').addEventListener('click', function() {
    const epochsInput = document.getElementById('epochsInput').value;
    const accuracyTargetInput = document.getElementById('accuracyTarget').value;

    document.getElementById('epochTot').textContent = epochsInput;

    const msg = {
        command: "train",
        parameters: `targetAccuracy=${accuracyTargetInput},epochs=${epochsInput}`
    };

    socket.send(JSON.stringify(msg));
    clearChart();

    document.getElementById('connectButton').disabled = true;
    document.getElementById('closeButton').disabled = false;
    document.getElementById('startBtn').disabled = true;
});



/*
PRECESSING INPUT FROM BACK-END
 */
function processingInput(input){
    // Since SockJS automatically parses JSON, we need to stringify it back
    // if the input is an object
    const inputStr = typeof input === 'object' ? JSON.stringify(input) : input;

    if (inputStr.startsWith("{initialized_nodes")) {
        initialized_nodes(inputStr);
    } else if (inputStr.startsWith("{train_mean_accuracy")) {
        train_accuracy(inputStr);
    } else if (inputStr.startsWith("{training_total_completed")) {
        document.getElementById('startBtn').disabled = false;
    } else if (inputStr.startsWith("{node_metrics")){
        node_metrics(inputStr);
    } else if (inputStr.startsWith("{node_up")){
        add_node(inputStr);
    } else if (inputStr.startsWith("{node_down")){
        delete_node(inputStr);
    }
}

function initialized_nodes(input){
    /* delete old nodes, useful in restart due to errors */
    const container = document.getElementById('containerNodes');
    const oldElements = container.querySelectorAll('.elemNode');
    oldElements.forEach(elem => elem.remove());


    const startIdx = input.indexOf("[");
    const endIdx = input.lastIndexOf("]");
    const content = input.substring(startIdx + 1, endIdx).trim();

    if (!content) {
        console.log("No active nodes.");
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
    const trainAccuracy = elements[1];
    const testAccuracy = elements[3];

    const cleanedTrainAccuracy = trainAccuracy.replace(/[\{\}]/g, '').trim();
    const cleanedTestAccuracy = testAccuracy.replace(/[\{\}]/g, '').trim();

    handleTraining(cleanedTrainAccuracy, cleanedTestAccuracy);
}



function node_metrics(input){
    let inputJSON = input.match(/"({.*})"/);

    const metricsData = JSON.parse(inputJSON[1]);
    let nodeId = metricsData.Node;

    let nodeElem = document.getElementById(nodeId);
    if (!nodeElem) {
        console.log("Node name does not exist")
    }



    metrics.forEach(metric => {
        const metricElem = nodeElem.querySelector(`#${metric.id}`);
        if (metricElem) {
            metricElem.textContent = metricsData[metric.label.replace(':', '').trim()] || 'N/A';
        }
    });
}



function add_node(input){
    const name = input.match(/,(.*?)}/)?.[1];
    createNode(name);
}

function delete_node(input){
    const name = input.match(/,(.*?)}/)?.[1];
    removeNode(name);
}