const homepageBtn = document.getElementById("homepage_btn");
const analyticsBtn = document.getElementById("analytics_btn");
const container = document.getElementById("container");
const dashboard = document.getElementById("dashboard");
let socket;

let host = document.location.host;
const url = "http://" + host + "/erlang-socket";

// Connection to the backend using SockJS
socket = new SockJS(url);

// Event handler when the connection is open
socket.onopen = function () {
};

// Event handler when a message is received
socket.onmessage = function (event) {
    processingInput(event.data);
};

socket.onclose = function () {
    console.log('SockJS connection closed');
    addLogMessage("received", "SockJS connection closed");
    uiDisabled();
    const container = document.getElementById('containerNodes');
    const oldElements = container.querySelectorAll('.elemNode');
    oldElements.forEach(elem => elem.remove());
    clearChart();
};

socket.onerror = function (error) {
    console.error('SockJS error:', error);
};

/* BUTTONS */
const CONNECT = document.getElementById('connectButton');
const CLOSE = document.getElementById('closeButton');
const MODEL = document.getElementById('loadButton');
const BACKUP = document.getElementById('loadBackUpButton');
const SAVE = document.getElementById('saveButton');
const START = document.getElementById('startBtn');
const STOP = document.getElementById('stopBtn');

buttonsInvisible();

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
CONNECT.addEventListener('click', function () {
        socket.send(JSON.stringify({command: "start", parameters: ""}));
        addLogMessage("sent", "start");
        showSpinner();
});

CLOSE.addEventListener('click', function () {
    socket.send(JSON.stringify({command: "stop", parameters: ""}));
    addLogMessage("sent", "stop");

    const container = document.getElementById('containerNodes');
    const elements = container.querySelectorAll('.elemNode');
    elements.forEach(elem => elem.remove());


    clearChart();
    uiDisabled();
});

START.addEventListener('click', function () {
    const epochsInput = document.getElementById('epochsInput').value;
    const accuracyTargetInput = document.getElementById('accuracyTarget').value;
    document.getElementById('epochTot').textContent = epochsInput;

    const msg = {
        command: "train",
        parameters: `targetAccuracy=${accuracyTargetInput},epochs=${epochsInput}`
    };

    socket.send(JSON.stringify(msg));
    addLogMessage("sent", msg.command + msg.parameters);

    clearChart();

    START.disabled = true;
    STOP.disabled = false;
});

SAVE.addEventListener('click', function () {
    socket.send(JSON.stringify({command: "save", parameters: ""}));
    addLogMessage("sent", "save model");
});

MODEL.addEventListener('click', function () {
    socket.send(JSON.stringify({command: "load", parameters: "model"}));
    addLogMessage("sent", "load model");
});

BACKUP.addEventListener('click', function () {
    socket.send(JSON.stringify({command: "load", parameters: "backup"}));
    addLogMessage("sent", "load backup");
});

STOP.addEventListener('click', function () {
    socket.send(JSON.stringify({command: "stop_training", parameters: ""}));
    addLogMessage("sent", "stop training");
    STOP.disabled = true;
});


/*
PRECESSING INPUT FROM BACK-END
 */
function processingInput(input) {
    // Since SockJS automatically parses JSON, we need to stringify it back
    // if the input is an object
    const inputStr = typeof input === 'object' ? JSON.stringify(input) : input;

    if (inputStr.startsWith("{initialized_nodes")) {
        initializedNodes(inputStr);
        addLogMessage("received", inputStr);
    } else if (inputStr.startsWith("{train_epoch_completed")) {
        trainAccuracy(inputStr);
    } else if (inputStr.startsWith("{training_total_completed")) {
        addLogMessage("received", inputStr);
        START.disabled = false;
        STOP.disabled = true;
    } else if (inputStr.startsWith("{node_metrics")){
        nodeMetrics(inputStr);
    } else if (inputStr.startsWith("{node_up")) {
        addLogMessage("received", inputStr);
        addNode(inputStr);
    } else if (inputStr.startsWith("{node_down")) {
        addLogMessage("received", inputStr);
        deleteNode(inputStr);
    } else if (inputStr.startsWith("{start uncorrectly")) {
        addLogMessage("received", inputStr);
        hideSpinner();
        uiDisabled();
    } else if (inputStr.startsWith("{train_refused")){
        addLogMessage("received", inputStr);
        START.disabled = false;
        STOP.disabled = true;
    } else if (inputStr.startsWith("{db_ack")){
        addLogMessage("received", inputStr);
        sizeDB(inputStr);
    } else if (inputStr.startsWith("{model_saved")) {
        addLogMessage("received", inputStr);
    } else if (inputStr.startsWith("{model_loaded")) {
        addLogMessage("received", inputStr);

        if(inputStr === "{model_loaded,true}") {
            const container = document.getElementById('containerNodes');
            const oldElements = container.querySelectorAll('.elemNode');
            oldElements.forEach(elem => {
                elem.querySelector("#train-accuracy-metric").textContent = "N/A"
                elem.querySelector("#test-accuracy-metric").textContent = "N/A"
            });
        }
    } else if (inputStr.startsWith("{new_train")) {
        clearChart();
        START.disabled = true;
        STOP.disabled = false;
        addLogMessage("received", inputStr);

        const regex = /{new_train,\{(\d+),([\d.]+)}}/; // Match the format {new_train,{<epochs>,<accuracy>}}
        const match = regex.exec(inputStr);

        document.getElementById('epochsInput').value = match[1];
        document.getElementById('accuracyTarget').value = match[2];
        document.getElementById('epochTot').textContent = match[1];

    } else if (inputStr.startsWith("{operator}")) {
        buttonsVisible();
    } else if (inputStr.startsWith("{master_terminating")) {
        addLogMessage("received", inputStr);
        uiDisabled();
        CONNECT.disabled = true;
        clearChart();

        const container = document.getElementById('containerNodes');
        const oldElements = container.querySelectorAll('.elemNode');
        oldElements.forEach(elem => elem.remove());

        showSpinner();
    }
}

function initializedNodes(input){
    hideSpinner();

    /* delete old nodes, useful in restart due to errors */
    const container = document.getElementById('containerNodes');
    const oldElements = container.querySelectorAll('.elemNode');
    oldElements.forEach(elem => elem.remove());
    START.disabled = false;

    clearChart();

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
    uiActive();
}

function trainAccuracy(input) {
    const content = input.substring(input.indexOf('['));
    const parts = content.split(/\],\[/).map(part => part.replace(/[\[\]{}]/g, ''));

    // Array PID
    const pids = parts[0].split(',').map(pid => pid.match(/<([^.]+)/)[1]);
    // Array Train Accuracy
    const trainAccuracies = parts[1].split(',').map(Number);
    // Array Test Accuracy
    const testAccuracies = parts[2].split(',').map(Number);

    pids.forEach((pid, index) => {
        let nodeElem = document.getElementById(pid);
        if (!nodeElem) {
            console.log(`Node with PID ${pid} does not exist`);
            return;
        }

        const trainElem = nodeElem.querySelector("#train-accuracy-metric");
        const testElem = nodeElem.querySelector("#test-accuracy-metric");

        trainElem.textContent = trainAccuracies[index]?.toFixed(3) || "N/A";
        testElem.textContent = testAccuracies[index]?.toFixed(3) || "N/A";
    });

    // Mean
    const trainMeanAccuracy = trainAccuracies.reduce((sum, val) => sum + val, 0) / trainAccuracies.length;
    const testMeanAccuracy = testAccuracies.reduce((sum, val) => sum + val, 0) / testAccuracies.length;


    handleTraining(trainMeanAccuracy, testMeanAccuracy);
}


function nodeMetrics(input) {

    let inputJSON = input.match(/"({.*})"/);

    const metricsData = JSON.parse(inputJSON[1]);
    let nodeId = metricsData.Node;

    let nodeElem = document.getElementById(nodeId);

    metrics.forEach(metric => {
        if (metric.id === "train-accuracy-metric" || metric.id === "test-accuracy-metric"
            || metric.id === "train-dataset-size" || metric.id === "test-dataset-size") {
            return;
        }

        let info = metricsData[metric.label.replace(':', '').trim()];

        if (metric.id === "cpu-metric") {
            updateProgressBar(nodeElem, "cpu-bar", parseFloat(info.replace('%', '')));
        } else if (metric.id === "memory-metric") {
            updateProgressBar(nodeElem, "memory-bar", parseFloat(info.replace('%', '')));
        }

        const metricElem = nodeElem.querySelector(`#${metric.id}`);
        if (metricElem) {
            metricElem.textContent = info;
        }
    });
}


function sizeDB(input) {
    const regex = /#Pid<([^\.]+)\.[^>]*>.*?\{(\d+),\d+,\d+\},\{(\d+),\d+,\d+\}/;
    const match = input.match(regex);


    let nodeElem = document.getElementById(match[1]);

    const trainElem = nodeElem.querySelector("#train-dataset-size");
    const testElem = nodeElem.querySelector("#test-dataset-size");

    trainElem.textContent = match[2] || "N/A";
    testElem.textContent = match[3] || "N/A";
}


function addNode(input) {
    const name = input.match(/,(.*?)}/)?.[1];
    createNode(name);
}

function deleteNode(input) {
    const name = input.match(/,(.*?)}/)?.[1];
    removeNode(name);
}


function uiDisabled(){
    CONNECT.disabled = false;
    CLOSE.disabled = true;
    MODEL.disabled = true;
    BACKUP.disabled = true;
    SAVE.disabled = true;
    START.disabled = true;
    STOP.disabled = true;
}


function uiActive() {
    CONNECT.disabled = true;
    CLOSE.disabled = false;
    MODEL.disabled = false;
    BACKUP.disabled = false;
    SAVE.disabled = false;
    START.disabled = false;
    STOP.disabled = true;
}


function buttonsInvisible() {
    CONNECT.style.display = 'none';
    CLOSE.style.display = 'none';
    MODEL.style.display = 'none';
    BACKUP.style.display = 'none';
    SAVE.style.display = 'none';
    START.style.display = 'none';
    STOP.style.display = 'none';

    document.getElementById('epochsInput').disabled = true;
    document.getElementById('accuracyTarget').disabled = true;
}

function buttonsVisible() {
    CONNECT.style.display = 'inline';
    CLOSE.style.display = 'inline';
    MODEL.style.display = 'inline';
    BACKUP.style.display = 'inline';
    SAVE.style.display = 'inline';
    START.style.display = 'inline';
    STOP.style.display = 'inline';

    document.getElementById('epochsInput').disabled = false;
    document.getElementById('accuracyTarget').disabled = false;
}


/* SPINNER */
function showSpinner() {
    document.getElementById('spinner').style.display = 'block';
}

function hideSpinner() {
    document.getElementById('spinner').style.display = 'none';
}


/* LOG SECTION */
function addLogMessage(type, message) {
    const logContainer = document.getElementById('log');
    const logEntry = document.createElement('div');

    logEntry.classList.add('log-message');
    if (type === 'received') {
        logEntry.classList.add('received');
        logEntry.textContent = "Received message: " + message;
    } else if (type === 'sent') {
        logEntry.classList.add('sent');
        logEntry.innerHTML = "Sent message: " + message;
    }

    logContainer.appendChild(logEntry);
    logContainer.scrollTop = logContainer.scrollHeight; // Scroll to the bottom
}