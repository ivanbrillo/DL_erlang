let nodesModel = [];

function addNodeModel(name) {
    let node = {
        name: name,
        trainAccuracy: null,
        testAccuracy: null,
        trainDataSize: null,
        testDataSize: null
    };
    
    nodesModel.push(node);
}

function updateNodeModel(name, trainAccuracy, testAccuracy, trainDataSize, testDataSize) {
    const node = nodesModel.find(node => node.name === name);
    
    if (node) {
        if (trainAccuracy !== null) node.trainAccuracy = trainAccuracy;
        if (testAccuracy !== null) node.testAccuracy = testAccuracy;
        if (trainDataSize !== null) node.trainDataSize = trainDataSize;
        if (testDataSize !== null) node.testDataSize = testDataSize;
    } else {
        console.log("Node not found");
    }
}

function removeNodeModel(name) {
    const index = nodesModel.findIndex(node => node.name === name);
    if (index !== -1) {
        nodesModel.splice(index, 1);
    } else {
        console.log("Node not found");
    }
}


const metrics = [
    { label: 'CPU: ', id: 'cpu-metric' },
    { label: 'Memory: ', id: 'memory-metric' },
    { label: 'Response Time: ', id: 'response-time-metric' },
    { label: 'Train Accuracy: ', id: 'train-accuracy-metric' },
    { label: 'Test Accuracy: ', id: 'test-accuracy-metric' },
    { label: 'Training Dataset Size: ', id: 'train-dataset-size' },
    { label: 'Test Dataset Size: ', id: 'test-dataset-size' }
];

let numNodi = 0;


function createNode(Name){
    numNodi++;
    const container = document.getElementById("containerNodes");

    let node = document.createElement('div');
    node.className = 'elemNode';
    node.id = Name;

    let title = document.createElement('h3');
    title.textContent = 'NODE: '+ Name;

    addNodeModel(Name);

    /* metrics */
    let metricsContainer = document.createElement('div');
    metricsContainer.className = 'metrics-container';

    metrics.forEach(metric => {
        const metricDiv = document.createElement('span');
        metricDiv.className = 'metric-item';

        const label = document.createElement('strong');
        label.textContent = metric.label;
        metricDiv.appendChild(label);

        let bar = document.createElement('div');

        if (metric.id === "cpu-metric") {
            bar.id = "cpu-bar";
            bar.className = 'progress-bar';
            metricDiv.appendChild(bar);
        } else if (metric.id === "memory-metric") {
            bar.id = "memory-bar";
            bar.className = 'progress-bar';
            metricDiv.appendChild(bar);
        } else {
            const value = document.createElement('span');
            value.id = metric.id;

            if (metric.id === "train-accuracy-metric" || metric.id === "test-accuracy-metric") {
                value.textContent = 'N/A';
            } else {
                value.textContent = 'Loading...';
            }

            metricDiv.appendChild(value);
        }

        metricsContainer.appendChild(metricDiv);
    });

    node.appendChild(title);
    node.appendChild(metricsContainer);

    container.appendChild(node);
}



function removeNode(name){
    const node = document.getElementById(name);
    if (node) {
        node.remove();
        removeNodeModel(name);
        console.log(`${name} Node eliminated.`);
    }
    numNodi--;
}



function updateProgressBar(nodeElem, metricId, value) {
    const progressBar = nodeElem.querySelector(`#${metricId}`);

    const progressBarInner = document.createElement("div");
    progressBarInner.classList.add("progress-bar-inner");
    progressBarInner.style.width = value + "%";


    let red = Math.min(255, Math.floor(255 * (value / 100)));
    let green = Math.min(255, Math.floor(255 * (1 - value / 100)));

    progressBarInner.style.backgroundColor = `rgb(${red}, ${green}, 0)`;


    progressBarInner.textContent = value.toFixed(1) + "%";


    progressBar.innerHTML = "";
    progressBar.appendChild(progressBarInner);
}