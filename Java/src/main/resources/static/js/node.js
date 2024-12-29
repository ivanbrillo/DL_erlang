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
    title.textContent = 'NODE '+ Name;

    /* metrics */
    let metricsContainer = document.createElement('div');
    metricsContainer.className = 'metrics-container';

    metrics.forEach(metric => {
        const metricDiv = document.createElement('span');
        metricDiv.className = 'metric-item';

        const label = document.createElement('strong');
        label.textContent = metric.label;

        const value = document.createElement('span');
        value.id = metric.id;

        if (metric.id === "train-accuracy-metric" || metric.id === "test-accuracy-metric") {
            value.textContent = 'Training not started...';
        } else {
            value.textContent = 'Loading...';
        }

        metricDiv.appendChild(label);
        metricDiv.appendChild(value);
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
        console.log(`${name} Node eliminated.`);
    }
    numNodi--;
}