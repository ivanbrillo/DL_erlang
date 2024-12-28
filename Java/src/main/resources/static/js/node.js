const metrics = [
    { label: 'CPU: ', id: 'cpu-metric' },
    { label: 'Memory: ', id: 'memory-metric' },
    { label: 'Response Time: ', id: 'response-time-metric' },
    { label: 'Accuracy: ', id: 'accuracy-metric' }
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
        value.textContent = 'Loading...';

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