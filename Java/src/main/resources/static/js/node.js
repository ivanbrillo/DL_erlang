const metrics = [
    'CPU:',
    'Memory:',
    'Disk I/O:',
    'Response Time:',
    'Throughput:',
    'Accuracy:'
];

/*
const results = {
    accuracy: Math.random().toFixed(2),
    loss: (Math.random() * 10).toFixed(2)
};
*/

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
        metricDiv.textContent = metric;
        metricsContainer.appendChild(metricDiv);
    });


    /* Status */
    const statusContainer = document.createElement('div');
    statusContainer.className = 'status-container';

    const statusDot = document.createElement('span');
    statusDot.className = 'status-dot';

    const statusText = document.createElement('span');
    statusText.textContent = 'Active';
    statusText.className = 'status-text';


    /* append */
    statusContainer.appendChild(statusDot);
    statusContainer.appendChild(statusText);


    node.appendChild(title);
    node.appendChild(metricsContainer);
    node.appendChild(statusContainer);

    container.appendChild(node);
}