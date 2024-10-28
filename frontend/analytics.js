let chart;
let trainingData = [];
let currentEpoch = 0;
let isTraining = false;
let trainingInterval;

// Colors
const mainColor = '#0a115b';
const textColor = '#ffffff';


// Initialization
document.addEventListener('DOMContentLoaded', () => {
    initChart();
    document.getElementById('startBtn').addEventListener('click', startTraining);
    document.getElementById('resetBtn').addEventListener('click', resetTraining);
});


// Start training
function startTraining() {
    isTraining = true;
    trainingInterval = setInterval(handleTraining, 1000);
    document.getElementById('startBtn').disabled = true;
}


//Stop training
function stopTraining() {
    isTraining = false;
    clearInterval(trainingInterval);
}


// Reset training
function resetTraining() {
    stopTraining();
    document.getElementById('startBtn').disabled = false;
    currentEpoch = 0;
    trainingData = [];
    document.getElementById('epochCounter').textContent = '0';
    chart.data.labels = [];
    chart.data.datasets[0].data = [];
}


function initChart() {
    const ctx = document.getElementById('accuracyChart').getContext('2d');
    chart = new Chart(ctx, {
        type: 'line',
        data: {
            labels: [],
            datasets: [{
                label: 'Training Accuracy',
                borderColor: mainColor,
                backgroundColor: mainColor,  
                pointBackgroundColor: mainColor, 
                pointBorderColor: mainColor,     
                pointRadius: 4,              
                tension: 0.1,
                borderWidth: 4
            }]
        },
        options: {
            responsive: true,
            scales: {
                y: {
                    beginAtZero: true,
                    max: 1,
                    grid: {
                        color: `${textColor}20`,
                        lineWidth: 5,
                        drawBorder: false
                    },
                    ticks: {
                        color: textColor,
                        font: {
                            size: 16
                        }
                    },
                    title: {
                        display: true,
                        text: 'Accuracy',
                        color: textColor,
                        font: {
                            size: 20,
                            weight: 'bold'
                        }
                    }
                },
                x: {
                    grid: {
                        color: textColor,
                    },
                    ticks: {
                        color: textColor,
                        font: {
                            size: 16
                        }
                    },
                    title: {
                        display: true,
                        text: 'Epoch',
                        color: textColor,
                        font: {
                            size: 20,
                            weight: 'bold'
                        }
                    }
                }
            },
            animation: {
                duration: 0
            },
            plugins: {
                legend: {
                    display: false
                },
                tooltip: {
                    enabled: true,
                    displayColors: false,
                    callbacks: {
                        title: (tooltipItems) => {
                            return `Epoch: ${tooltipItems[0].label}`;
                        },
                        label: (tooltipItem) => {
                            return `Accuracy: ${tooltipItem.raw}`;
                        }
                    },
                    backgroundColor: mainColor,
                    titleColor: '#ffffff',
                    bodyColor: '#ffffff',
                    borderColor: mainColor,
                    borderWidth: 1
                }
            },
            interaction: {
                intersect: false,
                mode: 'index'
            }
        }
    });
}


function simulateEpoch() {
    const baseAccuracy = 0.4;
    const maxImprovement = 0.5;
    const noise = 0.05;

    return {
        epoch: currentEpoch + 1,
        accuracy: Math.min(
            0.98,
            baseAccuracy + (maxImprovement * Math.log(currentEpoch + 1) / Math.log(50)) + 
            (Math.random() * noise - noise/2)
        ).toFixed(3)
    };
}


// Update graph
function updateChart(newData) {
    chart.data.labels.push(newData.epoch);
    chart.data.datasets[0].data.push(newData.accuracy);
    chart.update();
}

// handler 
function handleTraining() {
    if (currentEpoch >= 50) {
        stopTraining();
        return;
    }

    const newData = simulateEpoch();
    trainingData.push(newData);
    updateChart(newData);
    currentEpoch++;
    document.getElementById('epochCounter').textContent = currentEpoch;
}