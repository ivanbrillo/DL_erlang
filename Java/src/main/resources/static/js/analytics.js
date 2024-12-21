let chart;
let currentEpoch = 0;

// Colors
const mainColor = '#0a115b';
const textColor = '#ffffff';


// Initialization
document.addEventListener('DOMContentLoaded', () => {
    initChart();
});


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


// Update graph
function updateChart(newData) {
    chart.data.labels.push(newData.epoch);
    chart.data.datasets[0].data.push(newData.accuracy);
    chart.update();
}

// handler
function handleTraining(input) {
    const data = {
        epoch: currentEpoch + 1,
        accuracy: parseFloat(input).toFixed(3)
    }

    updateChart(data);
    currentEpoch++;
    document.getElementById('epochCounter').textContent = currentEpoch;
}