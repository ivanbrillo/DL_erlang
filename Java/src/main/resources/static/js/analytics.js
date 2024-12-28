let chart;
let currentEpoch = 0;

// Colors
const mainColor = '#0a115b';
const textColor = '#ffffff';


// Initialization
document.addEventListener('DOMContentLoaded', () => {
    initChart();
});



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
            },
            {
                label: 'Test Accuracy',
                borderColor: '#e74c3c',
                backgroundColor: '#e74c3c',
                pointBackgroundColor: '#e74c3c',
                pointBorderColor: '#e74c3c',
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
                    display: true,
                    position: 'top',
                    labels: {
                        color: textColor,
                        font: {
                            size: 14
                        }
                    }
                },
                tooltip: {
                    enabled: true,
                    displayColors: true,
                    callbacks: {
                        title: (tooltipItems) => {
                            return `Epoch: ${tooltipItems[0].label}`;
                        },
                        label: (tooltipItem) => {
                            return `${tooltipItem.dataset.label}: ${tooltipItem.raw}`;
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

function updateChart(newData) {
    chart.data.labels.push(newData.epoch);
    chart.data.datasets[0].data.push(newData.trainAccuracy);
    chart.data.datasets[1].data.push(newData.testAccuracy);
    chart.update();
}


function clearChart() {
    if (chart) {
        chart.data.labels = []; // Svuota le etichette
        chart.data.datasets.forEach(dataset => {
            dataset.data = []; // Svuota i dati di ogni dataset
        });
        chart.update(); // Aggiorna il grafico per mostrare le modifiche
    }
}

function handleTraining(trainAcc, testAcc) {
    const data = {
        epoch: currentEpoch + 1,
        trainAccuracy: parseFloat(trainAcc).toFixed(3),
        testAccuracy: parseFloat(testAcc).toFixed(3)
    }

    updateChart(data);
    currentEpoch++;
    document.getElementById('epochCounter').textContent = currentEpoch;
}