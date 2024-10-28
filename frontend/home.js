
const homepageBtn = document.getElementById("homepage_btn");
const analyticsBtn = document.getElementById("analytics_btn");
const container = document.getElementById("container");
const dashboard = document.getElementById("dashboard");


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
