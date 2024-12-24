const login = async (email, password) => {
    const response = await fetch('/authenticate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email, password })
    });
    if (response.ok) {
        const { token } = await response.json();
        localStorage.setItem('token', token);  // Save token
    } else {
        console.error("Authentication failed");
    }
};
