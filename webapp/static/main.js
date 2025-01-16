function updateImage() {
    const code = codeElement.value;
    fetch("/generate-image", {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({code: code})
        }
    ).then(response => {
        if (!response.ok) throw new Error("Non ok result status")
        return response.blob()
    })
        .then(blob => {
            document.getElementById("codeImage").src = URL.createObjectURL(blob);
        }).catch(err => err)
}

const codeElement = document.getElementById('codeInput');
codeElement.addEventListener('input', updateImage);
console.log(codeElement)
