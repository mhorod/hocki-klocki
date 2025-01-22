function update() {
    const code = codeElement.value;
    const expansionDepth = expansionDepthElement.value;
    const showTyping = showTypingElement.checked;
    fetch(`/generate-image/${expansionDepth}?typing=${showTyping}`, {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({code: code})
        }
    ).then(response => {
        if (!response.ok) throw new Error("Non ok result status");
        return response.blob()
    })
        .then(blob => {
            document.getElementById("codeImage").src = URL.createObjectURL(blob);
            if(showTyping) {
                console.log("HERERERRE")
                fetch('/get-typing').then(r => {
                    if(!r.ok) throw new Error("Non ok return status");
                    return r.json()
                }).then(txt => {
                    document.getElementById("typeBox").innerText = txt.ty;
                });
            } else {
                document.getElementById("typeBox").innerHTML = "N/A";
            }
        }).catch(err => err)
}

const codeElement = document.getElementById('codeInput');
const expansionDepthElement = document.getElementById('expansionDepth');
const showTypingElement = document.getElementById('showTyping');

const elements = [codeElement, expansionDepthElement, showTypingElement];
elements.forEach(element => element.addEventListener('input', update));

