const examples = [
`
def main = [X | Y]
    use -a [X | Y] as RemoveA
    use +a [X | Y] as AddA
    link
        X >-> RemoveA.X,
        RemoveA.Y >-> AddA.X,
        AddA.Y >-> Y
`,
`
def main = [X1, X2 | Y]
    use U{2} [X1, X2 | Y] as U2
    use +a [X | Y] as AddA
    link
        X1 >-> U2.X1, X2 >-> U2.X2,
        U2.Y >-> AddA.X, 
        AddA.Y >-> Y
`,
`
def main = [X1, X2 | Y]
    use U{2} [X1, X2 | Y] as U2
    use -a [X | Y] as RemoveA
    link
        X1 >-> U2.X1, X2 >-> U2.X2,
        U2.Y >-> RemoveA.X, 
        RemoveA.Y >-> Y
`,
`
def main = [X | Y]
    use +a [X | Y] as AddA
    use -a [X | Y] as RemoveA
    use U{2} [X1, X2 | Y] as U2
    link
        X >-> AddA.X,
        X >-> RemoveA.X,
        AddA.Y >-> U2.X1,
        RemoveA.Y >-> U2.X2,
        U2.Y >-> Y
`,
`
def main = [X1, X2 | Y]
    use f [X | Y] as F
    use g [X | Y] as G
    use U{2} [X1, X2 | Y] as U2
    link
        X1 >-> F.X,
        X2 >-> G.X,
        F.Y >-> U2.X1,
        G.Y >-> U2.X2,
        U2.Y >-> Y
        
def g = [X | Y]
    use f [X | Y] as F
    use -b [X | Y] as RemoveB
    link
        X >-> F.X,
        F.Y >-> RemoveB.X,
        RemoveB.Y >-> Y
        
def f = [X | Y]
    use +a [X | Y] as AddA
    link
        X >-> AddA.X,
        AddA.Y >-> Y

`,
`
def f = [X | Y]
    use +a [X | Y] as AddA
    use f [X | Y] as F
    use U{2} [X1, X2 | Y] as U2
    link
        X >-> AddA.X,
        X >-> F.X,
        AddA.Y >-> U2.X1,
        F.Y >-> U2.X2,
        U2.Y >-> Y
`
]

function selectExample() {
    const selection = exampleSelectElement.value;
    if (selection !== "") {
        const selectionValue = parseInt(selection);
        codeElement.value = examples[selectionValue];
    } else {
        codeElement.value = ""
    }
    update()
}

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
        return response.text()
    })
        .then(svg => {
            document.getElementById("codeImage").innerHTML = svg
            if (showTyping) {
                fetch('/get-typing').then(r => {
                    if (!r.ok) throw new Error("Non ok return status");
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
const exampleSelectElement = document.getElementById('exampleSelect');


const elements = [codeElement, expansionDepthElement, showTypingElement];

exampleSelectElement.addEventListener('change', selectExample);
elements.forEach(element => element.addEventListener('input', update));

