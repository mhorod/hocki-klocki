const examples = [
`
def f = <a, b |> [X0, X1 | Y]
    use -a [X | Y] as RemAUpper
    use U{2} [X0, X1 | Y] as Union
    use -a [X | Y] as RemALower
    use -b [X | Y] as RemB
    use +b [X | Y] as AddB
    link
        X0 >-> RemAUpper.X
        X1 >-> AddB.X
        RemAUpper.Y >-> Union.X0
        AddB.Y >-> Union.X1
        Union.Y >-> RemALower.X
        RemALower.Y >-> RemB.X
        RemB.Y >-> Y
`,
`
def f = <a |> [X | Y]
    use <> [X | Y0, Y1] as If
    use +a [X | Y] as AddA
    use *b [X | Y] as SpawnB
    use -b [X | Y] as RemB
    use f <a |> [X | Y] as Recursive
    use >< [X0, X1 | Y] as Join
    link
        X >-> If.X
        If.Y0 >-> AddA.X
        AddA.Y >-> Join.X0
        If.Y1 >-> SpawnB.X
        SpawnB.Y >-> Recursive.X
        Recursive.Y >-> RemB.X
        RemB.Y >-> Join.X1
        Join.Y >-> Y
`,
`
def f = <a, b|> [X1, X2, X3 | Y]
    use U{2} [X1, X2 | Y] as U
    use >< [X1, X2 | Y] as Join
    use -a [X | Y] as RemA
    use +b [X | Y] as AddB
    link
        X1 >-> U.X1 
        X2 >-> AddB.X, AddB.Y >-> U.X2
        U.Y >-> Join.X1
        X3 >-> RemA.X, RemA.Y >-> Join.X2
        Join.Y >-> Y 
`,
`
def f = [X|Y]
    use f [X | Y] as F
    link
        X >-> F.X
        F.Y >-> Y


def main = <a|> [X | Y]
  use f [X | Y] as F
  use +a [X | Y] as AddA
  link
    X >-> AddA.X
    AddA.Y >-> F.X
    F.Y >-> Y
`]

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

