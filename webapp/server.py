from flask import Flask, jsonify, request, send_file
import pathlib
import requests
import subprocess
import tempfile

import visualization

pathlib.Path('dfl').mkdir(parents=True, exist_ok=True)

with open('dfl/typing.txt', 'w') as f:
    f.write("")

app = Flask(__name__,
    static_url_path='/static',
    static_folder='static',
)

def generate_program(code, expansionDepth, show_typing):
    with open("dfl/file.dfl", "w") as f:
        f.write(code)

    command = [
        "java",
        "-jar",
        "dfl/app.jar",
        "dfl/file.dfl",
        "dfl/file.json",
        expansionDepth,
    ]
    if show_typing == 'true':
       command.append('dfl/typing.txt')
    print("Running " + ' '.join(map(str, command)))
    subprocess.run(command)
    return visualization.json_to_graphviz("dfl/file.json", int(expansionDepth))
        


@app.route('/generate-image/<expansionDepth>', methods=['POST'])
def generate_image(expansionDepth):
    data = request.get_json()
    code = data['code']
    show_typing = request.args.get("typing")

    graphviz_code = generate_program(code, expansionDepth, show_typing)
    print(graphviz_code)
    with tempfile.NamedTemporaryFile(suffix='.dot', delete=False) as f:
        f.write(graphviz_code.encode('utf-8'))
        f.flush()
        output_path_png = f.name.replace('.dot', '.png')
        output_path_svg = f.name.replace(".dot", ".svg")
        subprocess.run(['dot', '-n', '-Tpng', f.name, '-o', output_path_png])
        subprocess.run(['dot', '-n', '-Tsvg', f.name, '-o', output_path_svg])
        return send_file(output_path_svg, mimetype='image/svg')

@app.route('/get-typing', methods=['GET'])
def get_typing():
    with open('dfl/typing.txt') as f:
        ty = f.read()
    return jsonify({'ty': ty})

@app.route("/", methods=['GET'])
def index():
    return send_file('index.html')

if __name__ == '__main__':
    app.run(port=5000)

