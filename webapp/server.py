from flask import Flask, request, send_file
import requests
import subprocess
import tempfile

app = Flask(__name__,
    static_url_path='/static',
    static_folder='static',
)

def generate_graphviz_code(code, expansionDepth):
    with open("dfl/file.dfl", "w") as f:
        f.write(code)

    print("Running java -jar dfl/app.jar dfl/file.dfl dfl/file.dot", expansionDepth)
    subprocess.run(["java", "-jar", "dfl/app.jar", "dfl/file.dfl", "dfl/file.dot", expansionDepth])
    return open("dfl/file.dot").read()

@app.route('/generate-image/<expansionDepth>', methods=['POST'])
def generate_image(expansionDepth):
    data = request.get_json()
    code = data['code']

    graphviz_code = generate_graphviz_code(code, expansionDepth)
    with tempfile.NamedTemporaryFile(suffix='.dot', delete=False) as f:
        f.write(graphviz_code.encode('utf-8'))
        f.flush()
        output_path = f.name.replace('.dot', '.png')
        subprocess.run(['dot', '-Tpng', f.name, '-o', output_path])
        return send_file(output_path, mimetype='image/png')

@app.route("/", methods=['GET'])
def index():
    return send_file('index.html')

if __name__ == '__main__':
    app.run(port=5000)

