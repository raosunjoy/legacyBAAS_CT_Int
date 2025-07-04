import pytest
from app import CobolTranspiler
from pathlib import Path

@pytest.fixture
def fis_config():
    return Path('fis-ibs.yaml')

def test_fis_ibs_parsing(fis_config):
    transpiler = CobolTranspiler(fis_config)
    ast = transpiler.parse(Path('ibs_loan.cbl').read_text())
    assert ast['variables'][0]['name'] == 'principal'
    assert ast['logic'][0]['operation'] == 'compute'

def test_fiserv_dna_generation(tmp_path, fis_config):
    transpiler = CobolTranspiler(fis_config)
    ast = transpiler.parse(Path('dna_settle.cbl').read_text())
    output = tmp_path / "output.kt"
    code = transpiler.generate(ast, output)
    assert "require(amount <= 10000)" in code

@pytest.mark.asyncio
async def test_e2e_transpilation():
    import httpx
    async with httpx.AsyncClient() as client:
        files = {"files": open('ibs_loan.cbl', 'rb'), "config": open('fis-ibs.yaml', 'rb')}
        response = await client.post(
            "http://localhost:8000/transpile",
            files=files,
            headers={"Authorization": "Bearer valid-token"}
        )
        assert response.status_code == 200
        assert "contract IBSLoan" in response.json()['code']

@given(st.text())
def test_fuzz_parsing(cobol_input):
    transpiler = CobolTranspiler('fis-ibs.yaml')
    try:
        transpiler.parse(cobol_input)
    except SyntaxError:
        pass  # Expected for malformed input