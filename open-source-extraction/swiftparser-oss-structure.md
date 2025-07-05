# SwiftParser-OSS Repository Structure

## Directory Layout
```
swiftparser-oss/
├── src/
│   ├── parsers/
│   │   ├── swift/
│   │   │   ├── mt103.js      # Cross-border payments
│   │   │   ├── mt202.js      # Financial institution transfers
│   │   │   ├── mt515.js      # Securities (tokenized assets)
│   │   │   ├── mt700.js      # Trade finance
│   │   │   ├── mt950.js      # Statement messages
│   │   │   └── mt101.js      # Request for transfer
│   │   ├── iso20022/
│   │   │   ├── pacs008.js    # Customer credit transfer
│   │   │   ├── pacs009.js    # Financial institution credit transfer
│   │   │   ├── camt053.js    # Bank to customer statement
│   │   │   └── camt052.js    # Account report
│   │   ├── banking/
│   │   │   ├── bancs.js      # TCS BaNCS XML/flat/JSON parser
│   │   │   ├── fis.js        # FIS Systematics/Profile fixed-width/JSON parser
│   │   │   ├── fiserv.js     # Fiserv DNA formats parser
│   │   │   └── temenos.js    # Temenos Transact JSON/XML parser
│   │   └── additional/
│   │       ├── sepa.js       # SEPA SCT parser
│   │       ├── ach.js        # ACH NACHA parser
│   │       └── cobol-stub.js # COBOL detection stub (directs to enterprise)
│   ├── validators/
│   │   ├── swift-validator.js
│   │   ├── iso20022-validator.js
│   │   └── format-validator.js
│   ├── utils/
│   │   ├── bic-validator.js
│   │   ├── iban-validator.js
│   │   └── amount-parser.js
│   └── index.js              # Main entry point
├── test/
│   ├── parsers/
│   ├── validators/
│   └── fixtures/             # Sample messages
├── examples/
│   ├── cross-border-payment.js
│   ├── tokenized-assets.js
│   ├── trade-finance.js
│   ├── banking-formats.js    # BaNCS, FIS, Fiserv, Temenos examples
│   └── multi-format.js
├── docs/
│   ├── README.md
│   ├── API.md
│   ├── FORMATS.md
│   ├── CONTRIBUTING.md
│   └── ENTERPRISE.md
├── .github/
│   ├── workflows/
│   │   ├── ci.yml
│   │   └── publish.yml
│   └── ISSUE_TEMPLATE/
├── package.json
├── LICENSE                   # Apache 2.0
├── NOTICE                    # IP disclaimers
├── SECURITY.md
└── .gitignore
```

## Banking System Support

### Complete List of Banking Parsers:
1. **TCS BaNCS** (`bancs.js`)
   - XML format
   - Flat file format
   - JSON format

2. **FIS** (`fis.js`)
   - FIS Systematics fixed-width
   - FIS Profile formats
   - FIS JSON API format

3. **Fiserv** (`fiserv.js`)
   - Fiserv DNA message formats
   - Fiserv Premier formats
   - Fiserv Signature formats

4. **Temenos** (`temenos.js`)
   - Temenos Transact (T24) XML
   - Temenos JSON API
   - Temenos proprietary formats

## Key Design Decisions

### 1. **Clear Separation of Concerns**
- Pure parsing logic only (no blockchain conversion)
- No compliance extraction (proprietary)
- No smart routing logic (proprietary)
- No COBOL transpiler (proprietary)

### 2. **Extensible Architecture**
- Plugin-based parser system
- Easy to add new formats
- Community can contribute parsers

### 3. **Enterprise Hooks**
- Stubs for proprietary features
- Clear upgrade path messaging
- License validation points
- COBOL detection stub that directs to enterprise

### 4. **Format Support**
- All formats mentioned in requirements
- Standardized JSON output
- Consistent error handling

### 5. **Monetization Boundaries**
- Open Source: Parsing & validation only
- Enterprise: COBOL transpiler, compliance, blockchain conversion, smart routing
- Clear messaging about enterprise features