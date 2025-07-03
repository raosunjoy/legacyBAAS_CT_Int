# Legacy B2BaaS .NET SDK

Banking Legacy-to-Blockchain B2BaaS Platform SDK for .NET

## Installation

Install the package via NuGet:

```bash
dotnet add package LegacyBaaS.SDK
```

Or via Package Manager Console:

```powershell
Install-Package LegacyBaaS.SDK
```

## Quick Start

### Basic Usage

```csharp
using LegacyBaaS.SDK;
using LegacyBaaS.SDK.Models.Swift;

var client = new LegacyBaaSClient("your-api-key");

// Process SWIFT MT103 message
var mt103 = new MT103Message
{
    TransactionReference = "TXN123456",
    Currency = "USD",
    Amount = 10000m,
    OrderingCustomer = new Customer 
    { 
        Account = "123456789", 
        Name = "John Doe" 
    },
    BeneficiaryCustomer = new Customer 
    { 
        Account = "987654321", 
        Name = "Jane Smith" 
    }
};

var result = await client.Swift.ProcessMT103Async(mt103);
Console.WriteLine($"Transaction ID: {result.TransactionId}");
```

### ASP.NET Core Integration

Add to `Program.cs` or `Startup.cs`:

```csharp
// Using configuration
builder.Services.AddLegacyBaaS(builder.Configuration);

// Or with API key directly
builder.Services.AddLegacyBaaS("your-api-key", options =>
{
    options.Environment = LegacyBaaSEnvironment.Production;
    options.Timeout = TimeSpan.FromSeconds(30);
});
```

Configuration in `appsettings.json`:

```json
{
  "LegacyBaaS": {
    "ApiKey": "your-api-key",
    "Environment": "Production",
    "Timeout": "00:00:30",
    "MaxRetries": 3,
    "EnableLogging": true
  }
}
```

Use in controllers:

```csharp
[ApiController]
[Route("api/[controller]")]
public class PaymentsController : ControllerBase
{
    private readonly LegacyBaaSClient _legacyBaaSClient;

    public PaymentsController(LegacyBaaSClient legacyBaaSClient)
    {
        _legacyBaaSClient = legacyBaaSClient;
    }

    [HttpPost("mt103")]
    public async Task<IActionResult> ProcessMT103([FromBody] MT103Message message)
    {
        var result = await _legacyBaaSClient.Swift.ProcessMT103Async(message);
        return Ok(result);
    }
}
```

## Features

- **Enterprise-Grade**: Built for .NET 6, 7, and 8 with async/await support
- **ASP.NET Core Integration**: Native dependency injection and configuration
- **Azure Ready**: Optimized for Azure deployment and scaling
- **Type Safety**: Full IntelliSense support with comprehensive XML documentation
- **Performance**: High-throughput async operations with connection pooling
- **Security**: Enterprise authentication and certificate support
- **Compliance**: Banking-grade security and audit trails

## Supported Services

- **SWIFT Processing**: MT103, MT202, MT700 message handling
- **Blockchain Routing**: Multi-blockchain transaction routing
- **BaNCS Integration**: Legacy banking system connectivity
- **Analytics**: Real-time transaction analytics and reporting
- **Webhooks**: Event-driven notifications and callbacks

## Configuration Options

| Option | Description | Default |
|--------|-------------|---------|
| `ApiKey` | Your Legacy B2BaaS API key | Required |
| `Environment` | Target environment (Production/Staging/Sandbox) | Production |
| `BaseUrl` | Custom base URL (overrides environment) | Environment default |
| `Timeout` | Request timeout | 30 seconds |
| `MaxRetries` | Maximum retry attempts | 3 |
| `RetryDelay` | Delay between retries | 1 second |
| `EnableLogging` | Enable SDK logging | true |

## Error Handling

The SDK provides comprehensive error handling with specific exception types:

```csharp
try
{
    var result = await client.Swift.ProcessMT103Async(mt103);
}
catch (AuthenticationException ex)
{
    // Handle authentication errors
    Console.WriteLine($"Authentication failed: {ex.Message}");
}
catch (ValidationException ex)
{
    // Handle validation errors
    Console.WriteLine($"Validation failed: {ex.Message}");
    if (ex.Details != null)
    {
        Console.WriteLine($"Details: {ex.Details}");
    }
}
catch (RateLimitException ex)
{
    // Handle rate limiting
    Console.WriteLine($"Rate limited. Retry after: {ex.RetryAfterSeconds} seconds");
}
catch (LegacyBaaSException ex)
{
    // Handle general API errors
    Console.WriteLine($"API error: {ex.Message} (Code: {ex.ErrorCode})");
}
```

## Documentation

For complete documentation, visit [https://docs.legacybaas.com/sdks/dotnet](https://docs.legacybaas.com/sdks/dotnet)

## Support

- GitHub Issues: [https://github.com/legacybaas/dotnet-sdk/issues](https://github.com/legacybaas/dotnet-sdk/issues)
- Documentation: [https://docs.legacybaas.com](https://docs.legacybaas.com)
- Email: sdk@legacybaas.com

## License

This project is licensed under the MIT License - see the LICENSE file for details.