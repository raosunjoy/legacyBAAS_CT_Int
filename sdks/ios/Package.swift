// swift-tools-version: 5.7

import PackageDescription

let package = Package(
    name: "LegacyBaaSSDK",
    platforms: [
        .iOS(.v13),
        .macOS(.v10_15),
        .watchOS(.v6),
        .tvOS(.v13)
    ],
    products: [
        .library(
            name: "LegacyBaaSSDK",
            targets: ["LegacyBaaSSDK"]),
    ],
    dependencies: [
        .package(url: "https://github.com/daltoniam/Starscream.git", from: "4.0.0")
    ],
    targets: [
        .target(
            name: "LegacyBaaSSDK",
            dependencies: ["Starscream"],
            path: "Sources/LegacyBaaSSDK"
        ),
        .testTarget(
            name: "LegacyBaaSSDKTests",
            dependencies: ["LegacyBaaSSDK"],
            path: "Tests/LegacyBaaSSDKTests"
        ),
    ]
)