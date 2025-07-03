Pod::Spec.new do |spec|
  spec.name             = 'LegacyBaaSSDK'
  spec.version          = '1.0.0'
  spec.summary          = 'Banking Legacy-to-Blockchain B2BaaS Platform SDK for iOS'
  spec.description      = <<-DESC
    A comprehensive iOS SDK for integrating with the Legacy B2BaaS Platform.
    Supports SWIFT message processing, multi-blockchain routing, BaNCS integration,
    and real-time analytics with native iOS features.
                       DESC

  spec.homepage         = 'https://github.com/legacybaas/ios-sdk'
  spec.license          = { :type => 'MIT', :file => 'LICENSE' }
  spec.author           = { 'Legacy B2BaaS Platform' => 'sdk@legacybaas.com' }
  spec.source           = { :git => 'https://github.com/legacybaas/ios-sdk.git', :tag => spec.version.to_s }

  spec.ios.deployment_target = '13.0'
  spec.swift_version = '5.0'

  spec.source_files = 'Sources/LegacyBaaSSDK/**/*'
  spec.frameworks = 'Foundation', 'Network', 'Security'
  spec.requires_arc = true

  # Dependencies
  spec.dependency 'Starscream', '~> 4.0'  # WebSocket support

  # Test spec
  spec.test_spec 'Tests' do |test_spec|
    test_spec.source_files = 'Tests/LegacyBaaSSDKTests/**/*'
  end
end