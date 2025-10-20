# C2R Test Suite

Comprehensive test suite for identifying and debugging issues in Patternizer, Samplizer, and Handler systems.

## Test Organization

### 1. **patternizer_tests.rs** - Pattern Detection Tests

Tests Patternizer's ability to match C patterns:

- ✅ Function pattern matching
- ✅ Struct pattern matching
- ✅ Enum pattern matching
- ✅ Typedef pattern matching
- ✅ Loop pattern matching
- ✅ Array pattern matching
- ✅ Macro pattern matching
- ✅ Comment pattern matching
- ✅ Global variable pattern matching
- ✅ Type detection (is_c_type)
- ✅ Scoring functions (structure, type, syntax scores)
- ✅ Edge cases (empty tokens, single token)

**Run:**

```bash
cargo test patternizer --lib -- --nocapture
```

### 2. **samplizer_tests.rs** - Confidence Scoring Tests

Tests Samplizer's confidence calculation for patterns:

- ✅ Function confidence scoring
- ✅ Struct confidence scoring
- ✅ Loop confidence scoring
- ✅ Empty token handling
- ✅ Invalid pattern handling
- ✅ Multiple pattern analysis
- ✅ Confidence bounds validation (0.0-1.0)
- ✅ Range sensitivity testing
- ✅ Capture-based analysis
- ✅ Pattern priority determination

**Run:**

```bash
cargo test samplizer --lib -- --nocapture
```

### 3. **handler_integration_tests.rs** - Handler Pipeline Tests

Tests full handler detection and processing pipeline:

- ✅ Function handler detection
- ✅ Struct handler detection
- ✅ Enum handler detection
- ✅ Typedef handler detection
- ✅ Comment handler detection
- ✅ Loop handler detection
- ✅ Array handler detection
- ✅ Macro handler detection
- ✅ Global handler detection
- ✅ Variable handler detection
- ✅ Expression handler detection
- ✅ Confidence threshold testing
- ✅ Full pipeline (can_process → process → extract → convert)
- ✅ Edge case testing

**Run:**

```bash
cargo test handler_integration --lib -- --nocapture
```

## Running All Tests

```bash
# Run all test suites
cargo test --lib -- --nocapture

# Run specific test module
cargo test patternizer_tests --lib -- --nocapture
cargo test samplizer_tests --lib -- --nocapture
cargo test handler_integration_tests --lib -- --nocapture

# Run specific test
cargo test test_function_handler_detection --lib -- --nocapture
```

## Expected Issues to Find

Based on the low success rates (16.7-37.5%), these tests should reveal:

### Pattern Detection Issues

- ❌ Patternizer not matching valid patterns
- ❌ Pattern registry not populated correctly
- ❌ Token matching logic failures
- ❌ Structural analysis returning 0.0 scores

### Confidence Scoring Issues

- ❌ Samplizer returning low/zero confidence for valid patterns
- ❌ analyze_with_range returning incorrect values
- ❌ Pattern-specific confidence calculation failures
- ❌ Threshold mismatches in handlers

### Handler Issues

- ❌ can_process rejecting valid structures
- ❌ detect_pattern methods failing
- ❌ Confidence thresholds too high (>0.7)
- ❌ Context access issues in handlers
- ❌ Tokenizer integration problems
- ❌ Extract/convert pipeline failures

## Debugging Workflow

1. **Start with Patternizer tests** - Verify basic pattern matching works
2. **Move to Samplizer tests** - Check confidence calculation
3. **Test handler integration** - Verify full pipeline
4. **Identify failure points** - Look for error messages, panics, false negatives
5. **Fix systematically** - Start with lowest level (Patternizer) first

## Common Failure Patterns

### Pattern Not Registered

```
❌ NoMatch { reason: "Pattern 'function' not found in registry" }
```

**Fix**: Ensure pattern is registered in Patternizer initialization

### Low Confidence

```
Function confidence: 0.12
❌ ISSUE: Handler threshold (0.7) rejects valid pattern
```

**Fix**: Adjust Samplizer scoring or lower handler thresholds

### Token Access Issues

```
❌ Function handler error: Token slot empty
```

**Fix**: Verify tokenizer integration and token range validity

### Empty Pattern Match

```
❌ Pattern matched but consumed 0 tokens
```

**Fix**: Check pattern matching logic returns correct token count

## Test Output Interpretation

### ✅ PASS

Pattern detected correctly, confidence acceptable, handler accepts

### ❌ FAIL

Pattern not detected or handler rejects valid input

### ⚠️ WARNING

Test passes but with suboptimal behavior (low confidence, etc.)

## Adding New Tests

```rust
#[test]
fn test_my_pattern() {
    let mut patternizer = Patternizer::new();
    
    let tokens = vec![
        Token::s("token1"),
        Token::s("token2"),
    ];
    
    let result = patternizer.match_pattern("my_pattern", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Pattern matched: {} tokens", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Pattern failed: {}", reason);
        }
    }
}
```

## Success Criteria

- **Patternizer**: All basic patterns detected (function, struct, enum, etc.)
- **Samplizer**: Confidence > 0.5 for valid patterns
- **Handlers**: Success rate > 80% for well-formed C code
- **Pipeline**: Full extract→convert chain completes for valid inputs

## Known Issues from Output

From recent runs showing 16.7-37.5% success:

1. Most handlers fail `can_process` check
2. Confidence scores likely too low
3. Pattern detection may not be working
4. Handler thresholds may be too strict
5. Integration between Patternizer/Samplizer/Handlers broken

These tests will pinpoint exactly where each failure occurs.
