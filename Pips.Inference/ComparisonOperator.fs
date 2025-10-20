namespace Pips

type ComparisonOperator =
    | LessThan
    | LessThanOrEqualTo
    | EqualTo
    | GreaterThan
    | GreaterThanOrEqualTo

    override op.ToString() =
        match op with
            | LessThan -> "<"
            | LessThanOrEqualTo -> "≤"
            | EqualTo -> "="
            | GreaterThan -> ">"
            | GreaterThanOrEqualTo -> "≥"

module ComparisonOperator =

    let compare pipValue op target =
        match op with
            | LessThan -> pipValue < target
            | LessThanOrEqualTo -> pipValue <= target
            | EqualTo -> pipValue = target
            | GreaterThan -> pipValue > target
            | GreaterThanOrEqualTo -> pipValue >= target
