[<AutoOpen>]
module Doculisp.Tests.Helpers

open Xunit

let shouldBeEqualTo<'a when 'a: equality> (expected: 'a) (actual: 'a) =
    Assert.Equal (expected, actual)