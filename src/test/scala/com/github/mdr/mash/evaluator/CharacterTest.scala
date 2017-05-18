package com.github.mdr.mash.evaluator

class CharacterTest extends AbstractEvaluatorTest {

  "'1234'.all (.isDigit)" ==> true
  "'12b34'.all (.isDigit)" ==> false

  "'abc'[0].tag" ==> "Character"

  "'1'.toCharacter.isDigit" ==> true
  "'123'.toCharacter".shouldThrowAnException

}
