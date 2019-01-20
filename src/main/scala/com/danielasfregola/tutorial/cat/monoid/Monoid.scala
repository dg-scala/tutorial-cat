package com.danielasfregola.tutorial.cat.monoid

// Monoid - something that generates elements of a set
trait Monoid[A] {

  def identity: A

  def compose(x: A, y: A): A
}
