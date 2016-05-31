<?php

class Variable {
    private $counter;

    public function __construct($counter) {
        $this->counter = $counter;
    }

    public function getCounter() {
        return $this->counter;
    }

    public function equals(Variable $right) {
        return ($this->getCounter() == $right->getCounter());
    }
}

function isVariable($x) {
    return ($x instanceof Variable);
}

class Mapping {
    private $variable;
    private $value;

    public function __construct(Variable $variable, $value) {
        $this->variable = $variable;
        $this->value = $value;
    }

    public function getVariable() {
        return $this->variable;
    }

    public function getValue() {
        return $this->value;
    }
}

class Pair {
    private $first;
    private $second;

    public function __construct($first, $second) {
        $this->first = $first;
        $this->second = $second;
    }

    public function getFirst() {
        return $this->first;
    }

    public function getSecond() {
        return $this->second;
    }
}

function isPair($x) {
    return ($x instanceof Pair);
}

class Substitution {
    private $mappings;

    public function __construct($mappings = array()) {
        $this->mappings = $mappings;
    }

    public function walk($u) {
        if (!isVariable($u)) {
            return $u;
        }

        foreach ($this->mappings as $mapping) {
            $variable = $mapping->getVariable();
            $value = $mapping->getValue();

            if ($u->equals($variable)) {
                return $this->walk($value);
            }
        }

        return false;
    }

    public function extend(Mapping $mapping) {
        if ($this->occursCheck($mapping)) {
            return false;
        }

        return new Substitution(\array_merge(array($mapping), $this->mappings));
    }

    private function occursCheck(Mapping $mapping) {
        $variable = $mapping->getVariable();
        $value = $mapping->getValue();

        $v = $this->walk($value);

        if (isVariable($v)) {
            return ($variable->equals($v));
        }

        if (isPair($v)) {
            return $this->occursCheck(new Mapping($variable, $v->getFirst()))
                || $this->occursCheck(new Mapping($variable, $v->getSecond()));
        }

        return false;
    }
}

$s = (new Substitution())
    ->extend(new Mapping(new Variable(0), new Variable(1)))
    ->extend(new Mapping(new Variable(1), new Variable(2)));

$r = $s->walk(new Variable(0));
var_dump($r);

