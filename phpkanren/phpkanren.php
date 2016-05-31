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

class State {
    private $substitution;
    private $counter;

    public function __construct($substitution, $counter) {
        $this->substitution = $substitution;
        $this->counter = $counter;
    }

    public function getSubstitution() {
        return $this->substitution;
    }

    public function getCounter() {
        return $this->counter;
    }
}

class Goal {
    private $fn;

    public function __construct(callable $fn) {
        $this->fn = $fn;
    }

    public function execute(State $state) {
        return $this->fn($state);
    }
}

function unify($u, $v, Substitution $s) {
    $u = $s->walk($u);
    $v = $s->walk($v);

    if (isVariable($u) && isVariable($v)) {
        if ($u->equals($v)) {
            return $s;
        }
    } else if (isVariable($u)) {
        return $s->extend(new Mapping($u, $v));
    } else if (isVariable($v)) {
        return $s->extend(new Mapping($v, $u));
    } else if (isPair($u) && isPair($v)) {
        $s1 = unify($u->getFirst(), $v->getFirst(), $s);
        $s2 = unify($u->getSecond(), $v->getSecond(), $s1);

        return $s2;
    } else if ($u === $v) {
        return $s;
    } else {
        return null;
    }
}

function mzero() {
    return array();
}

function unit(State $state) {
    return array($state);
}

function eq($u, $v) {
    return new Goal(
        function (State $state) use ($u, $v) {
            $s = unify($u, $v, $state->getSubstitution());
            if (isset($s)) {
                return unit(new State($s, $state->getCounter()));
            } else {
                return mzero();
            }
        }
    );
}

function callFresh(callable $fn) {
    return new Goal(
        function (State $state) use ($fn) {
            $s = $state->getSubstitution();
            $c = $state->getCounter();

            $var = new Variable($c);
            $newState = new State($s, ($c + 1));
            $freshFn = $fn($c);

            return $freshFn($newState);
        }
    );
}

function mplus($S1, $S2) {
    if (!isset($S1)) {
        return $S2;
    }

    if (is_callable($S1)) {
        return function () use ($S1, $S2) {
            return mplus($S2, $S1());
        };
    }

    $first = \array_shift($S1);
    return \array_merge($first, mplus($S2, $S1));
}

function bind($S, Goal $g) {
    if (!isset($S)) {
        return mzero();
    }

    if (is_callable($S)) {
        return function () use ($S, $g) {
            return bind($S(), $g);
        };
    }

    $first = \array_shift($S);
    return \array_merge($first, bind($S, $g));
}

$s = (new Substitution())
    ->extend(new Mapping(new Variable(0), new Variable(1)))
    ->extend(new Mapping(new Variable(1), new Variable(2)))
    ->extend(new Mapping(new Variable(2), new Pair(new Variable(1), 5)));

$r = $s->walk(new Variable(0));
var_dump($r);

