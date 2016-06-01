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

        return $u;
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

    private function reifyName($number) {
        return "_.$number";
    }

    public function walkStar($v) {
        $newV = $this->walk($v);
        if (isVariable($newV)) {
            return $newV;
        }

        if (isPair($newV)) {
            return new Pair(
                $this->walkStar($newV->getFirst()),
                $this->walkStar($newV->getSecond())
            );
        }

        return $newV;
    }

}

class State {
    private $substitution;
    private $counter;

    public function __construct($substitution, $counter) {
        $this->substitution = $substitution;
        $this->counter = $counter;
    }

    public static function emptyState() {
        return new State(new Substitution(), 0);
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
        $fn = $this->fn;
        return $fn($state);
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
            $freshGoal = $fn($var);
            $newState = new State($s, ($c + 1));

            return $freshGoal->execute($newState);
        }
    );
}

function mplus($S1, $S2) {
    if (empty($S1)) {
        return $S2;
    }

    if (is_callable($S1)) {
        return function () use ($S1, $S2) {
            return mplus($S2, $S1());
        };
    }

    $first = \array_shift($S1);
    $rest = $S1;
    $result = mplus($S2, $rest);

    return array($first, $result);
}

function bind(array $S, Goal $g) {
    if (empty($S)) {
        return mzero();
    }

    if (is_callable($S)) {
        return function () use ($S, $g) {
            return bind($S(), $g);
        };
    }

    $first = \array_shift($S);
    $rest = $S;

    return mplus($g->execute($first), bind($rest, $g));
}

function disj(Goal $g1, Goal $g2) {
    return new Goal(function (State $state) use ($g1, $g2) {
        return mplus($g1->execute($state), $g2->execute($state));
    });
}

function conj(Goal $g1, Goal $g2) {
    return new Goal(function (State $state) use ($g1, $g2) {
        return bind($g1->execute($state), $g2);
    });
}

function pull($S) {
    if (is_callable($S)) {
        return pull($S());
    }

    return $S;
}

function takeAll($S) {
    $S1 = pull($S);
    if (empty($S1)) {
        return array();
    }

    $first = array_shift($S1);
    $rest = $S1;

    $result = takeAll($rest);
    if (empty($result)) {
        return array($first);
    }

    return array($first, $result);
}

function take($n, $S) {
    if ($n == 0) {
        return array();
    }

    $S1 = pull($S);
    if (empty($S1)) {
        return array();
    }

    $first = array_shift($S1);
    $rest = $S1;

    $result = take($n - 1, $rest);
    if (empty($result)) {
        return array($first);
    }

    return array($first, $result);
}

function fives ($x) {
    return disj(
        eq($x, 5),
        new Goal(
            function (State $state) use ($x) {
                return function () use ($x, $state) {
                    return $fives($x);
                };
            }
        )
    );
}

$r = take(2, callFresh('fives')->execute(State::emptyState()));
var_dump($r);

