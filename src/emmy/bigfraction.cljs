(ns bigfraction)

(def C-ONE (js/BigInt 1))
(def C-ZERO (js/BigInt 0))
(def C-TEN (js/BigInt 10))
(def C-TWO (js/BigInt 2))
(def C-FIVE (js/BigInt 5))

(deftype Fraction [s n d])

(defn parse [p1 p2])
  ;; const parse = function(p1, p2) {

  ;;   let n = C_ZERO, d = C_ONE, s = C_ONE;

  ;;   if (p1 === undefined || p1 === null) {
  ;;     /* void */
  ;;   } else if (p2 !== undefined) {
  ;;     n = BigInt(p1);
  ;;     d = BigInt(p2);
  ;;     s = n * d;

  ;;     if (n % C_ONE !== C_ZERO || d % C_ONE !== C_ZERO) {
  ;;       throw NonIntegerParameter();
  ;;     }

  ;;   } else if (typeof p1 === "object") {
  ;;     if ("d" in p1 && "n" in p1) {
  ;;       n = BigInt(p1["n"]);
  ;;       d = BigInt(p1["d"]);
  ;;       if ("s" in p1)
  ;;         n*= BigInt(p1["s"]);
  ;;     } else if (0 in p1) {
  ;;       n = BigInt(p1[0]);
  ;;       if (1 in p1)
  ;;         d = BigInt(p1[1]);
  ;;     } else if (p1 instanceof BigInt) {
  ;;       n = BigInt(p1);
  ;;     } else {
  ;;       throw InvalidParameter();
  ;;     }
  ;;     s = n * d;
  ;;   } else if (typeof p1 === "bigint") {
  ;;     n = p1;
  ;;     s = p1;
  ;;     d = C_ONE;
  ;;   } else if (typeof p1 === "number") {

  ;;     if (isNaN(p1)) {
  ;;       throw InvalidParameter();
  ;;     }

  ;;     if (p1 < 0) {
  ;;       s = -C_ONE;
  ;;       p1 = -p1;
  ;;     }

  ;;     if (p1 % 1 === 0) {
  ;;       n = BigInt(p1);
  ;;     } else if (p1 > 0) { // check for != 0, scale would become NaN (log(0)), which converges really slow

  ;;       let z = 1;

  ;;       let A = 0, B = 1;
  ;;       let C = 1, D = 1;

  ;;       let N = 10000000;

  ;;       if (p1 >= 1) {
  ;;         z = 10 ** Math.floor(1 + Math.log10(p1));
  ;;         p1/= z;
  ;;       }

  ;;       // Using Farey Sequences

  ;;       while (B <= N && D <= N) {
  ;;         let M = (A + C) / (B + D);

  ;;         if (p1 === M) {
  ;;           if (B + D <= N) {
  ;;             n = A + C;
  ;;             d = B + D;
  ;;           } else if (D > B) {
  ;;             n = C;
  ;;             d = D;
  ;;           } else {
  ;;             n = A;
  ;;             d = B;
  ;;           }
  ;;           break;

  ;;         } else {

  ;;           if (p1 > M) {
  ;;             A+= C;
  ;;             B+= D;
  ;;           } else {
  ;;             C+= A;
  ;;             D+= B;
  ;;           }

  ;;           if (B > N) {
  ;;             n = C;
  ;;             d = D;
  ;;           } else {
  ;;             n = A;
  ;;             d = B;
  ;;           }
  ;;         }
  ;;       }
  ;;       n = BigInt(n) * BigInt(z);
  ;;       d = BigInt(d);

  ;;     }

  ;;   } else if (typeof p1 === "string") {

  ;;     let ndx = 0;

  ;;     let v = C_ZERO, w = C_ZERO, x = C_ZERO, y = C_ONE, z = C_ONE;

  ;;     let match = p1.match(/\d+|./g);

  ;;     if (match === null)
  ;;       throw InvalidParameter();

  ;;     if (match[ndx] === '-') {// Check for minus sign at the beginning
  ;;       s = -C_ONE;
  ;;       ndx++;
  ;;     } else if (match[ndx] === '+') {// Check for plus sign at the beginning
  ;;       ndx++;
  ;;     }

  ;;     if (match.length === ndx + 1) { // Check if it's just a simple number "1234"
  ;;       w = assign(match[ndx++], s);
  ;;     } else if (match[ndx + 1] === '.' || match[ndx] === '.') { // Check if it's a decimal number

  ;;       if (match[ndx] !== '.') { // Handle 0.5 and .5
  ;;         v = assign(match[ndx++], s);
  ;;       }
  ;;       ndx++;

  ;;       // Check for decimal places
  ;;       if (ndx + 1 === match.length || match[ndx + 1] === '(' && match[ndx + 3] === ')' || match[ndx + 1] === "'" && match[ndx + 3] === "'") {
  ;;         w = assign(match[ndx], s);
  ;;         y = C_TEN ** BigInt(match[ndx].length);
  ;;         ndx++;
  ;;       }

  ;;       // Check for repeating places
  ;;       if (match[ndx] === '(' && match[ndx + 2] === ')' || match[ndx] === "'" && match[ndx + 2] === "'") {
  ;;         x = assign(match[ndx + 1], s);
  ;;         z = C_TEN ** BigInt(match[ndx + 1].length) - C_ONE;
  ;;         ndx+= 3;
  ;;       }

  ;;     } else if (match[ndx + 1] === '/' || match[ndx + 1] === ':') { // Check for a simple fraction "123/456" or "123:456"
  ;;       w = assign(match[ndx], s);
  ;;       y = assign(match[ndx + 2], C_ONE);
  ;;       ndx+= 3;
  ;;     } else if (match[ndx + 3] === '/' && match[ndx + 1] === ' ') { // Check for a complex fraction "123 1/2"
  ;;       v = assign(match[ndx], s);
  ;;       w = assign(match[ndx + 2], s);
  ;;       y = assign(match[ndx + 4], C_ONE);
  ;;       ndx+= 5;
  ;;     }

  ;;     if (match.length <= ndx) { // Check for more tokens on the stack
  ;;       d = y * z;
  ;;       s = /* void */
  ;;       n = x + d * v + z * w;
  ;;     } else {
  ;;       throw InvalidParameter();
  ;;     }

  ;;   } else {
  ;;     throw InvalidParameter();
  ;;   }

  ;;   if (d === C_ZERO) {
  ;;     throw DivisionByZero();
  ;;   }

  ;;   P["s"] = s < C_ZERO ? -C_ONE : C_ONE;
  ;;   P["n"] = n < C_ZERO ? -n : n;
  ;;   P["d"] = d < C_ZERO ? -d : d;
  ;; };

(defn gcd [a b])
;; function gcd(a, b) {

;;                     if (!a)
;;                     return b;
;;                     if (!b)
;;                     return a;

;;                     while (1) {
;;                                a%= b;
;;                                if (!a)
;;                                return b;
;;                                b%= a;
;;                                if (!b)
;;                                return a;
;;                                }
;;                     }

(defn new-fraction [])
;; // Creates a new Fraction internally without the need of the bulky constructor
;; function newFraction(n, d) {

;;                             if (d === C_ZERO) {
;;                                                throw DivisionByZero();
;;                                                }

;;                             const f = Object.create(Fraction.prototype);
;;                             f["s"] = n < C_ZERO ? -C_ONE : C_ONE;

;;                             n = n < C_ZERO ? -n : n;

;;                             const a = gcd(n, d);

;;                             f["n"] = n / a;
;;                             f["d"] = d / a;
;;                             return f;
;;                             }

(defn build [])
;; function Fraction(a, b) {

;;                          parse(a, b);

;;                          if (this instanceof Fraction) {
;;                                                         a = gcd(P["d"], P["n"]); // Abuse a
;;                                                         this["s"] = P["s"];
;;                                                         this["n"] = P["n"] / a;
;;                                                         this["d"] = P["d"] / a;
;;                                                         } else {
;;                                                                 return newFraction(P['s'] * P['n'], P['d']);
;;                                                                 }
;;                          }


(defn abs [x])
;; /**
;;      * Calculates the absolute value
;;      *
;;      * Ex: new Fraction(-4).abs() => 4
;;      **/
;;     "abs": function() {

;;       return newFraction(this["n"], this["d"]);
;;     },

(defn neg [^Fraction x]
  )
;;     /**
;;      * Inverts the sign of the current fraction
;;      *
;;      * Ex: new Fraction(-4).neg() => 4
;;      **/
;;     "neg": function() {

;;       return newFraction(-this["s"] * this["n"], this["d"]);
;;     },

(defn add [^Fraction a ^Fraction b]
  (Fraction. (* (.-s a))))
;;     /**
;;      * Adds two rational numbers
;;      *
;;      * Ex: new Fraction({n: 2, d: 3}).add("14.9") => 467 / 30
;;      **/
;;     "add": function(a, b) {

;;       parse(a, b);
;;       return newFraction(
;;         this["s"] * this["n"] * P["d"] + P["s"] * this["d"] * P["n"],
;;         this["d"] * P["d"]
;;       );
;;     },

(defn sub [a b])
;;     /**
;;      * Subtracts two rational numbers
;;      *
;;      * Ex: new Fraction({n: 2, d: 3}).add("14.9") => -427 / 30
;;      **/
;;     "sub": function(a, b) {

;;       parse(a, b);
;;       return newFraction(
;;         this["s"] * this["n"] * P["d"] - P["s"] * this["d"] * P["n"],
;;         this["d"] * P["d"]
;;       );
;;     },

(defn mul [a b])
;;     /**
;;      * Multiplies two rational numbers
;;      *
;;      * Ex: new Fraction("-17.(345)").mul(3) => 5776 / 111
;;      **/
;;     "mul": function(a, b) {

;;       parse(a, b);
;;       return newFraction(
;;         this["s"] * P["s"] * this["n"] * P["n"],
;;         this["d"] * P["d"]
;;       );
;;     },

(defn div [a b])
;;     /**
;;      * Divides two rational numbers
;;      *
;;      * Ex: new Fraction("-17.(345)").inverse().div(3)
;;      **/
;;     "div": function(a, b) {

;;       parse(a, b);
;;       return newFraction(
;;         this["s"] * P["s"] * this["n"] * P["d"],
;;         this["d"] * P["n"]
;;       );
;;     },

(defn clone [x])
;;     /**
;;      * Clones the actual object
;;      *
;;      * Ex: new Fraction("-17.(345)").clone()
;;      **/
;;     "clone": function() {
;;       return newFraction(this['s'] * this['n'], this['d']);
;;     },

(defn mod [x])
;;     /**
;;      * Calculates the modulo of two rational numbers - a more precise fmod
;;      *
;;      * Ex: new Fraction('4.(3)').mod([7, 8]) => (13/3) % (7/8) = (5/6)
;;      **/
;;     "mod": function(a, b) {

;;       if (a === undefined) {
;;         return newFraction(this["s"] * this["n"] % this["d"], C_ONE);
;;       }

;;       parse(a, b);
;;       if (0 === P["n"] && 0 === this["d"]) {
;;         throw DivisionByZero();
;;       }

;;       /*
;;        * First silly attempt, kinda slow
;;        *
;;        return that["sub"]({
;;        "n": num["n"] * Math.floor((this.n / this.d) / (num.n / num.d)),
;;        "d": num["d"],
;;        "s": this["s"]
;;        });*/

;;       /*
;;        * New attempt: a1 / b1 = a2 / b2 * q + r
;;        * => b2 * a1 = a2 * b1 * q + b1 * b2 * r
;;        * => (b2 * a1 % a2 * b1) / (b1 * b2)
;;        */
;;       return newFraction(
;;         this["s"] * (P["d"] * this["n"]) % (P["n"] * this["d"]),
;;         P["d"] * this["d"]
;;       );
;;     },

(defn gcd [x])
;;     /**
;;      * Calculates the fractional gcd of two rational numbers
;;      *
;;      * Ex: new Fraction(5,8).gcd(3,7) => 1/56
;;      */
;;     "gcd": function(a, b) {

;;       parse(a, b);

;;       // gcd(a / b, c / d) = gcd(a, c) / lcm(b, d)

;;       return newFraction(gcd(P["n"], this["n"]) * gcd(P["d"], this["d"]), P["d"] * this["d"]);
;;     },

(defn lcm [x])
;;     /**
;;      * Calculates the fractional lcm of two rational numbers
;;      *
;;      * Ex: new Fraction(5,8).lcm(3,7) => 15
;;      */
;;     "lcm": function(a, b) {

;;       parse(a, b);

;;       // lcm(a / b, c / d) = lcm(a, c) / gcd(b, d)

;;       if (P["n"] === C_ZERO && this["n"] === C_ZERO) {
;;         return newFraction(C_ZERO, C_ONE);
;;       }
;;       return newFraction(P["n"] * this["n"], gcd(P["n"], this["n"]) * gcd(P["d"], this["d"]));
;;     },

(defn inverse [x])
;;     /**
;;      * Gets the inverse of the fraction, means numerator and denominator are exchanged
;;      *
;;      * Ex: new Fraction([-3, 4]).inverse() => -4 / 3
;;      **/
;;     "inverse": function() {
;;       return newFraction(this["s"] * this["d"], this["n"]);
;;     },

(defn pow [a b])
;;     /**
;;      * Calculates the fraction to some integer exponent
;;      *
;;      * Ex: new Fraction(-1,2).pow(-3) => -8
;;      */
;;     "pow": function(a, b) {

;;       parse(a, b);

;;       // Trivial case when exp is an integer

;;       if (P['d'] === C_ONE) {

;;         if (P['s'] < C_ZERO) {
;;           return newFraction((this['s'] * this["d"]) ** P['n'], this["n"] ** P['n']);
;;         } else {
;;           return newFraction((this['s'] * this["n"]) ** P['n'], this["d"] ** P['n']);
;;         }
;;       }

;;       // Negative roots become complex
;;       //     (-a/b)^(c/d) = x
;;       // <=> (-1)^(c/d) * (a/b)^(c/d) = x
;;       // <=> (cos(pi) + i*sin(pi))^(c/d) * (a/b)^(c/d) = x
;;       // <=> (cos(c*pi/d) + i*sin(c*pi/d)) * (a/b)^(c/d) = x       # DeMoivre's formula
;;       // From which follows that only for c=0 the root is non-complex
;;       if (this['s'] < C_ZERO) return null;

;;       // Now prime factor n and d
;;       let N = factorize(this['n']);
;;       let D = factorize(this['d']);

;;       // Exponentiate and take root for n and d individually
;;       let n = C_ONE;
;;       let d = C_ONE;
;;       for (let k in N) {
;;         if (k === '1') continue;
;;         if (k === '0') {
;;           n = C_ZERO;
;;           break;
;;         }
;;         N[k]*= P['n'];

;;         if (N[k] % P['d'] === C_ZERO) {
;;           N[k]/= P['d'];
;;         } else return null;
;;         n*= BigInt(k) ** N[k];
;;       }

;;       for (let k in D) {
;;         if (k === '1') continue;
;;         D[k]*= P['n'];

;;         if (D[k] % P['d'] === C_ZERO) {
;;           D[k]/= P['d'];
;;         } else return null;
;;         d*= BigInt(k) ** D[k];
;;       }

;;       if (P['s'] < C_ZERO) {
;;         return newFraction(d, n);
;;       }
;;       return newFraction(n, d);
;;     },

(defn equals [a b])
;;     /**
;;      * Check if two rational numbers are the same
;;      *
;;      * Ex: new Fraction(19.6).equals([98, 5]);
;;      **/
;;     "equals": function(a, b) {

;;       parse(a, b);
;;       return this["s"] * this["n"] * P["d"] === P["s"] * P["n"] * this["d"]; // Same as compare() === 0
;;     },

(defn compare [a b])
;;     /**
;;      * Check if two rational numbers are the same
;;      *
;;      * Ex: new Fraction(19.6).equals([98, 5]);
;;      **/
;;     "compare": function(a, b) {

;;       parse(a, b);
;;       let t = (this["s"] * this["n"] * P["d"] - P["s"] * P["n"] * this["d"]);

;;       return (C_ZERO < t) - (t < C_ZERO);
;;     },

(defn ceil [x])
;;     /**
;;      * Calculates the ceil of a rational number
;;      *
;;      * Ex: new Fraction('4.(3)').ceil() => (5 / 1)
;;      **/
;;     "ceil": function(places) {

;;       places = C_TEN ** BigInt(places || 0);

;;       return newFraction(this["s"] * places * this["n"] / this["d"] +
;;         (places * this["n"] % this["d"] > C_ZERO && this["s"] >= C_ZERO ? C_ONE : C_ZERO),
;;         places);
;;     },

(defn floor [x])
;;     /**
;;      * Calculates the floor of a rational number
;;      *
;;      * Ex: new Fraction('4.(3)').floor() => (4 / 1)
;;      **/
;;     "floor": function(places) {

;;       places = C_TEN ** BigInt(places || 0);

;;       return newFraction(this["s"] * places * this["n"] / this["d"] -
;;         (places * this["n"] % this["d"] > C_ZERO && this["s"] < C_ZERO ? C_ONE : C_ZERO),
;;         places);
;;     },

(defn round [x])
;;     /**
;;      * Rounds a rational numbers
;;      *
;;      * Ex: new Fraction('4.(3)').round() => (4 / 1)
;;      **/
;;     "round": function(places) {

;;       places = C_TEN ** BigInt(places || 0);

;;       /* Derivation:

;;       s >= 0:
;;         round(n / d) = trunc(n / d) + (n % d) / d >= 0.5 ? 1 : 0
;;                      = trunc(n / d) + 2(n % d) >= d ? 1 : 0
;;       s < 0:
;;         round(n / d) =-trunc(n / d) - (n % d) / d > 0.5 ? 1 : 0
;;                      =-trunc(n / d) - 2(n % d) > d ? 1 : 0

;;       =>:

;;       round(s * n / d) = s * trunc(n / d) + s * (C + 2(n % d) > d ? 1 : 0)
;;           where C = s >= 0 ? 1 : 0, to fix the >= for the positve case.
;;       */

;;       return newFraction(this["s"] * places * this["n"] / this["d"] +
;;         this["s"] * ((this["s"] >= C_ZERO ? C_ONE : C_ZERO) + C_TWO * (places * this["n"] % this["d"]) > this["d"] ? C_ONE : C_ZERO),
;;         places);
;;     },

(defn divisible [a b])
;;     /**
;;      * Check if two rational numbers are divisible
;;      *
;;      * Ex: new Fraction(19.6).divisible(1.5);
;;      */
;;     "divisible": function(a, b) {

;;       parse(a, b);
;;       return !(!(P["n"] * this["d"]) || ((this["n"] * P["d"]) % (P["n"] * this["d"])));
;;     },

(defn value-of [x])
;;     /**
;;      * Returns a decimal representation of the fraction
;;      *
;;      * Ex: new Fraction("100.'91823'").valueOf() => 100.91823918239183
;;      **/
;;     'valueOf': function() {
;;       // Best we can do so far
;;       return Number(this["s"] * this["n"]) / Number(this["d"]);
;;     },


(defn to-string [x])
;; /**
;;      * Creates a string representation of a fraction with all digits
;;      *
;;      * Ex: new Fraction("100.'91823'").toString() => "100.(91823)"
;;      **/
;;     'toString': function(dec) {

;;       let N = this["n"];
;;       let D = this["d"];

;;       function trunc(x) {
;;           return typeof x === 'bigint' ? x : Math.floor(x);
;;       }

;;       dec = dec || 15; // 15 = decimal places when no repetition

;;       let cycLen = cycleLen(N, D); // Cycle length
;;       let cycOff = cycleStart(N, D, cycLen); // Cycle start

;;       let str = this['s'] < C_ZERO ? "-" : "";

;;       // Append integer part
;;       str+= trunc(N / D);

;;       N%= D;
;;       N*= C_TEN;

;;       if (N)
;;         str+= ".";

;;       if (cycLen) {

;;         for (let i = cycOff; i--;) {
;;           str+= trunc(N / D);
;;           N%= D;
;;           N*= C_TEN;
;;         }
;;         str+= "(";
;;         for (let i = cycLen; i--;) {
;;           str+= trunc(N / D);
;;           N%= D;
;;           N*= C_TEN;
;;         }
;;         str+= ")";
;;       } else {
;;         for (let i = dec; N && i--;) {
;;           str+= trunc(N / D);
;;           N%= D;
;;           N*= C_TEN;
;;         }
;;       }
;;       return str;
;;     },

(defn to-fraction [x])
;;     /**
;;      * Returns a string-fraction representation of a Fraction object
;;      *
;;      * Ex: new Fraction("1.'3'").toFraction() => "4 1/3"
;;      **/
;;     'toFraction': function(excludeWhole) {

;;       let n = this["n"];
;;       let d = this["d"];
;;       let str = this['s'] < C_ZERO ? "-" : "";

;;       if (d === C_ONE) {
;;         str+= n;
;;       } else {
;;         let whole = n / d;
;;         if (excludeWhole && whole > C_ZERO) {
;;           str+= whole;
;;           str+= " ";
;;           n%= d;
;;         }

;;         str+= n;
;;         str+= '/';
;;         str+= d;
;;       }
;;       return str;
;;     },

(defn to-latex [])
;;     /**
;;      * Returns a latex representation of a Fraction object
;;      *
;;      * Ex: new Fraction("1.'3'").toLatex() => "\frac{4}{3}"
;;      **/
;;     'toLatex': function(excludeWhole) {

;;       let n = this["n"];
;;       let d = this["d"];
;;       let str = this['s'] < C_ZERO ? "-" : "";

;;       if (d === C_ONE) {
;;         str+= n;
;;       } else {
;;         let whole = n / d;
;;         if (excludeWhole && whole > C_ZERO) {
;;           str+= whole;
;;           n%= d;
;;         }

;;         str+= "\\frac{";
;;         str+= n;
;;         str+= '}{';
;;         str+= d;
;;         str+= '}';
;;       }
;;       return str;
;;     },

(defn to-continued [])
;;     /**
;;      * Returns an array of continued fraction elements
;;      *
;;      * Ex: new Fraction("7/8").toContinued() => [0,1,7]
;;      */
;;     'toContinued': function() {

;;       let a = this['n'];
;;       let b = this['d'];
;;       let res = [];

;;       do {
;;         res.push(a / b);
;;         let t = a % b;
;;         a = b;
;;         b = t;
;;       } while (a !== C_ONE);

;;       return res;
;;     },

(defn simplify [])

;;     "simplify": function(eps) {

;;       eps = eps || 0.001;

;;       const thisABS = this['abs']();
;;       const cont = thisABS['toContinued']();

;;       for (let i = 1; i < cont.length; i++) {

;;         let s = newFraction(cont[i - 1], C_ONE);
;;         for (let k = i - 2; k >= 0; k--) {
;;           s = s['inverse']()['add'](cont[k]);
;;         }

;;         if (Math.abs(s['sub'](thisABS).valueOf()) < eps) {
;;           return s['mul'](this['s']);
;;         }
;;       }
;;       return this;
;;     }
;;   };
