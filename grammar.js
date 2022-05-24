const PREC = {
  first: $ => prec(100, $),
  last: $ => prec(-100, $),
  left: prec.left,
  right: prec.right,
};

const LEAF = {
  // https://en.wikipedia.org/wiki/Unicode_character_property#Whitespace
  whitespace: /[ \t\n\v\f\r\u{0085}\u{00A0}\u{1680}\u{2000}-\u{200A}\u{2028}\u{2029}\u{202F}\u{205F}\u{3000}]/u,
  newline: /[\r\n\u{85}\u{2028}\u{2029}]/,
  delimiter: /[ \t\n\v\f\r\u{0085}\u{00A0}\u{1680}\u{2000}-\u{200A}\u{2028}\u{2029}\u{202F}\u{205F}\u{3000}\u{FEFF}(){}",'`;\[\]]/u,
  non_delimiter: /[^ \t\n\v\f\r\u{0085}\u{00A0}\u{1680}\u{2000}-\u{200A}\u{2028}\u{2029}\u{202F}\u{205F}\u{3000}\u{FEFF}(){}",'`;\[\]]/u,

  // first character of symbol or number or extflonum
  // symbol_or_number_start = not (delimiter or "#")
  symbol_or_number_start: /[^# \t\n\v\f\r\u{0085}\u{00A0}\u{1680}\u{2000}-\u{200A}\u{2028}\u{2029}\u{202F}\u{205F}\u{3000}\u{FEFF}(){}",'`;\[\]]/u,

  any_char: /.|[\r\n\u{85}\u{2028}\u{2029}]/,
};

function paren(tok) {
  return choice(
    seq("(", tok, ")"),
    seq("[", tok, "]"),
    seq("{", tok, "}"));
}

module.exports = grammar({
  name: 'racket',

  extras: _ => [],

  externals: _ => [
    // TODO: $.here_string,
  ],

  rules: {
    program: $ => repeat($._token),

    _token: $ =>
      choice(
        // $._whitespace,
        LEAF.whitespace,
        $.comment,
        $.extension,
        $._datum),

    _skip: $ => choice(LEAF.whitespace, $.comment),

    dot: _ => ".",

    // comment {{{

    comment: $ =>
      choice(
        seq(/;.*/),
        $._block_comment,
        seq("#;",
          repeat($._skip),
          $._datum),
        /#[cC][iIsS]/, // read-case-sensitive parameter
        seq(
          choice("#! ", "#!/"),
          repeat(seq(/.*/, "\\", LEAF.newline)),
          /.*/)),

    _block_comment: $ =>
      seq("#|",
        repeat(
          choice(
            PREC.first($._block_comment),
            LEAF.any_char)),
        PREC.first("|#")),

    // comment }}}

    _datum: $ =>
      choice(
        $.boolean,
        $.string,
        $.byte_string,
        $.character,
        $.number,
        $.symbol,

        $.keyword,
        $.regex,
        $.box,
        $.graph,
        $.structure,
        $.hash,
        $.quote,

        $.list,
        $.vector),

    boolean: _ => token(choice("#true", "#t", "#T", "#false", "#f", "#F")),

    // string {{{

    string: $ => $._real_string,

    byte_string: $ =>
      seq("#", $._real_string),

    regex: $ =>
      seq(
        token(choice("#rx", "#px", "#rx#", "#px#")),
        $._real_string),

    _real_string: $ =>
      seq(
        '"',
        repeat(
          choice(
            $.escape_sequence,
            /[^"\\]/)),
        '"'),

    escape_sequence: $ =>
      choice(
        "\\a", "\\b", "\\t", "\\n", "\\v", "\\f", "\\r", "\\e",
        '\\"', "\\'", "\\\\",
        seq("\\", /[0-7]{1,3}/),
        seq("\\x", /[0-9a-fA-F]{1,2}/),
        seq("\\u", /[0-9a-fA-F]{1,4}/),
        seq("\\u", /[0-9a-fA-F]{4,4}/),
        seq("\\U", /[0-9a-fA-F]{1,8}/),
        seq("\\", LEAF.newline)),

    // string }}}

    number: _ =>
      token(
        choice(
          _number_base(2),
          _number_base(8),
          _number_base(10),
          _number_base(16))),

    character: _ =>
      token(
        seq(
          "#\\",
          choice(
            "nul", "null", "backspace",
            "tab", "newline", "linefeed",
            "vtab", "page", "return",
            "space", "rubout",
            /[0-7]{3,3}/, /u[0-9a-fA-F]{1,4}/, /U[0-9a-fA-F]{1,8}/,
            /./))),

    symbol: _ =>
      PREC.last(
        PREC.right(
          token(
            seq(
              choice(
                LEAF.symbol_or_number_start,
                "#%",
                /\|[^|]*\|/,
                /\\./),
              repeat(
                choice(
                  /\|[^|]*\|/,
                  /\\./,
                  LEAF.non_delimiter)))))),

    keyword: $ =>
      token(
        seq(
          "#:",
          repeat1(LEAF.non_delimiter))),

    box: $ =>
      seq(
        "#&",
        repeat($._skip),
        $._datum),

    list: $ =>
      paren(
        repeat(
          choice(
            $._token,
            $.dot))),

    vector: $ =>
      seq(
        choice("#", "#fl", "#fx"),
        repeat(/[0-9]/),
        $.list),

    structure: $ =>
      seq(
        "#s",
        $.list),

    hash: $ =>
      seq(
        choice("#hash", "#hasheq", "#hasheqv"),
        $.list),

    graph: $ =>
      seq(
        "#",
        repeat1(/[0-9]/),
        choice(
          "#",
          seq(
            "=",
            repeat($._skip),
            $._datum))),

    quote: $ =>
      seq(
        choice("'", "`", ",", ",@", "#'", "#`", "#,", "#,@"),
        repeat($._skip),
        $._datum),

    extension: $ =>
      choice(
        seq(
          "#reader",
          repeat($._skip),
          $._datum),
        seq(
          choice("#lang ", "#!"),
          /[a-zA-Z0-9+_/-]+/)),
  }
})

// number {{{

function _number_base(n) {
  const number = _ =>
    seq(
      choice(
        seq(radix(), optional(exactness())),
        seq(optional(exactness()), radix()),
      ),
      choice(
        // Inexact number pattern already contains exact pattern.
        // So we don't need to parse exact number explicitly
        inexact()));

  const sign = _ => /[+-]/;

  const digit = _ => {
    return {
      2: /[01]/,
      8: /[0-7]/,
      10: /[0-9]/,
      16: /[0-9a-fA-F]/,
    }[n];
  };

  const radix = _ => {
    return {
      2: /#[bB]/,
      8: /#[oO]/,
      10: optional(/#[dD]/),
      16: /#[xX]/,
    }[n];
  };

  const exactness = _ =>
    choice("#e", "#E", "#i", "#I");

  const exp_mark = _ => /[sldeftSLDEFT]/;

  const unsigned_integer = _ =>
    repeat1(digit());

  const inexact = _ =>
    choice(
      inexact_real(),
      inexact_complex());

  const inexact_real = _ =>
    choice(
      seq(
        optional(sign()),
        inexact_normal()),
      seq(
        sign(),
        inexact_special()));

  const inexact_complex = _ =>
    choice(
      seq(
        optional(inexact_real()),
        sign(),
        inexact_unsigned(),
        /[iI]/),
      seq(
        inexact_real(),
        "@",
        inexact_real()));

  const inexact_unsigned = _ =>
    choice(
      inexact_normal(),
      inexact_special());

  const inexact_normal = _ =>
    seq(
      inexact_simple(),
      optional(
        seq(
          exp_mark(),
          optional(sign()),
          unsigned_integer())));

  const inexact_special = _ =>
    choice(
      /[iI][nN][fF]\.0/,
      /[nN][aA][nN]\.0/,
      /[iI][nN][fF]\.[fFtT]/,
      /[nN][aA][nN]\.[fFtT]/,
    );

  const inexact_simple = _ =>
    choice(
      seq(
        digits(),
        optional("."),
        repeat("#")),
      seq(
        optional(unsigned_integer()),
        ".",
        digits()),
      seq(
        digits(),
        "/",
        digits()));

  const digits = _ =>
    seq(
      unsigned_integer(),
      repeat("#"));

  return token(number());
}

// number }}}
