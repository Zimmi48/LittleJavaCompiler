#!/usr/bin/perl
# © Najib Idrissi Kaïtouni

use 5.012;

my $prog = "pjava";

[ -x "$prog" ] || die "$prog manquant";
[ -d "tests" ] || die "Dossier de tests manquant";

sub chk_exit {
    my ($dir, $flags, $ret) = @_;
    say "==> $dir...";
    my @err;
    for my $file (<$dir/*.java>) {
        qx{./$prog $flags $file 2>&1};
        push @err, $file if $? >> 8 != $ret;
    }
    if (@err) {
        say "/!\\ Errors in $dir !";
        say "/!\\ $_" for @err;
        exit 1
    } else {
        say 'OK'
    }
}

chk_exit('tests/syntax/bad', '-type-only', 1); # modifié pour passer avec les valeurs gauche
chk_exit('tests/typing/bad', '-type-only', 1);
chk_exit('tests/typing/good', '-type-only', 0);
chk_exit('tests/exec-fail', '', 0);
chk_exit('tests/exec', '', 0);

say '==> Testing output...';
my @err;
for my $file (<tests/exec/*.s>) {
    $file =~ s/\.s$//;
    my $out = qx{spim -file $file.s 2>&1};
    my $expected = qx{cat $file.out};
    # Pas moyen d'enlever le header...
    $out =~ s/^(.*\n){5}//;
    if ($out ne $expected) {
        push @err, $file;
    }
}
if (@err) {
        say "/!\\ $_" for @err;
        exit 1;
}
say 'OK';
