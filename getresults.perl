use warnings;
use JSON::XS qw(encode_json decode_json);
use REST::Client;
use Date::Parse;
use Socket;
 
$ENV{TZ} = 'UTC';

# msm_ids to get results for
my @msmts = qw / 10001 10004 10005 10006 10008 10009 10010 10011 10012 10013 10014 10015 10016 /;

# start and end time
my $startt = str2time("2017-05-15 00:00:00"); 
my $stopt =  str2time("2017-06-14 23:59:59");

# chunk size in seconds
my $chunk = 3600*6;

my $client = REST::Client->new();


sub getresults {
	my($msmt, $start, $stop) = @_;
	my $base = 'https://atlas.ripe.net/api/v2/measurements/';
	my $url =  $base. $msmt. '/results/'. '?start='. $start. '&stop='. $stop;
	
print STDERR "$url: ";
	$client->GET($url);
	while ($client->responseCode() != 200) {
		 $client->GET($url);
printf STDERR "%s retrying ...", $client->responseCode();
	}

printf STDERR "%s\n", $client->responseCode();
	
	my $results = decode_json $client->responseContent();

	for my $r (@{$results}) {
		if (defined($r->{result}->{rt}))  {
			printf "%d %d %d %f\n", $r->{msm_id}, $r->{timestamp}, $r->{prb_id}, $r->{result}->{rt};
		}
		else {
			printf "%d %d %d NA\n", $r->{msm_id}, $r->{timestamp}, $r->{prb_id};
		}
	}
}
	
print "msm_id timestamp prb_id rtt\n";

for $m (@msmts) {
	my($start,$stop) = ($startt, $stopt);
	while ($start<$stop) {
		if ($stop > $start+$chunk) {
			$end = $start + $chunk-1;
		}
		else {
			$end = $stop;
		}
		getresults($m, $start, $end);
		$start = $end+1;
	}
}
