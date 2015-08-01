package Modules::MemberLog;

#----------------------------------------------------------------------------
#
#   Copyright (C) 2011 -
#
#   This file is part of ExSite WebWare (ExSite, for short).
#
#   ExSite is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   ExSite is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with ExSite; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#   Users requiring warranty coverage and/or support may arrange alternate
#   commercial licensing for ExSite, by contacting Exware Solutions
#   via the website noted above.
#
#----------------------------------------------------------------------------

use strict;

# useful kernel libs; you can/should remove any of these that are not needed

use ExSite::Config;           # required
use ExSite::Input;            # optional input manager
use ExSite::HTML;
use ExSite::Mail;
use ExSite::Misc;             # optional utils
use ExSite::Util;             # optional utils
use ExSite::ML;               # optional markup-language generation
use ExSite::FormBuilder;      # optional form generation
use ExSite::ReportBuilder;    # optional report generation
use ExSite::Form;             # optional database manager

use ExSite::Object;
use Modules::Membership::Member;
use Modules::MemberLog::MemberLog;
use Modules::Finance::Receivable;

# recommended base class for plug-in modules

use Modules::BaseDCD;
use JSON;
use DateTime::Format::Excel;
use Archive::Zip qw( :ERROR_CODES :CONSTANTS );

# declare package globals

use vars qw(@ISA $ml);    # Perl inheritance global & markup language object

# define our class inheritance

@ISA = qw(Modules::BaseDCD);    # inherit from this base class
$ml  = &get_obj("ML");

# read method (sets up input data)
sub read {
    my ($this, $options) = @_;

    # default behaviour is to fetch overlayed GET and POST data
    # and store them in the object for use by other methods

    my $in = new ExSite::Input;
    $this->{post}       = $in->post;
    $this->{query}      = $in->query;
    $this->{input}      = $in->combine;
    $this->{section_id} = $this->get_section_id;
    $this->setup_queries();
    map { $this->{tools}{$_} = 1 } split(",", &preference("MemberLog.tools") || "view,update,delete,copy");
}

# write method (builds output for regular web pages)
sub write {
    my ($this, $options) = @_;

    # build a block of HTML to return to the CMS for inlining into
    # a template or other content.
    my $out;    # our output buffer

    # based on information in $options or $this->{input}, add HTML to $out.
    my %opt = &DecodeString($options);
    my $uid = $this->{input}{uid} || $opt{uid} || $share{DB}->my_uid;
    if ($opt{_self} || $this->{input}{_self}) {
        $uid = $share{DB}->my_uid();
    }
    my $fid = $this->{input}{form_id} || $this->{input}{member_log_form_id} || $opt{form_id};

    # no input allows oyster to ignore query strings
    my $cmd = $opt{noinput} == 1 ? $opt{log} : ($this->{input}{log} || $opt{log});
    my $type = $opt{type} || $this->{input}{type};
    if ($cmd eq "search_form" || $cmd eq "do_search_form") {
        $opt{type} = $type;
        $out = $this->search_form(%opt);
    } elsif ($cmd eq "member_report") {
        $out = $this->member_report($uid, $opt{types});
    } elsif ($cmd eq "list_entry_forms") {
        $out = $this->list_entry_forms();
    } elsif ($cmd eq "trash") {
        $out = $this->trash($this->{input}{log_id}, $uid);
    } elsif ($cmd eq "feature") {
        $out .= $this->feature($opt{type}, $opt{limit}, $opt{template});
    } elsif ($cmd eq "view") {
        $out .= $this->view($this->{input}{log_id});
    } elsif ($cmd eq "update") {
        $out .= $this->update($this->{input}{log_id}, $uid, "member_log_listing");
    } elsif ($cmd eq "copy") {
        $out .= $this->copy($this->{input}{log_id}, $uid, "member_log_listing");
    } elsif ($cmd eq "deactivate") {
	$out .= $this->deactivate($this->{input}{log_id}, $uid);
    } elsif ($cmd eq "import") {
        $out = $this->import();
    } elsif ($cmd eq "new") {
        if ($fid) {
            my $f = $share{DB}->fetch("member_log_form", $fid);
            if (!$this->is_open($f)) {
                $out .= $ml->error("Sorry, this form is no longer open for submission.");
            } else {
                $out .= $this->log_form(form_id => $fid, type => $f->{type}, data => jsonToObj($f->{presets} || {}));
            }
        } elsif ($type) {
            $out .= $this->log_form(type => $type);
        }
    } elsif ($cmd eq "saved_report") {
        if ($opt{rid}) {
            $out .= $this->generate_report(saved_report => $opt{rid});
        }
    } elsif ($cmd eq "list") {
        my $match;
        $match->{type} = $type;
        if ($this->{input}{parent_id}) {
            $match->{parent_id} = $this->{input}{parent_id};
        } elsif ($this->{input}{uid} || $opt{_self} || $this->{input}{_self}) {
            $match->{member_id} = $uid;
        }
        $out .= $this->list($match, %opt);
    } else {
        $out = $this->search_form(%opt);
    }
    return $out;
}

# ioctl method (used by ExSite to query the module for its functionality)
sub ioctl {
    my $this = shift;
    $_ = shift;    # $_ is the ioctl request

    if (/isRestricted/) {
        return 0;
    } elsif (/isService/) {
        return 1;
    } elsif (/ModuleName/) {
        return &preference("MemberLog.module_name") || "Member Log";
    } elsif (/ControlPanel/) {
        return \&ctrl_panel;
    } elsif (/Dependencies/) {
        return ["Finance", "Membership", "AddrBook"];
    } elsif (/Cron/) {
        return \&cron;
    } elsif (/Publish/) {
        return \&publish;
    }
}

# type = log type
# noinput = 1|0
# - for displaying "searchbars" which appear in conjunction with search results
# fields = fields to display on search form
# template = content name of search form template
# use_set_over_list = 0|1
sub search_form {
    my ($this, %opt) = @_;
    my $type  = $opt{type};
    # generic error message if no type is passed
    if ($share{Page} && !$type) {
        return $ml->p($msg{"The page you have requested does not exist."});
    }
    my $db    = $share{DB};
    my %pdata = $db->parse_parts(%{$this->{query}});
    if (!$pdata{type}) {
        $pdata{type} = $this->{query}{type};
    }
    $this->{search_params}{$pdata{type}} = \%pdata;
    $db->set_action("search");
    my @multiselect;
    if ($this->{input}{log} eq "do_search_form" && !$opt{noinput}) {
        if (!$share{MemberLog}{do_search_form}) {
            $share{MemberLog}{do_search_form} = 1;
            delete $pdata{section_id};
            return $this->do_search_form(%pdata);
        }
    }
    my $out;
    if (!$type && $this->{admin}) {
        my @types = $this->visible_log_types;
        my @options;
        foreach my $o (@types) {
            push(@options, [$o, title($o)]);
        }
        unshift(@options, [undef, "== select =="]);
        $out .= <<END;
<script type='text/javascript'>
function refresh_page(type) {
var url = window.location.href;
if (type) {
if (url.indexOf('?') > -1){
url += '&type=' + type;
} else {
url += '?type=' + type;
}
window.location.href = url;
}
}
</script>
END
        $db->form->input(
            prompt   => "Type",
            type     => "select",
            name     => "type",
            size     => 30,
            options  => \@options,
            onchange => "refresh_page(this.value)"
        );
        $db->form->buttons(submit => 0, cancel => 0, reset => 0);
        $out .= $db->form->make;
        return $out;
    }
    my $search_fields;
    foreach my $type ($this->visible_log_types) {
        $search_fields->{$type} = $opt{fields} || &preference("MemberLog.search_fields.$type");
    }
    my $search_preset;
    my $search_value;
    if ($share{Page}) {
        foreach my $type ($this->visible_log_types) {
            foreach my $field (split(/,/, $search_fields->{$type})) {
                $search_preset->{$type}{$field} = &preference("MemberLog.search_preset.$type.$field");
                my @contact_types = split(/,/, &preference("MemberLog.contact_types"));
                if ($field =~ /contact_(.*)/) {
                    $search_value->{$type}{$field} =
                      $this->{search_params}{$type}{"contact_" . $contact_types[0] . "_${1}"};
                } else {
                    $search_value->{$type}{$field} = $this->{search_params}{$type}{$field};
                }
            }
        }
    }
    my @search_fields = split(',', $search_fields->{$type});
    unshift(@search_fields, "type") if (!$type);
    foreach my $f (@search_fields) {
        my $datatype = &get_datatype($f, $type);
        my $field_meta = $this->get_field_meta($f, $type);
        my $value;
        if ($field_meta->{table} eq "attribute") {
            my $log = new Modules::MemberLog::MemberLog(type => $type);
            my $read = $log->meta()->get_map_info($field_meta->{name}, "read");
            next if ($log->meta()->get_map_info($field_meta->{name}, "read") > $db->level);
            if (my $val = $search_preset->{$type}{$f}) {
                $db->form()->input(type => "hidden", name => $f, value => $val);
            } elsif ($log->meta()->is_allowed($field_meta->{name})) {

                # check if we can convert to set based on available datatypes
                # allows for searching of multiple values
                my $datatype = &get_datatype($f, $type);
                if (   $this->{admin}
                    && $datatype =~ /^list:(.*)/
                    && keys %{$share{DB}->{map}->get_datatype("list:${1}_multi")})
                {
                    $datatype      = "list:${1}_multi";
                    $opt{datatype} = $datatype;
                    $opt{multiple} = 1;
                    push(@multiselect, &safetext($f));
                } elsif ($datatype =~ /^o?list:(.*)/) {
                    if (keys %{$share{DB}->{map}->get_datatype("set:${1}")} && $opt{use_set_over_list}) {
                        $datatype = "set:${1}";
                    }
                }
                my $input = $log->meta()
                  ->input($f, $field_meta->{name}, $search_value->{$type}{$f}, $datatype, {multiple => $opt{multiple}});
            }
        } elsif ($field_meta->{table} eq "member_attribute") {
            my $m = new Modules::Membership::Member();
            my $read = $m->meta()->get_map_info($field_meta->{name}, "read");
            next if ($m->meta()->get_map_info($field_meta->{name}, "read") > $db->level);
            if (my $val = $search_preset->{$type}->{$f}) {
                $db->form()->input(type => "hidden", name => $f, value => $val);
            } elsif ($m->meta()->is_allowed($field_meta->{name})) {

                # check if we can convert to set based on available datatypes
                # allows for searching of multiple values
                my $datatype = &get_datatype($f, $type);
                if (   $this->{admin}
                    && $datatype =~ /^list:(.*)/
                    && keys %{$share{DB}->{map}->get_datatype("list:${1}_multi")})
                {
                    $datatype      = "list:${1}_multi";
                    $opt{datatype} = $datatype;
                    $opt{multiple} = 1;
                    push(@multiselect, &safetext($f));
                }
                my $input = $m->meta()->input($f, $field_meta->{name}, undef, $datatype, {multiple => $opt{multiple}});
            }
        } elsif ($field_meta->{table} eq "contact") {
            my $name = $f;
            $name =~ /contact_(.*)/;
            my @contact_types = split(/,/, &preference("MemberLog.contact_types"));
            $name = "contact_" . $contact_types[0] . "_$1";

            # check if we can convert to set based on available datatypes
            my $datatype = &get_datatype($f, $type);
            my %opt = (
                datatype => $datatype,
                name     => $name,
                column   => $field_meta->{name},
                table    => $field_meta->{table},
                value    => $search_value->{$type}{$f}
            );
            if (   $this->{admin}
                && $datatype =~ /^list:(.*)/
                && keys %{$share{DB}->{map}->get_datatype("list:${1}_multi")})
            {
                $datatype      = "list:${1}_multi";
                $opt{datatype} = $datatype;
                $opt{multiple} = 1;
                push(@multiselect, &safetext($name));
            }
            $db->input_column(%opt);
        } elsif ($f eq "_keyword") {
            $db->form->input(
                prompt => $msg{"Keyword"},
                type   => "string",
                name   => "_keyword",
                value  => $this->{input}{_keyword},
                size   => 30
            );
        } else {
            $db->input_column(
                name   => $f,
                column => $field_meta->{name},
                table  => $field_meta->{table},
                value  => $search_value->{$type}{$f}
            );
        }
    }
    $db->form->name("search_form");
    $db->form->action($this->link(log => "do_search_form", uid => undef));
    $db->form()->input(type => "hidden", name => "type", value => $type) if ($type);
    $db->form()->input(type => "hidden", name => "section_id", value => $this->{section_id});    
    $db->form()->input(type => "hidden", name => "log", value => "do_search_form");
    $db->form()->method("get");
    if (!$share{Page}) {
        my $datefrom = $ml->input(undef,
            {name => "ctime", type => "text", size => 10, placeholder => $msg{"YYYY-MM-DD"}, class => "date-pick"});
        my $dateto = $ml->input(undef,
            {name => "ctime", type => "text", size => 10, placeholder => $msg{"YYYY-MM-DD"}, class => "date-pick"});
        $db->form()->input(
            type   => "preformatted",
            name   => "datefrom",
            input  => $datefrom,
            prompt => $ml->div($msg{"Find submissions between"})
        );
        $db->form()->input(type => "preformatted", name => "dateto", input => $dateto, prompt => "");

        if (!$share{datePicker}) {

            # should include the datePicker code once on the page
            $share{datePicker} = 1;
            my $head;
            $head .=
              $ml->script(undef, {type => "text/javascript", src => "$config{server}{HTMLpath}/_ExSite/js/date.js"});
            $head .= $ml->script(undef,
                {type => "text/javascript", src => "$config{server}{HTMLpath}/_ExSite/js/jquery.datePicker.js"});
            $head .= $ml->link(
                undef,
                {
                    rel   => "stylesheet",
                    type  => "text/css",
                    media => "screen",
                    href  => "$config{server}{HTMLpath}/_ExSite/css/datePicker.css"
                }
            );
            $head .= $ml->script(
"Date.firstDayOfWeek = 0;\nDate.format = 'yyyy-mm-dd';\n\$(function() {\n\$('.date-pick').datePicker({startDate:'$config{form}{date_picker_start_date}'});\n});\n",
                {type => "text/javascript", charset => "utf-8"}
            );
            $out .= $head;
        }
    }
    my $tname = $opt{template} || "search_${type}_template";
    my $template = $this->get_template($tname);
    $db->form()->template($template);
    $db->form()->buttons(submit => $msg{"Search"}, reset => 1);
    $out .= $db->form->make;
    foreach my $id (@multiselect) {
        $out .= &ExSite::HTML::MultiSelect(id => $id);
    }
    $out .= $this->search_footer($type);
    $db->form()->init();
    return $out;
}

sub search_footer {
    my ($this, $type) = @_;
    my $stat = $this->run_handler("MemberLog_search_footer", $type);
    return $stat if ($stat);
    return "<!--content(${type}_search_footer)-->";
}

sub do_search_form {
    my ($this, %pdata) = @_;
    my $stat = $this->run_handler("MemberLog_do_search_form");
    return $stat if (defined $stat);

    my $out;
    my $filters;
    my $header;
    foreach my $key (keys %pdata) {
        next if (!$key || $key eq "importable" || $key eq "rpt_format" || $key eq "log");
        my $value = $pdata{$key};
        next if (!$value);
        my @value;
        if ($value =~ /; /) {
            @value = split(/; /, $value);
            next if (scalar @value == 0);
        }
        my $datatype = &get_datatype($key);
        my @label    = $this->report_header($key);
        if (
            scalar @value == 2
            && (   ($value[0] =~ /\d+\-\d+\-\d+/ && $value[1] =~ /\d+\-\d+\-\d+/)
                || ($value[0] =~ /\d+/ && $value[1] =~ /\d+/))
          )
        {
            $filters->{$key}->{value} = \@value;
            $filters->{$key}->{cond}  = "between";
            $header .= $ml->li(
                &substitute(
                    "[[param]]: [[from]] - [[to]]",
                    {param => $label[0] || $msg{"Date"}, from => $value[0], to => $value[1]}
                )
            );
        } elsif (scalar @value) {
            $filters->{$key}->{value} = \@value;
            $filters->{$key}->{cond}  = "contains";
            @value = map { $msg{$_} } @value;
            $header .= $ml->li(&substitute("[[param]]: [[value]]", {param => $msg{$label[0]}, value => join(" / ", @value)}));
        } elsif ($datatype =~ /text|string/) {
            $filters->{$key}->{value} = $value;
            $filters->{$key}->{cond}  = "contains";
            $header .= $ml->li(&substitute("[[param]]: [[value]]", {param => $msg{$label[0]}, value => ($value)}));
        } else {
            $filters->{$key}->{value} = $value;
            $filters->{$key}->{cond}  = "is";
            if ($key eq "_keyword") {
                $header .= $ml->li(&substitute("[[param]]: [[value]]", {param => $msg{"Keyword"}, value => $value}));
            } elsif ($key ne "type") {
                $header .= $ml->li(&substitute("[[param]]: [[value]]", {param => $label[0], value => $msg{$value}}));
            }
        }
    }
    $out .= $ml->h1($msg{"Search Results"}, {class => "search_results_header"});
    if (&preference("MemberLog.search_form.display_on_result")) {
        $out .= $this->search_form(type => $pdata{type});
        $out .= $ml->hr();
    } else {
        if ($header) {
            $out .= $ml->div($msg{"Search parameters:"}, {class => "search_param_header"});
            $out .= $ml->ul($header, {class => "search_param_list"});
        }
        $out .=
          $ml->p(
            $ml->a($msg{"Return to search form"}, {href => service_link(log => "search_form", type => $pdata{type})}),
            {class => "return_link"});
    }
    $out .= $this->generate_report(filters => $filters);
    return $out;
}

sub get_report_data {
    my ($this, $filter) = @_;

    my $db = $share{DB};
    my (@member, @meta, @log, @log_meta, @contact);

    my @types = $this->visible_log_types;
    if ($filter->{type}->{value}) {
        @types = ($filter->{type}->{value});
    }
    my $types = join("|", @types);

    @member = $db->get_query("all member records with status", $this->{section_id});
    my @meta = split(/,/, &preference("MemberLog.member_meta"));
    my $meta = join("|", @meta);
    if ($meta) {
        @meta = $db->get_query("all member metadata", ($this->{section_id}, $meta));
    }
    if ($share{DB}->is_manager() && $this->{admin}) {
        if ($filter->{ctime}{cond} eq "between") {
            my $start = $filter->{ctime}{value}[0] . " 00:00:00";
            my $end   = $filter->{ctime}{value}[1] . " 23:59:59";
            @log = $db->get_query("all logs between", ($types, $start, $end));
        } else {
            @log = $db->get_query("all logs", ($types));
        }
    } else {
        @log = $db->get_query("all active logs", ($types));
    }
    @log_meta = $db->get_query("all log metadata", ($types));
    my @contact_types = split(",", &preference("MemberLog.contact_types"));
    @contact_types = ("billing", "shipping") if (!scalar @contact_types);
    my $types = join("|", @contact_types);
    if ($share{DB}->{map}->is_mapped("account")) {
        @contact = $db->get_query("all member contacts", ($this->{section_id}, $types));
    }
    my %member = &keywise("member_id", \@member);
    my %meta;
    foreach my $m (@meta) {
        $meta{$m->{member_id}}{$m->{name}} = $m->{value};
    }
    my %log = &keywise("member_log_id", \@log);
    my (%log_meta);
    foreach my $m (@log_meta) {
        $log_meta{$m->{id}}{$m->{name}} = $m->{value};
    }
    my (%contact);
    foreach my $c (@contact) {
        $contact{$c->{member_id}}{"contact_" . $c->{type}} = $c;
    }

    $this->{log}      = \%log;
    $this->{member}   = \%member;
    $this->{meta}     = \%meta;
    $this->{log_meta} = \%log_meta;
    $this->{contact}  = \%contact;
    return;
}

sub sort_function {
    my $stat = $share{DB}->run_handler("MemberLog_sort", $a, $b);
    return $stat if ($stat);
    my @sort = split(/,/, &preference("MemberLog.sort"));
    foreach my $s (@sort) {
        my $data_a    = $a->{data}{$s};
        my $data_b    = $b->{data}{$s};
        my $direction = "asc";
        my $datatype  = &get_datatype($s, $a->{data}{type});
        my $c;
        if ($datatype eq "int") {
            if ($direction eq "asc") {
                $c = $data_b <=> $data_a;
            } else {
                $c = $data_a <=> $data_b;
            }
            return $c if ($c);
        } elsif ($datatype eq "string") {
            if ($direction eq "asc") {
                $c = $data_a cmp $data_b;
            } else {
                $c = $data_b cmp $data_a;
            }
            return $c if ($c);
        } else {
            if ($direction eq "asc") {
                $c = $data_a cmp $data_b;
            } else {
                $c = $data_b cmp $data_a;
            }
            return $c if ($c);
        }
    }
    return 0;
}

#------------------------------------------------------------------------
#
# Reports are a listing of logs based on advanced queries
# public output is templatable, admin output is report based
#
# type => log type
# skip => array ref of fields to skip
# filters => hash ref of hash refs { key1 => { cond=>"is|is not|contains", value=>"..." }, { key2 => { } ... }
# saved_report => report parameter id defined in preferences
#
#------------------------------------------------------------------------
sub generate_report {
    my ($this, %opt) = @_;
    my $type    = $opt{type};
    my $skip    = $opt{skip};
    my $filters = $opt{filters};
    my $keyword = $filters->{_keyword}{value};
    if (my $rid = $opt{saved_report}) {
        my $ref = jsonToObj(&preference("MemberLog.saved_report.$rid"));
        %opt = %$ref;
    }
    my $stat = $this->run_handler("MemberLog_generate_report", %opt);
    return $stat if ($stat);
    my $out;

    # get report terms
    my $format = $opt{format} || $this->{input}{rpt_format} || "HTML";
    my $display = "brief";
    if ($format ne "HTML") {
        $config{report}{date}{date}      = "sql_date";
        $config{report}{date}{timestamp} = "sql_timestamp";
        $display                         = "full";
    }
    my $match = "all";

    # get report data
    my $db         = $share{DB};
    my $pagelength = 1000;
    $this->get_report_data($filters);
    my $type = $opt{type} || $filters->{type}->{value};
    my $skip = $opt{skip};
    if (!$skip) {
        my $p = &preference("MemberLog.report.skip.$type");
        if (ref($p) eq "ARRAY") {
            $skip = $p;
        } elsif ($p) {
            $skip = [$p];
        } else {
            $skip = [];
        }
    }
    my $r = new ExSite::ReportBuilder(title => title($type), dynamic => 1);
    $r->foot($this->report_footer());
    my @column = $this->report_columns(type => $type, action => "read", display => $display, skip => $skip);
    my @contact_columns = $this->contact_columns($format);
    if (scalar @contact_columns) {
        unshift(@column, @contact_columns);
    }
    my $mcol = $this->get_member_columns(brief => 1, skip => $skip);
    my @mcol = keys %$mcol;
    map { unshift @column, "mem_$_" } @mcol;
    $r->nodata("No matches found.");

    # setup with correct type
    $this->log->setdata("type", $type);
    $r->headers($this->report_header(@column));

    if ($format eq "excel_no_data") {
        $ml->cancel;
        print "Content-type: application/vnd.ms-excel\ncontent-disposition: attachment; filename=${type}.xls\n\n";
        $r->export("excel");
        exit;
    }
    my ($rowcount, $currentrow);

    # list of results
    my @results;
  ID:
    foreach my $id (sort { $b <=> $a } keys %{$this->{log}}) {
        my $uid      = $this->{log}{$id}{member_id};
        my $member   = $this->{member}{$uid};
        my $meta     = $this->{meta}{$uid};
        my $log      = $this->{log}{$id};
        my $log_meta = $this->{log_meta}{$id};

        # compile a super-row with all pertinent data for the log
        my %data = %$log;
        foreach my $key (keys %$member)   { $data{"mem_$key"}  = $member->{$key}; }
        foreach my $key (keys %$meta)     { $data{"memm_$key"} = $meta->{$key}; }
        foreach my $key (keys %$log_meta) { $data{"meta_$key"} = $log_meta->{$key}; }

        # append contact information to row once we have filtered results
        my $contact = $this->{contact}{$uid};
        foreach my $key (keys %$contact) {
            my $record = $contact->{$key};
            foreach my $col (keys %$record) {
                $data{"${key}_${col}"} = $contact->{$key}{$col};
            }
        }
    
        $this->run_handler("MemberLog_log_data", \%data);

        # a concatenation of the entire row for keyword matching
        my $fulltext = join('; ', values %data);

        # try to exclude the row
        my $pass = 0;
        if (keys %$filters) {
            foreach my $f (keys %$filters) {
                my $cond     = $filters->{$f};
                my $value    = $cond->{value};
                my $datatype = &get_datatype($f, $data{type});
                next if (!$value);
                if ($f eq "_keyword") {
                    $pass = ($fulltext =~ /$cond->{value}/i);
                } elsif ($cond->{cond} eq "is") {
                    if ($data{$f} =~ /; /) {
                        my @set = split(/; /, $data{$f});
                        $pass = grep(/$cond->{value}/, @set);
                    } else {
                        $pass = ($data{$f} eq $cond->{value});
                    }
                } elsif ($cond->{cond} eq "is not") {
                    $pass = ($data{$f} ne $cond->{value});
                } elsif ($cond->{cond} eq "contains") {

                    # escape regex special characters with \Q \E
                    if (ref $value eq "ARRAY") {
                        foreach my $v (@$value) {
                            if ($data{$f} =~ /\Q$v\E/i) {
                                $pass = 1;
                                last;
                            } elsif ($v eq "Other" || $data{$f} !~ /^($datatype)$/) {
                                $pass = 1;
                            } else {
                                $pass = 0;
                            }
                        }
                    } else {
                        my $regexp = $share{DB}->{map}->regexp($datatype);
                        if ($data{$f} eq "Other" || $data{$f} !~ /^($regexp)$/) {
                            $pass = 1;
                        } else {
                            $pass = ($data{$f} =~ /\Q$value\E/i);
                        }
                    }
                } elsif ($cond->{cond} eq "does not contain") {
                    $pass = ($data{$f} !~ /$cond->{value}/i);
                } elsif ($cond->{cond} eq "is greater than") {
                    if ($data{$f} =~ /^\d+(\.\d*)?$/) {
                        $pass = ($data{$f} > $cond->{value});
                    } else {
                        $pass = ($data{$f} gt $cond->{value});
                    }
                } elsif ($cond->{cond} eq "is less than") {
                    if ($data{$f} =~ /^\d+(\.\d*)?$/) {
                        $pass = ($data{$f} < $cond->{value});
                    }
                } elsif ($cond->{cond} eq "between") {
                    my ($t, $t1, $t2);
                    if ($cond->{value}->[0] =~ /\d+\-\d+\-\d+/ && $cond->{value}->[1] =~ /\d+\-\d+\-\d+/) {
                        $t  = new ExSite::Time($data{$f},           "sql_datetime") if ($data{$f});
                        $t1 = new ExSite::Time($cond->{value}->[0], "sql_date");
                        $t2 = new ExSite::Time($cond->{value}->[1], "sql_date");
                    } else {
                        $t  = $data{$f};
                        $t1 = $cond->{value}->[0];
                        $t2 = $cond->{value}->[1];

                    }
                    $pass = ($t <= $t2 && $t >= $t1);
                }
                next ID if ($match eq "all" && !$pass);
            }
        } else {
            $pass = 1;
        }
        next if (!$pass);

        # add row to the report
        my @row;
        my $log = new Modules::MemberLog::MemberLog(type => $data{type}, id => $data{member_log_id});
        $log->get();
        my $data = $log->get_log_data();        
        my @metafields = $log->meta_allowed();
        foreach my $c (
            sort {
                     $log->meta()->get_map_info($a, "rank_brief") <=> $log->meta()->get_map_info($b, "rank_brief")
                  || $log->meta()->get_map_info($a, "rank") <=> $log->meta()->get_map_info($b, "rank")
            } @column
          )
        {
            my $datatype = &get_datatype($c, $data{type});
            next if ($log->meta()->get_map_info($a, "display") =~ /no|full/);
            my $child_type  = $log->child_type();
            my $parent_type = $log->parent_type();        
            if ($c =~ /^meta_(.*)/) {
                if ($log->meta->is_allowed($1)) {
                    push(@row,
                        $share{DB}->show_data_noauth("attribute", "value", $data{$c}, $data{attribute_id}, $datatype));
                }
            } elsif ($c =~ /^mem_(.*)/) {
                my $show = $share{DB}->show_data_noauth("member", $1, $data{$c}, $data{mem_member_id}, $datatype);
                push(@row, $show);
            } elsif ($c =~ /^contact_(.*)/) {
                my $show = $share{DB}->show_data_noauth("contact", $1, $data{$c}, $data{mem_member_id}, $datatype);
                push(@row, $show);
            } elsif ($c eq "type") {
                push(@row, title($data{type}));
            } elsif ($c =~ /$child_type/ || $c =~ /$parent_type/) {
                push(@row, $data->{$child_type});
            } elsif ($c eq "receivable_date") {
                my $r = &get_obj("receivable", $data{receivable_id});
                if ($r && $r->ok) {
                    push(@row, $r->showdata("date"));
                } else {
                    push(@row, undef);
                }
            } elsif ($c eq "amount" && &preference("MemberLog.acctcode")) {
                my $r = &get_obj("receivable", $data{receivable_id});
                if ($r && $r->ok) {
                    push(@row, $r->subtotal(&preference("MemberLog.acctcode")));
                } else {
                    push(@row, undef);
                }
            } else {
                my $show = $share{DB}->show_data_noauth("member_log", $c, $data{$c}, $data{member_log_id}, $datatype);
                push(@row, $show);
            }
        }

        if ($format eq "HTML") {
            my $name = title($type);
            $name =~ s/["|']//g;
            my $path = $this->clear_link(__plaintext => 1, log => "trash", log_id => $log->id);
            my $actions =
              $ml->a($msg{"view"}, {href => $this->clear_link(log => "view", log_id => $data{member_log_id}), class => "button_sm"})
              . "&nbsp;"
              . $ml->a($msg{"update"},
                {href => $this->clear_link(log => "update", log_id => $data{member_log_id}), class => "button_sm"})
              . "&nbsp;"
              . $ml->a($msg{"delete"}, {href => "javascript:delete_log('$name','$path')", class => "button_sm"});
            push @row, $actions;
        }
        my $log_item = {data => \%data, row => \@row};
        push(@results, $log_item);
        $currentrow++;
    }

    if (!scalar @results) {
        return $ml->p($msg{"No search results were found. Please refine your search terms."});
    }

    if ($share{Page} && &preference("MemberLog.report_type.$type") ne "ReportBuilder") {
        return $this->templated_report(list => \@results);
    } else {
        foreach my $log_item (sort sort_function @results) {
            $r->push(@{$log_item->{row}});
            $rowcount++;
        }
    }

    if (!$rowcount) {
        $out .= $ml->p($msg{"No search results were found. Please refine your search terms."});
    }
    $out .= $this->delete_javascript();
    if ($format eq "excel") {
        $ml->cancel;
        my $filename = $type;
        foreach my $key (keys %{$this->{input}}) {
            if ($key =~ /^meta_/ && $this->{input}{$key}) {
                $filename = $this->{input}{$key};
            }
        }
        $filename = $filename . ".xls";
        print "Content-type: application/vnd.ms-excel\ncontent-disposition: attachment; filename=$filename\n\n";
        $r->export("excel");
    } elsif ($format eq "dump") {
        return $r;
    } else {
        $out .= $r->make();
    }
    return $out;
}

sub report_header {
    my ($this, @column) = @_;
    my @header;
    if ($this->{input}{importable}) {
        foreach my $c (@column) {
            if ($c =~ /^meta_(\w+)/) {
                push(@header, $c);
            } elsif ($c =~ /^mem_(\w+)/) {
                push(@header, $c);
            } elsif ($c =~ /^contact_(\w+)_(\w+)/) {
                push(@header, $c);
            } else {
                push(@header, $c) if ($c !~ /ctime|mtime/);
            }
        }
    } else {
        foreach my $c (@column) {
            if ($c =~ /^meta_(\w+)/) {
                push(@header, $msg{$this->log->label($1)});
            } elsif ($c =~ /^mem_(\w+)/) {
                push(@header, $msg{$share{DB}->{map}->get_column_attr("member", $1, "label")});
            } elsif ($c =~ /^contact_(\w+)_(\w+)/) {
                my $type = $1;
                push(@header, $msg{$1} . " " . $msg{$share{DB}->{map}->get_column_attr("contact", $2, "label")});
            } elsif ($c eq "receivable_date" && &preference("MemberLog.acctcode")) {
                push(@header, $msg{"Invoice Date"});
            } elsif ($c eq "amount" && &preference("MemberLog.acctcode")) {
                push(@header, $msg{"Amount"});
            } else {
                my $type = $this->log->getdata("type") || "member_log";
                push(@header, $msg{$share{DB}->{map}->get_column_attr($type, $c, "label")});
            }
        }
    }
    push(@column, undef);
    push(@header, undef);
    return @header;
}

sub report_footer {
    my ($this) = @_;
    my $foot;
    my $uri   = new ExSite::URI;
    my $post  = $this->{post};
    my %query = %$post;
    my $f     = new ExSite::FormBuilder(action => $this->link(rpt_format => "excel"));
    foreach my $key (keys %$post) {
        $f->input(type => "hidden", name => $key, value => $post->{$key});
    }
    if (&preference("MemberLog.import") > 0) {
        $f->input(type => "checkbox", prompt => "Importable column headers", name => "importable");
    }
    $f->template("", "<p>[[input]]&nbsp;[[prompt]]</p>", "");
    $f->{buttons} = $ml->button("Export", {type => "submit"});
    $foot .= $f->make;
    return $foot;
}

sub get_field_meta {
    my ($this, $f, $type) = @_;
    my $meta;
    if ($f =~ /^meta_(.+)$/) {
        $meta->{table} = "attribute";
        $meta->{name}  = $1;
    } elsif ($f =~ /^member_attribute_(.*)/) {
        $meta->{table} = "member_attribute";
        $meta->{name}  = $1;
    } elsif ($f =~ /^mem_(.*)/) {
        $meta->{table} = "member";
        $meta->{name}  = $1;
    } elsif ($f =~ /^contact_(.*)/) {
        $meta->{table} = "contact";
        $meta->{name}  = $1;
    } else {
        $meta->{table} = "member_log";
        $meta->{name}  = $f;
    }
    return $meta;
}

# get datatype for any field
# type - log type must be specified for meta fields
sub get_datatype {
    my ($f, $type) = @_;
    my $stat = $share{DB}->run_handler("MemberLog_get_datatype", $f, $type);
    return $stat if ($stat);
    my $datatype = $share{Cache}->get("MemberLog_datatype", "$f.$type");
    return $datatype if ($datatype);
    if ($f =~ /^meta_(.+)$/) {
        my $log = new Modules::MemberLog::MemberLog(type => $type);
        $datatype = $log->meta->get_datatype($1);
    } elsif ($f =~ /^mem_(.*)/) {
        $datatype = $share{DB}->{map}->get_column_attr("member", $1, "datatype");
    } elsif ($f =~ /^member_attribute_(.*)/) {
        my $m = new Modules::Membership::Member();
        $datatype = $m->meta->get_datatype($1);
    } elsif ($f =~ /^contact_(.*)/) {
        $datatype = $share{DB}->{map}->get_column_attr("contact", $1, "datatype");
    } else {
        $datatype = $share{DB}->{map}->get_column_attr("member_log", $f, "datatype");
    }
    $share{Cache}->save($datatype, "MemberLog_datatype", "$f.$type");
    return $datatype;
}

# scope can be local or global depending on which section has been selected
sub scope {
    my ($this) = @_;
    my $sid = $this->{section_id};
    return "local";
}

sub setup_queries {
    my $this = shift;
    my $db   = $share{DB};

    my $mcol = $this->get_member_columns();
    my @mcol = keys %$mcol;
    @mcol = map { "m." . $_ } @mcol;
    my $mcol = join ",", @mcol;
    my $nparam;

    # all member records with status
    my ($where, $nparam);
    if ($this->scope() eq "local") {
        $where  = "where m.section_id = ? or m.section_id = 0";
        $nparam = 1;
    } else {
        $where  = "";
        $nparam = 0;
    }
    $db->set_query(
        "all member records with status",
        (
            sql =>
"select $mcol,(select status from member_status force index (member_id) where member_status.member_id=m.member_id order by member_status_id desc limit 1) status from member m $where order by m.last_name,m.first_name",
            nparam => $nparam,
            mode   => "r",
            keys   => ["member", "member_status"],
        )
    );

    # all member meta data
    if ($this->scope() eq "local") {
        $where  = "where a.member_id=m.member_id and m.type like 'member%' and m.section_id = ?";
        $nparam = 2;
    } else {
        $where  = " where a.member_id=m.member_id and m.type like 'member%'";
        $nparam = 1;
    }
    $db->set_query(
        "all member metadata",
        (
            sql =>
"select m.type,a.* from member_attribute a, member m, section s $where and a.name regexp ? order by a.member_id",
            nparam => $nparam,
            mode   => "r",
            keys   => ["member", "member_attribute"],
        )
    );

    # all log records
    $db->set_query(
        "all logs",
        (
            sql    => "select * from member_log where type regexp ?",
            nparam => 1,
            mode   => "r",
            keys   => ["member_log"],
        )
    );

    # all log records created between
    $db->set_query(
        "all logs between",
        (
            sql    => "select * from member_log where type regexp ? and ctime >= ? and ctime <= ?",
            nparam => 3,
            mode   => "r",
            keys   => ["member_log"],
        )
    );

    $db->set_query(
        "all active logs",
        (
            sql    => "select * from member_log where status = 'active' and type regexp ?",
            nparam => 1,
            mode   => "r",
            keys   => ["member_log"],
        )
    );

    $db->set_query(
        "group logs",
        (
            sql =>
"select * from member_log log left join member m on (log.member_id = m.member_id) where (m.parent_id = ? or m.member_id = ?) and log.type = ? and log.status = ?",
            nparam => 4,
            mode   => "r",
            keys   => ["member_log", "member"],
        )
    );

    # all log meta data
    # filter out mimedata which is assumed to be anything over 10000 bytes
    $db->set_query(
        "all log metadata",
        (
            sql =>
"select log.type,a.id,a.name,a.value from attribute a, member_log log where a.id=log.member_log_id and tablename regexp ? and length(value) < 10000",
            nparam => 1,
            mode   => "r",
            keys   => ["member_log", "attribute"],
        )
    );

    # all member contact data
    if ($this->scope() eq "local") {
        $where = "where a.member_id=m.member_id and c.account_id=a.account_id and m.section_id = ? and c.type regexp ?";
        $nparam = 2;
    } else {
        $where  = "where a.member_id=m.member_id and c.account_id=a.account_id and c.type regexp ?";
        $nparam = 1;
    }
    $db->set_query(
        "all member contacts",
        (
            sql =>
"select m.member_id,c.contact_id,c.type,c.address,c.city,c.provstate,c.pcode,c.phone1,c.phone2,c.web,c.country from member m, account a, contact c $where order by c.contact_id",
            nparam => $nparam,
            mode   => "r",
            keys   => ["member", "account", "contact"],
        )
    );

    # all log forms
    $db->set_query(
        "member log forms",
        (
            sql    => "select * from member_log_form",
            nparam => 0,
            mode   => "r",
            keys   => ["member_log_form"],
        )
    );
    
    $db->set_query(
        "featured logs",
        (
            sql    => "select * from member_log where status = 'active' and type = ? order by member_log_id desc limit ?",
            nparam => 2,
            mode   => "r",
            keys   => ["member_log"],
        )
    );    
}

sub visible_log_types {
    my ($this) = @_;
    my $stat = $this->run_handler("MemberLog_visible_log_types");
    if ($stat) {
        return wantarray ? @$stat : $stat;
    }
    my @types = split(/,/, &preference("MemberLog.types"));
    return wantarray ? @types : \@types;
}

# if true logs are shared between related members
sub is_group_log {
    my ($this) = @_;
    return &preference("MemberLog.group_log");
}

# get names of member columns to include in reports
# skip => array ref of fields to skip
# brief => is a brief report?
sub get_member_columns {
    my ($this, %opt) = @_;
    my $skip   = $opt{skip};
    my $brief  = $opt{brief};
    my @skip   = @$skip if ($skip);
    my @allcol = $share{DB}->get_columns("member");
    my $mcol;
    foreach my $col (@allcol) {
        next if ($col =~ /photo|password/);
        my $label   = $share{DB}->{map}->get_column_attr("member", $col, "label");
        my $read    = $share{DB}->{map}->get_column_attr("member", $col, "read");
        my $display = $share{DB}->{map}->get_column_attr("member", $col, "display");

        # member_id and parent_id are required for joins
        $read = 0 if ($col =~ /member_id|parent_id/);
        next if ($read > $share{DB}->authorize);
        next if ($brief && $display =~ /full|no/);
        next if (grep(/$col/, @skip));
        $mcol->{$col} = $label if ($label);
    }
    return $mcol;
}

sub contact_columns {
    my ($this, $format) = @_;
    my @contact_types = split(/,/, &preference("MemberLog.contact_types"));
    return () if (!scalar @contact_types || $format eq "HTML");
    my @columns;
    foreach my $type (@contact_types) {
        my @contact_columns = split(/,/, &preference("MemberLog.contact_columns"));
        if (!scalar @contact_columns) {
            @contact_columns = qw(address city provstate pcode phone1 phone2);
        }
        foreach my $col (@contact_columns) {
            push(@columns, "contact_${type}_${col}");
        }
    }
    return @columns;
}

sub report_columns {
    my ($this, %opt) = @_;
    my $type    = $opt{type}    || "member_log";
    my $display = $opt{display} || "brief";
    my $skip    = $opt{skip};
    my $map     = $share{DB}->{map};
    my $columns = $map->get_columns($type);
    my @columns;

    # regular database columns
    foreach my $col (@$columns) {
        next if ($map->get_column_attr($type, $col, "read") > $share{DB}->authorize);
        next if (grep(/$col/, @$skip));
        if ($display eq "brief") {
            next if ($map->get_column_attr($type, $col, "display") ne "brief");
        } elsif ($display eq "full") {
            next if ($map->get_column_attr($type, $col, "display") eq "no");
        }
        next if ($col =~ /^(mtime|ctime|receivable_id|documents)$/ && $this->{input}{importable});
        push(@columns, $col);
        if ($col eq "receivable_id" && &preference("MemberLog.acctcode")) {
            push(@columns, "receivable_date");
            push(@columns, "amount");
        }
    }
    my $log = new Modules::MemberLog::MemberLog(type => $type);
    my @metafields = $log->meta_allowed;
    my $ncol_meta;

    # attributes
    foreach my $col (
        sort {
                 $log->meta()->get_map_info($a, "rank_brief") <=> $log->meta()->get_map_info($b, "rank_brief")
              || $log->meta()->get_map_info($a, "rank") <=> $log->meta()->get_map_info($b, "rank")
        } @metafields
      )
    {
        my $display_value = $log->meta()->get_map_info($col, "display");
        my $read          = $log->meta()->get_map_info($col, "read");
        next if (!$this->{input}{rpt_format} && $display_value && $display_value ne "brief");
        next if ($log->meta()->get_map_info($col, "display") eq "no");
        next if (grep(/$col/, @$skip));
        next if ($read > $share{DB}->authorize);
        push(@columns, "meta_$col");
        $ncol_meta++;
    }

    # special data
    unshift(@columns, $log->parent_type()) if ($log->parent_type());
    unshift(@columns, $log->child_type())  if ($log->child_type());
    unshift(@columns, "member_log_id")     if ($this->{input}{importable});
    return wantarray ? @columns : \@columns;
}

sub login {
    my ($this, $uid) = @_;
    my $user = $share{DB}->fetch($config{auth}{user_table}, $uid);
    if ($user) {
        if ($user->{$config{auth}{user_access_column}} < $share{DB}->level()) {
            $share{DB}->clear_login;
            $share{DB}->do_login($user);
            my $s = new ExSite::Section(id => $this->{section_id});
            &redirect($s->get_url());
            return;
        }
    }
    return $this->error("Sorry, you cannot switch to that user ID.");
}

# display a report for a particular member
sub member_report {
    my ($this, $uid, $types) = @_;
    my $out = $this->run_handler("MemberLog_report_header", $uid);
    my $member = &get_obj("member", $uid);
    if ($this->{admin}) {
        my $name = $member->name;
        my $sid  = $this->get_section_id();
        if (eval "require Modules::Membership") {
            $out .= $ml->p(
                $ml->a(
                    "Profile for $name",
                    {
                        href =>
"$config{server}{server}$config{server}{CGIpath}/ctrl-panel.cgi/Membership?section_id=$sid&uid="
                          . $uid,
                        target => "_blank"
                    }
                )
            );
        }
        $out .= $ml->p($ml->a("Login as $name", {href => $this->link(log => "login")}));
    }

    my @types = split("; ", $types);
    if (!$types) {
        if (!$share{DB}->is_manager) {
            @types = $this->visible_log_types();
        } else {
            my $datatype = $share{DB}->{map}->get_column_attr("member_log", "type", "datatype");
            @types = split(/\|/, $share{DB}->{map}->regexp($datatype));
        }
    }

    my %tabtitle;
    my (%pane, @tabs, @panes, $thistab);
    foreach my $type (@types) {
        my $match = {type => $type, member_id => $uid};
        $match->{status} = "active" if ($share{DB}->my_uid != $uid);
        if (scalar @types > 1 && &preference("MemberLog.enable_tabs")) {
            $pane{$type}     = $this->list($match);
            $tabtitle{$type} = title($type, "tabtitle");
            $thistab         = $type;
        } else {
            $out .= $this->list($match);
        }
    }
    $out .= &ExSite::HTML::CSS();
    my $ntab   = 0;
    my $tabnum = 0;
    foreach my $tab (@types) {
        if ($pane{$tab}) {
            push @panes, $pane{$tab};
            push @tabs,  $tabtitle{$tab};
            if ($thistab eq $tab) { $tabnum = $ntab; }
            $ntab++;
        }
    }
    $out .= &ExSite::HTML::DynTabBox(
        tabs    => \@tabs,
        panes   => \@panes,
        thistab => $tabnum,
        width   => "100%",
    ) if ($thistab);

    return $out;
}

# type = member log type
# limit(optional) = number of items to display
# template(optional) = mini template name
sub feature {
    my ($this, $type, $limit, $template) = @_;
    $limit = $limit || 1;
    my @r = $share{DB}->get_query("featured logs", ($type, $limit));
    my $out;
    if (! scalar @r) {
        return $this->nodata($type);
    }
    foreach my $record (@r) {
        my $object = new Modules::MemberLog::MemberLog(type => $record->{type}, data => $record);
        $object->set("mini_template_name", $template) if ($template);
        $out .= $object->show_mini();
    }
    return $out;
}

# message to display when there are no logs
sub nodata {
    my ($this,$type) = @_;
    my $title_pl = title($type, undef, 1);
    my $nodata = confrule("messages.nodata","logtype",$type) || 
        $msg{&preference("MemberLog.messages.$type.nodata")} ||
        &substitute($msg{"There are currently no [[title]]."}, {title => $title_pl});
    return $nodata;    
}

# check permission to view index
sub allow {
    my ($this, $match) = @_;
    my $type = $match->{type};
    my $pid = $match->{parent_id};
    my $mid = $match->{member_id};
    if ($share{DB}->my_uid && $share{DB}->my_uid() == $match->{member_id}) {
        return 1;
    }
    if ($pid) {
        my $p = new Modules::MemberLog::MemberLog(id => $pid);
        # ownership of parent implies access
        if ($p->owns()) {
        return 1;
        } elsif (!$p->allow()) {
        return 0;
        }
    }
    my $m = new Modules::MemberLog::MemberLog(type=>$type);
    return $m->allow();
}

# display a list based on a simple match hash
# match: match hash of key value pairs for member_log record
# options
# _self = list items owned by logged in user
sub list {
    my ($this, $match, %opt) = @_;
    my $allow = $this->allow($match);
    my $out;
    my $self = $this->{input}{_self} || $opt{_self};
    my $type = $match->{type}        || "member_log";
    if (!$allow) {
        if ($share{DB}->authorize) {
        return $this->error(
            &substitute(
                $msg{"Sorry, you do not have permission to view this [[title]] listing."},
                {title => title($type)}
            )
        );
        } else {
            return login_form($type);
        }
    }
    my $object      = new Modules::MemberLog::MemberLog(type => $type);
    my $child_type  = $object->child_type();
    my $parent_type = $object->parent_type();
    $out .= $this->run_handler("MemberLog_report_list_header", $match);
    my $items = new ExSite::ObjectList(type => $type);
    my $map   = $share{DB}->{map};
    my $uid   = $match->{member_id};
    my $member;

    if ($uid) {
        $member = &get_obj("member", $uid);
    }

    if (!$self && !$this->{admin}) {
        $match->{status} = "active";
    }

    $items->load($match, "member_log_id desc");
    if ($member && $this->is_group_log) {
        if (my $pid = $member->getdata("parent_id")) {
            $match->{member_id} = $pid;
            $items->loadmore($match, "member_log_id");
        } else {
            my $secondary = $member->get_child("member");
            if ($secondary && $secondary->count > 0) {
                while (my $sec = $secondary->next()) {
                    $match->{member_id} = $sec->id();
                    $items->loadmore($match, "member_log_id");
                }
            }
        }
    }
    my @results;
    my $headers = [];
    my $template;
    if ($uid == $share{DB}->my_uid) {
        $template = $this->get_template("${type}_mini_self");
    }
    if (!$template) {
        $template = $this->get_template("${type}_mini");
    }
    while (my $item = $items->next()) {
        my $data = $item->get();
        $member = $item->member();
        my $mdata = $member->get();
        foreach my $key (keys %$mdata)          { $data->{"mem_$key"}  = $member->showdata($key); }
        foreach my $key ($member->meta_allowed) { $data->{"memm_$key"} = $member->meta_get($key); }
        foreach my $key ($item->meta_allowed)   { $data->{"meta_$key"} = $item->meta_show($key); }
        my $parent = $item->parent();
        if ($parent) {
            my $label = &substitute("[[title]]", {title => $parent->name});
            my $link = $ml->a($label, {href => $this->link(log => "view", log_id => $parent->id())});
            $data->{$parent_type} = $link;
        }
        if ($item->allow()) {
            $data->{"url_view"} = $this->log_url($item, "view");
        }
        if ($item->allow_edit()) {
            $data->{"url_update"} = $this->log_url($item, "update");
            $data->{"url_delete"} = $this->log_url($item, "delete");
            $data->{"url_copy"}   = $this->log_url($item, "copy");
        }
        my $log_item = {data => $data, object => $item};
        push(@results, $log_item);
    }
    @results = sort sort_function @results;
    my @newlist;
    foreach my $r (@results) {
        my $object = $r->{object};
        push(@newlist, $object->get());
    }
    $items->setlist(@newlist);

    if (my $pid = $match->{parent_id}) {
        my $parent = $share{DB}->fetch("member_log", $pid);
        my $p = new Modules::MemberLog::MemberLog(type => $parent->{type}, id => $pid);
        my @path;
        my $top_link = $this->link(type => $parent->{type}, parent_id => undef);
        if ($p->allow_edit()) {
            $top_link =
              $this->link(type => $parent->{type}, _self => 1, log => "list", parent_id => undef, page => undef);
        }
        push(@path, $ml->a(title($parent->{type}, undef, 0), {href => $top_link}));
        push(@path, $p->name);
        $out .= $ml->div(join(" &gt; ", @path), {class => "MemberLogPath"});
    }

    # templated list
    if ($template && !$self && !$this->{admin}) {
        $out .= $this->templated_list(type => $type, items => $items);
        return $out;
    }

    my $r = new ExSite::ReportBuilder(title => title($type, undef, 1));
    my $columns = $this->report_columns(type => $type, action => "read");
    foreach my $col (@$columns) {
        next if ($map->get_column_attr($type, $col, "display") eq "full");
        if ($col =~ /^meta_(.*)/) {
            push @$headers, $msg{$object->meta()->get_map_info($1, "label")};
        } elsif ($col =~ /$child_type/ || $col =~ /$parent_type/) {
            push(@$headers, title($col, undef));
        } else {
            push @$headers, $msg{$map->get_column_attr($type, $col, "label")};
        }
    }
    my $allow_edit;
    my $allow_view;
    while (my $item = $items->next()) {
        my @data;
        foreach my $col (@$columns) {
            if ($col =~ /^meta_(.*)/) {
                push @data, $item->meta()->showdata($1);
            } elsif ($col eq $child_type) {
                my %data = $item->get_log_data();
                push @data, $data{$child_type};
            } else {
                push @data, $item->showdata($col);
            }
        }
        $allow_edit = $item->allow_edit();
        $allow_view = $item->allow();
        if ($allow_edit) {
            my @actions;
            if (confrule("tools","logtype",$type) =~ /view/) {
                push(@actions,$ml->a($msg{"view"},
                {href => $this->log_url($item, "view"), class => "log_view button_sm", title => "view"}));
            }
            if (confrule("tools","logtype",$type) =~ /update/) {
                push(@actions,$ml->a($msg{"update"},
                {href => $this->log_url($item, "update"), class => "log_update button_sm", title => "update"}));
            }
            if (confrule("tools","logtype",$type) =~ /delete/) {
                push(@actions,$ml->a($msg{"delete"},
                    {href => $this->log_url($item, "delete"), class => "log_delete button_sm", title => "delete"}));
            }
            if (confrule("tools","logtype",$type) =~ /copy/) {
                push(@actions,$ml->a($msg{"copy"},
                    {href => $this->log_url($item, "copy"), class => "log_copy button_sm", title => "copy"}));
            }
	    if (confrule("tools","logtype",$type) =~ /deactivate/ && $item->is_active()) {
		push(@actions,$ml->a($msg{"deactivate"},
		    {href => $this->log_url($item, "deactivate"), class => "log_deactivate button_sm", title => "deactivate"}));
	    }
            push @data, join("&nbsp;",@actions);
        } elsif ($allow_view && confrule("tools","logtype",$type) =~ /view/) {
            push @data,
              $ml->a(
                $msg{"view"},
                {
                    href  => $this->link(log => "view", log_id => $item->id),
                    class => "log_view button_sm",
                    title => "view"
                }
              );
        }
        $r->push(@data);
    }
    if ($allow_edit || $allow_view) {
        push @$headers, "";
    }
    my $nodata = $msg{&preference("MemberLog.messages.$type.nodata")}
      || $msg{"There are currently no entries for this log."};
    $r->headers(@$headers);
    $r->nodata($nodata);
    $r->class($this->module_name . " Report table");
    if ($object->allow_add()) {
        my $tools = $ml->a(
            &substitute($msg{"New [[type]]"}, {type => title($type)}),
            {
                href  => $this->link(log => "new", type => $type, log_id => undef),
                class => "button_sm",
                style => "float:right"
            }
        );
        if (!$object->parent_type() || $match->{parent_id}) {
            $r->tools($tools);
        }
    }
    $out .= $this->delete_javascript();
    $out .= $ml->div($r->make(), {class => "table-responsive"});
    return $out;
}

# confrule: find and return a configuration rule
# context: context within MemberLog system (e.g type)
# value: value of context
# e.g context: membertype value: Professional
sub confrule {
    my ($preference, $context, $value) = @_;

    my $stat = $share{DB}->run_handler("MemberLog_confrule", $preference, $context, $value);
    return $stat if (defined $stat);

    if ($context eq "logtype" && $value) {
        my $value = &preference("MemberLog._type.$value.$preference");
        return $value if ($value);
    }
    return &preference("MemberLog.$preference");
}

sub log_url {
    my ($this, $log, $action) = @_;
    my $name = $this->run_handler("MemberLog_log_delete_name", $log) || title($log->type());
    $name =~ s/["|']//g;
    my $path = $this->link(__plaintext => 1, log => "trash", log_id => $log->id);
    my $deactivate_link = $this->link(log => "deactivate", log_id => $log->id);
    my %action = (
        view       => $this->link(log => "view",   log_id => $log->id),
        update     => $this->link(log => "update", log_id => $log->id),
        delete     => "javascript:delete_log('$name','$path')",
        copy       => $this->link(log => "copy",   log_id => $log->id),
	deactivate => "javascript:if(confirm('" . &substitute($msg{"Are you sure you want to deactivate this [[log_name]]?"}, { log_name => $msg{$name} }) . "')) { window.location = '$deactivate_link'; }"
    );
    return $action{$action};
}

sub templated_report {
    my ($this, %opt) = @_;
    my $stat = $this->run_handler("MemberLog_templated_report", %opt);
    return $stat if ($stat);
    my $out;
    if (my @results = @{$opt{list}}) {
        foreach my $log_item (sort sort_function @results) {
            my $data = $log_item->{data};
            my $item = new Modules::MemberLog::MemberLog(type => $data->{type});
            $out .= $item->show_mini(%$data);
        }
    }
    return $out;
}

# templated_list also has pagination
# items = list of items
sub templated_list {
    my ($this, %opt) = @_;
    my $stat = $this->run_handler("MemberLog_templated_list", %opt);
    return $stat if ($stat);
    my $type     = $opt{type};
    my $object   = new Modules::MemberLog::MemberLog(type => $type);
    my $template = $this->get_template("${type}_list");
    my $title    = title($type);
    my $title_pl = title($type, undef, 1);
    if (!$template) {
        $template = $ml->h1($msg{$title_pl});
        if (!$object->parent_type()) {
            my $label = &substitute($msg{"Add [[title]]"}, {title => $msg{$title}});
            $template .=
              "[[include:add_url]]<p><a class='button_sm' href='[[add_url]]'>$label</a></p>[[/include:add_url]]";
        } elsif ($object->parent_type() && $this->{input}{parent_id}) {
            my $p = new Modules::MemberLog::MemberLog(type => $object->parent_type(), id => $this->{input}{parent_id});
            $template = $ml->h1($msg{$title_pl} . " - " . $p->name);
        }
        $template .= $ml->div("[[list]]");
    }
    my $items = $opt{items};
    my $list;
    my $per_page = &preference("MemberLog.items_per_page") || 10;
    my $pages = int($items->count / $per_page + 0.99);
    if (my $page = $this->{input}{page}) {
        my $start = ($page - 1) * $per_page;
        $items->index($start - 1);
    }
    my $count;
    while (my $item = $items->next()) {
        $count++;
        $list .= $item->show_mini();
        last if ($count == $per_page);
    }
    if ($items->count == 0) {
        my $nodata = $msg{&preference("MemberLog.messages.$type.nodata")}
          || &substitute($msg{"There are currently no [[title]]."}, {title => $title_pl});
        $list = $ml->p($nodata);
    } elsif (!$list) {
        $list = &ShowHash($items->next()->get());
    }
    my $out;
    my $user = $this->member();
    my %data = (
        user    => &trim($user->name()),
        add_url => $object->allow_add() ? $this->link(log => "new", type => $type) : undef,
        list    => $list
    );
    $out .= &substitute($template, \%data);
    $out .= $this->paginate($items->count);
    $out .= $this->delete_javascript();
    return $out;
}

sub paginate {
    my ($this, $count) = @_;
    my $per_page     = &preference("MemberLog.items_per_page") || 10;
    my $current_page = $this->{input}{page}                    || 1;
    my $pages        = int($count / $per_page + 0.99);
    my @indexlink;
    my $uri = new ExSite::URI;
    $uri->setup();
    for (my $page = 1 ; $page <= $pages ; $page++) {
        $uri->query(page => $page);
        my $url = $uri->write();
        my $class = ($current_page == $page) ? "PagedIndexCurrent" : undef;
        push @indexlink, $ml->a($page, {href => $url, class => $class});
    }
    if ($current_page < $pages) {
        my $next = $current_page + 1;
        $uri->query(page => $next);
        my $url = $uri->write();
        push @indexlink, $ml->a($msg{"Next"} . "&nbsp;&gt;", {href => $url});
    }
    my $out;
    if (@indexlink > 1) {
        $out .=
          $ml->div(join("&nbsp;", @indexlink), {class => "PagedIndex"});
    }
    return $out;
}

sub trim {
    my $text = shift;
    $text =~ s/^\s+|\s+$//g;
    return $text;
}

sub virusscan {
    my ($this, $filename, $data) = @_;
    require File::Scan::ClamAV;
    my $av = new File::Scan::ClamAV(port => '/var/run/clamav/clamd.ctl');
    my ($code, $virus) = $av->streamscan($data);
    if ($code eq 'OK') {
        $this->info("The file: $filename did not contain any virus known to ClamAV");
    } elsif ($code eq 'FOUND') {
        return &substitute($msg{"The file: [[filename]] contained the virus: [[virus]]"}, {filename=>$filename,virus=>$virus});
    } else {
        return $av->errstr;
    }
    return undef;
}

sub add {
    my ($this, %opt) = @_;
    my %data     = %{$opt{data}};
    my %metadata = %{$opt{metadata}};
    my @err;
    my $type = $data{type};
    $this->{Log} = new Modules::MemberLog::MemberLog(type => $type);

    # setup regular data
    foreach my $key (keys %data) {
        if (!$share{DB}{map}->is_mapped($type, $key)) {
            push(@err, $this->error("$key: ". $msg{"invalid log attribute"}));
        }
        $this->log->setdata($key, $data{$key});
    }

    # update metadata
    foreach my $key (keys %metadata) {
        if (!$this->log->meta_allowed($key)) {
            push(@err, $this->error("$key: " . $msg{"invalid member metadata"}));
        } elsif ($metadata{$key}) {
            if ($this->log->meta()->get_map_info($key, "datatype") =~ /bigfile/) {
                my ($file, $fdata) = split /$config{form}{sepchar}/, $metadata{$key};
                require MIME::Base64;
                my $data = MIME::Base64::decode_base64($fdata);
                my $result = $this->virusscan($file, $data);
                push(@err, $this->error($result)) if ($result);
            }
            $this->log->meta_set($key, $metadata{$key});
        } elsif ($this->log->meta->required($key) && !$this->{admin}) {
            my $label = $this->log->label($key);
            my $message = &substitute($msg{"you must provide a value for [[label]]"}, {label=>$label});
            push(@err, $this->error($msg{$message}));
        }
    }
    if ($this->log->meta()->errorcheck) {
        my @meta_err = $this->log->meta()->fetch_diagnostics("error");
        push(@err, map { $this->error($_) } @meta_err);
    }
    if (!$this->log->uid) {
        $this->log->setdata("member_id", $this->{input}{uid} || $share{DB}->my_uid);
    }
    my $t = new ExSite::Time;
    $this->log->setdata("ctime", $t->write("sql_datetime")) if (!$data{ctime});
    my $id = $this->log->save();
    $this->log->meta()->save();
    if (!$id) {
        my @log_err = $this->log->fetch_diagnostics("error");
        push(@err, map { $this->error($_) } @log_err);
    }
    if (scalar @err) {
        my $back = $ml->input(undef, {type => "button", value => $msg{"Back"}, onclick => "history.back()"});
        $this->log->delete();
        return $ml->p(
            $msg{"There was an issue while trying to process your form. Please correct the following errors and try again."})
          . join("\n", @err)
          . $back;
    }
    return $id;
}

sub get_template {
    my ($this, $name, $section_id, $revision) = @_;
    $revision = $revision || "newest";
    my $loc = $share{Page} || $share{Section};
    if (!$loc && $section_id) {
        $loc = new ExSite::Section(id => $section_id);
    }
    if ($loc) {
        my $ctemplate = $loc->find($name);
        $loc->set_revision($revision);
        my $html = $ctemplate->get_html();
        return $html;
    }
    return undef;
}

sub login_form {
    my ($type) = @_;
    my $stat = $share{DB}->run_handler("MemberLog_login_form", $type);
    return $stat if ($stat);
    my $uri = new ExSite::URI;
    $uri->plaintext();
    my $out;
    $out .= $ml->p(
        $msg{
"You must be logged in to access this form. Please register for the site if you have not already done so."
        }
    );
    $out .= $share{DB}->login_form(undef, $uri->write_full);
    return $out;
}

# type = type of log
sub log_form {
    my ($this, %opt) = @_;

    my $stat = $this->run_handler("MemberLog_log_form", \%opt);
    return $stat if ($stat);

    my $out;
    my $type = $opt{type};
    if (scalar keys %{$this->{post}} > 0 && $this->{post}{type}) {
        return $this->do_log_form(data => $this->{post});
    } else {
        $share{DB}->set_action("insert");
        $share{DB}->form->action($this->link(log => "new"));
        if (!$share{DB}->authorize) {
            return login_form($type);
        }
        my $data = $opt{data} || {};
        my $prepop = $this->run_handler("MemberLog_prepop_log_form", \%opt) || $opt{prepop} || $this->{input} || {};
        my $log   = new Modules::MemberLog::MemberLog(type => $type, id => $data->{member_log_id});
        my $map   = $share{DB}{map};
        my $ncols = $map->get_ncol($type);
        for (my $icolumn = 0 ; $icolumn < $ncols ; $icolumn++) {
            my %column = $map->get_column($type, $icolumn);
            if (!$column{label}) { $column{label} = $column{column}; }
            my $allow = ($column{write} == 1);
            if (!$allow && $share{DB}->is_manager()) {
                $allow = ($column{write} <= $share{DB}->level());
            }
            if ($allow || $column{column} eq "type") {

                # emphasize required fields
                my $required = ($column{validate} =~ /soft|hard/);
                my $value    = $data->{$column{column}};
                $value = $type if ($column{column} eq "type");
                $share{DB}->input_column(
                    table    => $type,
                    name     => $column{column},
                    column   => $column{column},
                    size     => $column{size},
                    datatype => $column{datatype},
                    required => $required,
                    value    => $value,
                );

            }
        }

        # add form_id as hidden input
        if ($opt{form_id}) {
            $share{DB}->form->input(type => "hidden", name => "member_log_form_id", value => $opt{form_id});
        }

        # add log_id as hidden input
        if ($log->id) {
            $share{DB}->form->input(type => "hidden", name => "member_log_id", value => $log->id);
        }
        my $pid = $this->{input}{parent_id};
        if ($pid) {
            $share{DB}->form->input(type => "hidden", name => "parent_id", value => $pid);
        }

        my @metafields = $log->meta()->get_allowed();
        if (@metafields == 0) {
            my %meta = $log->meta()->get_all();
            @metafields = keys %meta;
        }
        foreach my $key (sort { $log->meta()->get_map_info($a, "rank") <=> $log->meta()->get_map_info($b, "rank") }
            @metafields)
        {
            my $write_access = $log->meta()->get_map_info($key, "write");
            if (!defined $write_access
                || $share{DB}->level >= $write_access)
            {
                if ($log->id || !$data->{"meta_$key"}) {
                    $log->meta()->input("meta_$key", $key, $prepop->{"meta_$key"});
                } elsif ($data->{"meta_$key"}) {
                    $share{DB}->form->input(type => "hidden", name => "meta_$key", value => $data->{"meta_$key"});
                }
            } elsif ($data->{"meta_$key"}) {
                $share{DB}->form->input(type => "hidden", name => "meta_$key", value => $data->{"meta_$key"});
            }
        }
        $opt{log} = $log;
        my $template = $this->get_form_template(%opt);
        $share{DB}->form->template($template);
        if ($template !~ /^\s+<h[1|2]/) {
            if ($log->id()) {
                $out .= $ml->h1(&substitute($msg{"Edit [[type]]"}, {type => title($type)}));
            } else {
                $out .= $ml->h1(&substitute($msg{"New [[type]]"}, {type => title($type)}));
            }
        }
        $out .= $share{DB}->form()->make();
        $share{DB}->form->init();
    }
    return $out;
}

sub get_form_template {
    my ($this, %opt) = @_;
    my $log  = $opt{log};
    my $type = $opt{type};

    # custom template?
    my $template;
    my $loc = $share{Page} || $share{Section};
    if ($loc) {
        my $template_name;
        $template_name =
          $log->id
          ? "update_${type}_template"
          : "create_${type}_template";
        my $stat = $this->run_handler("MemberLog_get_form_template", %opt);
        $template_name = $stat if ($stat);
        my $ctemplate = $loc->find($template_name);
        if (!$ctemplate->get_html()) {
            $ctemplate = $loc->find("create_${type}_template");
        }
        $template = $ctemplate->get_html();
    }
    return $template;
}

sub do_log_form {
	my ($this,%opt) = @_;
	my $out;
	my %data = %{$opt{data}};
	my $stat = $this->run_handler("MemberLog_do_form", %opt);
	return $stat if ($stat);

	my $type = $data{type};
	my $title = title($type);
	if (scalar keys %data > 2) {
	# get the formdata, parsed into columns
	my %pdata = $share{DB}->parse_parts(%data);
	$share{DB}->set_action("update");
	# extract the metadata from the form
	my %metadata;
	foreach my $key (keys %pdata) {
		if ($key =~ /^meta_(.+)$/) {
			if ($pdata{$key} eq "0000-00-00") {
				$pdata{$key} = undef;
			}
			$metadata{$1} = $pdata{$key};
			delete $pdata{$key};
		}
	}
	# are we processing an existing record?
	my $log_id = $pdata{member_log_id};
	my $pid;
	if ($log_id) {
		$this->log->setup(type=>$type,id=>$log_id);
		$type = $this->log->type if (!$type);
		if (!$this->log->allow_edit) {
		return $this->error("permission denied");
		}
	}
	if ($log_id) {
		# update regular data
		my $olddata = $this->log->get();
	    my $access = $share{DB}->level;
	    foreach my $key (keys %pdata) {
		if (! $share{DB}{map}->is_mapped("member_log",$key)) {
		    $this->warn("$key: invalid member_log attribute");
		}
		elsif ($share{DB}{map}->get_column_attr($type,$key,"write") > $access) {
		    $this->warn("$key: permission denied");
		}
		else {
			$this->log->setdata($key,$pdata{$key});
		}
	    }
   	    # update metadata
	    $olddata = $this->log->meta_get();
	    my @meta_err;
	    foreach my $key (keys %metadata) {
		if (! $this->log->meta_allowed($key)) {
		    push(@meta_err,$this->error("$key: invalid log metadata"));
		}
		elsif ($metadata{$key} ne $olddata->{$key}) {
			$this->log->meta_set($key,$metadata{$key});
		}
		}
		if ($this->log->meta()->errorcheck) {
		push(@meta_err,$this->log->meta()->show_diagnostics("error","html"));
		}
		if (scalar @meta_err) {
			return join("\n",@meta_err);
		}
		if ($this->log->save()) {
			$out .= $ml->info(&substitute($msg{"[[title]] updated."},{title=>$title}));
			if ($opt{view} eq "member_log_listing") {
				my $uid = $this->log->getdata("member_id") || $opt{uid};
				$out .= $this->member_report($uid);
			}
			else {
				$out .= $this->view($this->log->id);
			}
			$this->run_handler("MemberLog_post_update");
			return $out;
		} else {
			return $this->log->show_diagnostics("error","html") . $ml->br() . $ml->input(undef,{type=>"button",value=>$msg{"Back"},onclick=>"history.back()"});
		}
	}
	else {
	    # setup a new record
	    # set universal defaults
	    $pdata{status} = $pdata{status} || "active";
		my $stat = $this->add(data=>\%pdata,metadata=>\%metadata);
		if ($stat > 0) {
			my $title = title($type);
			my $page = &ExSite::Module::service_page("MemberLog") || &ExSite::Module::service_page("Membership");
			my $member_log_location = &preference("MemberLog.log_location") || "your profile";
			my $link = $msg{$member_log_location};
			if ($page) {
			$link = $ml->a($link, {href=>$page->link()});
			}
			if ($this->get_template("${type}_thank_you")) {
				$out .= &substitute($this->get_template("${type}_thank_you"),$this->log->get());
			} else {
				if ($share{Page}) {
					my $thanks = &preference("MemberLog.$type.thank_you") || "Thank you for your submission";
					my $data = $this->log->get_log_data();
					$out .= &substitute($ml->info($msg{$thanks}),$data);
				}
				$out .= $this->view($this->log->id);
				if ($share{DB}->my_uid() == $this->log->uid()) {
					my $label = &substitute($msg{"Return to my [[name]]"}, { name => title($this->log->type(),undef,1) });
					$out .= $ml->p($ml->a($label,{ href=>$this->link(log=>"list",parent_id=>undef,_self=>1,type=>$this->log->getdata("type")) }));
				}
			}
			my $owner = &preference("MemberLog.notify_email");
			my $site = $share{DB}->this_site;
			my $site_title = ref $site eq "HASH" ? $site->{title} : $config{site}{name};
			my $sid = $this->get_section_id();
			my $subject = &preference("MemberLog.notify_subject") || "$site_title - $title";
			my $member = $this->log->member();
			my $link = $ml->a("this link",{
				href=>"$config{server}{server}$config{server}{CGIpath}/ctrl-panel.cgi/MemberLog?section_id=$sid&log=view&log_id=".$this->log->id});
			my $message = &substitute("A new [[label]] was submitted by [[member_name]]".
			$ml->br.$ml->br.
			"Click [[link]] to view the submission.", {
				label=>$title, 
				member_name=>$member->name,
				link=>$link});
			# email confirmation?
			my $loc = $share{Page};
			if ($loc) {
			my $template_name = "${type}_confirmation";
			my $ctemplate = $loc->find($template_name);
			my $template = $ctemplate->get_html();
			if ($template) {
				my %data = (%{$this->log->get},%{$member->get});
				my $message = &substitute($template,\%data);
				my $to = $this->run_handler("MemberLog_confirmation_recipient");
				if (!$to) {
					$to = $member->email;
				}
				&ExSite::Mail::send(
						to=>$to,
						from=>$owner,
						subject=>title($type) . " Receipt Confirmation",
						body=>$message,
						);
				}
			}
			if ($loc && $owner) {
				&ExSite::Mail::send(
						to=>$owner,
						from=>$owner,
						subject=>$subject,
						body=>$message,
						);
			}
		} else {
			return $stat;
		}
		if (my $fee = $this->log_fee(%opt)) {
			$this->log->disable();
			my %pay = (
				cart        => "add",
				item        => $fee->{item_name} || $msg{$title},
				description => $fee->{description} || $this->log->name(),
				cost        => $fee->{cost},
				acctcode_id => $fee->{acctcode} || &preference("MemberLog.acctcode"),
				member_id   => $fee->{member_id} || $share{DB}->my_uid(),
				objid       => $fee->{objid} || $this->log->id(),
				objtype     => $fee->{objtype} || $type,
			);
			my $payargs = &EncodeHash(%pay);
			$session{continue_shopping_url} = -1;		
			return "<!--&Pay($payargs)-->";
		}
		$out .= $this->run_handler("MemberLog_post_create");		
	}
	}
	return $out;
}

sub log_fee {
    my ($this, %opt) = @_;
    my $fee = $this->run_handler("MemberLog_fee", %opt);
    return $fee;
}

sub get_invoice {
    my ($this) = @_;
    if ($session{invoice} && $share{Page}) {

        # already have a regular invoice going
        $this->{invoice} = new Modules::Finance::Receivable(id => $session{invoice});
    } else {

        # start a new invoice in admin panel
        my $uid    = $this->{input}{uid};
        my $member = new Modules::Membership::Member(id => $uid);
        my $acct   = $member->account();
        if ($acct) {
            my $inv = new Modules::Finance::Receivable();
            $inv->setdata("account_id", $acct->id);
            $inv->setdata("type",       "receivable");
            $inv->setdata("status",     "inactive");
            if (!$inv->save()) {
                return $this->error("Failed to setup invoice.");
            }
            $this->{invoice} = $inv;
        } else {
            $this->error("Failed to setup account.");
        }
    }
    return $this->{invoice};
}

sub log_menu {
    my ($this, $log) = @_;
    my @menu;
    my $log_id = $log->id;
    my $name   = $log->getdata("type");
    $name =~ s/["|']//g;
    my $path = $this->link(__plaintext => 1, log => "trash", log_id => $log_id);
    push(
        @menu,
        (
            $ml->a("update", {href => $this->link(log => "update", log_id => $log_id)}),
            $ml->a("delete", {href => "javascript:delete_log('$name','$path')"}),
        )
    );
    my $tout = $ml->tr($ml->th($name));
    foreach my $item (@menu) { $tout .= $ml->tr($ml->td($item)); }
    my $out;
    $out .= $this->delete_javascript();
    $out .= $ml->table($tout, {class => "Report LogMenu"});
    return $out;
}

sub update {
    my ($this, $id, $uid, $view, $copy, $meta) = @_;
    my $out;
    my $rec = $share{DB}->fetch("member_log", $id);
    $uid = $uid || $rec->{member_id} || $this->{input}{uid};
    my $reply = $this->link(log => "view", uid => $uid, log_id => $this->{input}{log_id}, __plaintext => 1);
    my $log = $this->setup_log($id);

    if (scalar keys %{$this->{post}}) {
        return $this->do_log_form(data => $this->{post}, view => $view);
    }
    $share{DB}->set_action("update");
    my $right_col = $this->list_documents($id);
    if (&preference("MemberLog.document_lib")) {
        my $form =
          $ml->p($msg{"Upload file:"}
              . $ml->br
              . $ml->input(undef, {name => "log_id", type => "hidden", value => $id})
              . $ml->input(undef, {name => "uploads[]", type => "file", multiple => undef, size => 30}));
        $form .= $ml->p($ml->input(undef, {type => "submit", value => "Submit"}));
        $right_col .= $ml->form(
            $form,
            {
                action  => $this->link(log => "upload_doc"),
                method  => "post",
                enctype => "multipart/form-data"
            }
        );
    }

    my $tr = "tr";
    my $prepop;
    if ($copy) {
        $rec->{member_log_id} = undef;
        foreach my $name (keys %$meta) {
            $prepop->{"meta_$name"} = $meta->{$name};
        }
    }
    my $form = $this->log_form(type => $rec->{type}, data => $rec, prepop => $prepop);
    $out .= $ml->table($ml->$tr($ml->td($form) . $ml->td($right_col)));
    return $out;
}

sub copy {
    my ($this, $id) = @_;
    $this->setup_log($id) if ($id);
    my $out;
    if (scalar keys %{$this->{post}}) {
        return $this->update($id, undef, "member_log_listing");
    }
    my %meta = $this->log->meta()->get();
    $this->log()->clone();
    $out .= $ml->h1(&substitute($msg{"Copy [[type]]"}, {type => title($this->log->getdata("type"))}));
    $out .= $ml->p($msg{"Make a copy of this entry by making the necessary changes and submitting below."});
    $out .= $this->update($this->log()->id(), undef, "member_log_listing", 1, \%meta);
    return $out;
}

sub deactivate {
	my $this = shift;
	my $log_id = shift;
	my $uid = shift;
	my $out;

	my $stat = $this->run_handler("MemberLog_deactivate_log", $log_id, $uid);
	return $stat if ($stat);

	$this->setup_log($log_id);
	
	if (!$this->log->allow_edit()) {
		return $this->error("permission denied");
	}

	$this->log->disable();

	$uid = $uid || $this->{input}{uid};
	$out .= $ml->info(&substitute($msg{"[[title]] deactivated."},{title=>title($this->log->type())}));
	$out .= $this->member_report($uid) if ($uid);

	return $out;
}

sub getlib {
    my ($this) = @_;
    my @album = $share{DB}->fetch_match(
        "page",
        {
            section_id => $this->get_section_id,
            type       => "library",
            filename   => "member_log",
            status     => "active"
        },
        "title"
    );
    my $pid;
    if (scalar @album) {
        $pid = $album[0]->{page_id};
    } else {
        my $p = new ExSite::Page();
        $pid = $p->make(
            {
                type           => "library",
                title          => "Member Log Documents",
                filename       => "member_log",
                description    => "documents for service log",
                label          => "",
                status         => "active",
                access         => "members",
                publish_method => "static",
                section_id     => $this->get_section_id,
            }
        );
    }
    return $pid;
}

sub upload_doc {
    my ($this) = @_;
    my $out;
    my $pid      = $this->getlib;
    my $in       = new ExSite::Input;
    my $data     = $in->post;
    my $log_id   = $data->{log_id};
    my @uploads  = $this->get_upload("uploads[]", $data);
    my $numfiles = scalar @uploads;
    $this->setup_log(id => $log_id);
    my @documents = split(", ", $this->log()->getdata("documents"));
    my $existfiles = scalar @documents;

    if (($numfiles + $existfiles) <= 6) {
        foreach my $upload (@uploads) {
            my ($filename, $data) = split /$config{form}{sepchar}/, $upload;
            $filename = &clean_filename($filename);
            my $mimetype = &MimeType($filename);
            my $content  = new ExSite::Content();
            my $t        = new ExSite::Time;
            my $name     = $t->get_ss . "_$filename";
            my $cid      = $content->make(
                {
                    name        => $name,
                    description => undef,
                    type        => "design",
                    page_id     => $pid,
                },
                {
                    mime_type => $mimetype,
                    fdata     => $upload,
                }
            );
            push(@documents, $cid);
        }
        my $documents = join(", ", @documents);
        $share{DB}->update("member_log", {member_log_id => $log_id, documents => $documents});
        &run_publisher("page", $pid);
        $out .= $ml->info(&substitute($msg{"You successfully uploaded [[count]] files."},{count=>$numfiles}));
    } else {
        $out .= $ml->info($msg{"You can only upload 6 files per record."});
    }
    $out .= $this->update($log_id);
}

sub get_upload {
    my ($this, $key, $data) = @_;

    my @fnames = split(/; /, $data->{$key});
    require CGI;
    my $cgi = CGI::new();
    my @fh  = $cgi->upload($key);

    my @uploads;
    my $i;
    foreach my $fh (@fh) {
        my $fname = &clean_filename($fnames[$i]);
        my $fdata = undef;
        my $size  = 0;

        # read file contents
        my $continue = 1;
        while ($continue) {
            $continue = read($fh, $fdata, 1024, $size);
            $size += $continue;
        }
        my $img = new ExSite::Image($fname, $fdata);
        push(@uploads, $img->encode);
        $i++;
    }
    return @uploads;
}

sub documents {
    my ($this, $id) = @_;
    my $log = $share{DB}->fetch("member_log", $id);
    if (my $docs = $log->{documents}) {
        my @documents = split(", ", $docs);
        return wantarray ? @documents : $docs;
    }
    return undef;
}

# list documents by date
sub list_documents {
    my ($this, $id) = @_;
    my $db = $share{DB};
    my $ml = new ExSite::ML;
    my $in = $this->{input};

    my $log = new Modules::MemberLog::MemberLog(id => $id);
    return $log->show_documents();
}

sub ddel {
    my $this = shift;
    my $db   = $share{DB};
    my $ml   = new ExSite::ML;
    my $in   = $this->{input};
    my $cid;
    my $lib = $this->getlib;
    if (!$lib) {
        return $ml->info($msg{"No document library found."});
    }

    if ($in->{confirm}) {
        if ($in->{docid}) {

            # validate
            my $doc = $db->fetch("content", $in->{docid});
            if ($doc->{page_id} eq $lib) {

                # okay to proceed
                # use recursive delete to remove all versions
                $db->delete_r("content", $in->{docid}, 1);

                # remove document from list in log record
                my @documents = $this->documents($in->{log_id});
                my %hash = map { $_ => 1 } @documents;
                delete $hash{$in->{docid}};
                my $docs = join(", ", keys %hash);
                $db->update("member_log", {member_log_id => $in->{log_id}, documents => $docs});

                return $ml->info("\"$doc->{name}\" deleted") . $this->update($in->{log_id});

            } else {
                return $ml->info($msg{"Invalid document ID."});
            }
        } else {
            return $ml->info($msg{"No document was specified."});
        }
    } else {
        my $url = $this->link(confirm => 1);
        my $doc = $db->fetch("content", $in->{docid});
        return $ml->h4($msg{"Confirm deletion of"} . " \"$doc->{name}\"")
          . $ml->ul($ml->li($ml->a($msg{"Yes, delete this document."}, {href => $url}))
              . $ml->li($ml->a($msg{"No, I changed my mind."}, {href => "javascript:history.back()"})));
    }
}

#----------------------------------------------------------------------------
# Everything after this point consists of private methods.

# ctrl_panel() generates the contents of the administrator control panel

sub ctrl_panel {
    my $this = shift;
    $this->{admin} = $share{MemberLog}{admin} = 1;
    if (!$this->{section_id}) { return $this->set_section_id; }

    $this->{section} = new ExSite::Section(id => $this->{section_id});
    $this->setup_log($this->{input}{log_id});
    my $cmd = $this->{input}{log};

    # declare a markup generator
    $ml = new ExSite::ML;

    # build a block of HTML to display the control panel
    $this->{admin} = 1;

    my (%out, @tabs, @panes, $thistab);
    if ($cmd eq "login" && $this->{input}{uid}) {
        return $this->login($this->{input}{uid});
    }

    my $out;    # our output buffer
    my $uid = $this->{input}{uid};

    $out .= $this->top_menu();
    $out .= $this->toolbar;

    if ($cmd =~ /add_entry_form|list_entry_forms|edit_form/) {
        my $pane = $this->$cmd;
        $out .= &ExSite::HTML::DynTabBox(
            tabs    => ["Forms"],
            panes   => [$pane],
            thistab => 0,
            width   => "100%",
        );
        return $out;
    }

    if ($cmd =~ /import|do_import1/) {
        my $pane = $this->$cmd;
        $out .= &ExSite::HTML::DynTabBox(
            tabs    => ["Import"],
            panes   => [$pane],
            thistab => 0,
            width   => "100%",
        );
        return $out;
    }

    if (!$cmd || $cmd =~ /search_form|do_search_form/) {
        $thistab = "search";
        my $datatype = $share{DB}->{map}->get_column_attr("member_log", "type", "datatype");
        my @types = split(/\|/, $share{DB}->{map}->regexp($datatype));
        my $type = $this->{input}{type};
        if (scalar @types == 1) {
            $type = $types[0];
        }
        $out{search} .= $this->search_form(type => $type);
    } elsif ($cmd eq "dump") {
        my $datatype = $share{DB}->{map}->get_column_attr("member_log", "type", "datatype");
        my @types = split(/\|/, $share{DB}->{map}->regexp($datatype));
        my $type = $this->{input}{type};
        if (scalar @types == 1) {
            $type = $types[0];
        }
        $out{search} = $this->dump(type => $type);
    } elsif ($cmd eq "member_report" && $uid) {
        $out{report} .= $this->member_report($uid);
    } elsif ($cmd eq "browse_members") {
        $out{report} .= $this->browse_members();
    }

    if ($cmd eq "new") {
        $thistab = "view";
        my $data;
        my $type = $this->{input}{type};
        if (my $fid = $this->{input}{form_id}) {
            my $f = $share{DB}->fetch("member_log_form", $fid);
            $data                       = jsonToObj($f->{presets});
            $data->{member_log_form_id} = $fid;
            $type                       = $f->{type};
        }
        $out{view} .= $this->log_form(type => $type, data => $data);
    } elsif ($cmd eq "view") {
        $thistab = "view";
        $out{view} .= $this->view($this->{input}{log_id});
    } elsif ($cmd eq "update") {
        $thistab = "view";
        $out{view} .= $this->update($this->{input}{log_id});
    } elsif ($cmd eq "copy") {
        $thistab = "view";
        $out{view} .= $this->copy($this->{input}{log_id});
    } elsif ($cmd eq "trash") {
        $thistab = "view";
        $out{view} .= $this->trash($this->{input}{log_id});
    } elsif ($cmd eq "upload_doc") {
        $thistab = "view";
        $out{view} .= $this->upload_doc();
    } elsif ($cmd eq "ddel") {
        $thistab = "view";
        $out{view} .= $this->ddel($this->{input}{docid});
    }

    if (!$thistab) {
        $thistab = "search";
    }
    my %tabtitle = (search => "Search", report => "Report", view => "View");

    my $ntab   = 0;
    my $tabnum = 0;
    foreach my $tab (qw(search report view)) {
        if ($out{$tab}) {
            push @panes, $out{$tab};
            push @tabs,  $tabtitle{$tab};
            if ($thistab eq $tab) { $tabnum = $ntab; }
            $ntab++;
        }
    }
    $out .= &ExSite::HTML::DynTabBox(
        tabs    => \@tabs,
        panes   => \@panes,
        thistab => $tabnum,
        width   => "100%",
    ) if ($thistab);
    return $out;
}

sub toolbar {
    my ($this) = @_;
    my $path   = "$config{server}{HTMLpath}/_ExSite/images/icons/";
    my $uri    = new ExSite::URI;
    $uri->setup("$config{server}{CGIpath}/$config{prog}{ctrlpanel}/MemberLog");
    $uri->query(section_id => $this->get_section_id());
    my $links = [];
    $uri->query(log => "search_form");
    push(
        @$links,
        {
            label => "Search",
            img   => "$path/search.png",
            url   => $uri->write_full()
        }
    );
    $uri->query(log => "browse_members");
    push(
        @$links,
        {
            label => $msg{"Members"},
            img   => "$path/report.png",
            url   => $uri->write_full()
        }
    );

    if (&preference("MemberLog.forms") >= 0) {
        $uri->query(log => "list_entry_forms");
        push(
            @$links,
            {
                label => "Forms",
                img   => "$path/form.png",
                url   => $uri->write_full()
            }
        );
    }
    if (&preference("MemberLog.import") > 0) {
        $uri->query(log => "import");
        push(
            @$links,
            {
                label => "Import",
                img   => "$path/info.png",
                url   => $uri->write_full()
            }
        );
    }
    if (&preference("MemberLog.dump.group_by")) {
        $uri->query(log => "dump");
        push(
            @$links,
            {
                label => "Dump",
                img   => "$path/down.png",
                url   => $uri->write_full()
            }
        );
    }
    foreach my $label (keys %{$config{MemberLog}{toolbar}}) {
        push(
            @$links,
            {
                label => $label,
                img   => $config{MemberLog}{toolbar}{$label}{icon},
                url   => $config{MemberLog}{toolbar}{$label}{url}
            }
        );
    }
    my $out = &ExSite::HTML::IconBar(links => $links);
}

sub dump {
    my ($this, %opt) = @_;
    my $db = $share{DB};
    my $out;
    if (!keys %{$this->{post}}) {
        $db->form->name("search_form");
        $db->form->action($this->link(log => "dump"));
        if (!$share{Page}) {
            my $datefrom = $ml->input(undef,
                {name => "ctime", type => "text", size => 10, placeholder => "YYYY-MM-DD", class => "date-pick"});
            my $dateto = $ml->input(undef,
                {name => "ctime", type => "text", size => 10, placeholder => "YYYY-MM-DD", class => "date-pick"});
            $db->form()->input(
                type   => "preformatted",
                name   => "datefrom",
                input  => $datefrom,
                prompt => $ml->div("Export submissions between")
            );
            $db->form()->input(type => "preformatted", name => "dateto", input => $dateto, prompt => "");

            if (!$share{datePicker}) {

                # should include the datePicker code once on the page
                $share{datePicker} = 1;
                my $head;
                $head .= $ml->script(undef,
                    {type => "text/javascript", src => "$config{server}{HTMLpath}/_ExSite/js/date.js"});
                $head .= $ml->script(undef,
                    {type => "text/javascript", src => "$config{server}{HTMLpath}/_ExSite/js/jquery.datePicker.js"});
                $head .= $ml->link(
                    undef,
                    {
                        rel   => "stylesheet",
                        type  => "text/css",
                        media => "screen",
                        href  => "$config{server}{HTMLpath}/_ExSite/css/datePicker.css"
                    }
                );
                $head .= $ml->script(
"Date.firstDayOfWeek = 0;\nDate.format = 'yyyy-mm-dd';\n\$(function() {\n\$('.date-pick').datePicker({startDate:'$config{form}{date_picker_start_date}'});\n});\n",
                    {type => "text/javascript", charset => "utf-8"}
                );
                $out .= $head;
            }
        }
        $out .= $db->form()->make();
        return $out;
    }

    my %index;
    my $group_by = $opt{group_by} || &preference("MemberLog.dump.group_by");
    my $datatype = &get_datatype($group_by, $opt{type});
    my @ctimes = split(/; /, $this->{post}{ctime});
    foreach my $group (split(/\|/, $share{DB}->{map}->regexp($datatype))) {
        my $filters;
        if (scalar @ctimes == 2 && $ctimes[0] =~ /\d+\-\d+\-\d+/ && $ctimes[1] =~ /\d+\-\d+\-\d+/) {
            $filters->{ctime}->{value} = \@ctimes;
            $filters->{ctime}->{cond}  = "between";
        }
        $filters->{$group_by}->{value} = $group;
        $filters->{$group_by}->{cond}  = "is";
        $index{$group} = $this->generate_report(type => $opt{type}, filters => $filters, format => "dump");
    }
    my $tmp    = $config{server}{temppath};
    my $folder = "member-log-" . time;
    my $dir    = "$tmp/$folder";
    mkdir $dir if (!-e $dir);
    foreach my $key (keys %index) {
        if (chdir $dir) {
            open FILE, ">$key.csv";
            if ($index{$key} =~ /ReportBuilder/) {
                print FILE $index{$key}->export("csv");
            }
            close FILE;
        }
    }

    # create a zip file
    my $zip = Archive::Zip->new();

    # add all files from disk
    $zip->addTreeMatching($dir, $folder, '\.csv$');

    # save the zip file
    my $filename = title($opt{type}) . ".zip";
    $filename = &clean_filename($filename);
    my $file = $config{server}{temppath} . "/" . $filename;
    unless ($zip->writeToFileNamed($file) == AZ_OK) {
        die 'write error';
    }
    my $raw = &get_file($file);
    $ml->cancel();
    print "Content-type: application/octet-stream\ncontent-disposition: attachment; filename=$filename\n\n";
    print $raw;
}

# is_open - are we between the start and end dates?
sub is_open {
    my ($this, $form, $date) = @_;
    if ($form) {
        my $open  = new ExSite::Time($form->{startdate}, "sql_date");
        my $close = new ExSite::Time($form->{enddate},   "sql_date");
        if ($open && $close) {
            $date or $date = new ExSite::Time();
            if ($date->compare($open) <= 0) {

                # open date is in the past
                if ($date->compare($close) >= 0) {
                    return 1;
                }
            }
            return 0;
        }
    }
    return undef;
}

#----------------------------------------------------------------------------
# log operations

# log() - return a log object for the current operation
sub log {
    my $this = shift;
    if (!exists $this->{Log}) {
        $this->{Log} = new Modules::MemberLog::MemberLog();
    }
    return $this->{Log};
}

# member() - return a member object for the current operation
sub member {
    my ($this, $uid) = @_;
    $uid = $uid || $share{DB}->my_uid();
    if (!exists $this->{Member}{$uid}) {
        $this->{Member}{$uid} = new Modules::Membership::Member(id => $uid);
    }
    return $this->{Member}{$uid};
}

sub setup_log {
    my ($this, $id) = @_;
    my $log = $this->log();
    $log->load($id);
    $log->setup(type => $log->getdata("type"), id => $id);
    return $log->loaded();
}

sub view {
    my ($this, $id) = @_;
    $this->setup_log($id) if ($id);
    my $out;
    if ($this->{admin}) {
        $out .= $ml->table([[$this->log()->show, $this->log_menu($this->log())]], {class => "LogView"});
    } else {
        $out .= $this->log()->show();
    }
    return $out;
}

sub delete_javascript {
    my ($this) = @_;
    my $out = $ml->script(
        "function delete_log(name,path) {
	if (confirm('Are you sure you want to delete this ' + name + '?')) {
		location = path;
	} else {
		return;
	}
}", {type => 'text/javascript'}
    );
    return $out;
}

# move log to trash
sub trash {
    my ($this, $log_id, $uid) = @_;
    $this->setup_log($log_id) if ($log_id);
    if (!$this->log()->exists()) {
        return $this->error($msg{"Could not find log to delete."});
    }
    my $stat = $this->log()->delete;
    my $out;
    if ($stat) {
        $out .= $ml->info($msg{"Deletion successful."});
    } else {
        $out .= $this->log()->show_diagnostics();
    }
    $uid = $uid || $this->{input}{uid};
    $out .= $this->member_report($uid) if ($uid);
    return $out;
}

#----------------------------------------------------------------------------
# control panel ui

sub list_entry_forms {
    my $this  = shift;
    my @forms = $share{DB}->get_query("member log forms");
    my $out;
    if (!$share{Page}) {
        my $rept = new ExSite::ReportBuilder(
            title   => "Entry Forms",
            headers => ["ID", "Type", "Title", "Start Date", "End Date", ""]
        );

        foreach my $f (@forms) {
            my $actions =
              $ml->a("update",
                {href => $this->link(log => "edit_form", form_id => $f->{member_log_form_id}), class => "button_sm"});
            $rept->push($f->{member_log_form_id}, $f->{type}, $f->{title}, $f->{startdate}, $f->{enddate}, $actions);
        }
        $rept->tools($ml->a($msg{"new form"}, {href => $this->link(log => "add_entry_form")}));
        $out .= $rept->make;
    } else {
        my $title = lc($share{DB}->{map}->table_title("member_log_form", 1));
        if (!scalar @forms) {
            return $ml->p(
                "Sorry, we do not currently have any open $title. Please check back with us at a later time.");
        }
        my $links;
        foreach my $f (@forms) {
            next if (!$this->is_open($f));
            my $url = $this->link(log => "new", form_id => $f->{member_log_form_id});
            $links .= $ml->li($ml->a($f->{title}, {href => $url}), {class => $f->{type}});
        }
        $out .= $ml->ul($links);
    }
    return $out;
}

sub edit_form {
    my ($this) = @_;
    my $form_id = $this->{input}{form_id};
    my $form = new ExSite::Object(type => "member_log_form", id => $form_id);
    my $db = $share{DB};
    $db->get_data;
    if (keys %{$db->{form}{member_log_form}}) {
        my @id   = keys %{$db->{form}{member_log_form}};
        my $id   = $id[0] || "_";
        my $data = $db->{form}{member_log_form}{$id};
        foreach my $key (keys %$data) {
            $form->setdata($key, $data->{$key});
        }
        if ($form->save()) {
            return $this->update_form_preset($id);
        } else {
            return $this->error("An error occured saving your data." . $form->show_diagnostics("error", "html"));
        }
    }
    return $form->edit(action => $this->link(), hide => ["type"]);
}

sub add_entry_form {
    my ($this, $type) = @_;
    my @allcol = $share{DB}->get_columns("member_log_form");
    my $db     = $share{DB};
    foreach my $f (@allcol) {
        my $datatype = &get_datatype($f, $type);
        if ($f eq "type") {
            my @types = $this->visible_log_types;
            my @options;
            foreach my $o (@types) {
                push(@options, [$o, title($o)]);
            }
            unshift(@options, [undef, "== select =="]);
            $db->form->input(
                prompt   => "Type",
                type     => "select",
                name     => "type",
                size     => 30,
                options  => \@options,
                required => 1
            );
            next;
        }
        my $label = $share{DB}->{map}->get_column_attr("member_log_form", $f, "label");
        my $write = $share{DB}->{map}->get_column_attr("member_log_form", $f, "write");
        if ($write <= $db->authorize) {
            $db->input_column(prompt => $label, name => $f, column => $f, table => "member_log_form");
        }
    }
    $db->form->action($this->link(log => "add_entry_form2", uid => undef));
    my $out = $db->form->make;
    $db->form()->init();
    return $out;
}

sub add_entry_form2 {
    my ($this) = @_;
    my $db = $share{DB};
    my $data;
    foreach my $key (keys %{$this->{post}}) {
        my $col = $key;
        $col =~ s/#date//g;
        $data->{$col} = $this->{post}{$key};
    }
    my $id = $db->insert("member_log_form", $data);
    return $this->update_form_preset($id);
}

sub update_form_preset {
    my ($this, $form_id) = @_;
    my $db         = $share{DB};
    my @admin_cols = split(/,/, &preference("MemberLog.presets"));
    my $form       = new ExSite::Object(type => "member_log_form", id => $form_id);
    my $log        = new Modules::MemberLog::MemberLog(type => $form->getdata("type"));
    my $preset;
    if ($form->getdata("presets")) {
        $preset = jsonToObj($form->getdata("presets"));
    }
    foreach my $f (@admin_cols) {
        my $field_meta = $this->get_field_meta($f, $form->getdata("type"));
        if ($log->meta()->is_allowed($field_meta->{name})) {
            $log->meta()->input("meta_$f", $field_meta->{name}, $preset->{"meta_$f"});
        }
    }
    $db->form->input(type => "hidden", name => "member_log_form_id", value => $form_id);
    $db->form->action($this->link(log => "add_entry_form3", uid => undef));
    my $out = $db->form()->make();
    $db->form()->init();
    return $out;
}

sub add_entry_form3 {
    my ($this) = @_;
    my $db     = $share{DB};
    my $data   = $this->{post};
    my $id     = $data->{member_log_form_id};
    delete $data->{member_log_form_id};
    my $form = new ExSite::Object(type => "member_log_form", id => $id);
    my %presets;
    foreach my $key (keys %$data) {
        $presets{$key} = $data->{$key};
    }
    $form->setdata("presets", objToJson(\%presets));
    if (my $template = $this->form_title_template($form->getdata("type"))) {
        $form->setdata("title", &substitute($template, \%presets));
    }
    if (!$form->getdata("title")) {
        $form->delete;
        return $this->error("Sorry, your form does not have a title.  Please make the necessary corrections.");
    }
    my $id    = $form->force_save();
    my $title = $form->getdata("title");
    my $out   = $ml->p("Your form \"$title\" was saved.");
    $out .= $this->list_entry_forms;
    return $out;
}

sub form_title_template {
    my ($this, $type) = @_;
    my $template = &preference("MemberLog.form_title_template.$type");
    return $template;
}

sub top_menu {
    my $this = shift;
    if (!$this->{admin}) {
        return undef;
    }
    my @link;
    push @link, $ml->a("Top", {href => $this->link(log => undef, section_id => undef, uid => undef, log_id => undef)});
    if ($this->{section}) {
        push @link,
          $ml->a($this->{section}->get_my("title"), {href => $this->link(log => undef, uid => undef, log_id => undef)});
    }
    my $uid;
    if ($this->{Log}) {
        $uid = $this->{Log}->getdata("member_id");
    } else {
        $uid = $this->{input}{uid};
    }
    if ($uid) {
        my $member = &get_obj("member", $uid);
        push @link, $ml->a($member->name, {href => $this->link(log => "member_report", uid => $uid)});
    }
    return &ExSite::HTML::PathBar(links => \@link);
    return $ml->p(join(" &gt; ", @link));
}

# control panel search logs by member
sub browse_members {
    my ($this) = @_;
    my $out;
    my $db   = $share{DB};
    my $char = 'A%';
    my $full_text_match;
    my $nparam = 1;
    my $term   = $this->{input}{searchterm};
    if ($term) {
        $term .= "*" if (length($term) < 4);
        $full_text_match = &preference("MemberLog.full_text_query")
          || "and match (first_name,last_name,organization) against (? in boolean mode)";
        $nparam++;
    }
    my $sql = &substitute(&preference("MemberLog.member_query"), {full_text_match => $full_text_match})
      || "select m.member_id,m.first_name,m.last_name,m.organization,(select status from member_status where member_status.member_id=m.member_id order by member_status_id desc limit 1) status, count(log.member_log_id) as count from member m left join member_log log on m.member_id = log.member_id where m.section_id = ? $full_text_match group by m.member_id order by organization";
    $db->set_query(
        "member log report",
        (
            sql    => $sql,
            nparam => $nparam,
            mode   => "r",
            keys   => ["member", "member_log", "member_status"],
        )
    );
    my @params = ($this->get_section_id);
    push(@params, $term) if ($term);
    if (!$term && $db->count("member") > 500) {
        $out .= $ml->h3("Search for members by name");
        $out .= $ml->p("You can search for multiple names by typing them in separated by a space");
        $out .= $ml->p("The * character can be appended to the end of your terms to find partial matches.");

        $out .= $ml->form(
            $ml->input(undef, {type => "text", name => "searchterm", value => undef})
              . $ml->input(undef, {type => "hidden", name => "section_id", value => $this->{section}->id})
              . $ml->input(undef, {type => "submit", value => "Search"}),
            {method => "post"}
        );
        return $out;
    }
    my @members = $db->get_query("member log report", @params);
    my $rept = new ExSite::ReportBuilder(
        title   => "Members",
        headers => ["ID", "Organization", "First Name", "Last Name", "Records", ""],
        dynamic => 1
    );

    foreach my $m (@members) {
        my $member = &get_obj("member", $m->{member_id});
        $member->setup(data => $m);
        my $count = $m->{count};
        my $uid   = $m->{member_id};
        my $name  = $member->name;
        my $option;
        $option = $ml->a(
            $ml->img(
                undef,
                {
                    src    => "$config{server}{HTMLpath}/_Modules/MemberLog/table_go.png",
                    align  => "middle",
                    border => 0,
                    alt    => "",
                    title  => "view log for $name"
                }
            ),
            {href => $this->link(log => "member_report", uid => $uid, log_id => undef)}
        );
        $rept->push(
            $uid,
            $member->getdata("organization"),
            $member->getdata("first_name"),
            $member->getdata("last_name"),
            $count, $option
        );
    }
    $out .= $rept->make();
    return $out;
}

# cron() allows this plug-in to respond to scheduling events.
sub cron {
    my ($this, $command, $type, $id) = @_;

    # $command, $type, $id are message parameters passed to us by the
    # scheduler.  They have no predefined meaning, so you can do as you
    # please with them.

    # You can optionally return some status messages
    my $out;

    # perform your scheduled tasks here.
    my $stat = $this->run_handler("MemberLog_cron", $command, $type, $id);
    return $stat if ($stat);
    return $out;
}

#----------------------------------------------------------------------------
# IMPORTING

# import() allows importing of logs from spreadsheet
sub import {
    my ($this) = @_;
    my $fb = new ExSite::FormBuilder;
    $fb->action($this->link(log => "do_import1"));

    my @types = $this->visible_log_types;
    my @options;
    foreach my $o (@types) {
        push(@options, [$o, $share{DB}->{map}->table_title($o)]);
    }
    unshift(@options, [undef, "== select =="]);
    $fb->input(
        prompt   => "Type",
        type     => "select",
        name     => "type",
        size     => 30,
        required => 1,
        options  => \@options
    );

    $fb->input(
        type     => "file",
        name     => "spreadsheet",
        prompt   => "Select your excel spreadsheet",
        required => 1
    );
    $fb->input(type => "hidden", name => "vcard", value => 1000000);
    $fb->buttons(submit => 1);
    my $instructions = $ml->ol(
        [
            "This feature allows you to add or update multiple entries in your database.",
"Use the above templates OR export a small subset of data as per usual using the \"Search by type\" tab making sure to check off the \"importable column headers\" checkbox.",
"Open your spreadsheet - the member_id column denotes the member being referenced in your membership database - if this is a new member this column should be empty.",
"The member_log_id column denotes an existing log entry. If this is a new log entry this column should also be empty.",
"If this is your first time using the tool it suggested that you try inserting/updating a small number of rows before moving on to larger updates.",
            "Under no circumstances should column headings be modified, deleted or moved.",
            "Data rows which are not being updated or inserted should be deleted.",
            "Once your spreadsheet is ready, attach the file and choose the log type and submit.",
            "The system will allow you to view any issues before allowing you to confirm your changes.",
"Important note: do not attempt to insert more than two entries for a new user otherwise multiple profiles will be created."
        ]
    );

    my $out = $ml->h3("Import data");
    foreach my $type (@types) {
        my $label = "Download template for new " . $share{DB}->{map}->table_title($type, undef, 1);
        $out .= $ml->p(
            $ml->a(
                $label,
                {
                    href => $this->link(
                        log        => "do_search_form",
                        type       => $type,
                        importable => 1,
                        rpt_format => "excel_no_data"
                    )
                }
            )
        );
    }
    $out .= &ExSite::HTML::DynList(
        {
            label => " Instructions (please read)",
            text  => $ml->div($instructions),
            open  => 0
        }
    );
    $out .= $fb->make;
    return $out;
}

sub do_import1 {
    my ($this) = @_;

    require Spreadsheet::ParseExcel;
    require Text::Iconv;
    require Spreadsheet::XLSX;

    my $input = new ExSite::Input;

    my $t     = new ExSite::Time;
    my $stamp = $t->write("unix_timestamp");
    my $trial;
    my $filename;
    my $sec = $t->write("raw_second");
    if ($this->{input}{confirm}) {
        $trial    = 0;
        $filename = $this->{input}{filename};
    } else {
        $trial = 1;
        $filename = $input->fetch_file("spreadsheet", "name");
        return $this->error("File was not found.") if (!$filename);
        my $raw = $input->fetch_file("spreadsheet", "raw");
        open FILE, ">/tmp/${filename}" or die $!;
        print FILE $raw;
        close FILE;
    }

    my $book;
    my $out;
    if ($filename =~ /\.xlsx$/) {
        my $converter = Text::Iconv->new("windows-1251", "windows-1251");
        $book = Spreadsheet::XLSX->new("/tmp/${filename}", $converter);
    } elsif ($filename =~ /\.xls$/) {
        $book = Spreadsheet::ParseExcel::Workbook->Parse("/tmp/${filename}");
    } else {
        $out .= $ml->error("
	Please try again. Your file does not seem to be an excel spreadsheet ( does not have a xls or xlsx extension ).");
        $out .= $this->import_form;
        return $out;
    }
    my $sheet          = $book->{Worksheet}[0];
    my $rmax           = $sheet->{MaxRow} + 1;
    my @header         = map { $_->{Val} } @{$sheet->{Cells}[1]};
    my $mcol           = $this->get_member_columns();
    my @member_columns = keys %$mcol;
    my $out;
    my $num_errors;
    my $num_inserts;
    my $num_updates;
    my $position = $this->{input}{position} || $sheet->{MinRow} + 2;

    if (!eval "require Modules::Membership") {
        return $ml->error("This feature is not available on this system");
    }
    my $m = &ExSite::Module::get_module("Membership");
    for (my $row = $position ; defined $sheet->{MaxRow} && $row <= $sheet->{MaxRow} ; $row++) {
        my @data;
        foreach my $cell (@{$sheet->{Cells}[$row]}) {
            push(@data, &fix_XML($cell->{Val}));
        }
        my @err;
        my $type = $this->{input}{type};
        my $is_exist;
        my $contact;
        my $c;
        my $member = new Modules::Membership::Member;
        my $log = new Modules::MemberLog::MemberLog(type => $type);
        $log->setdata("type", $type);
        my $i = 0;

        foreach my $col (@header) {

            # are we updating an existing member
            if ($col eq "mem_member_id") {
                if ($data[$i]) {
                    $is_exist->{member} = 1;
                    $member = new Modules::Membership::Member(id => $data[$i]);
                }
            } elsif ($col eq "member_log_id" && $data[$i]) {

                # are we updating an existing log
                $is_exist->{member_log} = 1;
                $log = new Modules::MemberLog::MemberLog(type => $type, id => $data[$i]);
            } elsif ($col =~ /contact_(\w+)_(\w+)/) {

                # check for contact data
                $contact->{$1}->{$2} = $data[$i];

                # column names can collide with attribute names
                # check for member data
            } elsif ($col =~ /mem_(\w+)/ && scalar grep(/$1/, @member_columns) > 0) {
                $member->setdata($1, $data[$i]);
            } elsif ($col =~ /meta_([\w-]+)/) {
                my $meta = $1;
                if ($log->meta()->is_allowed($meta)) {
                    my $datatype = &get_datatype($col, $type);
                    if ($datatype =~ /^date/ && $data[$i]) {
                        my $datetime = DateTime::Format::Excel->parse_datetime($data[$i]);
                        $data[$i] = $datetime->ymd();
                    } elsif ($datatype eq "money") {
                        $data[$i] =~ s/^\$//;
                    }
                    $log->meta_set($meta, $data[$i]);
                } else {
                    push(@err, "$meta is not a valid $type field.");
                }
            }
            $i++;
        }
        my $rnum = $row + 1;
        $out .= $ml->p("Analyzing row " . $rnum) if ($trial);
        if ($log->id && !$member->id) {
            push(
                @err,
                &substitute(
                    "Not allowed to update existing entry and attach to new member. (member_log_id: [[log_id]])",
                    {log_id => $log->id}
                )
            );
        }
        push(@err, $member->validate());
        if (!$log->getdata("member_id")) {
            $log->setdata("member_id", $share{DB}->my_uid);
        }
        if (!$log->getdata("status")) {
            $log->setdata("status", "active");
        }
        push(@err, $log->validate());
        push(@err, $log->meta()->fetch_diagnostics("error")) if scalar($log->meta()->fetch_diagnostics("error"));
        if (scalar @err) {
            $out .= join("\n", map { $ml->error($_) } @err);
            $num_errors += scalar @err;
            next;
        }

        # inserting a new member
        if (!$is_exist->{member}) {
            $member->setdata("ctime",      undef);
            $member->setdata("access",     0);
            $member->setdata("section_id", $this->get_section_id);
            if (!$trial) {
                $member->save();
                $m->{Member} = $member;
                $m->finalize_application();
                $num_inserts++;
            }

            # member exists - do an update
        } else {
            if (!$trial) {
                my $null = "NULL";
                $member->setdata("mtime", \$null);
                $member->save();
                $num_updates++;
            }
        }

        # inserting a new log
        if (!$is_exist->{member_log}) {
            $log->setdata("member_id", $member->id());
            $log->setdata("ctime",     undef);
            if (!$trial) {
                my $id = $log->save();
                $num_inserts++;
            }

            # log exists - do an update
        } else {
            if (!$trial) {
                my $null = "NULL";
                $log->setdata("mtime", \$null);
                my $id = $log->save();
                $num_updates++;
            }
        }

        # output for review
        if ($trial) {
            $out .= $member->show_generic();
            $out .= $log->show();
        }
        foreach my $type (keys %$contact) {
            my $c = new Modules::ID::Contact(data => $contact->{$type});
            if (grep(/\w+/, values %{$contact->{$type}})) {
                $c->setdata("type", $type);
                $c->setdata("privacy", "administrators") if (!$contact->{privacy});
                if (!$trial) {
                    my $account = $member->account;
                    my $old     = $account->get_contact($type);
                    if (!$old) {
                        $c->setdata("account_id", $account->id);
                        $c->save();
                    } else {
                        my $data = $contact->{$type};
                        foreach my $key (keys %$data) {
                            $old->setdata($key, $data->{$key});
                        }
                        $old->save();
                    }
                } else {
                    $out .= $c->show();
                }
            }
        }

        $position = $row + 1;
        last if (($num_inserts + $num_updates) >= 5);
    }
    my $summary;
    if ($num_errors) {
        $summary =
          $ml->info(
            "You have $num_errors errors to resolve before you can import this spreadsheet. See below for details.");
    } elsif ($trial) {
        $summary .= $ml->info(
"Your spreadsheet has been validated and an overview of the members to be imported are shown below. If the information below is correct please click the button below to have the system begin the import."
        );

        my $f = new ExSite::FormBuilder();
        $f->input(name => "filename", type => "hidden", value => $filename);
        $f->input(name => "confirm",  type => "hidden", value => 1);
        $f->input(name => "type",     type => "hidden", value => $this->{input}{type});
        $f->buttons(submit => "Begin Import");
        $summary .= $f->make;
    } elsif ($position <= $sheet->{MaxRow}) {
        $summary .= $ml->p("Please wait while your members are imported.");
        $summary .= $ml->p("Progress: working on row $position / " . $rmax);
        my $f = new ExSite::FormBuilder();
        $f->name("background_import");
        $f->input(name => "filename", type => "hidden", value => $filename);
        $f->input(name => "confirm",  type => "hidden", value => 1);
        $f->input(name => "type",     type => "hidden", value => $this->{input}{type});
        $f->input(name => "position", type => "hidden", value => $position);
        $f->buttons(submit => 0, cancel => 0, reset => 0);
        my $autosubmit =
          $ml->script("setTimeout(document.background_import.submit(),10000)", {type => "text/javascript"});
        $summary .= $f->make;
        $summary .= $autosubmit;
    } else {
        $summary .= $ml->p(
"The system has finished importing your updates to the membership database. Please carefully review your data to ensure that all expected changes have been made."
        );
    }
    return $summary . $out;
}

#----------------------------------------------------------------------------
# PUBLISHING

# publish() can be used to write out any files that will benefit from
# being available statically
sub publish {
    my $this = shift;
    my $ml   = &get_obj("ML");
    my $out  = $ml->h2("Publishing MemberLog");
    my $q    = &DecodeQuery($ENV{QUERY_STRING});
    my $sid  = $q->{section};
    my $logs;
    $logs = $share{DB}->custom_query("select * from member_log");
    foreach my $log (@$logs) {
        $out .= $this->publish_log($log);
    }
    return $out;
}

sub publish_log {
    my ($this, $data) = @_;
    my $out;
    my $log        = new Modules::MemberLog::MemberLog(type => $data->{type}, id => $data->{member_log_id});
    my @metafields = $log->meta_allowed;
    my $path       = $this->diskpath . "/" . $log->id();

    foreach my $c (@metafields) {
        my $datatype = &get_datatype("meta_" . $c, $data->{type});
        my $read = $log->meta()->get_map_info($c, "read");
        if ($datatype =~ /file/ && $read < 1) {
            if (my $file = $log->meta_get($c)) {
                my $cdata    = new ExSite::ContentData;
                my $mkdir    = $cdata->mkdir($path);
                my $img      = new ExSite::Image($file);
                my $filename = &clean_filename($img->name);
                $out .= $this->publish_file("$path/$filename", $img);
            }
        }
    }
    return $out;
}

# diskpath to published files
sub diskpath {
    my $this = shift;
    return $config{server}{HTMLroot} . $this->htmlpath;
}

# htmlpath to published files
sub htmlpath {
    my $this = shift;
    my $path = "$config{server}{HTMLpath}/_Modules/MemberLog";
    return $path;
}

sub publish_file {
    my ($this, $file, $img) = @_;
    my $out;
    my $ml = &get_obj("ML");
    $file =~ /^(((\/[\w-]+)+)\/([\w-]+\.[\w]+))$/;
    my $secure_filename = $1;
    my $imgdata         = $img->get;
    if (!-e $secure_filename
        || length $imgdata != -s "$secure_filename")
    {
        if (open OUTFILE, ">$secure_filename") {
            print OUTFILE $imgdata;
            close OUTFILE;
            $out .= $ml->span("Publishing $secure_filename");
        } else {
            $this->error("Membership::publish: publish $secure_filename failed: $!");
            $out .= $ml->span("FAILED: $!", {class => "error"}) . $ml->br;
        }
    }
    return $out;
}

sub clear_link {
    my ($this,%opt) = @_;
    my $uri = new ExSite::URI;
    foreach my $key (keys %{$this->{input}}) {
        $uri->query($key=>undef);
    }
    $uri->query(%opt);
    return $uri->write();
}

#----------------------------------------------------------------------------
# class methods

sub service_link {
    my (%opt) = @_;
    my $uri = new ExSite::URI;
    if ($share{Page}) {
        $uri->setup($share{Page}->get_url_dynamic);
        $uri->service_page("MemberLog");
    }
    $uri->query(%opt);
    return $uri->write();
}

sub title {
    my ($type, $context, $plural) = @_;
    if ($context) {
        my $title = &preference("MemberLog.label.$type.$context");
        return $title if ($title);
    }
    return $msg{$share{DB}->{map}->table_title($type, $plural)};
}

sub fix_XML {
    my $value = shift;
    $value =~ s/&amp;/&/g;
    $value =~ s/&gt;/>/g;
    $value =~ s/&lt;/</g;
    $value =~ s/&quot;/"/g;
    $value =~ s/&apos;/'/g;
    return $value;
}

1;
