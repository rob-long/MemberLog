#!/usr/bin/perl
#-----------------------------------------------------------------------
#
#   Copyright 2001-2009 Exware Solutions, Inc.  http://www.exware.com
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

package Modules::MemberLog::MemberLog;

use strict;
use ExSite::Config;
use ExSite::Misc;
use ExSite::Module;
use ExSite::ObjectMeta;
use ExSite::HTML;
use ExSite::URI;
use ExSite::Time;

use vars qw(@ISA $ml);
@ISA = qw(ExSite::ObjectMeta);

#=== OBJECT CREATION

$ml = &get_obj("ML");

sub my_type { "member_log"; }

sub new ($%) {
    my ($this, %opt) = @_;
    my $obj = {};
    my $class = ref($this) || $this;
    bless $obj, $class;
    $obj->initialize_object;
    $obj->setup(%opt);
    &ExSite::Module::read_conf("MemberLog");
    $ml = &get_obj("ML");
    my $in = new ExSite::Input;
    $obj->{query} = $in->query();
    $obj->{post}  = $in->post();
    return $obj;
}

sub name {
    my $this = shift;
    my $type = $this->type();
    if (&preference("MemberLog.$type.meta_key")) {
        my @key = split(/,/, &preference("MemberLog.$type.meta_key"));
        return join(" ", map { $this->meta_get($_) } @key);
    }
    return $this->SUPER::name(@_);
}

# ObjectMeta::update
sub update {
    my $this = shift;
    $this->setdata("mtime", undef);
    my $stat = $this->SUPER::update(@_);
    return $stat;
}

# logged in user owns this object
sub owns {
    my $this = shift;
    return 1 if ($this->uid() && $this->uid() == $share{DB}->my_uid);
    if (my $is_group_log = &preference("MemberLog.group_log")) {
        my $member = $this->member();
        return 1 if ($member->can("is_child")   && $member->is_child());
        return 1 if ($member->can("is_sibling") && $member->is_sibling());
        return 1 if ($member->can("is_parent")  && $member->is_parent());
        
    }
    $this->info("returning 0");    
    return 0;
}

# permission for adding logs in regular listings
sub allow_add {
    my $this = shift;
    my $stat = $this->run_handler("MemberLog_allow_add");
    return $stat if (defined $stat);
    return 1     if ($share{DB}->is_manager());
    return 1     if (&preference("MemberLog.allow_member_add"));
}

# permission for viewing full log
sub allow {
    my $this = shift;
#    my $stat = $this->run_handler("MemberLog_allow");
#    return $stat if (defined $stat);
    my $type = $this->type();
    my $privacy =
         Modules::MemberLog::confrule("privacy","logtype",$type)
      || &preference("MemberLog.privacy")
      || "public";
    return 1 if ($share{DB}->is_manager());
    if ($privacy eq "public") {
        return 1;
    } elsif ($privacy eq "users only") {
        return 1 if ($share{DB}->is_user());
    } elsif ($privacy eq "members only") {
        return 1 if ($share{DB}->is_member());
    }
    return $this->owns();
}

# permission for updating log
sub allow_edit {
    my $this = shift;
    my $stat = $this->run_handler("MemberLog_allow_edit");
    return $stat if (defined $stat);
    return 1     if ($share{DB}->is_manager());
    return 0     if (!&preference("MemberLog.allow_member_edit"));
    return $this->owns();
}

sub uid {
    my $this = shift;
    my $id   = $this->getdata("member_id");
    return $id;
}

sub member {
    my $this = shift;
    my $uid  = $this->uid();
    return new Modules::Membership::Member() if (!$uid);
    return &get_obj("member", $uid);
}

sub showdata {
    my ($this, $key) = @_;
    if ($this->defined) {
        if ($key eq $this->parent_type) {
            return $this->getdata($key);
        } elsif ($key eq "receivable_id") {
            my $id = $this->getdata("receivable_id");
            return "n/a" if (!$id);
            require Modules::Finance::Receivable;
            my $r = new Modules::Finance::Receivable(id => $id);
            my $id = $r->id;
            if ($share{Page}) {
                my $popup = &ExSite::HTML::Popup(
                    pane       => $r->show(summarize_surcharges => 1),
                    label      => $ml->span("Invoice $id"),
                    width      => 400,
                    height     => 300,
                    closelabel => "&times;",
                );
                return $popup;
            } else {
                my $sid = $r->account->getdata("section_id");
                return $ml->a(
                    "Invoice $id",
                    {
                        href =>
                          "$config{server}{server}$config{server}{CGIpath}/ctrl-panel.cgi/Pay?section_id=${sid}&inv="
                          . $id,
                        target => "_blank"
                    }
                );
            }
        } elsif ($key eq "documents") {
            return $this->show_documents();
        } elsif ($key eq "ctime") {
            if (&preference("MemberLog.ctime.hide_time")) {
                my $t = new ExSite::Time($this->getdata($key), "sql_timestamp");
                return $t->write("date_long");
            }
        }
    }
    return $this->SUPER::showdata($key);
}

sub show_mini {
    my ($this, %data) = @_;
    my $out;
    if (keys %data == 0) {
        %data = $this->get_log_data();
    }
    my $template = $this->mini_template();
    my $thumb;
    my $id = $data{member_log_id};
    foreach my $meta ($this->meta_allowed()) {
        if ($this->meta()->get_map_info($meta, "datatype") =~ /bigfile/) {
            my $rec = $share{DB}->fetch_match("attribute", {name => $meta, id => $id});
            if (scalar @$rec && $rec->[-1]->{value}) {
                $data{thumbnail} = $this->picture($meta, $rec->[-1]->{value}, $rec->[-1]->{attribute_id}, $id);
            } else {
                $data{thumbnail} = $ml->img(undef, {src => "[[nophoto.png]]"});
            }
        }
    }
    my $search_params;
    if ($share{MemberLog}{do_search_form}) {
        $search_params = $this->{query};
        $search_params->{log} = "do_search_form";
    }
    $data{log_url} = Modules::MemberLog::service_link(
        log           => "view",
        log_id        => $id,
        search_params => &EncodeHash(%$search_params) || undef
    );
    if (!$template || length($template) < 10) {
        $out .= &ShowHash(%data);
    } else {
        $out .= &substitute($template, \%data);
    }

    return $out;
}

sub picture {
    my ($this, $meta, $value, $attribute_id, $id) = @_;
    my $out;
    my $ml  = &get_obj("ML");
    my $img = new ExSite::Image($value);
    my ($width, $height) = $img->dim;
    my $filename = &clean_filename($img->{filename});
    my $out;

    if (!-e $this->diskpath . "/" . $id . "/" . $filename) {

        # not published; go to the database
        return $share{DB}->show_data_noauth("attribute", "value", $value, $attribute_id, "bigfile",);
    }
    my $mimetype = &MimeType($img->{filename});
    my $size     = int(length($value) * 6 / 8 / 100) / 10;
    if ($mimetype =~ /^image/) {
        $out = $ml->img(
            undef,
            {
                src    => $this->htmlpath . "/" . $id . "/" . $filename,
                height => $height,
                width  => $width,
                alt    => $img->{filename}
            }
        );
    } elsif ($size) {
        $out = $ml->a($filename, {href => $this->htmlpath . "/" . $id . "/" . $filename})
          . "&nbsp;($mimetype,&nbsp;$size&nbsp;K)";
    }
    return $out;
}

# diskpath to published files

sub diskpath {
    my $this = shift;
    return $config{server}{HTMLroot} . $this->htmlpath;
}

sub htmlpath {
    my $this = shift;
    my $path = "$config{server}{HTMLpath}/_Modules/MemberLog";
    return $path;
}

# get data for show functions
sub get_log_data {
	my ($this) = @_;
	my %data;
	# fetch basic data
	my %data = %{$this->{data}};
	my @fields = $this->meta_allowed();	
	if (defined $this->meta()->get()) {
		foreach my $f (@fields) {
		if ($this->meta()->get_map_info($f,"datatype") =~ /bigfile/) {
		$data{"meta_$f"} = $this->picture($f,$this->meta_get($f),$this->meta()->meta_id($f),$this->id);
		} else {
		$data{"meta_$f"} = $this->meta_show($f);
		}
		}
	}
	if (my $params = $this->{query}{search_params}) {
		my %query = &DecodeString($params);
		$data{url_search} = Modules::MemberLog::service_link(%query);
	}
	if ($this->allow_edit()) {
		$data{url_update} = Modules::MemberLog::service_link(log=>"update",log_id=>$this->id);
		if (my $clist = $this->children()) {
			$data{children_count} = $clist->count();
		}
	}
	my $page = &ExSite::Module::service_page("Membership");
	if ($this->member->allow()) {
		if ($page) {
		$data{url_profile} = $page->link(pro=>undef,uid=>$this->member->id);
		}
	}
	if (my $child_type = $this->child_type()) {
		$data{add_child_url} = Modules::MemberLog::service_link(log=>"new",parent_id=>$this->id,type=>$child_type);
		my $children = $this->children();
		if ($children) {
			my $label = &substitute("[[count]] [[title]]", {count=>$children->count(), title=>Modules::MemberLog::title($child_type,"manage",1)});
			my $url = Modules::MemberLog::service_link(log=>"list",type=>$child_type, parent_id=>$this->id());
			my $link = $ml->a($label, { href=>$url } );
			if ($children->count()) {
				$data{$child_type} = $link;
			} else {
				$data{$child_type} = $label;
			}
		}
	}
	if (my $parent_type = $this->parent_type()) {
		my $parent = $this->parent();
		if ($parent) {
			$data{$parent_type} = $parent->name();
		}
	}
	$data{member_name} = $share{DB}->user_name($this->member->id);
	$this->run_handler("MemberLog_log_data", \%data);
	return wantarray ? %data : \%data;
}

sub parent {
    my $this = shift;
    my $pid  = $this->getdata("parent_id");
    if ($pid) {
        my $p = $share{DB}->fetch("member_log", $pid);
        return new Modules::MemberLog::MemberLog(type => $p->{type}, id => $pid);
    }
    return undef;
}

# get objectlist of log children
sub children {
    my $this = shift;
    if ($this->child_type()) {
        my %children = $this->get_children($this->child_type());
        return $children{$this->child_type()};
    }
    return undef;
}

sub parent_type {
    my $this     = shift;
    my $type     = $this->type();
    my $map      = $share{DB}->{map};
    my $datatype = $map->get_column_attr($type, "parent_id", "datatype");
    if ($datatype =~ /key:(\w+)/) {
        return $1 if ($1 ne "member_log");
    }
    return undef;
}

sub child_type {
    my $this = shift;
    my $type = $this->type();
    return &preference("MemberLog.$type.child");
}

sub mini_template {
    my ($this) = @_;
    my $type = $this->type();
    my $html = $this->get_template($this->mini_template_name()) || $this->get_template("member_log_template");
    return $html if ($html);
    return undef;
}

sub mini_template_name {
    my $this = shift;
    if (exists $this->{mini_template_name}) {
        return $this->{mini_template_name};
    }
    my $type = $this->type();
    return "${type}_mini";
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

        # always get the newest revision of templates
        $loc->set_revision($revision);
        my $html = $ctemplate->get_html();
        return $html;
    }
    return undef;
}

sub show {
    my ($this, %opt) = @_;
    if ($this->exists) {
        if (!$this->allow()) {
            if ($share{DB}->authorize) {
            return $this->error($msg{"Sorry, you do not have permission to view this."});
            } else {
            return Modules::MemberLog::login_form($this->type());
            }
        }

        my %data   = $this->get_log_data();
        my @fields = $this->meta_allowed();
        if ($share{Page}) {
            my @path;
            my $p = $this->parent();
            if ($p && $p->allow_edit()) {
                push(
                    @path,
                    $ml->a(
                        Modules::MemberLog::title($p->type(), undef, 0),
                        {
                            href =>
                              Modules::MemberLog::service_link(type => $p->type(), log => "list", parent_id => undef)
                        }
                    )
                );
                push(
                    @path,
                    $ml->a(
                        Modules::MemberLog::title($p->name(), undef, 0),
                        {
                            href =>
                              Modules::MemberLog::service_link(type => $p->type(), log => "view", log_id => $p->id())
                        }
                    )
                );
            }
            if ($this->allow_edit()) {
                if (!$p) {
                    push(
                        @path,
                        $ml->a(
                            Modules::MemberLog::title($this->type(), undef, 0),
                            {
                                href => Modules::MemberLog::service_link(
                                    type      => $this->type(),
                                    log       => "list",
                                    parent_id => undef
                                )
                            }
                        )
                    );
                }
                push(@path, $this->name);
            }
            $data{pathbar} = $ml->div(join(" &gt; ", @path), {class => "MemberLogPath"});
        }

        # custom template?
        my $loc           = $share{Page} || $share{Section};
        my $type          = $this->type;
        my $template_name = $type . "_template";
        my $ctemplate     = $loc->find($template_name);
        my $template      = $ctemplate->get_html();
        if (!$share{MemberLog}{admin} && $template) {
            return $data{pathbar} . &substitute($template, \%data);
        }

        # generic table view
        my @reportable;
        my @fields = $this->meta_allowed();
        foreach my $f (@fields) {
            push(@reportable, $f) if ($this->meta()->get_map_info($f, "read") <= $share{DB}->authorize);
        }
        my @head =
          map { $this->meta->label($_) }
          sort { $this->meta()->get_map_info($a, "rank") <=> $this->meta()->get_map_info($b, "rank") } @reportable;
        my @data;
        foreach my $h (sort { $this->meta()->get_map_info($a, "rank") <=> $this->meta()->get_map_info($b, "rank") }
            @reportable)
        {
            push @data, $data{"meta_$h"};
        }
        my $title = $this->run_handler("MemberLog_log_title") || $this->name();
        my $r = new ExSite::ReportBuilder(
            title   => $title,
            headers => \@head,
            data    => \@data,
        );
        return $data{pathbar} . $r->make();
    }
    return ExSite::Object::show($this, %opt);
}

# ExSite::ObjectMeta::editmeta with ranking

sub editmeta {
    my ($this, %opt) = @_;
    if ($this->ok) {
        $this->load();
        $this->meta()->load();
        $this->DB()->set_action($this->action());    # insert/update
        $this->DB()->form(%opt);
        $this->DB()->form()->multipart;
        $this->DB()->input_record(
            {
                table  => $this->{type},
                record => $this->{id},
                data   => $this->get(),
            }
        );

        my @metafields = $this->meta()->get_allowed();
        if (@metafields == 0) {
            my %meta = $this->meta()->get_all();
            @metafields = keys %meta;
        }
        foreach my $key (sort { $this->meta()->get_map_info($a, "rank") <=> $this->meta()->get_map_info($b, "rank") }
            @metafields)
        {

            # make record if allowed and it does not yet exist in core DB
            if (!exists $this->meta()->{raw}{$key}) {
                my $data = $this->meta()->make_record($key);
                $this->meta()->insert($data);
                $this->meta()->{loaded} = 0;
                $this->meta()->load();
            }
            $this->meta()->input(undef, $key);
        }

        # custom template?
        my $loc = $share{Page} || $share{Section};
        if ($loc) {
            my $type          = $this->{type};
            my $template_name = "${type}_form_template";
            my $ctemplate     = $loc->find($template_name);
            my $template      = $ctemplate->get_html();
            my $template_map;
            foreach
              my $key (sort { $this->meta()->get_map_info($a, "rank") <=> $this->meta()->get_map_info($b, "rank") }
                @metafields)
            {
                foreach my $type (qw(prompt input)) {
                    $template_map->{"meta_$key:$type"} =
                      "[[attribute#" . $this->meta()->recid($key) . "#value:$type]]";
                }
            }
            $template = &substitute($template, $template_map);
            $this->DB()->form()->template($template);
        }
        $this->DB()->form()->input(type => "hidden", name => "reply", value => $opt{extra}->{reply});
        return $this->DB()->form()->make;
    }
    return $this->show_diagnostics("error", "html");
}

sub show_documents {
    my ($this) = @_;
    my $out;
    my $db   = $share{DB};
    my $docs = $this->getdata("documents");
    return undef if (!$docs);
    my $sql             = "select * from content where content_id in ($docs)";
    my @content_records = $share{DB}->custom_query($sql);
    $out .= $ml->style("dd { margin: 0; padding: 0 0 0.5em 0; }");
    my @content;
    my $nrec = scalar @content_records;

    my @dlist;
    foreach my $crec (@content_records) {
        my $listitem;
        my $c = new ExSite::Content(content => $crec, version => "newest");
        $c->set_context();
        my $url = $c->get_url;
        my ($file, $size, $mime) = $c->get_fileinfo;
        my $time = $db->show_data_noauth("content_data", "ctime", $c->timestamp);
        my $descr = $db->show_data_noauth("content", "description", $c->get("content", "description"));
        if ($descr) { $descr .= $ml->br(); }
        my $info;

        if ($file) {

            # save file info to an array
            my @fileinfo;

            # show file post date
            push(@fileinfo, $ml->span($msg{"posted on"} . " $time", {class => "DocumentDate"}));

            # show file info
            push(@fileinfo, $ml->span("($size K)", {class => "DocumentFileInfo"}));
            if (@fileinfo == 0) {
                push @fileinfo, $msg{"(no file available for download)"};
            }
            $info = $ml->span(join(" ", @fileinfo), {style => "font-size: 11.5px"});
        }
        my ($dt, $dd);
        if ($c->has_content()) {
            my %a = (
                href  => $url,
                class => &MimeToFile($mime),
            );

            $dt = $ml->a($file, \%a);
        }
        $dd = $descr . $info;
        my $uri = new ExSite::URI;
        if ($uri->write() =~ /$config{prog}{ctrlpanel}/ && $db->owns($this->{section_id})) {
            $uri->query(log => "ddel", docid => $crec->{content_id}, confirm => undef);
            my $del_url = $uri->write;
            $dd .= "&nbsp;"
              . $ml->a(
                $ml->img(
                    undef,
                    {
                        src    => "$config{server}{HTMLpath}/_Modules/Document/del.gif",
                        height => 12,
                        width  => 12,
                        border => 0,
                        alt    => "delete",
                        title  => "delete"
                    }
                ),
                {href => $del_url}
              );
        }
        push @dlist, {dt => $dt, dd => $dd};
    }
    $out .= $ml->dl(\@dlist);
    return $ml->div($out, {class => "Document"});
}

sub is_active {
    my $this = shift;
    return ($this->status eq "active");
}

sub status {
    my ($this) = @_;
    return $this->getdata("status");
}

sub disable {
    my $this = shift;
    $this->setdata("status", "inactive");
    return $this->save();
}

sub enable {
    my $this = shift;
    $this->setdata("status", "active");
    $this->save();
    return;
}

sub label {
    my ($this, $name) = @_;
    my $label = ucfirst $name;
    $label =~ s/_/ /g;
    return $msg{$this->meta()->get_map_info($name, "label")} || $msg{$label};
}

1;
