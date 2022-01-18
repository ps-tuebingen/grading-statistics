#!/usr/bin/env fish

set base_dir production
set tutors_view tutors-view/
set tutors_view_alt $base_dir/tutors-view-alt/

mkdir -p $tutors_view_alt

pushd $base_dir
				set tutors $tutors_view/*
popd
for i in (seq 1 (count $tutors))
				set tutors[$i] (basename $tutors[$i])
end

for dir in $base_dir/{0,1}*/
				set -l hw (basename $dir)
				mkdir $tutors_view_alt/$hw/
				for tut in $tutors
								mkdir $tutors_view_alt/$hw/$tut
				end
end
for tut in $tutors
				#set students (for stud in $base_dir/$tutors_view/$tut/*/*/; basename $stud; end | sort | uniq)
				for hwp in $base_dir/$tutors_view/$tut/*/
								set hw (basename $hwp)
								cp -d $base_dir/$tutors_view/$tut/$hw/* $tutors_view_alt/$hw/$tut/
				end
end


