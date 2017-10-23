#!/usr/bin/env python

import os
import sys
import subprocess

output_file_name = 'classtbl.ml'

classpath_tbl = {
    'linux2' : [ '/usr/lib/jvm/java-8-openjdk-amd64/jre/lib',
                 '/usr/lib/jvm/java-8-openjdk/jre/lib',
                 '/usr/lib/jvm/java-7-openjdk-amd64/jre/lib',
                 '/usr/lib/jvm/java-7-openjdk/jre/lib',
                 '/usr/lib/jvm/java-6-openjdk-amd64/jre/lib',
                 '/usr/lib/jvm/java-6-openjdk/jre/lib',
             ],
    'darwin' : [ '/Library/Java/JavaVirtualMachines/jdk1.8.0_151.jdk/Contents/Home/jre/lib',
                 '/Library/Java/JavaVirtualMachines/jdk1.7.0_75.jdk/Contents/Home/jre/lib',
                 '/System/Library/Frameworks/JavaVM.framework/Classes',
             ],
    }

core_jars = ['rt.jar', 'classes.jar', 'dt.jar']

###

jar_cmd_fmt = 'jar tf %s'

template_upper = '''
let stdtbl =
  let pmap =
      [
'''

template_lower = '''
      ]
  in
  let tbl = Hashtbl.create 0 in
  List.iter
    (fun (pname, llist) ->
      let s = Xset.create (List.length llist) in
      List.iter
        (fun lname ->
          Xset.add s lname
        ) llist;
      Hashtbl.add tbl pname s
    ) pmap;
  tbl
'''


def getjars():
    jars = []
    cps = classpath_tbl[sys.platform]
    if cps:
        for cp in cps:
            for core_jar in core_jars:
                jar = os.path.join(cp, core_jar)
                if os.path.exists(jar):
                    jars.append(jar)

    return jars



def gen(jars):
    
    mapping = {} # package name -> local name list

    for jar in jars:

        cmd = jar_cmd_fmt % jar

        proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, close_fds=True)
        f = proc.stdout

        for l in f:
            line = l.rstrip()
            if line.endswith('.class'):
                if True: # line.find('$') == -1:
                    s = os.path.splitext(line)[0]
                    cname = os.path.basename(s)
                    pname = os.path.dirname(s).rstrip('/').replace('/', '.')
                    try:
                        mapping[pname].append(cname)
                    except KeyError:
                        mapping[pname] = [cname]


    ofile = open(output_file_name, 'w')

    ofile.write(template_upper)

    for pname in mapping.keys():

        ofile.write('"%s", [\n' % pname)

        for cname in mapping[pname]:
            ofile.write('"%s";\n' % cname)

        ofile.write('];\n')

    ofile.write(template_lower)

    ofile.close()




def main():
    jars = getjars()

    for jar in jars:
        print 'using "%s"' % jar

    gen(jars)
    

if __name__ == '__main__':
    main()
