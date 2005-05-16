#!/usr/bin/python
"""
 makedepf90 doesn't work. A down to earth solution "grep" based solution :)
 usage:
 makedepf95 *.f90 (and cross you fingers it'll work)

 juha.vierinen@csc.fi
"""
import sys
import re
import os

class F90DepFinder:
    def __init__(self,files):
        usewhat = re.compile('USE ([^ ]*)[\n ]')
        self.fileToObject = re.compile('^([a-zA-Z0-9_-]+).[a-zA-Z0-9]+$')
        self.files=files
        
        self.modToFileHash = {}
        self.objects = {}

        for f in files:
            # get object name
            oname=self.getObjectName(f)
            print f
            deps = {}
            self.objects[oname]=deps
            deps[f]=1

            input = open(f,'r')
            
            # get use statements
            a=input.readline()
            while a:
                m = usewhat.search(a,re.I)
                if m:
                    module = m.group(1).lower()

                    depFileName=self.findFile(module)

                    if depFileName != None:
                        deps[self.getObjectName(depFileName)]=1

                a=input.readline()
            input.close()
        
    def findFile(self,module):
        """ find file that contains module. """
        # sanity check
        if len(module) <= 1:
            return None
        
        if self.modToFileHash.has_key(module):
            return self.modToFileHash[module]
        
        moduleName = re.compile('module ([^ ]*)[\n ]',re.I)

        for f in self.files:
            input = open(f,'r')

            # get module statements
            a=input.readline()
            while a:
                m = moduleName.search(a,re.I)
                if m:
                    modFound = m.group(1).lower()
                    if modFound == module:

                        self.modToFileHash[module]=f
                        input.close()
                        return f
                a=input.readline()
                m=None

        print "Warning, file not found for module ", module
        input.close()
        return None
    
    def getObjectName(self,fname):
        """ simple regexp to create object name from blah.f90 """
        # get object name 
        om=self.fileToObject.search(fname)
        if om:
            oname = om.group(1) + ".o"
            return oname
        else:
            print "fatal, name couldn't be created " , fname

            return None

if __name__ == "__main__":
    depfinder = F90DepFinder(sys.argv[1:])
    print "Found dependencies: "
    for k in depfinder.objects:
        line = k + ": "

        for ok in depfinder.objects[k]:
            line = line + " " + ok 
        print line
    

