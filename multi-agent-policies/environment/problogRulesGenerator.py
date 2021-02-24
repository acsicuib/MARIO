"""
A interface to generate string SWI rules
"""
class Rules():
    def __init__(self,rules=None):
        if rules == None:
            self.common = []
        else:
            self.common = rules
        self.statements = []

    def and_rule(self,name,*args):
        st_arg = []
        for ar in args:
            if type(ar)==list:
                st_arg .append("[%s]"%','.join(ar))
            else:
                st_arg .append(str(ar))
        # args = [str(ar) for ar in args ]
        param = ",".join(st_arg )
        self.statements.append("%s(%s).\n"%(name,param))

    def inner_rule(self, name, *args):
        args = [str(ar) for ar in args]
        param = ",".join(args)
        self.statements.append("%s(%s)" % (name, param))

    def or_rule(self, name, *args):
        args = [str(ar) for ar in args]
        param = ",".join(args)
        self.statements.append("%s(%s);\n" % (name, param))

    def __len__(self):
        return len(self.statements)

    def clear(self):
        self.statements = []

    def __str__(self):
        if len(self.common)>0 and len(self.statements):
            return str(self.common)+"".join(self.statements)
        elif len(self.statements):
            return "".join(self.statements)
        else:
            return ""
