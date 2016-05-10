# Proxy object
# For the sake of simplicity doesn't redirect special methods
class Box:
    def __init__(self, obj):
        obj.parentBox = self
        object.__setattr__(self, "_obj", obj)
    def __getattribute__(self, name):
        if name in [ 'getContent', 'setContent' ]:
            return object.__getattribute__(self, name)
        return getattr(object.__getattribute__(self, "_obj"), name)
    def __delattr__(self, name):
        delattr(object.__getattribute__(self, "_obj"), name)
    def __setattr__(self, name, value):
        setattr(object.__getattribute__(self, "_obj"), name, value)
    def __str__(self):
        return "Box(%s)" % str(object.__getattribute__(self, "_obj"))
    def __repr__(self):
        return "Box(%s)" % repr(object.__getattribute__(self, "_obj"))

    def getContent(self):
        return object.__getattribute__(self, "_obj")

    def setContent(self, newObj):
        object.__getattribute__(self, "_obj").parentBox = None
        newObj.parentBox = self
        object.__setattr__(self, "_obj", newObj)
