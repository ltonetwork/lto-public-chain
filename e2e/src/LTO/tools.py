class Tools:

    # overriding __eq__ method
    def __eq__(self, first, second):
        return first.__dict__ == second.__dict__
