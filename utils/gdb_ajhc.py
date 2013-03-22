import gdb

def _gpe(arg):
    """TOO SLOW!!!"""
    return gdb.parse_and_eval(arg)

class PrintHsHeapCommand (gdb.Command):
    """Dump Haskell heap."""
    def __init__ (self):
        super (PrintHsHeapCommand, self).__init__ ("print_hsheap", \
                                                       gdb.COMMAND_OBSCURE)

    def _p_arena(self):
        block_used = _gpe('arena->block_used')
        block_threshold = _gpe('arena->block_threshold')
        number_gcs = _gpe('arena->number_gcs')
        print 'block_used :     ', int(block_used)
        print 'block_threshold :', int(block_threshold)
        print 'number_gcs :     ', int(number_gcs)

    def _p_free_blocks(self):
        dict_normal = {}
        dict_mono = {}
        s_block = _gpe('arena->free_blocks.slh_first')
        while int(str(s_block), 16) != 0:
            s_block_dref = s_block.dereference()
            flags = s_block_dref['flags']
            if flags & 16 == 0:
                num_ptrs = s_block_dref['u']['pi']['num_ptrs']
                size = s_block_dref['u']['pi']['size']
                key = (int(num_ptrs), int(size))
                if key not in dict_normal:
                    dict_normal[(key)] = 1
                else:
                    dict_normal[(key)] += 1
            else:
                num_ptrs = s_block_dref['u']['m']['num_ptrs']
                key = int(num_ptrs)
                if key not in dict_mono:
                    dict_mono[(key)] = 1
                else:
                    dict_mono[(key)] += 1
            s_block = s_block_dref['link']['sle_next']
        print "free normal blocks:"
        for k, v in dict_normal.iteritems():
            print ' ', k, ':',  v
        print "free monolithic blocks:"
        for k, v in dict_mono.iteritems():
            print ' ', k, ':', v

    def invoke (self, arg, from_tty):
        self._p_arena()
        self._p_free_blocks()

PrintHsHeapCommand()
