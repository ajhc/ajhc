define detag
    print (void *)((uintptr_t)$arg0 & ~0x3)
end


define pnode
        if ((uintptr_t)$arg0 & 1)
                set $dt = (node_t *)((uintptr_t)$arg0 & ~0x3)
                printf "--- lazy node %p %p\n", $arg0, $dt
                if ((uintptr_t)$dt->head & 1)
                printf "--- unevaled node\n"
                else
                printf "--- indirection\n"
                end
                printf "head: %p\n", $dt->head
                printf "a1: %p\n", $dt->rest[0]
                printf "a2: %p\n", $dt->rest[1]
                printf "--- line %u\n", *(uintptr_t *)((uintptr_t)$dt - sizeof(uintptr_t))
        else
                printf "--- WHNF node %p\n", $arg0
                printf "tag: %p %u\n", ((dnode_t *)$arg0)->what, ((dnode_t *)$arg0)->what
                printf "a1:  %p %u\n", ((dnode_t *)$arg0)->rest[0], ((node_t *)$arg0)->rest[0]
                printf "a2:  %p %u\n", ((dnode_t *)$arg0)->rest[1], ((node_t *)$arg0)->rest[1]
                printf "a3:  %p %u\n", ((dnode_t *)$arg0)->rest[2], ((node_t *)$arg0)->rest[2]
                printf "--- line %u\n", *(uintptr_t *)((uintptr_t)$arg0 - sizeof(uintptr_t))
        end

end


