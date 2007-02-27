define detag
    print (void *)((uintptr_t)$arg0 & ~0x3)
end


define pnode
        if ((uintptr_t)$arg0 & 1)
        printf "--- lazy node %p %p\n", $arg0, ((uintptr_t)$arg0 & ~0x3)
        if (((node_t *)((uintptr_t)$arg0 & ~0x3))->tag & 1)
        printf "--- unevaled node\n"
        else
        printf "--- indirection\n"
        end
        printf "tag: %p\n", ((node_t *)((uintptr_t)$arg0 & ~0x3))->tag
        printf "a1: %p\n", ((node_t *)((uintptr_t)$arg0 & ~0x3))->rest[0]
        printf "a2: %p\n", ((node_t *)((uintptr_t)$arg0 & ~0x3))->rest[1]

        else
        printf "--- WHNF node %p\n", $arg0
        printf "tag: %p %u\n", ((node_t *)$arg0)->tag, ((node_t *)$arg0)->tag
        printf "a1:  %p %u\n", ((node_t *)$arg0)->rest[0], ((node_t *)$arg0)->rest[0]
        printf "a2:  %p %u\n", ((node_t *)$arg0)->rest[1], ((node_t *)$arg0)->rest[1]
        printf "a3:  %p %u\n", ((node_t *)$arg0)->rest[2], ((node_t *)$arg0)->rest[2]
        end

end
