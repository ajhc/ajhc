#if _JHC_STANDALONE
int
main(int argc, char *argv[])
{
        hs_init(&argc,&argv);
        if (jhc_setjmp(&jhc_uncaught))
                jhc_error("Uncaught Exception");
        else
                _amain();
        hs_exit();
        return 0;
}
#endif
