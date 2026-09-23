000010 01  W61184.                                                              
000020*                                 RENSNINGSFIL F÷R W6G3                   
000030*                                                                         
000040     03 IDPGM                PIC X(8).                                    
000050*                                 PROGRAM IDENTITET                       
000060     03 TIREGDAT             PIC S9(7)           COMP-3.                  
000070*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000080     03 TIKLOCK              PIC S9(9)           COMP-3.                  
000090*                                 KLOCKSLAG (TTMMSSTH)                    
000100     03 IDSEKVNR             PIC S9(3)           COMP-3.                  
000110*                                 GENERELLT SEKVENSNUMMER                 
000120     03 IDCPYTXT.                                                         
000130*                                 COPYTEXT IDENTITET                      
000140        05 CT-IDSYSTEM       PIC X(4).                                    
000150*                                 SKAPANDE SYSTEMNUMMER                   
000160        05 CT-IDPTYP         PIC X(3).                                    
000170*                                 POSTTYP                                 
000180        05 CT-IDVTYP         PIC X.                                       
000190*                                 POSTTYPSVERSION                         
      *** END COPY W6118401    LENGTH=27                                        
