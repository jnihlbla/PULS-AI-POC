000100 01  W61237.                                                              
000200*                                 BORTTAG DATA WDR301                     
000300     03 IDPGM                PIC X(8).                                    
000400*                                 PROGRAM IDENTITET                       
000500     03 TIREGDAT             PIC S9(7)           COMP-3.                  
000600*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000700     03 TIKLOCK              PIC S9(9)           COMP-3.                  
000800*                                 KLOCKSLAG (TTMMSSTH)                    
000900     03 IDSEKVNR             PIC S9(3)           COMP-3.                  
001000*                                 GENERELLT SEKVENSNUMMER                 
001100     03 IDCPYTXT.                                                         
001200*                                 COPYTEXT IDENTITET                      
001300        05 CT-IDSYSTEM       PIC X(4).                                    
001400*                                 VOLVO VCAS SYSTEMNUMMER                 
001500        05 CT-IDPTYP         PIC X(3).                                    
001600*                                 POSTTYP                                 
001700        05 CT-IDVTYP         PIC X.                                       
001800*                                 POSTTYPSVERSION                         
001900*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
