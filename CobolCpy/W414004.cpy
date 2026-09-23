000100 01  RENS-W414004.                                                        
000200*                                 SKAPAS F÷R POSTER SOM SKA               
000300*                                 RENSAS FR≈N DISTRIBUTIONS               
000400*                                 TRANSAKTIONSBAS.                        
000500     03 RENS-IDPGM           PIC X(8).                                    
000600*                                 PROGRAM IDENTITET                       
000700     03 RENS-TIREGDAT        PIC S9(7)           COMP-3.                  
000800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000900     03 RENS-TIKLOCK         PIC S9(9)           COMP-3.                  
001000*                                 KLOCKSLAG (TTMMSSTH)                    
001100     03 RENS-IDSEKVNR        PIC S9(3)           COMP-3.                  
001200*                                 GENERELLT SEKVENSNUMMER                 
001300     03 RENS-IDCPYTXT.                                                    
001400*                                 COPYTEXT IDENTITET                      
001500        05 RENS-CT-IDSYSTEM  PIC X(4).                                    
001600*                                 SKAPANDE SYSTEMNUMMER                   
001700        05 RENS-CT-IDPTYP    PIC X(3).                                    
001800*                                 POSTTYP                                 
001900        05 RENS-CT-IDVTYP    PIC X.                                       
002000*                                 POSTTYPSVERSION                         
002100*** END COPY W414004     LENGTH=27                                        
