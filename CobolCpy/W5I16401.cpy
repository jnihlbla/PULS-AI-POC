000100 01  MID-W5I16401.                                                        
000200*                                 MID-COPY TEXT FÖR W5016400              
000300     03 MID-IDARTNR-UT       PIC 9(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDHUVTYP-UT      PIC X(4).                                    
000800*                                 LOGGTYP EKONOMISK HÄNDELSE              
000900     03 MID-IDSUBTYP-UT      PIC X(3).                                    
001000*                                 LOGGTYP EKONOMISK HÄNDELSE              
001100     03 MID-TIREGDAT-FOM-UT  PIC 9(6).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001300     03 MID-TIREGDAT-TOM-UT  PIC 9(6).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500     03 MID-IDTRANS-UT       PIC X(4).                                    
001600*                                 BILDNUMMER                              
001700     03 MID-BEART-UT         PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900     03 MID-INPUT.                                                        
002000*                                 RADINFORMATION                          
002100        05 MID-IDDC          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300        05 MID-IDHUVTYP      PIC X(4).                                    
002400*                                 LOGGTYP EKONOMISK HÄNDELSE              
002500        05 MID-IDSUBTYP      PIC X(3).                                    
002600*                                 LOGGTYP EKONOMISK HÄNDELSE              
002700        05 MID-IDTRANS       PIC X(4).                                    
002800*                                 BILDNUMMER                              
002900        05 MID-IDPGM         PIC X(8).                                    
003000*                                 PROGRAM IDENTITET                       
003100        05 MID-IDUSER        PIC X(8).                                    
003200*                                 ANVÄNDARENS SÄKERHETS ID                
003300        05 MID-TIREGDAT      PIC 9(6).                                    
003400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003500        05 MID-TIKLOCK       PIC 9(9).                                    
003600*                                 KLOCKSLAG (TTMMSSTH)                    
003700        05 MID-REF1          PIC X(25).                                   
003800        05 MID-REF2          PIC X(25).                                   
003900*** END OF VILMAII-COPY LENGTH= 153 BYTES                                 
