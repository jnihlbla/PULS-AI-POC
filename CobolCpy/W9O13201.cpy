000100 01  W9O13201.                                                            
000200*                                 COPYTEXT F÷R MOD                        
000300*                                 W9O13201                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-IN           PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDDISTR-UT           PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDARTNR-IN           PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 IDARTNR-UT           PIC X(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 IDARTNR-FIRST        PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 TIREGDAT-FIRST       PIC X(6).                                    
001900*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002000     03 KDOBJEKT-FIRST       PIC X.                                       
002100*                                 OBJEKTSKOD                              
002200     03 IDORDNR-KEY-FIRST    PIC X(5).                                    
002300*                                 ORDERNUMMER                             
002400     03 IDARTNR-LAST         PIC X(9).                                    
002500*                                 ARTIKELNUMMER                           
002600     03 TIREGDAT-LAST        PIC X(6).                                    
002700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002800     03 KDOBJEKT-LAST        PIC X.                                       
002900*                                 OBJEKTSKOD                              
003000     03 IDORDNR-KEY-LAST     PIC X(5).                                    
003100*                                 ORDERNUMMER                             
003200     03 AREA.                                                             
003300        05 LINES             OCCURS 12 TIMES.                             
003400           07 IDARTNR-OBJ    PIC Z(7)9.                                   
003500*                                 OBJEKTNUMMER                            
003600           07 IDORDNR        PIC Z(4)9.                                   
003700*                                 ORDERNUMMER                             
003800           07 TIAAVV-REG     PIC 9(4).                                    
003900*                                 ≈R - VECKA  (≈≈VV)                      
004000           07 KVRETUR        PIC Z(6)9.                                   
004100*                                 ANTAL I RETUR                           
004200           07 KVANTAL-FAKT   PIC Z(5)9.                                   
004300*                                 FAKTURERAT ANTAL                        
004400*                                                                         
004500           07 REST-ANT       PIC -(7)9.                                   
004600*                                 ANTAL ALLMƒNT                           
004700           07 TIAAVV-RENS    PIC 9(4).                                    
004800*                                 ≈R - VECKA  (≈≈VV)                      
004900     03 TEMFSINF             PIC X(61).                                   
005000*                                 INFORMATIONSMEDDELANDE                  
005100*** END COPY W9O13201C0  LENGTH=677                                       
