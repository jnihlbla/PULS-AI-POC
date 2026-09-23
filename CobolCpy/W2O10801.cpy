000100 01  W2O10801-CTX.                                                        
000200*                                 COPYTEXT FÖR MOD W2O10801               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE-RAD1         PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDINLEV              PIC 9(15).                                   
001200*                                 INLEVERANS NUMMER                       
001300     03 W2O10801-001-GRP.                                                 
001400*                                                                         
001500        05 W2O10801-002-GRP  OCCURS 13 TIMES.                             
001600*                                                                         
001700           07 W2O10801-003-GRP                                            
001800                             OCCURS 2 TIMES.                              
001900*                                                                         
002000              09 TIAVSDAT    PIC 9(6).                                    
002100*                                 AVISERINGSDATUM (YYMMDD)                
002200              09 FILLERX1    PIC X.                                       
002300              09 IDFS        PIC X(8).                                    
002400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002500              09 IDAVINR REDEFINES IDFS                                   
002600                             PIC Z(7)9.                                   
002700*                                 AVINUMMER           IDAVINR-002         
002800              09 KVAVIS      PIC -(7)9.                                   
002900*                                 AVISERAT ANTAL                          
003000              09 FILLERX1    PIC X.                                       
003100              09 IDLEVNR     PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300              09 FILLERX1    PIC X.                                       
003400              09 IDPTYP      PIC X(3).                                    
003500*                                 POSTTYP                                 
003600              09 KDRT        PIC Z9(2).                                   
003700*                                 REDOVISNINGSTYP                         
003800              09 FILLERX3    PIC X(3).                                    
003900        05 MESSAGE-RAD23     PIC X(78).                                   
004000*                                 MEDDELANDEFÄLT PÅ RAD 23                
004100*** END OF VILMAII-COPY LENGTH= 1169 BYTES                                
