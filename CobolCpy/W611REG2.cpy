000100 01  REG2-W611REG2.                                                       
000200*                                 LÄNKAREA TILL W611REG -                 
000300*                                 REGISTRERING  AV FÖLJESEDEL             
000400     03 REG2-IDTRANS         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 REG2-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 REG2-IDLEVNR         PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001300     03 REG2-IDLEVNR-OK      PIC X.                                       
001400     03 REG2-KDRT            PIC S9(3)           COMP-3.                  
001500*                                 REDOVISNINGSTYP                         
001600*                                 TYPE OF ACCOUNTING                      
001700     03 REG2-KDRT-OK         PIC X.                                       
001800     03 REG2-TIAVIDAT        PIC S9(7)           COMP-3.                  
001900*                                 AVISERINGSDATUM (YYMMDD)                
002000*                                 ADVICE NOTE DATE                        
002100     03 REG2-TIAVIDAT-OK     PIC X.                                       
002200     03 REG2-IDLBBET         PIC X(12).                                   
002300*                                 LASTBÄRARBETECKNING                     
002400*                                 TRAILER NUMBER                          
002500     03 REG2-FLGODK          PIC X.                                       
002600     03 REG2-FLGODK-IDFS     PIC X.                                       
002700     03 REG2-FLGODK-IDARTNR  PIC X.                                       
002800     03 REG2-IDARTNR         OCCURS 24 TIMES                              
002900                             PIC S9(9)           COMP-3.                  
003000*                                 ARTIKELNUMMER                           
003100*                                 PART NUMBER                             
003200     03 REG2-IDARTNR-OK      OCCURS 24 TIMES                              
003300                             PIC X.                                       
003400     03 REG2-IDMFSFEL        OCCURS 24 TIMES                              
003500                             PIC X(3).                                    
003600*                                 MFS FELMEDDELANDE NUMMER                
003700*                                 MFS ERROR MESSAGE NUMBER                
003800     03 REG2-KVAVIS          OCCURS 24 TIMES                              
003900                             PIC S9(7)           COMP-3.                  
004000*                                 AVISERAT ANTAL                          
004100*                                 QUANTITY NOTIFIED                       
004200     03 REG2-IDFS            OCCURS 24 TIMES                              
004300                             PIC X(8).                                    
004400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
004500*                                 ADVICE NOTE NUMBER ODETTE               
004600     03 REG2-IDFS-OK         OCCURS 24 TIMES                              
004700                             PIC X.                                       
004800*** END OF VILMAII-COPY LENGTH= 563 BYTES                                 
