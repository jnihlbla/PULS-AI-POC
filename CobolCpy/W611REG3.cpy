000100 01  REG3-W611REG3.                                                       
000200*                                 LÄNKAREA TILL W611REG -                 
000300*                                 REGISTRERING  AV FÖLJESEDEL             
000400     03 REG3-IDTRANS         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 REG3-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 REG3-IDLBBET         PIC X(12).                                   
001100*                                 LASTBÄRARBETECKNING                     
001200*                                 TRAILER NUMBER                          
001300     03 REG3-FLGODK          PIC X.                                       
001400     03 REG3-FLGODK-IDFS     PIC X.                                       
001500     03 REG3-FLGODK-IDARTNR  PIC X.                                       
001600     03 REG3-IDLEVNR         OCCURS 12 TIMES                              
001700                             PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002000     03 REG3-IDLEVNR-OK      OCCURS 12 TIMES                              
002100                             PIC X.                                       
002200     03 REG3-KDRT            OCCURS 12 TIMES                              
002300                             PIC S9(3)           COMP-3.                  
002400*                                 REDOVISNINGSTYP                         
002500*                                 TYPE OF ACCOUNTING                      
002600     03 REG3-KDRT-OK         OCCURS 12 TIMES                              
002700                             PIC X.                                       
002800     03 REG3-IDFS            OCCURS 12 TIMES                              
002900                             PIC X(8).                                    
003000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003100*                                 ADVICE NOTE NUMBER ODETTE               
003200     03 REG3-IDFS-OK         OCCURS 12 TIMES                              
003300                             PIC X.                                       
003400     03 REG3-TIAVIDAT        OCCURS 12 TIMES                              
003500                             PIC S9(7)           COMP-3.                  
003600*                                 AVISERINGSDATUM (YYMMDD)                
003700*                                 ADVICE NOTE DATE                        
003800     03 REG3-TIAVIDAT-OK     OCCURS 12 TIMES                              
003900                             PIC X.                                       
004000     03 REG3-IDARTNR         OCCURS 12 TIMES                              
004100                             PIC S9(9)           COMP-3.                  
004200*                                 ARTIKELNUMMER                           
004300*                                 PART NUMBER                             
004400     03 REG3-IDARTNR-OK      OCCURS 12 TIMES                              
004500                             PIC X.                                       
004600     03 REG3-IDMFSFEL        OCCURS 12 TIMES                              
004700                             PIC X(3).                                    
004800*                                 MFS FELMEDDELANDE NUMMER                
004900*                                 MFS ERROR MESSAGE NUMBER                
005000     03 REG3-KVAVIS          OCCURS 12 TIMES                              
005100                             PIC S9(7)           COMP-3.                  
005200*                                 AVISERAT ANTAL                          
005300*                                 QUANTITY NOTIFIED                       
005400*** END OF VILMAII-COPY LENGTH= 453 BYTES                                 
