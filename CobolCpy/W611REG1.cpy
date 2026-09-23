000100 01  REG1-W611REG1.                                                       
000200*                                 LÄNKAREA TILL W611REG -                 
000300*                                 REGISTRERING AV FÖLJESEDEL              
000400     03 REG1-IDTRANS         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 REG1-KVRADER-MAX     PIC S9(5)           COMP-3.                  
000800*                                 MAX INDEX KOPPLAT TILL OCCURS N         
000900*                                 EDAN.                                   
001000     03 REG1-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 REG1-IDLEVNR         PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001600     03 REG1-IDLEVNR-OK      PIC X.                                       
001700     03 REG1-KDRT            PIC S9(3)           COMP-3.                  
001800*                                 REDOVISNINGSTYP                         
001900*                                 TYPE OF ACCOUNTING                      
002000     03 REG1-KDRT-OK         PIC X.                                       
002100     03 REG1-IDFS            PIC X(8).                                    
002200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002300*                                 ADVICE NOTE NUMBER ODETTE               
002400     03 REG1-IDFS-OK         PIC X.                                       
002500     03 REG1-TIAVIDAT        PIC S9(7)           COMP-3.                  
002600*                                 AVISERINGSDATUM (YYMMDD)                
002700*                                 ADVICE NOTE DATE                        
002800     03 REG1-TIAVIDAT-OK     PIC X.                                       
002900     03 REG1-IDLBBET         PIC X(12).                                   
003000*                                 LASTBÄRARBETECKNING                     
003100*                                 TRAILER NUMBER                          
003200     03 REG1-FLGODK          PIC X.                                       
003300     03 REG1-FLGODK-IDFS     PIC X.                                       
003400     03 REG1-FLGODK-IDARTNR  PIC X.                                       
003500     03 REG1-IDARTNR         OCCURS 500 TIMES                             
003600                             PIC S9(9)           COMP-3.                  
003700*                                 ARTIKELNUMMER                           
003800*                                 PART NUMBER                             
003900     03 REG1-IDARTNR-OK      OCCURS 500 TIMES                             
004000                             PIC X.                                       
004100     03 REG1-IDMFSFEL        OCCURS 500 TIMES                             
004200                             PIC X(3).                                    
004300*                                 MFS FELMEDDELANDE NUMMER                
004400*                                 MFS ERROR MESSAGE NUMBER                
004500     03 REG1-KVAVIS          OCCURS 500 TIMES                             
004600                             PIC S9(7)           COMP-3.                  
004700*                                 AVISERAT ANTAL                          
004800*                                 QUANTITY NOTIFIED                       
004900*** END OF VILMAII-COPY LENGTH= 6547 BYTES                                
