000100 01  REG6-W611REG6.                                                       
000200*                                 LÄNKAREA TILL W611REG -                 
000300*                                 REGISTRERING  AV FÖLJESEDEL             
000400     03 REG6-IDTRANS         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 REG6-KVRADER-MAX     PIC S9(5)           COMP-3.                  
000800*                                 MAX INDEX KOPPLAT TILL OCCURS N         
000900*                                 EDAN.                                   
001000     03 REG6-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 REG6-IDLEVNR         PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001600     03 REG6-IDLEVNR-OK      PIC X.                                       
001700     03 REG6-KDRT            PIC S9(3)           COMP-3.                  
001800*                                 REDOVISNINGSTYP                         
001900*                                 TYPE OF ACCOUNTING                      
002000     03 REG6-KDRT-OK         PIC X.                                       
002100     03 REG6-IDFS            PIC X(8).                                    
002200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002300*                                 ADVICE NOTE NUMBER ODETTE               
002400     03 REG6-IDFS-OK         PIC X.                                       
002500     03 REG6-TIAVIDAT        PIC S9(7)           COMP-3.                  
002600*                                 AVISERINGSDATUM (YYMMDD)                
002700*                                 ADVICE NOTE DATE                        
002800     03 REG6-TIAVIDAT-OK     PIC X.                                       
002900     03 REG6-IDLBBET         PIC X(12).                                   
003000*                                 LASTBÄRARBETECKNING                     
003100*                                 TRAILER NUMBER                          
003200     03 REG6-FLGODK          PIC X.                                       
003300     03 REG6-FLGODK-IDARTNR  PIC X.                                       
003400     03 REG6-FLGODK-IDFS     PIC X.                                       
003500     03 REG6-IDFTG           PIC 9(2).                                    
003600      88 REG6-FTG-US         VALUE 53.                                    
003700      88 REG6-FTG-CA         VALUE 54.                                    
003800      88 REG6-FTG-PV         VALUE 57.                                    
003900      88 REG6-FTG-CN         VALUE 60.                                    
004000      88 REG6-FTG-IN         VALUE 61.                                    
004100*                                 FÖRETAGSID EKONOM REDOVISNING           
004200*                                 COMPANY IDENTITY ACCOUNTING             
004300     03 REG6-IDFTG-OK        PIC X.                                       
004400     03 REG6-IDKONTO         PIC S9(11)          COMP-3.                  
004500*                                 KONTO                                   
004600*                                 ACCOUNT                                 
004700     03 REG6-IDKONTO-OK      PIC X.                                       
004800     03 REG6-IDANALYS        PIC X(12).                                   
004900*                                 ANALYSNUMMER                            
005000*                                 ANALYSIS NUMBER                         
005100     03 REG6-IDANALYS-OK     PIC X.                                       
005200     03 REG6-IDKST           PIC X(10).                                   
005300*                                 KOSTNADSSTÄLLE                          
005400*                                 COST CENTRE                             
005500     03 REG6-IDKST-OK        PIC X.                                       
005600     03 REG6-IDARTNR         OCCURS 500 TIMES                             
005700                             PIC S9(9)           COMP-3.                  
005800*                                 ARTIKELNUMMER                           
005900*                                 PART NUMBER                             
006000     03 REG6-IDARTNR-OK      OCCURS 500 TIMES                             
006100                             PIC X.                                       
006200     03 REG6-IDMFSFEL        OCCURS 500 TIMES                             
006300                             PIC X(3).                                    
006400*                                 MFS FELMEDDELANDE NUMMER                
006500*                                 MFS ERROR MESSAGE NUMBER                
006600     03 REG6-KVAVIS          OCCURS 500 TIMES                             
006700                             PIC S9(7)           COMP-3.                  
006800*                                 AVISERAT ANTAL                          
006900*                                 QUANTITY NOTIFIED                       
007000*** END OF VILMAII-COPY LENGTH= 6581 BYTES                                
