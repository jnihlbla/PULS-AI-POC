000100 01  REG4-W611REG4.                                                       
000200*                                 LÄNKAREA TILL W611REG -                 
000300*                                 REGISTRERING  AV FÖLJESEDEL             
000400*                                                                         
000500*                                 KDBEH  1 = TILLÄGG AV ART               
000600*                                 KDBEH  2 = ÄNDRING AV ART               
000700*                                            KDRT                         
000800*                                 KDBEH  3 = ÄNDRING AV ART               
000900*                                            ARTNR/KVAVIS                 
001000     03 REG4-IDTRANS         PIC X(4).                                    
001100*                                 BILDNUMMER                              
001200*                                 SCREEN NUMBER                           
001300     03 REG4-KVRADER-MAX     PIC S9(5)           COMP-3.                  
001400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001500*                                 EDAN.                                   
001600     03 REG4-IDDC            PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 REG4-TIAVIDAT        PIC S9(7)           COMP-3.                  
002000*                                 AVISERINGSDATUM (YYMMDD)                
002100*                                 ADVICE NOTE DATE                        
002200     03 REG4-IDFS            PIC X(8).                                    
002300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002400*                                 ADVICE NOTE NUMBER ODETTE               
002500     03 REG4-IDLEVNR         PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002800     03 REG4-IDFTG           PIC 9(2).                                    
002900      88 REG4-FTG-US         VALUE 53.                                    
003000      88 REG4-FTG-CA         VALUE 54.                                    
003100      88 REG4-FTG-PV         VALUE 57.                                    
003200      88 REG4-FTG-CN         VALUE 60.                                    
003300      88 REG4-FTG-IN         VALUE 61.                                    
003400*                                 FÖRETAGSID EKONOM REDOVISNING           
003500*                                 COMPANY IDENTITY ACCOUNTING             
003600     03 REG4-IDFTG-OK        PIC X.                                       
003700     03 REG4-IDKONTO         PIC S9(11)          COMP-3.                  
003800*                                 KONTO                                   
003900*                                 ACCOUNT                                 
004000     03 REG4-IDKONTO-OK      PIC X.                                       
004100     03 REG4-IDANALYS        PIC X(12).                                   
004200*                                 ANALYSNUMMER                            
004300*                                 ANALYSIS NUMBER                         
004400     03 REG4-IDANALYS-OK     PIC X.                                       
004500     03 REG4-IDKST           PIC X(10).                                   
004600*                                 KOSTNADSSTÄLLE                          
004700*                                 COST CENTRE                             
004800     03 REG4-IDKST-OK        PIC X.                                       
004900     03 REG4-KDBEH           OCCURS 500 TIMES                             
005000                             PIC S9              COMP-3.                  
005100*                                 BEHANDLINGSKOD                          
005200*                                 TREATMENT STATUS CODE                   
005300     03 REG4-IDARTNR-NY      OCCURS 500 TIMES                             
005400                             PIC S9(9)           COMP-3.                  
005500*                                 ARTIKELNUMMER                           
005600*                                 PART NUMBER                             
005700     03 REG4-IDARTNR-GAMMAL  OCCURS 500 TIMES                             
005800                             PIC S9(9)           COMP-3.                  
005900*                                 ARTIKELNUMMER                           
006000*                                 PART NUMBER                             
006100     03 REG4-IDRADNR-INL-GAMMAL                                           
006200                             OCCURS 500 TIMES                             
006300                             PIC S9(5)           COMP-3.                  
006400*                                 RADNUMMER INLEVERANS                    
006500*                                 LINE NUMBER GOODS RECEIVING             
006600     03 REG4-IDARTNR-OK      OCCURS 500 TIMES                             
006700                             PIC X.                                       
006800     03 REG4-IDMFSFEL        OCCURS 500 TIMES                             
006900                             PIC X(3).                                    
007000*                                 MFS FELMEDDELANDE NUMMER                
007100*                                 MFS ERROR MESSAGE NUMBER                
007200     03 REG4-KVAVIS          OCCURS 500 TIMES                             
007300                             PIC S9(7)           COMP-3.                  
007400*                                 AVISERAT ANTAL                          
007500*                                 QUANTITY NOTIFIED                       
007600     03 REG4-KDRT            OCCURS 500 TIMES                             
007700                             PIC S9(3)           COMP-3.                  
007800*                                 REDOVISNINGSTYP                         
007900*                                 TYPE OF ACCOUNTING                      
008000     03 REG4-KDRT-OK         OCCURS 500 TIMES                             
008100                             PIC X.                                       
008200*** END OF VILMAII-COPY LENGTH= 12560 BYTES                               
