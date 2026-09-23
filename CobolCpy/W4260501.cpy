000100 01  W4260501.                                                            
000200*                                 KVALITET FELFÖRDELN. KONTR.OMR          
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDKVAOMR             PIC X.                                       
001100*                                 KVALITET KONTROLLOMRÅDE                 
001200*                                 QUALITY CONTROL AREA                    
001300     03 IDKVATRG             PIC 9(2).                                    
001400*                                 KVALITET KONTROLLTORG                   
001500*                                 QUALITY CONTROL AREA                    
001600     03 BEKVAOMR             OCCURS 2 TIMES                               
001700                             INDEXED IX1                                  
001800                             PIC X(10).                                   
001900*                                 KVALITET KONTROLLOMRÅDESNAMN            
002000*                                 QUALITY NAME OF CONTROL AREA            
002100     03 TIAARP-FOM           PIC S9(5)           COMP-3.                  
002200*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
002300*                                 FOM 12 PER ÅR                           
002400*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002500*                                 FROM 12 PER YEAR                        
002600     03 TIAARP-TOM           PIC S9(5)           COMP-3.                  
002700*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
002800*                                 TOM 12 PER ÅR                           
002900*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
003000*                                 TO 12 PER YEAR                          
003100     03 IDKVAFEL             PIC 9(2).                                    
003200*                                 KVALITET FELKOD FÖR ARTIKEL             
003300*                                 ERROR CODE FOR PARTNUMBER               
003400     03 RAD                  OCCURS 2 TIMES                               
003500                             INDEXED IX1.                                 
003600*                                  TABELL-RADER                           
003700*                                                                         
003800        05 BEKVAFEL          PIC X(40).                                   
003900*                                 KVALITET FELKODSBETECKNING              
004000*                                 ERROR FOR ERROR CODE                    
004100        05 BEKVAFGR          PIC X(20).                                   
004200*                                 KVALITET ALLVARLIGHETSGRAD FELK         
004300*                                 OD                                      
004400*                                 QUALITY SERIOUS FOR ERROR CODE          
004500     03 KVKVAFEL             PIC S9(5)           COMP-3.                  
004600*                                 ANTAL FEL FÖR KVALITETSKONTROLL         
004700*                                 NUMBER OF ERROR FOR QUALITY CON         
004800*                                 TROL                                    
004900     03 KVKVAFPO             PIC S9(3)           COMP-3.                  
005000*                                 KVALITET POÄNG FÖR FELKOD               
005100*                                 QUALITY POINT FOR ERROR CODE            
005200     03 REKVAFEL             PIC S9(5)           COMP-3.                  
005300*                                 KVALITET FEL-FAKTOR AV FELAKTIG         
005400*                                 A ARTIKLAR                              
005500*                                 QUALITY ERROR FACTOR FOR DEFECT         
005600*                                  PARTS                                  
005700     03 REKVAFKA             PIC S9(5)V9(1)      COMP-3.                  
005800*                                 FEL-FAKT. AV KONTR. ARTIKLAR            
005900*                                 ERROR FACTOR FOR INSP. PARTS            
006000*** END OF VILMAII-COPY LENGTH= 168 BYTES                                 
