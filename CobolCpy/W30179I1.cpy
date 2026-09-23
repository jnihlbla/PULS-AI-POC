000100 01  MID-W30179I1.                                                        
000200*                                 MID-COPYTEXT TILL PGM W30179            
000300*                                 RENOVATOR ADMINISTRATION                
000400     03 MID-IDDISTR          PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 MID-IDARTNR          PIC 9(8).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 MID-DAAAPP-START     PIC 9(6).                                    
001100*                                 ≈R - PLANERINGSPERIOD (≈≈≈≈RP)          
001200*                                 YEAR - PLANNING PERIOD (YYYYAP)         
001300     03 MID-DAAAPP-END       PIC 9(6).                                    
001400*                                 ≈R - PLANERINGSPERIOD (≈≈≈≈RP)          
001500*                                 YEAR - PLANNING PERIOD (YYYYAP)         
001600     03 MID-IDDIARAD         PIC 9(6).                                    
001700*                                 F÷RSTA RAD ATT VISA                     
001800*                                 FIRST LINE TO SHOW                      
001900     03 MID-KVDIARAD-MAX     PIC 9(6).                                    
002000*                                 ANTAL RADER ATT VISA                    
002100*                                 NUMBER OF LINES TO BE SHOWN             
002200     03 MID-INPUT.                                                        
002300        05 MID-PERIOD.                                                    
002400           07 MID-PERIOD-INFO                                             
002500                             OCCURS 250 TIMES.                            
002600              09 MID-DAAAPP  PIC 9(6).                                    
002700*                                 ≈R - PLANERINGSPERIOD (≈≈≈≈RP)          
002800*                                 YEAR - PLANNING PERIOD (YYYYAP)         
002900           07 MID-PERIOD-INPUT.                                           
003000              09 MID-PERIOD-LINE                                          
003100                             OCCURS 250 TIMES.                            
003200                 11 MID-IDPTYP                                            
003300                             PIC X.                                       
003400*                                 POSTTYP              IDPTYP-001         
003500*                                 RECORD TYPE          IDPTYP-001         
003600                 11 MID-KVANTAL-SKROT-UPD                                 
003700                             PIC X(7).                                    
003800*                                 ANTAL                                   
003900*                                 NUMBER                                  
004000                 11 MID-TESKROT-UPD                                       
004100                             PIC X(20).                                   
004200*                                 SKROTNINGS ORSAK I NES                  
004300*                                 REASON FOR SCRAP IN NES                 
004400                 11 MID-KVANTAL-INVEST-UPD                                
004500                             PIC X(7).                                    
004600*                                 ANTAL                                   
004700*                                 NUMBER                                  
004800                 11 MID-KDINVEST-UPD                                      
004900                             PIC X.                                       
005000*                                 TYP AV INVESTERING I NES                
005100*                                 (N=NEW OR C=CORE)                       
005200*                                 TYPE OF INVESTMENT IN NES               
005300*                                 (N=NEW OR C=CORE)                       
005400                 11 MID-KVANTAL-INVENT-UPD                                
005500                             PIC X(7).                                    
005600*                                 ANTAL                                   
005700*                                 NUMBER                                  
005800                 11 MID-TEINVENT-UPD                                      
005900                             PIC X(20).                                   
006000*                                 INVENTERINGS ORSAK I NES                
006100*                                 REASON FOR ADJUSTMENT IN NES            
006200*** END OF VILMAII-COPY LENGTH= 17286 BYTES                               
