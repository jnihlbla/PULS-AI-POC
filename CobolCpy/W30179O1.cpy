000100 01  MOD-W30179O1.                                                        
000200*                                 MOD-COPYTEXT FÖR PGM W30179             
000300*                                 RENOVATOR CONFIRMATION                  
000400     03 MOD-IDDISTR          PIC Z(3)9.                                   
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 MOD-IDDIARAD-START   PIC 9(6).                                    
000800*                                 FÖRSTA RAD ATT VISA                     
000900*                                 FIRST LINE TO SHOW                      
001000     03 MOD-KVDIARAD-AKTUELLT                                             
001100                             PIC 9(6).                                    
001200*                                 ANTAL RADER ATT VISA                    
001300*                                 NUMBER OF LINES TO BE SHOWN             
001400     03 MOD-KDFEL-KEYS       PIC X(3).                                    
001500*                                 FELKOD                                  
001600     03 MOD-FLFEL            PIC X.                                       
001700*                                 ALLMÄN FELFLAGGA                        
001800*                                 GENERAL ERROR FLAG                      
001900     03 MOD-STOCK-BALANCE-LINE.                                           
002000*                                 GRUPP MED STOCK-BALANCE-RAD             
002100        05 MOD-KVLS-REM      PIC -(6)9.                                   
002200*                                 LAGERSALDO                              
002300*                                 STOCK BALANCE                           
002400        05 MOD-KVLS-91       PIC -(6)9.                                   
002500*                                 LAGERSALDO                              
002600*                                 STOCK BALANCE                           
002700        05 MOD-BEART         PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900*                                 PART DESCRIPTION                        
003000     03 MOD-PERIOD.                                                       
003100*                                 GRUPP MED PERIODRADER                   
003200        05 MOD-PERIOD-LINE   OCCURS 250 TIMES.                            
003300*                                 GRUPP MED TABELLRADER                   
003400           07 MOD-IDPTYP     PIC X.                                       
003500*                                 POSTTYP              IDPTYP-001         
003600*                                 RECORD TYPE          IDPTYP-001         
003700           07 MOD-DAAAPP     PIC 9(6).                                    
003800*                                 ÅR - PLANERINGSPERIOD (ÅÅÅÅRP)          
003900*                                 YEAR - PLANNING PERIOD (YYYYAP)         
004000           07 MOD-TIREGDAT   PIC 9(6).                                    
004100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004200*                                 REGISTRATION DATE (YYMMDD)              
004300           07 MOD-KDFEL-SCRAP                                             
004400                             PIC X(3).                                    
004500*                                 FELKOD                                  
004600           07 MOD-SUSKROT2-REM                                            
004700                             PIC -(8)9.                                   
004800*                                 SUMMA SKROTAT ANTAL                     
004900*                                 AV 1 ARTIKEL                            
005000*                                 SUMMARY SCRAPPED OF AN ITEM             
005100           07 MOD-KVANTAL-SKROT-UPD                                       
005200                             PIC Z(7).                                    
005300*                                 ANTAL                                   
005400*                                 NUMBER                                  
005500           07 MOD-KDFEL-TESKROT-UPD                                       
005600                             PIC X(3).                                    
005700*                                 FELKOD                                  
005800           07 MOD-TESKROT-UPD                                             
005900                             PIC X(20).                                   
006000*                                 SKROTNINGS ORSAK I NES                  
006100*                                 REASON FOR SCRAP IN NES                 
006200           07 MOD-KDFEL-INVEST                                            
006300                             PIC X(3).                                    
006400*                                 FELKOD                                  
006500           07 MOD-SUINVEST-REM                                            
006600                             PIC -(8)9.                                   
006700*                                 SUMMA INVESTERAT ANTAL                  
006800*                                 AV 1 ARTIKEL                            
006900*                                 SUMMARY INVESTED OF AN ITEM             
007000           07 MOD-KVANTAL-INVEST-UPD                                      
007100                             PIC Z(7).                                    
007200*                                 ANTAL                                   
007300*                                 NUMBER                                  
007400           07 MOD-KDFEL-KDINVEST-UPD                                      
007500                             PIC X(3).                                    
007600*                                 FELKOD                                  
007700           07 MOD-KDINVEST-UPD                                            
007800                             PIC X.                                       
007900*                                 TYP AV INVESTERING I NES                
008000*                                 (N=NEW OR C=CORE)                       
008100*                                 TYPE OF INVESTMENT IN NES               
008200*                                 (N=NEW OR C=CORE)                       
008300           07 MOD-KDFEL-INVENT                                            
008400                             PIC X(3).                                    
008500*                                 FELKOD                                  
008600           07 MOD-SUINVENT-REM                                            
008700                             PIC -(8)9.                                   
008800*                                 SUMMA INVENTERAT ANTAL AV EN AR         
008900*                                 TIKEL                                   
009000*                                 TOTAL STOCK-TAKING QUANTITY OF          
009100*                                 AN ARTICHLE                             
009200           07 MOD-KVANTAL-INVENT-UPD                                      
009300                             PIC Z(7).                                    
009400*                                 ANTAL                                   
009500*                                 NUMBER                                  
009600           07 MOD-KDFEL-TEINVENT-UPD                                      
009700                             PIC X(3).                                    
009800*                                 FELKOD                                  
009900           07 MOD-TEINVENT-UPD                                            
010000                             PIC X(20).                                   
010100*                                 INVENTERINGS ORSAK I NES                
010200*                                 REASON FOR ADJUSTMENT IN NES            
010300     03 MOD-KVRAD-TOT        PIC X(6).                                    
010400*                                 ANTAL ORDERRADER                        
010500*                                 NUMBER OF ITEMS                         
010600*** END OF VILMAII-COPY LENGTH= 30065 BYTES                               
