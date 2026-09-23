000100 01  TIME-W411TIME.                                                       
000200*                                 -----------------------------           
000300*                                 PARAMETRAR TILL W411TIME FÖR            
000400*                                 BERÄKNING AV STARTTID ELLER             
000500*                                 STOPTID ELLER ARBETSTIMMAR.             
000600*                                 RESULTATET ERHÅLLES I DET               
000700*                                 FÄLT AV STARTDAT, STOPDAT               
000800*                                 ELLER TIARB SOM NOLLSTÄLLTS.            
000900*                                 ENDAST ETT AV DESSA FÄLT KAN            
001000*                                 NOLLSTÄLLAS.                            
001100*                                 EXEMPEL PÅ ANROP:                       
001200*                                 MOVE   1234 TO TIME-IDPRC               
001300*                                 MOVE      1 TO TIME-KDCLAGER            
001400*                                 MOVE    012 TO TIME-KDCALL              
001500*                                 MOVE   ZERO TO TIME-STARTDAT            
001600*                                 MOVE 900610 TO                          
001700*                                            TIME-STOP-TIAAMMDD           
001800*                                 MOVE 124500 TO                          
001900*                                            TIME-STOP-TIHHMMSS           
002000*                                 MOVE  35.45 TO TIME-TIARB               
002100*                                 CALL W411TIME USING                     
002200*                                 TIME-W411TIME XXKD-PCB                  
002300*                                 -----------------------------           
002400*                                 FÖLJANDE VÄRDEN PÅ                      
002500*                                 TIME-KDCALL ÄR TILLÅTNA:                
002600*                                 ENTALSSIFFRAN:                          
002700*                                 1 = ANGER PAC-TID                       
002800*                                 2 = ANGER ADM-TID                       
002900*                                 3 = ANGER LAST-TID                      
003000*                                                                         
003100*                                 TIOTALSSIFFRAN:                         
003200*                                 1 = ANROP FRÅN WOPS                     
003300*                                 2 = ANROP FRÅN ORDER ENTRY              
003400*                                                                         
003500*                                 HUNDRATALSSIFFRAN:                      
003600*                                 0 = ANGES ALLTID                        
003700*                                                                         
003800*                                 TILLÅTNA KOMBINATIONER AV               
003900*                                 TIME-KDCALL:                            
004000*                                 011                                     
004100*                                 012                                     
004200*                                 013                                     
004300*                                 022                                     
004400*                                 -----------------------------           
004500*                                 VID ANROP FRÅN WOPS GES                 
004600*                                 VÄRDEN FRÅN ARBETSTIDS-                 
004700*                                 TABELLEN(WLXXKD) OCH ANROP              
004800*                                 FRÅN ORDER ENTRY GER VÄRDEN             
004900*                                 FRÅN RARBKONV.                          
005000*                                 OM ANGIVEN IDPRC FRÅN WOPS EJ           
005100*                                 FINNS GES VÄRDEN FRÅN                   
005200*                                 IDPRC = 9999.                           
005300*                                 VID ANOP FRÅN ORDER ENTRY               
005400*                                 SÄTTS TIME-IDPRC TILL ZERO.             
005500*                                 TIME-TIARB GES I TIMMAR OCH             
005600*                                 MINUTER DVS 2.45 AVSER                  
005700*                                 2 TIMMAR OCH 45 MINUTER.                
005800*                                 OM SEGMENT SAKNAS I ARBETS-             
005900*                                 TIDSTABELLEN GES KDSVAR-FEL.            
006000*                                 ÖVRIGA TYPER AV FEL LEDER               
006100*                                 TILL ABEND.                             
006200*                                 -----------------------------           
006300     03 TIME-IDPRC.                                                       
006400*                                 PRODUKTIONSKANAL                        
006500*                                 PRODUCTION CHANNEL                      
006600        05 TIME-IDPRCBAS     PIC X(3).                                    
006700*                                 PRC-BAS                                 
006800*                                 PRC-BASIC                               
006900        05 TIME-IDPRCVAR     PIC X.                                       
007000*                                 PRC-VARIANT                             
007100*                                 PRC-VARIANT                             
007200     03 TIME-IDDC            PIC X(2).                                    
007300*                                 IDENTIFIERARE LAGER                     
007400*                                 WAREHOUSE IDENTIFIER                    
007500     03 TIME-KDCALL          PIC 9(3).                                    
007600*                                 ANROPSTYP                               
007700*                                 CALL TYPE                               
007800     03 TIME-STARTDAT.                                                    
007900*                                                                         
008000        05 TIME-START-TIAAMMDD                                            
008100                             PIC 9(6).                                    
008200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
008300*                                 YEAR - MONTH - DAY  (YYMMDD)            
008400        05 TIME-START-TIHHMMSS                                            
008500                             PIC 9(6).                                    
008600*                                 TIM - MIN - SEK   (HHMMSS)              
008700*                                 HOUR - MINUTE - SEC (HHMMSS)            
008800     03 TIME-STOPDAT.                                                     
008900*                                                                         
009000        05 TIME-STOP-TIAAMMDD                                             
009100                             PIC 9(6).                                    
009200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
009300*                                 YEAR - MONTH - DAY  (YYMMDD)            
009400        05 TIME-STOP-TIHHMMSS                                             
009500                             PIC 9(6).                                    
009600*                                 TIM - MIN - SEK   (HHMMSS)              
009700*                                 HOUR - MINUTE - SEC (HHMMSS)            
009800     03 TIME-TIARB           PIC 9(3)V9(2).                               
009900*                                 UTFÖRD ARBETSTID (TIMMAR)               
010000     03 TIME-KDSVAR          PIC X.                                       
010100      88 TIME-KDSVAR-OK      VALUE ' '.                                   
010200      88 TIME-KDSVAR-FEL     VALUE 'F'.                                   
010300*                                                       KDSVAR-88         
010400*                                 SVARSKOD FRÅN SUBPROGRAM                
010500*                                                       KDSVAR-88         
010600*                                 RETURN CODE FROM SUBPROGRAM             
010700*** END COPY W411TIME    LENGTH=39                                        
