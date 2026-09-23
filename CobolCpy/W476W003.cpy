000010*** EDIT ALLOWED                                                          
000100*             ***  KUNDBILAGOR                                            
000200*                                   - SUMMERINGS-TABELER                  
000300*                                   - TABEL GRÄNSER                       
000400*                                   - LEDTEXTER                           
000500*                                                                         
000600*                                                                         
000700     SKIP2                                                                
000800 01    KUNDBILAGA-LEDTEXTER.                                              
000900    03  KUNDBIL-RUBRIK.                                                   
001000       05  FILLER  PIC X(50) VALUE                                        
001100          'KUND BILAGA                                       '.           
001200       05  FILLER  PIC X(50) VALUE                                        
001300          'CUSTOMER APPENDIX      (ATTACHMENT TO INVOICE)    '.           
001400       05  FILLER  PIC X(50) VALUE                                        
001500          'ANNEXE STATISTIQUE CLIENT                         '.           
001600       05  FILLER  PIC X(50) VALUE                                        
001700          'CUSTOMER APPENDIX      (ATTACHMENT TO INVOICE)    '.           
001800       05  FILLER  PIC X(50) VALUE                                        
001900          'CUSTOMER APPENDIX      (ATTACHMENT TO INVOICE)    '.           
002000       05  FILLER  PIC X(50) VALUE                                        
002100          'CUSTOMER APPENDIX      (ATTACHMENT TO INVOICE)    '.           
002200    03  FILLER  REDEFINES  KUNDBIL-RUBRIK.                                
002300       05  KUNDBILAGA-BETECKNING PIC X(50) OCCURS 6 TIMES.                
002400*                                                                         
002500    03  KBIL-KVANT-LEDTEXTER.                                             
002600       05  FILLER  PIC X(09) VALUE 'KVANTITET'.                           
002700       05  FILLER  PIC X(09) VALUE ' QUANTITY'.                           
002800       05  FILLER  PIC X(09) VALUE ' QUANTITE'.                           
002900       05  FILLER  PIC X(09) VALUE ' CANTITAD'.                           
003000       05  FILLER  PIC X(09) VALUE '    MENGE'.                           
003100       05  FILLER  PIC X(09) VALUE ' QUANTITY'.                           
003200    03  FILLER  REDEFINES  KBIL-KVANT-LEDTEXTER.                          
003300       05  KBIL-KVANT-LEDTEXT PIC X(09) OCCURS 6 TIMES.                   
003400*                                                                         
003500    03  KBIL-EEC-LEDTEXTER.                                               
003600       05  FILLER  PIC X(14) VALUE '     EEC TOTAL'.                      
003700       05  FILLER  PIC X(14) VALUE '     EG  TOTAL'.                      
003800       05  FILLER  PIC X(14) VALUE '     TOTAL EEC'.                      
003900       05  FILLER  PIC X(14) VALUE '     TOTAL EEC'.                      
004000       05  FILLER  PIC X(14) VALUE '    EEC GESAMT'.                      
004100       05  FILLER  PIC X(14) VALUE '    EEC TOTAAL'.                      
004200    03  FILLER  REDEFINES  KBIL-EEC-LEDTEXTER.                            
004300       05  KBIL-EEC-LEDTEXT PIC X(14) OCCURS 6 TIMES.                     
004400*                                                                         
004500    03  KBIL-EFTA-LEDTEXTER.                                              
004600       05  FILLER  PIC X(14) VALUE '    EFTA TOTAL'.                      
004700       05  FILLER  PIC X(14) VALUE '    EFTA TOTAL'.                      
004800       05  FILLER  PIC X(14) VALUE '    TOTAL EFTA'.                      
004900       05  FILLER  PIC X(14) VALUE '    TOTAL EFTA'.                      
005000       05  FILLER  PIC X(14) VALUE '   EFTA GESAMT'.                      
005100       05  FILLER  PIC X(14) VALUE '   EFTA TOTAAL'.                      
005200    03  FILLER  REDEFINES  KBIL-EFTA-LEDTEXTER.                           
005300       05  KBIL-EFTA-LEDTEXT PIC X(14) OCCURS 6 TIMES.                    
005400*                                                                         
005500    03  KBIL-OEVR-LEDTEXTER.                                              
005600       05  FILLER  PIC X(14) VALUE '    NON-ORIGIN'.                      
005700       05  FILLER  PIC X(14) VALUE '    NON-ORIGIN'.                      
005800       05  FILLER  PIC X(14) VALUE '  TOTAL DIVERS'.                      
005900       05  FILLER  PIC X(14) VALUE '    NON-ORIGIN'.                      
006000       05  FILLER  PIC X(14) VALUE '    NON-ORIGIN'.                      
006100       05  FILLER  PIC X(14) VALUE '    NON-ORIGIN'.                      
006200    03  FILLER  REDEFINES  KBIL-OEVR-LEDTEXTER.                           
006300       05  KBIL-OEVR-LEDTEXT PIC X(14) OCCURS 6 TIMES.                    
006400*                                                                         
006500    03  KBIL-TOTAL-LEDTEXTER.                                             
006600       05  FILLER  PIC X(14) VALUE '         TOTAL'.                      
006700       05  FILLER  PIC X(14) VALUE '         TOTAL'.                      
006800       05  FILLER  PIC X(14) VALUE '         TOTAL'.                      
006900       05  FILLER  PIC X(14) VALUE '         TOTAL'.                      
007000       05  FILLER  PIC X(14) VALUE '         TOTAL'.                      
007100       05  FILLER  PIC X(14) VALUE '         TOTAL'.                      
007200    03  FILLER  REDEFINES  KBIL-TOTAL-LEDTEXTER.                          
007300       05  KBIL-TOTAL-LEDTEXT PIC X(14) OCCURS 6 TIMES.                   
007400*                                                                         
009600*    *** STATNUMMER-URSPRUNGSTABELL                                       
009700*                                                                         
011400 01    ST-URS-TABEL.                                                      
011500*                                                                         
011600*                                                                         
011700*    *** FÖR TOTALER OCH SUBTOTALER SUMMERINGSTABELLER                    
011800*                                                                         
011900*                                                                         
012000   03    ST-URS-SUBTOT.                                                   
012100       05    ST-URS-SUBTOT-KVART        PIC S9(7)        COMP-3.          
012200       05    ST-URS-SUBTOT-VKORDNTO     PIC S9(6)V9(3)   COMP-3.          
012210       05    ST-URS-SUBTOT-VKORDBTO     PIC S9(6)V9(3)   COMP-3.          
012300       05    ST-URS-SUBTOT-SUORDV-EEC   PIC S9(9)V9(2)   COMP-3.          
012400       05    ST-URS-SUBTOT-SUORDV-EFTA  PIC S9(9)V9(2)   COMP-3.          
012500       05    ST-URS-SUBTOT-SUORDV-TOT   PIC S9(9)V9(2)   COMP-3.          
012600   03    ST-URS-TOT.                                                      
012700       05    ST-URS-TOT-KVKOLLI         PIC S9(7)        COMP-3.          
012710       05    ST-URS-TOT-KVART           PIC S9(7)        COMP-3.          
012800       05    ST-URS-TOT-VKORDNTO        PIC S9(6)V9(3)   COMP-3.          
012810       05    ST-URS-TOT-VKORDBTO        PIC S9(6)V9(3)   COMP-3.          
012900       05    ST-URS-TOT-SUORDV-EEC      PIC S9(9)V9(2)   COMP-3.          
013000       05    ST-URS-TOT-SUORDV-EFTA     PIC S9(9)V9(2)   COMP-3.          
013100       05    ST-URS-TOT-SUORDV-TOT      PIC S9(9)V9(2)   COMP-3.          
013200*                                                                         
013300*    *** FÖR BEHANDLING AV SUMMERING/EDITERING TABELLER                   
013400*                                                                         
013500   03    ST-URS-MAX-ANTAL-STATNR     PIC S9(4) COMP VALUE +300.           
013600   03    ST-URS-ANTAL-STATNR         PIC S9(4) COMP VALUE ZERO.           
013700   03    ST-URS-MAX-ANTAL-ARTURS     PIC S9(4) COMP VALUE +50.            
013800   03    ST-URS-ANTAL-ARTURS         PIC S9(4) COMP VALUE ZERO.           
013900*                                                                         
014000*    *** FÖR SORTERING  AV IX-IDSTATNR TABELLEN                           
014100*                                                                         
014200   03    ST-URS-IDSTATNR-IX-LGD      PIC S9(3) COMP-3 VALUE +7.           
014300   03    ST-URS-IDSTATNR-LAENGD      PIC S9(3) COMP-3 VALUE +5.           
014400*                                                                         
014500*                                                                         
014600*  DEN HÄR TABELLEN FÖR ATT FÅ ORDNING PÅ KUNDBILAGA                      
014700*  PER URSPRUNGSKOD/STAT-NR                                               
014800*                                                                         
014900   03   ST-URS-AKTUELL-KDARTURS PIC X(2)          OCCURS  50.             
015000*                                                                         
015100*                                                                         
015200*  DEN HÄR TABELLEN BARA FÖR ATT 'INTSOR' KLARAR INTE ATT SORTERA         
015300*  TABELLER MED POSTER STÖRRE ÄN 256 BYTES.                               
015400*                                                                         
015500   03   ST-URS-IDSTATNR-IX-TABEL OCCURS 300.                              
015600     05   ST-URS-IDSTATNR         PIC S9(9) COMP-3.                       
015700*                          ***    STATISTIK NUMMER                        
015800     05   ST-URS-IDSTATNR-PNR     PIC S9(3) COMP-3.                       
015900*                          ***    POSITION I NÄSTA TABELL                 
016000*                                                                         
016100*    *** UPPDELAT P. G. A. TABELL STÖRRE ÄN 131K (140K).                  
016200*                                                                         
016300*    *** STATNUMMER-URSPRUNGSTABELL DEL 1                                 
016400*                                                                         
016500 01  ST-URS-TABELL-DEL1.                                                  
016600   02  ST-URS-POST-1 OCCURS  300.                                         
016700     03  ST-URS-POST-2  OCCURS  50.                                       
016800*                                                                         
016900         05  ST-URS-KDARTURS    PIC X(2).                                 
017000*                                      ARTIKELURSPRUNG                    
017100         05  ST-URS-KVART              PIC S9(5)       COMP-3.            
017200*                                      LEVERERAT ANTAL                    
017300*                                                                         
017400*                                                                         
017500*    *** STATNUMMER-URSPRUNGSTABELL DEL 2                                 
017600*                                                                         
017700 01  ST-URS-TABELL-DEL2.                                                  
017800   02  ST-URS-POST-3 OCCURS  300.                                         
017900     03  ST-URS-POST-4  OCCURS  50.                                       
018000*                                                                         
018100         05  ST-URS-VKORDNTO           PIC S9(06)V9(3) COMP-3.            
018200*                                      NETTOVIKT                          
018210         05  ST-URS-VKORDBTO           PIC S9(06)V9(3) COMP-3.            
018220*                                      BRUTTOVIKT                         
018300         05  ST-URS-SUORDV-TOT         PIC S9(9)V9(2)  COMP-3.            
018400*                                      VARUVÄRDE                          
018500*                                                                         
018600*                                                                         
018700*    *** STATNUMMER-URSPRUNGSTABELL DEL 3                                 
018800*                                                                         
018900 01  ST-URS-TABELL-DEL3.                                                  
019000   02  ST-URS-POST-5 OCCURS  300.                                         
019100     03  ST-URS-POST-6  OCCURS  50.                                       
019200*                                                                         
019300         05  ST-URS-SUORDV-EEC         PIC S9(9)V9(2)  COMP-3.            
019400*                                      VARUVÄRDE                          
019500         05  ST-URS-SUORDV-EFTA        PIC S9(9)V9(2)  COMP-3.            
019600*                                      VARUVÄRDE                          
019700*** END COPY W475W558C0  LENGTH=      OLD LENGTH=                         
