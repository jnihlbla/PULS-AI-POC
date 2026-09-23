000010*** EDIT ALLOWED                                                          
000100  01  W475W557C0.                                                         
000200*                                                                         
000300*                                                                         
000400*           TULLAPPENDIX-C2  TABELL                                       
000500*                            TABEL GRÄNSER                                
000600*                            LEDTEXTER                                    
000700     SKIP3                                                                
000800*                                                                         
000900   03  TULLC2-APPENDIX-LEDTEXTER.                                         
001000*                                                                         
001100    04  TULLC2-KVKOLLI-LEDTEXT.                                           
001200       05  FILLER  PIC X(21) VALUE 'TOTAAL AANTAL KOLLI: '.               
001300*                                                                         
001400    04  TULLC2-VKORDBTO-LEDTEXT.                                          
001500       05  FILLER  PIC X(18) VALUE 'TOT.BRUTO GEWICHT '.                  
001600*                                                                         
001700    04  TULLC2-VIKT-LEDTEXT.                                              
001800       05  FILLER  PIC X(04) VALUE ' KG.'.                                
001900*                                                                         
002000    04  TULLC2-VLORDBTO-LEDTEXT.                                          
002100       05  FILLER  PIC X(17) VALUE 'TOT.BRUTO VOLUME '.                   
002200*                                                                         
002300    04  TULLC2-VOL-LEDTEXT.                                               
002400       05  FILLER  PIC X(04) VALUE ' M3.'.                                
002500*                                                                         
002600    04  TULLC2-EG-LEDTEXT.                                                
002700       05  FILLER  PIC X(55) VALUE 'APPENDIX FOR CUSTOMS:    PART         
002800-                                  'ONE   COMMUNITY GOODS    '.           
002900*                                                                         
003000    04  TULLC2-EJ-EG-LEDTEXT.                                             
003100       05  FILLER  PIC X(55) VALUE 'APPENDIX FOR CUSTOMS:    PART         
003200-                                  'THREE NON COMMUNITY GOODS'.           
003300*                                                                         
003400    04  TULLC2-VERZAMEL-LEDTEXT.                                          
003500       05  FILLER  PIC X(35) VALUE 'VERZAMELBLAD VOOR DOUANE DOKUM        
003600-                                  'ENTEN'.                               
003700*                                                                         
003800    04  TULLC2-IDSKEPPN-LEDTEXT.                                          
003900       05  FILLER  PIC X(08) VALUE 'ZENDNR: '.                            
004000*                                                                         
004100    04  TULLC2-SUMRAD-LEDTEXT.                                            
004200       05  FILLER  PIC X(07) VALUE 'TOTALS '.                             
004300*                                                                         
004400    04  TULLC2-VOOR-EG-LEDTEXT.                                           
004500       05  FILLER  PIC X(17) VALUE 'VOOR DOKUMENT 1: '.                   
004600*                                                                         
004700    04  TULLC2-VOOR-EJ-EG-LEDTEXT.                                        
004800       05  FILLER  PIC X(17) VALUE 'VOOR DOKUMENT 3: '.                   
004900*                                                                         
005000    04  TULLC2-TOTRAD-LEDTEXT.                                            
005100       05  FILLER  PIC X(08) VALUE 'TOTALEN:'.                            
005200*                                                                         
005300    04  TULLC2-RUBRIK-RAD-LEDTEXT.                                        
005400       05  FILLER  PIC X(56) VALUE 'STAT.NO.     ORG         QTY          
005500-                                  '      NET WGHT   VALUE NLG'.          
005600*                                                                         
005700    04  TULLC2-RUBRIK-TOT-LEDTEXT.                                        
005800       05  FILLER  PIC X(76) VALUE '                         TOT.N        
005900-          'ETTO KG.   STAT.WAARDE NLG   FAKTUURWAARDE NLG'.              
006000*                                                                         
006100     EJECT                                                                
006200*                                                                         
006300   03  FILLER.                                                            
006400*                                                                         
006500*    *** FÖR BEHANDLING AV TABELLEN                                       
006600*                                                                         
006800       05 TULLC2-MAX-ANTAL-POST PIC S9(9)  COMP VALUE +9999.              
006900       05 TULLC2-ANTAL-POST     PIC S9(9)  COMP VALUE ZERO.               
007000*                                                                         
007100*      *** FÖR SORTERING AV TABELLEN                                      
007200*                                                                         
007300       05 TULLC2-POST-LAENGD    PIC S9(9) COMP VALUE +25.                 
007400       05 TULLC2-IDPTYP-LAENGD  PIC S9(9) COMP VALUE +11.                 
007500*                                                                         
007600*                                                                         
007700   03  TULLC2-TABELL.                                                     
007800       05 TULLC2-TABELL-POST OCCURS  9999 TIMES.                          
007900         07 TULLC2-SORTFAELT.                                             
008000           09 TULLC2-IDPTYP    PIC  X(3).                                 
008100*                      *** POSTTYP                                        
008200           09 TULLC2-IDSTATNR  PIC S9(9)       COMP-3.                    
008300*                      *** STATISTIKNR                                    
008400           09 TULLC2-KDARTURS  PIC  X(2).                                 
008500*                      *** ARTIKELS URSPRUNGSLAND                         
008600*                                                                         
008700         07 TULLC2-KVLEVART    PIC S9(07)      COMP-3.                    
008800*                      *** LEVERERAT ANTAL                                
008900         07 TULLC2-VKFKTNTO    PIC S9(06)V9(3) COMP-3.                    
009000*                      *** SUMMA NETTOVIKT * LEVERERAT ANTAL              
009100         07 TULLC2-SUFKTBEC    PIC S9(09)      COMP-3.                    
009200*                      *** VÄRDE I BELGISK VALUTA                         
009300*** END COPY W475W557C0  LENGTH=      OLD LENGTH=                         
