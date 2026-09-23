000010*** EDIT ALLOWED                                                          
000100 01  W475W559.                                                            
000200*             ***  SPEDITIONSUNDERLAG                                     
000300*                                   - SUMMERINGS-TABEL                    
000400*                                   - TABEL GRÄNS                         
000500*                                   - LEDTEXTER                           
000600*                                                                         
000700*                                                                         
000800     SKIP2                                                                
000900   03  SPED-UNDERLAG-LEDTEXTER.                                           
001000*                                                                         
001100    04  SPED-UND-RUBRIK.                                                  
001200       05  FILLER  PIC X(50)  VALUE                                       
001300       'SPEDITIONSUNDERLAG                                '.              
001400       05  FILLER  PIC X(50)  VALUE                                       
001500       'TRANSPORT APPENDIX                                '.              
001600       05  FILLER  PIC X(50)  VALUE                                       
001700       'ANNEXE BORDEREAU D EXPEDITION                     '.              
001800       05  FILLER  PIC X(50)  VALUE                                       
001900       'TRANSPORT APPENDIX                                '.              
002000       05  FILLER  PIC X(50)  VALUE                                       
002100       'TRANSPORT APPENDIX                                '.              
002200       05  FILLER  PIC X(50)  VALUE                                       
002300       'TRANSPORT APPENDIX                                '.              
002400    04  FILLER  REDEFINES SPED-UND-RUBRIK.                                
002500       05  SPED-UND-BETECKNING PIC X(50) OCCURS 6.                        
002600*                                                                         
002700    04  BEROUTE-LEDTEXTER.                                                
002800       05  FILLER  PIC X(14) VALUE 'ROUTE         '.                      
002900       05  FILLER  PIC X(14) VALUE 'DESTINATION   '.                      
003000       05  FILLER  PIC X(14) VALUE 'DESTINATION   '.                      
003100       05  FILLER  PIC X(14) VALUE 'DESTINACION   '.                      
003200       05  FILLER  PIC X(14) VALUE 'DESTINATION   '.                      
003300       05  FILLER  PIC X(14) VALUE 'DESTINATION   '.                      
003400    04  FILLER  REDEFINES  BEROUTE-LEDTEXTER.                             
003500       05  BEROUTE-LEDTEXT     PIC X(14) OCCURS 6 TIMES.                  
003600*                                                                         
003700    04  SPED-BETOTAL-LEDTEXTER.                                           
003800       05  FILLER  PIC X(25) VALUE 'SUMMA BILDELAR  8708.999 '.           
003900       05  FILLER  PIC X(25) VALUE 'TOTAL           8708.999 '.           
004000       05  FILLER  PIC X(25) VALUE 'TOTAL           8708.999 '.           
004100       05  FILLER  PIC X(25) VALUE 'TOTAL           8708.999 '.           
004200       05  FILLER  PIC X(25) VALUE 'TOTAL           8708.999 '.           
004300       05  FILLER  PIC X(25) VALUE 'TOTAL           8708.999 '.           
004400    04  FILLER  REDEFINES  SPED-BETOTAL-LEDTEXTER.                        
004500       05  SPED-BETOTAL-LEDTEXT     PIC X(25) OCCURS 6 TIMES.             
004600*                                                                         
004700    04  TISKEPPN-LEDTEXTER.                                               
004800       05  FILLER  PIC X(12) VALUE 'SKEPPN.DATUM'.                        
004900       05  FILLER  PIC X(12) VALUE 'SHIPPING DTE'.                        
005000       05  FILLER  PIC X(12) VALUE 'DATE D ENVOI'.                        
005100       05  FILLER  PIC X(12) VALUE 'SHIPPING DTE'.                        
005200       05  FILLER  PIC X(12) VALUE 'SHIPPING DTE'.                        
005300       05  FILLER  PIC X(12) VALUE 'SHIPPING ATE'.                        
005400    04  FILLER  REDEFINES  TISKEPPN-LEDTEXTER.                            
005500       05  TISKEPPN-LEDTEXT     PIC X(12) OCCURS 6 TIMES.                 
005600*                                                                         
005700    04  IDTRANSP-NAMN-LEDTEXTER.                                          
005800       05  FILLER  PIC X(14) VALUE 'TRANSPORTNAMN '.                      
005900       05  FILLER  PIC X(14) VALUE 'TRANSPORTED BY'.                      
006000       05  FILLER  PIC X(14) VALUE 'NOM TRANSPORT '.                      
006100       05  FILLER  PIC X(14) VALUE 'TRANSPORTED BY'.                      
006200       05  FILLER  PIC X(14) VALUE 'TRANSPORTED BY'.                      
006300       05  FILLER  PIC X(14) VALUE 'TRANSPORTED BY'.                      
006400    04  FILLER  REDEFINES  IDTRANSP-NAMN-LEDTEXTER.                       
006500       05  IDTRANSP-NAMN-LEDTEXT     PIC X(14) OCCURS 6 TIMES.            
006600*                                                                         
006700    04  KVKOLLI-LEDTEXTER.                                                
006800       05  FILLER  PIC X(08) VALUE '   ANTAL'.                            
006900       05  FILLER  PIC X(08) VALUE 'QUANTITY'.                            
007000       05  FILLER  PIC X(08) VALUE 'QUANTITE'.                            
007100       05  FILLER  PIC X(08) VALUE 'CANTITAD'.                            
007200       05  FILLER  PIC X(08) VALUE '   MENGE'.                            
007300       05  FILLER  PIC X(08) VALUE 'QUANTITY'.                            
007400    04  FILLER  REDEFINES  KVKOLLI-LEDTEXTER.                             
007500       05  KVKOLLI-LEDTEXT           PIC X(08) OCCURS 6 TIMES.            
007600*                                                                         
007700     EJECT                                                                
007800   03  KTAB-TABELL.                                                       
007900*                                                                         
008000     05  KTAB-MAX-ANTAL-POST PIC S9(4) COMP  VALUE +2500.                 
008100     05  KTAB-ANTAL-POST     PIC S9(4) COMP  VALUE ZERO.                  
008200*                                                                         
008300*                      *** KOLLIINTERVALLSTABELL                          
008400     05  KTAB-POST  OCCURS  2500 TIMES.                                   
008500*                                                                         
008600         07  KTAB-IDPRODNR             PIC S9(7)    COMP-3.               
008700*              *** LAGRETS ORDER - PRODUKTIONSNR                          
008800         07  KTAB-IDKOLLI-FROM        PIC S9(5)    COMP-3.                
008900*                      *** KOLLINUMMER FRÅN OCH MED                       
009000         07  KTAB-IDKOLLI-TOM         PIC S9(5)    COMP-3.                
009100*                      *** KOLLINUMMER TILL OCH MED                       
009200*                                                                         
009300     SKIP3                                                                
009400   03  EMBTYP-TABELL.                                                     
009500*                                                                         
009600     05  ETAB-MAX-ANTAL-POST   PIC S9(4) COMP  VALUE +12.                 
009700     05  ETAB-ANTAL-POST       PIC S9(4) COMP  VALUE ZERO.                
009800*                                                                         
009900*                      *** EMBALLAGETYPSTABELL                            
010000     05  ETAB-POST    OCCURS 12  TIMES.                                   
010100*                                                                         
010200         07  ETAB-KDEMBTYP            PIC S9(3)    COMP-3.                
010300*                      *** EMBALLAGETYPSKOD                               
010400         07  ETAB-KVKOLLI             PIC S9(5)    COMP-3.                
010500*                      *** ANTAL KOLLI                                    
010600*                                                                         
010700                                                                          
010800   03  SPED-ACK.                                                          
010900     05  SPED-VKORDNTO-BDEL      PIC S9(06)V9(3)     COMP-3.              
011000     05  SPED-SUORDV-BDEL        PIC S9(09)V9(2)     COMP-3.              
011100     05  SPED-VKORDNTO-KAROSS    PIC S9(06)V9(3)     COMP-3.              
011200     05  SPED-SUORDV-KAROSS      PIC S9(09)V9(2)     COMP-3.              
011300     05  SPED-VKORDNTO-MOTOR-B   PIC S9(06)V9(3)     COMP-3.              
011400     05  SPED-SUORDV-MOTOR-B     PIC S9(09)V9(2)     COMP-3.              
011500     05  SPED-VKORDNTO-MOTOR-D   PIC S9(06)V9(3)     COMP-3.              
011600     05  SPED-SUORDV-MOTOR-D     PIC S9(09)V9(2)     COMP-3.              
011700     05  SPED-VKORDNTO-VERKTYG   PIC S9(06)V9(3)     COMP-3.              
011800     05  SPED-SUORDV-VERKTYG     PIC S9(09)V9(2)     COMP-3.              
011900     05  SPED-VKORDNTO-FARG      PIC S9(06)V9(3)     COMP-3.              
012000     05  SPED-SUORDV-FARG        PIC S9(09)V9(2)     COMP-3.              
012100   03  FILLER  REDEFINES  SPED-ACK.                                       
012200     05  FILLER  OCCURS  6  TIMES.                                        
012300        07  SPED-VKORDNTO        PIC S9(06)V9(3)     COMP-3.              
012400        07  SPED-SUORDV          PIC S9(09)V9(2)     COMP-3.              
012500   03  SPED-TEXTER.                                                       
012600     05  FILLER     PIC X(20)     VALUE 'RESERVDELAR         '.           
012700     05  FILLER     PIC X(20)     VALUE 'KAROSSER            '.           
012800     05  FILLER     PIC X(20)     VALUE 'MOTORER B           '.           
012900     05  FILLER     PIC X(20)     VALUE 'MOTORER D           '.           
013000     05  FILLER     PIC X(20)     VALUE 'VERKTYG             '.           
013100     05  FILLER     PIC X(20)     VALUE 'FÄRG                '.           
013200   03  FILLER  REDEFINES  SPED-TEXTER.                                    
013300     05  SPED-TEXT  PIC X(20)     OCCURS  6  TIMES.                       
013400*** END COPY W475W559C0  LENGTH=9992  OLD LENGTH=9992                     
