000100 01  W213L121.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W21312 MOT                              
000400*                                 LEVERANTÖR/ARTIKELREG.                  
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAES-WLOGAR01       VALUE +101.                                  
000800      88 ISRT-WLOGAR01       VALUE +102.                                  
000900      88 DLET-WLOGAR01       VALUE +103.                                  
001000      88 REPL-WLOGAR01       VALUE +104.                                  
001100      88 LAES-ARTIKELINFO    VALUE +105.                                  
001200      88 REPL-ARTIKELINFO    VALUE +106.                                  
001300      88 FINNS-C2            VALUE +107.                                  
001400      88 ISRT-2213           VALUE +108.                                  
001500      88 LAES-ARTIKELINFO-R17                                             
001600                             VALUE +109.                                  
001700      88 REPL-ARTNFO-R17-MFG VALUE +110.                                  
001800      88 REPL-ARTINFO-R17-SHP                                             
001900                             VALUE +111.                                  
002000      88 REPL-WLOGAR01-R02   VALUE +204.                                  
002100      88 REPL-ARTIKELINFO-R02                                             
002200                             VALUE +206.                                  
002300*                                 ANROPSTYP FÖR SYSTEM R2XX               
002400     03 FLJANEJ-ANROP        PIC X.                                       
002500      88 ANROP-OK            VALUE 'J'.                                   
002600      88 ANROP-FEL           VALUE 'N'.                                   
002700*                                 JA/NEJ-FLAGGA FÖR R2XX                  
002800     03 IDLEVNR              PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000     03 IOAREA-LEVERANTOR.                                                
003100*                                                                         
003200        05 KDLEVTYP          PIC S9              COMP-3.                  
003300*                                 LEVERANTÖRTYP                           
003400        05 FLEMBPOL          PIC X.                                       
003500*                                 MEDLEM EMBALLAGEPOOL                    
003600        05 KDSPRAK           PIC S9              COMP-3.                  
003700         88 KDSPRAK-SVENSKA  VALUE +0.                                    
003800         88 KDSPRAK-ENGELSKA VALUE +1.                                    
003900         88 KDSPRAK-FRANSKA  VALUE +2.                                    
004000         88 KDSPRAK-SPANSKA  VALUE +3.                                    
004100         88 KDSPRAK-TYSKA    VALUE +4.                                    
004200         88 KDSPRAK-ITALIENSKA                                            
004300                             VALUE +5.                                    
004400*                                 SPRÅKKOD                                
004500        05 FLRSADR           PIC X.                                       
004600*                                 RS-UNIK LEV-ADRESS                      
004700        05 KDGK              PIC S9              COMP-3.                  
004800*                                 GODSMOTTAGAREKOD                        
004900        05 KVDAGAR-TTC1      PIC S9(3)           COMP-3.                  
005000*                                 DAGAR TULL- OCH TRANSPORT-TID           
005100*                                 C1                                      
005200        05 KVDAGAR-TTC2      PIC S9(3)           COMP-3.                  
005300*                                 DAGAR TULL- & TRANSPORT-TID  C2         
005400        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
005500*                                 ANTAL VECKOR LEDTID                     
005600        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
005700*                                 ANTAL VECKOR ANSKAFFNINGSTID            
005800        05 IDLPKOLL          PIC S9              COMP-3.                  
005900*                                 KONTROLLVECKA LEVERANSPLANER            
006000        05 PG-TABELL.                                                     
006100           07 IDANSK-PG      OCCURS 8 TIMES                               
006200                             PIC S9(3)           COMP-3.                  
006300*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
006400        05 LEVDAGTAB.                                                     
006500           07 TILEVDAG       OCCURS 5 TIMES                               
006600                             PIC S9              COMP-3.                  
006700*                                 AVSÄNDNINGSDAG INOM VECKA               
006800     03 IOAREA-ARTIKEL-FILLER REDEFINES IOAREA-LEVERANTOR.                
006900        05 IOAREA-ARTIKEL.                                                
007000*                                                                         
007100           07 IDARTNR        PIC S9(9)           COMP-3.                  
007200*                                 ARTIKELNUMMER                           
007300           07 KDGK-ARTNR     PIC S9              COMP-3.                  
007400*                                 GODSMOTTAGAREKOD                        
007500           07 KVDAGAR-TT-ARTNR                                            
007600                             PIC S9(3)           COMP-3.                  
007700*                                 ANTAL DAGAR                             
007800           07 KVVECKOR-LT-ARTNR                                           
007900                             PIC S9(3)           COMP-3.                  
008000*                                 ANTAL VECKOR                            
008100           07 KVVECKOR-AT-ARTNR                                           
008200                             PIC S9(3)           COMP-3.                  
008300*                                 ANTAL VECKOR                            
008400           07 FLMANLT-ARTNR  PIC X.                                       
008500*                                 MANUELLT SATT LEDTID ?                  
008600           07 FLMANGK-ARTNR  PIC X.                                       
008700*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
008800           07 FLMANAT-ARTNR  PIC X.                                       
008900*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
009000           07 KDAVT-ARTNR    PIC S9              COMP-3.                  
009100*                                 AVTALSMÄRKNING                          
009200           07 KDKSP-ARTNR    PIC S9              COMP-3.                  
009300*                                 KÖPSPÄRR                                
009400           07 IDPLANGR-AG-ARTNR                                           
009500                             PIC S9              COMP-3.                  
009600*                                 PLANERINGSGRUPP                         
009700           07 IDANSK-ARTNR   PIC S9(3)           COMP-3.                  
009800*                                 ANSKAFFARNUMMER                         
009900           07 KDLTK-ARTNR    PIC S9              COMP-3.                  
010000*                                 LAGERTILLHÖRIGHETSKOD                   
010100           07 KDERS-ARTNR    PIC S9(3)           COMP-3.                  
010200*                                 ERSÄTTNINGSKOD                          
010300           07 KDHF           PIC S9              COMP-3.                  
010400*                                 HUVUDFÖRRÅDSMÄRKNING                    
010500        05 FILLER            PIC X(11).                                   
010600*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
