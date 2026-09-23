000100     EJECT                                                                
000200*----------------------------------------------------------------*        
000300*    SQLCODER                                                             
000400*----------------------------------------------------------------*        
000500*----  TABELL FÖR SQLCODER.ÄNDRA ELLER LÄGG TILL EFTER BEHOV  ---*        
000600*----  INNEHÅLLER:                                            ---*        
000700*----  SQLCODER                                               ---*        
000800*----  SWITCHAR:     DISPTX: 'Y' = DISPLAYA DSNTIAR-FELTEXT   ---*        
000900*----                ZFINIT: 'Y' = ZFINIT + GOBACK             --*        
001000*----                ROLBCK: 'Y' = GÖR ROLLBACK               ---*        
001100*----------------------------------------------------------------*        
001200*----  MATCHANDE SECTIONS FÖR PROCEDURE DIVISION FINNS PÅ     ---*        
001300*----     F1PUVD.PROD.SOURCE(VDB2PS95C0)                      ---*        
001400*----------------------------------------------------------------*        
001500 01  GODK-SQLCODER-VALUES.                                                
001600                                                                          
001700     03  C000   PIC S9(9) COMP-4 VALUE +000.                              
001800*    'DB2 GAV RETURKOD 0 = DU FICK VAD DU VILLE.                '.        
001900     03  FILLER PIC X(01)        VALUE 'N'.                               
002000     03  FILLER PIC X(01)        VALUE 'N'.                               
002100     03  FILLER PIC X(01)        VALUE 'N'.                               
002200                                                                          
002300     03  C100   PIC S9(9) COMP-4 VALUE +100.                              
002400*    'INGA FLER RADER SOM UPPFYLLER SÖKVILLKORET                '.        
002500     03  FILLER PIC X(01)        VALUE 'N'.                               
002600     03  FILLER PIC X(01)        VALUE 'N'.                               
002700     03  FILLER PIC X(01)        VALUE 'N'.                               
002800                                                                          
002900     03  C-530  PIC S9(9) COMP-4 VALUE -530.                              
003000*    'OGILTIGT VÄRDE FÖR FOREIGN KEY VID TILLÄGG / ÄNDRING      '.        
003100     03  FILLER PIC X(01)        VALUE 'Y'.                               
003200     03  FILLER PIC X(01)        VALUE 'N'.                               
003300     03  FILLER PIC X(01)        VALUE 'N'.                               
003400                                                                          
003500     03  C-531  PIC S9(9) COMP-4 VALUE -531.                              
003600*    'EJ TILLÅTET ATT ÄNDRA VÄRDE FÖR PRIMARY KEY               '.        
003700     03  FILLER PIC X(01)        VALUE 'Y'.                               
003800     03  FILLER PIC X(01)        VALUE 'N'.                               
003900     03  FILLER PIC X(01)        VALUE 'N'.                               
004000                                                                          
004100     03  C-532  PIC S9(9) COMP-4 VALUE -532.                              
004200*    'BORTTAG EJ TILLÅTET P.G.A. BORTTAGSREGLER (RESTRICT)      '.        
004300     03  FILLER PIC X(01)        VALUE 'Y'.                               
004400     03  FILLER PIC X(01)        VALUE 'N'.                               
004500     03  FILLER PIC X(01)        VALUE 'N'.                               
004600                                                                          
004700     03  C-551  PIC S9(9) COMP-4 VALUE -551.                              
004800*    'DU SAKNAR NÅGON FORM AV AUKTORITET FÖR DETTA (GRANT)      '.        
004900     03  FILLER PIC X(01)        VALUE 'Y'.                               
005000     03  FILLER PIC X(01)        VALUE 'Y'.                               
005100     03  FILLER PIC X(01)        VALUE 'N'.                               
005200                                                                          
005300     03  C-552  PIC S9(9) COMP-4 VALUE -552.                              
005400*    'DU SAKNAR NÅGON FORM AV AUKTORITET FÖR DETTA (GRANT)      '.        
005500     03  FILLER PIC X(01)        VALUE 'Y'.                               
005600     03  FILLER PIC X(01)        VALUE 'Y'.                               
005700     03  FILLER PIC X(01)        VALUE 'N'.                               
005800                                                                          
005900     03  C-803  PIC S9(9) COMP-4 VALUE -803.                              
006000*    'INSERT AV RAD DÄR KOLUMN HAR INDEX OCH VÄRDET REDAN FINNS '.        
006100     03  FILLER PIC X(01)        VALUE 'Y'.                               
006200     03  FILLER PIC X(01)        VALUE 'N'.                               
006300     03  FILLER PIC X(01)        VALUE 'N'.                               
006400                                                                          
006500     03  C-818  PIC S9(9) COMP-4 VALUE -818.                              
006600*    'TIME STAMP I PGM HÖGRE ÄN TIME STAMP I PLAN, KÖR OM BIND  '.        
006700     03  FILLER PIC X(01)        VALUE 'Y'.                               
006800     03  FILLER PIC X(01)        VALUE 'Y'.                               
006900     03  FILLER PIC X(01)        VALUE 'N'.                               
007000                                                                          
007100     03  C-901  PIC S9(9) COMP-4 VALUE -901.                              
007200*    'INGA SQL-SATSER UTFÖRDA      SQLCODE -901                 '.        
007300     03  FILLER PIC X(01)        VALUE 'Y'.                               
007400     03  FILLER PIC X(01)        VALUE 'Y'.                               
007500     03  FILLER PIC X(01)        VALUE 'N'.                               
007600                                                                          
007700     03  C-902  PIC S9(9) COMP-4 VALUE -902.                              
007800*    'FANNS SQL-SATSER UTFÖRDA ÄR ROLLBACK UTFÖRD, SQLCODE -902 '.        
007900     03  FILLER PIC X(01)        VALUE 'Y'.                               
008000     03  FILLER PIC X(01)        VALUE 'Y'.                               
008100     03  FILLER PIC X(01)        VALUE 'Y'.                               
008200                                                                          
008300     03  C-904  PIC S9(9) COMP-4 VALUE -904.                              
008400*    'NÅGON ANNAN HÅLLER EN TABELL ELLER TABLESPACE             '.        
008500     03  FILLER PIC X(01)        VALUE 'Y'.                               
008600     03  FILLER PIC X(01)        VALUE 'N'.                               
008700     03  FILLER PIC X(01)        VALUE 'N'.                               
008800                                                                          
008900     03  C-911  PIC S9(9) COMP-4 VALUE -911.                              
009000*    'SQL-ANROP EJ UTFÖRDA PGA LÅSNING.  SQLCODE -911           '.        
009100     03  FILLER PIC X(01)        VALUE 'Y'.                               
009200     03  FILLER PIC X(01)        VALUE 'N'.                               
009300     03  FILLER PIC X(01)        VALUE 'N'.                               
009400                                                                          
009500     03  C-922  PIC S9(9) COMP-4 VALUE -922.                              
009600*    'DU SAKNAR AUKTORISATION FÖR DENNA FUNKTION (PLAN)         '.        
009700     03  FILLER PIC X(01)        VALUE 'Y'.                               
009800     03  FILLER PIC X(01)        VALUE 'Y'.                               
009900     03  FILLER PIC X(01)        VALUE 'N'.                               
010000                                                                          
010100     EJECT                                                                
010200 01  FILLER REDEFINES GODK-SQLCODER-VALUES.                               
010300     02  GODK-SQLCODER           OCCURS 14 TIMES                          
010400                                 INDEXED BY GODK-SQLCODE-INDEX.           
010500         03  GODK-SQLCODE          PIC S9(9) COMP-4.                      
010600*        --- DISPTX,ZFINIT,ROLBCK: Y OR N ---                             
010700         03  GODK-SQLCODE-DISPTX   PIC X(01).                             
010800         03  GODK-SQLCODE-ZFINIT   PIC X(01).                             
010900         03  GODK-SQLCODE-ROLBCK   PIC X(01).                             
011000 01  GODK-SQLCODER-ANT-RADER       PIC S9(4) COMP VALUE +14.              
011100     EJECT                                                                
011200*----------------------------------------------------------------*        
011300*     AREOR FÖR DSNTIAR                                                   
011400*----------------------------------------------------------------*        
011500     SKIP2                                                                
011600 01  S98-DSNTIAR             PIC X(8)  VALUE 'DSNTIAR'.                   
011700 01  ERROR-MESSAGE.                                                       
011800     03  ERROR-LEN           PIC S9(4) COMP VALUE +960.                   
011900     03  ERROR-TEXTS.                                                     
012000         05  ERROR-TEXT      PIC X(80) OCCURS 12 TIMES                    
012100                             INDEXED BY ERROR-INDEX.                      
012200 01  ERROR-TEXT-LEN          PIC S9(9) COMP VALUE +80.                    
012300*                                                                         
012400*L1  GODK-SQLCODER-VALUES PIC X(168).                                     
012500*L1  FILLER REDEFINES GODK-SQLCODER-VALUES PIC X(168).                    
012600*L1  GODK-SQLCODER-ANT-RADER       PIC S9(4) COMP VALUE +14.              
012700*L1  S98-DSNTIAR             PIC X(8)  VALUE 'DSNTIAR'.                   
012800*L1  ERROR-MESSAGE       PIC X(962).                                      
012900*L1  ERROR-TEXT-LEN          PIC S9(9) COMP VALUE +80.                    
013000*** END COPY V161FELCC0  LENGTH=      OLD LENGTH=                         
