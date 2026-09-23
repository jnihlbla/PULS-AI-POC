000100                                                                          
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0133      *        
000400******************************************************************        
000500                                                                          
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W4037700.                                                
000800 AUTHOR.         ROGER OLSSON.                                            
000900 DATE-WRITTEN.   90/08/20.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001300*    FUNKTION.                                                            
001400*        LÄSER AKTUELL PLOCKSATS (WL400711).                              
001500*        BEHANDLAR ALLA PU-RADER SOM INGÅR I PLOCKSATSEN.                 
001600*        PROGRAMMET STARTAS OM EFTER ETT ANTAL BEHANDLADE SIDOR.          
001700*        NÄR ALLA PU-RADER HAR BEHANDLATS STARTAS                         
001800*        PROGRAMMET SOM LADDAR E4/E6 (W4037800).                          
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T377X                                             
002200*        MID:       : W4I37701                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        TRANSAKTION: W4T377X (GÄLLER ENDAST VID OMSTART AV PGM)          
002600*                     W4T378X                                             
002700* CHANGE LOG:                                                             
002800*                                                                         
002900* LINDA NILSSON NOV-2004                                                  
003000* ETRACKER 1519549                                                        
003100*                                                                         
003200* LINDA NILSSON DEC-2004                                                  
003300* ETRACKER 899268                                                         
003400*                                                                         
003500                                                                          
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)   VALUE 'W4037700'.             
004300                                                                          
004400 77  FELTEXT                     PIC X(48)  VALUE SPACE.                  
004500 77  PGMPOS                      PIC X(24)  VALUE SPACE.                  
004600 77  FILLER                      PIC X(8)   VALUE ALL 'A'.                
004700                                                                          
004800 77  ALLT-NOK1                   PIC X       VALUE 'J'.                   
004900 77  ALLT-NOK2                   PIC X       VALUE 'J'.                   
005000                                                                          
005100 77  YES                         PIC X       VALUE 'Y'.                   
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
005500                                                                          
005600 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005700                                                                          
005800 77  FILLER                      PIC X(8)   VALUE ALL 'C'.                
005900 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
006000 77  IX2                         PIC S9(9)  VALUE +0    COMP SYNC.        
006100 77  RAD-IX                      PIC S9(9)  VALUE +99   COMP SYNC.        
006200 77  SID-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
006300 77  TAB-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
006400 77  DC-INDX                     PIC S9(1)  VALUE +0    COMP SYNC.        
006500 77  FK-INDX                     PIC S9(9)  VALUE +0    COMP SYNC.        
006600 77  MAX-FK-INDX                 PIC S9(9)  VALUE +6    COMP SYNC.        
006700 77  MAX-SIDA                    PIC S9(9)  VALUE +1000 COMP SYNC.        
006800 77  MAX-LAGOMR                  PIC S9(9)  VALUE +99   COMP SYNC.        
006900                                                                          
007000 77  SPAR-IDORDER                PIC S9(9)  COMP-3.                       
007100 77  SPAR-IDPRC                  PIC  9(4).                               
007200 77  SPAR-PRINTER                PIC X(3).                                
007300 77  SPAR-KDSS-PU                PIC X(1).                                
007400 77  SPAR-ADLAGOMR               PIC S9(3)  COMP-3.                       
007500 77  INITIERA-LDC-TABELL         PIC X      VALUE SPACE.                  
007600                                                                          
007700 77  WS-ADPLATS                  PIC X(5).                                
007800 77  WS-VLORDNTO                 PIC S9(4)V9(3) COMP-3.                   
007900 77  WS-DATUM-TID                PIC 9(11).                               
008000 77  WS-DATUM                    PIC 9(6).                                
008100 77  WS-TID                      PIC 9(6).                                
008200 77  WS-IDORDER                  PIC S9(7)  COMP-3.                       
008300 77  WS-IDPRODNR                 PIC S9(7)  COMP-3.                       
008400 77  WS-IDPLKLST                 PIC S9(3)  COMP-3.                       
008500 77  WS-ODEL-IDLEVNR             PIC  X(5).                               
008600 77  WS-VIP-LOPNR                PIC  9(3).                               
008700 77  WS-BETRPFIR                 PIC X(15)  VALUE SPACE.                  
008800 77  WS-BETRPFIR-ALT             PIC X(15)  VALUE SPACE.                  
008900 77  WS-KDMATT                   PIC X(1).                                
009000     88 US-MATT                  VALUE 'U'.                               
009100*                                                                         
009200 77  WS-KDPRODKL                 PIC X(1).                                
009300     88 CDC-DAGORDER             VALUE 'D' 'V' 'E' 'F'.                   
009400     88 SDC-DAGORDER             VALUE 'D' 'V' 'W' 'X' 'Y' 'Z'.           
009500*                                                                         
009600 01  WS-GMT-BEGMT-OVR.                                                    
009700     03 WS-GMT-BEGMT-OVR-RAD1    PIC X(35) VALUE SPACE.                   
009800     03 WS-GMT-BEGMT-OVR-RAD2    PIC X(35) VALUE SPACE.                   
009900*                                                                         
010000 01  WS-GMT-ADGMT-OVR.                                                    
010100     03 WS-GMT-ADGMT-OVR-GATA    PIC X(35) VALUE SPACE.                   
010200     03 WS-GMT-ADGMT-OVR-PADR    PIC X(35) VALUE SPACE.                   
010300     03 WS-GMT-ADGMT-OVR-LAND    PIC X(35) VALUE SPACE.                   
010400*                                                                         
010500 01  IDDC-USER.                                                           
010600     03  FILLER                  PIC X(5) VALUE 'WIDDC'.                  
010700     03  IDDC-XX                 PIC X(2).                                
010800     03  FILLER                  PIC X(1) VALUE SPACE.                    
010900*                                                                         
011000 77  ALLT-SW                     PIC X.                                   
011100     88  ALLT-OK                             VALUE 'J'.                   
011200     88  ALLT-FEL                            VALUE 'N'.                   
011300                                                                          
011400 77  FILLER                      PIC X(8)   VALUE ALL 'B'.                
011500 77  PLOCKSATS-KLAR-SW           PIC X.                                   
011600     88  PLOCKSATS-EJ-KLAR                   VALUE 'N'.                   
011700     88  PLOCKSATS-KLAR                      VALUE 'J'.                   
011800                                                                          
011900 77  PU-SW                       PIC X.                                   
012000     88  PU-CLOSE                            VALUE 'N'.                   
012100     88  PU-OPEN                             VALUE 'J'.                   
012200                                                                          
012300 77  NY-PRINTER-SW               PIC X.                                   
012400     88  EJ-NY-PRINTER                       VALUE 'N'.                   
012500     88  NY-PRINTER                          VALUE 'J'.                   
012600                                                                          
012700 77  WS-SKRIVARTYP-SW            PIC X.                                   
012800     88  MATRIS-SKRIVARE                     VALUE 'N'.                   
012900     88  LASER-SKRIVARE                      VALUE 'J'.                   
013000                                                                          
013100 77  FORSTA-SIDA-SW              PIC X       VALUE 'J'.                   
013200     88  FORSTA-SIDA                         VALUE 'J'.                   
013300     88  EJ-FORSTA-SIDA                      VALUE 'N'.                   
013400                                                                          
013500 77  SKRIV-TOTAL-RAD-SW          PIC X.                                   
013600     88  SKRIV-TOTAL                         VALUE 'J'.                   
013700     88  SKRIV-EJ-TOTAL                      VALUE 'N'.                   
013800                                                                          
013900 77  NY-ORDER-SW                 PIC X.                                   
014000     88  NY-ORDER                            VALUE 'J'.                   
014100     88  EJ-NY-ORDER                         VALUE 'N'.                   
014200                                                                          
014300 77  PICK-UP-OK-SW               PIC X.                                   
014400     88  PICK-UP-EJ-OK                       VALUE 'N'.                   
014500     88  PICK-UP-OK                          VALUE 'J'.                   
014600                                                                          
014700 77  BRYT-TOTAL-SW               PIC X.                                   
014800     88  TOTAL-BRYTNING                      VALUE 'J'.                   
014900                                                                          
015000 77  FORSTA-LIST-SIDA-SW         PIC X.                                   
015100     88  FORSTA-LIST-SIDA                    VALUE 'J'.                   
015200                                                                          
015300 77  SW-SKRIV-INTE-RAD14         PIC X       VALUE 'N'.                   
015400                                                                          
015500 77  VIP-ID-HITTAT-SW            PIC X.                                   
015600     88  VIP-ID-HITTAT                       VALUE 'J'.                   
015700                                                                          
015800 77  SKRIV-EN-GANG-SW            PIC X.                                   
015900     88  SKRIV-EN-GANG                       VALUE 'N'.                   
016000     88  SKRIVIT-EN-GANG                     VALUE 'J'.                   
016100                                                                          
016200 77  SW-DIST-SVE-GISLAVED        PIC 9(5).                                
016300     88 DIST-SVERIGE-778                     VALUE 00778.                 
016400     88 DIST-SVERIGE-76                      VALUE 00076.                 
016500                                                                          
016600 77  SW-KUND-GISLAVED            PIC 9(7).                                
016700     88 KUND-GISLAVED-765                    VALUE 0000765.               
016800     88 KUND-GISLAVED-37015                  VALUE 0037015.               
016900                                                                          
017000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
017100     88  GODK-MID                            VALUE '4375' '4377'.         
017200     EJECT                                                                
017300*************************************                                     
017400*  TABELL FÖR ATT LAGRA PICKUP-PRC  *                                     
017500*************************************                                     
017600                                                                          
017700 01  TAB-PICKUP-PRC.                                                      
017800     03     FILLER             OCCURS 10.                                 
017900       05   TAB-VLKOLGR        PIC S9(1)V9(3) COMP-3.                     
018000       05   TAB-SUVOLYM        PIC S9(2)V9(3) COMP-3.                     
018100       05   FILLER             OCCURS 99.                                 
018200         07 TAB-IDPRC          PIC  X(4).                                 
018300         07 TAB-IDPRODNR       PIC S9(7)      COMP-3.                     
018400         07 TAB-IDPLKLST       PIC S9(7)      COMP-3.                     
018500         07 TAB-VLORDNTO       PIC S9(4)V9(3) COMP-3.                     
018600*                                                                         
018700******************************************************************        
018800*TABELL FÖR ATT LAGRA LDC VIP-ORDER INNAN UTSKRIFT PÅ SISTA SIDA *        
018900******************************************************************        
019000 01  FILLER                    PIC X(8)    VALUE 'LDC-TAB '.              
019100*LDC-GB                                                                   
019200 01 LDC-TABELL.                                                           
019300    03 LDC-TAB-VIP-ORDER  OCCURS 100.                                     
019400       05   LDC-TAB-VIPID            PIC  X(10) VALUE SPACE.              
019500       05   LDC-TAB-ANTAL-ART        PIC  9(3)  VALUE ZERO.               
019600*                                                                         
019700 01    FILLER             PIC X(8)    VALUE 'LDC-IX  '.                   
019800 01    LDC-TAB-IX                    PIC S9(3) COMP-3.                    
019900 01    LDC-TAB-IX-MAX                PIC S9(3) COMP-3 VALUE +100.         
020000 01    LDC-TOT-ANT-ART               PIC S9(3) COMP-3.                    
020100*      05   LDC-TAB-LOPNR               PIC  9(3)  VALUE ZERO.            
020200******************************************************************        
020300 01         FILLER             PIC X(8)    VALUE 'WWDIST  '.              
020400 01         TEST-IDDISTR       PIC  9(5)   COMP-3.                        
020500 01         FILLER REDEFINES TEST-IDDISTR.                                
020600     SKIP3                                                                
020700*    03  -COPY WWDIST08                                                   
020800                                                                          
020900 01  FILLER                    PIC X(16)  VALUE 'SDC-DISTRIKT'.           
021000*01  -COPY WWDIST34                                                       
021100     EJECT                                                                
021200 01  FILLER                    PIC X(8)   VALUE 'WWDIST03'.               
021300*01  -COPY WWDIST03                                                       
021400     EJECT                                                                
021500 01  FILLER                    PIC X(16)  VALUE 'DC99KONSTAN'.            
021600*    -COPY WWDC99    -PRE DC99-                                           
021700     EJECT                                                                
021800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021900 01  GENERELLA-SUBPROGRAM.                                                
022000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
022100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
022200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
022300     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
022400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
022500     EJECT                                                                
022600 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT'.             
022700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
022800*01 -COPY WMSGINIT                                                        
022900     EJECT                                                                
023000 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND'.             
023100*    --- PARAMETRAR TILL SUBPROGRAM WWOMVAND                              
023200*01 -COPY WWOMVAND                                                        
023300     EJECT                                                                
023400*    --- AREA FÖR SUBPROGRAM W006PRR1                                     
023500*                                                                         
023600 01  FILLER                      PIC X(16)  VALUE 'W006PRR1'.             
023700                                                                          
023800*01  -COPY W006PRAR                                                       
023900     SKIP3                                                                
024000 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
024100*   -COPY W006PRT                                                         
024200     EJECT                                                                
024300                                                                          
024400 01  WS-PU-AREA.                                                          
024500     03 WS-IDPRTLST.                                                      
024600        05 WS-SYSTDEL            PIC X(1).                                
024700        05 WS-LISTTYP            PIC X(2).                                
024800        05 WS-KDPRT              PIC X(3).                                
024900        05 FILLER-2              PIC X(2)      VALUE SPACE.               
025000     03 WS-PU-LISTID.                                                     
025100        05 WS-PU-IDPRC           PIC X(4).                                
025200        05 WS-PU-STRECK          PIC X(1).                                
025300        05 WS-PU-IDLOPNR         PIC X(5).                                
025400     03 WS-PU-LISTRAD.                                                    
025500        05 FILLER                PIC X(2)    VALUE SPACE.                 
025600        05 WS-PU-RAD             PIC X(78)   VALUE SPACE.                 
025700        05 FILLER                PIC X(52)   VALUE SPACE.                 
025800     03 WS-PU-LISTRAD-LDC REDEFINES WS-PU-LISTRAD.                        
025900        05 FILLERL1              PIC X(2).                                
026000        05 WS-PU-RAD-LDC         PIC X(82).                               
026100        05 FILLERL2              PIC X(48).                               
026200     03 WS-DUMMY                 PIC X(132)  VALUE SPACE.                 
026300                                                                          
026400     EJECT                                                                
026500*    --- MID-AREA                                                         
026600*                                                                         
026700 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
026800                                                                          
026900*01  -COPY W4I37701                                                       
027000     EJECT                                                                
027100*    --- AREOR FÖR MSG-HANTERING                                          
027200*                                                                         
027300 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
027400                                                                          
027500*01  -COPY WMSGAREA                                                       
027600     EJECT                                                                
027700 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
027800 01  P-TO-P-SW1.                                                          
027900     03  PTOP1-LL                PIC S9(4)   VALUE 34 COMP SYNC.          
028000     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
028100     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
028200     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T377X'.             
028300     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
028400     03  FILLER                  PIC  X(4)   VALUE '4377'.                
028500     03  PTOP1-KDMFSFOR          PIC  X(1).                               
028600     03  -COPY W4I37701  -PRE PTOP1-                                      
028700     EJECT                                                                
028800 01  P-TO-P-SW2.                                                          
028900     03  PTOP2-LL                PIC S9(4)   VALUE 34 COMP SYNC.          
029000     03  PTOP2-Z1                PIC  X(1)   VALUE LOW-VALUE.             
029100     03  PTOP2-Z2                PIC  X(1)   VALUE LOW-VALUE.             
029200     03  PTOP2-TRANSKOD          PIC  X(7)   VALUE 'W4T378X'.             
029300     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
029400     03  FILLER                  PIC  X(4)   VALUE '4377'.                
029500     03  PTOP2-KDMFSFOR          PIC  X(1).                               
029600     03  -COPY W4I37801  -PRE PTOP2-                                      
029700     EJECT                                                                
029800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029900*                                                                         
030000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
030100                                                                          
030200 01  NYCKLAR-TILL-DLI.                                                    
030300                                                                          
030400*----> KUNDREG.                                                           
030500                                                                          
030600*----> PLOCKSATSKÖN.                                                      
030700                                                                          
030800     03  W-4001-IDHTYP-X.                                                 
030900         05  W-4001-IDHTYP       PIC  X(4)  VALUE '4007'.                 
031000         05  W-4001-IDPRODNR     PIC  9(7).                               
031100         05  W-4001-IDPLKLST     PIC  9(3).                               
031200         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
031300                                                                          
031400*----> DIREKTNYCKEL TILL PU-RADEN.                                        
031500                                                                          
031600     03  W-4010-IDHTYP-X.                                                 
031700         05  W-4010-IDORDER      PIC S9(7) COMP-3.                        
031800         05  W-4010-KDPRT        PIC  X(3).                               
031900         05  W-4010-KDSS-PU      PIC  X(1).                               
032000         05  W-4010-ADLAGOMR     PIC S9(3) COMP-3.                        
032100         05  W-4010-ADGANG       PIC S9(3) COMP-3.                        
032200         05  W-4010-ADPLATS      PIC S9(5) COMP-3.                        
032300         05  W-4010-IDARTNR      PIC S9(9) COMP-3.                        
032400         05  W-4010-IDLOPNR      PIC S9(3) COMP-3.                        
032500                                                                          
032600     03  W-4010-KDPRT-MAX-X.                                              
032700         05  W-4010-KDPRT-MAX    PIC  X(3) VALUE '999'.                   
032800                                                                          
032900*----> DIREKTNYCKEL TILL ORDERHUVUDET.                                    
033000                                                                          
033100     03  W-IDORDER-X.                                                     
033200         05  W-IDORDER           PIC S9(7) COMP-3.                        
033300                                                                          
033400*----> DIREKTNYCKEL TILL ORDERHUVUD ARBETSTABELL.                         
033500                                                                          
033600     03  W-IDDC-X.                                                        
033700         05  W-IDDC                      PIC X(2).                        
033800                                                                          
033900*----> DIREKTNYCKEL TILL ORDERDEL.                                        
034000                                                                          
034100     03  W-WDQ301KY-MIN-X.                                                
034200         05  W-Q301-MIN-IDORDER  PIC S9(7) COMP-3.                        
034300         05  W-Q301-MIN-IDDC     PIC X(2).                                
034400         05  W-Q301-MIN-IDPRODNR PIC S9(7) COMP-3.                        
034500         05  W-Q301-MIN-IDPLKLST PIC S9(3) COMP-3.                        
034600                                                                          
034700     03  W-WDQ301KY-MAX-X.                                                
034800         05  W-Q301-MAX-IDORDER  PIC S9(7) COMP-3.                        
034900         05  W-Q301-MAX-IDDC     PIC X(2).                                
035000         05  W-Q301-MAX-IDPRODNR PIC S9(7) COMP-3.                        
035100         05  W-Q301-MAX-IDPLKLST PIC S9(3) COMP-3.                        
035200                                                                          
035300*----> PRC-KANALEN.                                                       
035400                                                                          
035500     03  W-4447-IDHTYP-X.                                                 
035600         05  W-4447-IDHTYP       PIC  X(4)  VALUE '4447'.                 
035700         05  W-4447-IDDC         PIC  X(2).                               
035800         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
035900                                                                          
036000     03  W-4448-IDPRC-X.                                                  
036100         05  W-4448-IDPRC-KEY    PIC  X(4).                               
036200         05  W-4448-LOW-VALUE    PIC  X(1)  VALUE LOW-VALUE.              
036300                                                                          
036400*----> FÖRRÅDSDATATEXT/EMBALLAGEKOD.                                      
036500                                                                          
036600     03  W-4535-IDHTYP-X.                                                 
036700         05  W-4535-IDHTYP       PIC  X(4)  VALUE '4535'.                 
036800         05  W-4535-KDFDKRAV     PIC S9(3)  COMP-3.                       
036900         05  W-4535-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
037000                                                                          
037100     03  W-4536-IDSKYLT-X.                                                
037200         05  W-4536-IDSKYLT      PIC  X(3).                               
037300         05  W-4536-LOW-VALUE    PIC  X(2)  VALUE LOW-VALUE.              
037400                                                                          
037500*----> FRAKTTEXT.                                                         
037600                                                                          
037700     03  W-4732-IDHTYP-X.                                                 
037800         05  W-4732-IDHTYP       PIC  X(4)  VALUE '4732'.                 
037900         05  W-4732-KDFRAKT      PIC S9(3)  COMP-3.                       
038000         05  W-4732-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
038100                                                                          
038200*----> TRANSPORTÖR NDC                                                    
038300                                                                          
038400     03  W-WDGXKEY-4433-X.                                                
038500         05  W-4433-IDHTYP       PIC X(4)    VALUE '4433'.                
038600         05  W-4433-IDDC         PIC X(2).                                
038700         05  W-4433-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
038800                                                                          
038900     03  W-WDGXKEY-4434-X.                                                
039000         05  W-4434-IDTRP        PIC X(5)    VALUE SPACE.                 
039100         05  W-4434-TITRPAVG     PIC S9(7)   COMP-3 VALUE ZERO.           
039200         05  W-4434-LOW-VALUE    PIC X       VALUE LOW-VALUE.             
039300                                                                          
039400     03  W-IDGMT-X.                                                       
039500         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
039600         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
039700                                                                          
039800*----> LAGRA LDC-GB VIPID-TABELL                                          
039900                                                                          
040000     03  W-4017-IDHTYP-X.                                                 
040100         05  W-4017-IDHTYP       PIC  X(4)  VALUE '4017'.                 
040200         05  W-4017-IDPRODNR     PIC  9(7).                               
040300         05  W-4017-IDPLKLST     PIC  9(3).                               
040400         05  W-4017-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
040500                                                                          
040600     03  W-4018-KDSEGKEY-X.                                               
040700         05  W-4018-KDSEGKEY     PIC   X      VALUE '1'.                  
040800                                                                          
040900     03  W-IDDC-B6-X.                                                     
041000         05 W-IDDC-B6                  PIC X(2).                          
041100                                                                          
041200     03  W-IDARTNR-X.                                                     
041300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
041400                                                                          
041500*    --- STATUS-KOD FRÅN IMS                                              
041600                                                                          
041700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
041800 01  STATUS-WS                   PIC X(2).                                
041900     88  SEGMENT-FINNS                       VALUE '  '.                  
042000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
042100     88  END-OF-DATA                         VALUE 'GB'.                  
042200     SKIP2                                                                
042300 01  GODK-STATUSKODER.                                                    
042400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
042500                                                                          
042600 01  SSA1                        PIC X(128).                              
042700 01  SSA2                        PIC X(128).                              
042800 01  SSA3                        PIC X(128).                              
042900     EJECT                                                                
043000****************************                                              
043100*  SPAR-AREA PLOCKSATS PU  *                                              
043200****************************                                              
043300 01  W-PU-PLOCKSATS.                                                      
043400*    03  -COPY WDGX4008   -PRE W-PU-                                      
043500     EJECT                                                                
043600***********************                                                   
043700*  ARBETSAREA FÖR PRC *                                                   
043800***********************                                                   
043900 01  W-SPAR-IDPRC.                                                        
044000*    03  -COPY WDGX4448   -PRE W-SPAR-                                    
044100     EJECT                                                                
044200*PURAD*********************************                                   
044300* UTSKRIFTS-RADER FÖR PACKUNDERLAGET  *                                   
044400***************************************                                   
044500 01  PU-HRAD0.                                                            
044600     03   FILLER                  PIC X(62) VALUE SPACE.                  
044700     03   HRAD0-COPY              PIC X(6)  VALUE '*COPY*'.               
044800                                                                          
044900 01  PU-HRAD1.                                                            
045000     03   FILLER                  PIC X(13) VALUE SPACE.                  
045100     03   HRAD1-TEXT              PIC X(30).                              
045200     03   FILLER                  PIC X(31) VALUE SPACE.                  
045300     03   HRAD1-IDSID             PIC ZZ9.                                
045400                                                                          
045500 01  PU-HRAD2.                                                            
045600     03   HRAD2-PV-LV             PIC X(2).                               
045700     03   HRAD2-IDDISTR           PIC Z(4)9.                              
045800     03   FILLER                  PIC X(6)  VALUE SPACE.                  
045900     03   HRAD2-IDKUNDNR          PIC Z(6)9.                              
046000     03   FILLER                  PIC X(3)  VALUE SPACE.                  
046100     03   HRAD2-IDKUNDRF          PIC X(7).                               
046200     03   FILLER                  PIC X(7)  VALUE SPACE.                  
046300     03   HRAD2-KDORDKL           PIC X(1).                               
046400     03   FILLER                  PIC X(4)  VALUE SPACE.                  
046500     03   HRAD2-IDBORD            PIC X(3).                               
046600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
046700     03   HRAD2-IDUSER            PIC X(8).                               
046800     03   FILLER                  PIC X(4)  VALUE SPACE.                  
046900     03   HRAD2-IDPRODNR          PIC Z(6)9.                              
047000     03   FILLER                  PIC X(1)  VALUE '-'.                    
047100     03   HRAD2-IDPLKLST          PIC X(3).                               
047200                                                                          
047300 01  PU-HRAD3.                                                            
047400     03   HRAD3-BEGMT-RAD1        PIC X(29).                              
047500     03   HRAD3-PRC               PIC X(9).                               
047600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
047700     03   HRAD3-LOPNR             PIC X(6).                               
047800     03   FILLER                  PIC X(3)  VALUE SPACE.                  
047900     03   PU-HRAD3-TEXT           PIC X(4).                               
048000     03   FILLER                  PIC X(3)  VALUE SPACE.                  
048100     03   HRAD3-ZON               PIC X(4)  VALUE SPACE.                  
048200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
048300     03   HRAD3-REGDATUM          PIC X(12).                              
048400                                                                          
048500 01  PU-HRAD3-ITA.                                                        
048600     03   HRAD3-BEGMT-RAD1-ITA    PIC X(29).                              
048700     03   HRAD3-PRC-ITA           PIC X(12).                              
048800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
048900     03   HRAD3-LOPNR-ITA         PIC X(6).                               
049000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
049100     03   PU-HRAD3-TEXT-ITA       PIC X(4).                               
049200     03   FILLER                  PIC X(10) VALUE SPACE.                  
049300     03   HRAD3-REGDATUM-ITA      PIC X(12).                              
049400                                                                          
049500 01  PU-HRAD3-SPA.                                                        
049600     03   HRAD3-BEGMT-RAD1-SPA    PIC X(30).                              
049700     03   HRAD3-PRC-SPA           PIC X(9).                               
049800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
049900     03   HRAD3-LOPNR-SPA         PIC X(6).                               
050000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
050100     03   PU-HRAD3-TEXT-SPA       PIC X(4).                               
050200     03   FILLER                  PIC X(10) VALUE SPACE.                  
050300     03   HRAD3-REGDATUM-SPA      PIC X(12).                              
050400                                                                          
050500 01  PU-HRAD3-OST.                                                        
050600     03   HRAD3-BEGMT-RAD1-OST    PIC X(30).                              
050700     03   HRAD3-PRC-OST           PIC X(9).                               
050800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
050900     03   HRAD3-LOPNR-OST         PIC X(8).                               
051000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
051100     03   PU-HRAD3-TEXT-OST       PIC X(4).                               
051200     03   FILLER                  PIC X(10) VALUE SPACE.                  
051300     03   HRAD3-REGDATUM-OST      PIC X(12).                              
051400                                                                          
051500 01  PU-HRAD3-NDC.                                                        
051600     03   HRAD3-BEGMT-RAD1-NDC    PIC X(29).                              
051700     03   HRAD3-PRC-NDC           PIC X(9).                               
051800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
051900     03   HRAD3-LOPNR-NDC         PIC X(6).                               
052000     03   FILLER                  PIC X(3)  VALUE SPACE.                  
052100     03   HRAD3-TEXT-NDC          PIC X(4).                               
052200     03   FILLER                  PIC X(5)  VALUE SPACE.                  
052300     03   FILLER                  PIC X(7)  VALUE 'REG  : '.              
052400     03   HRAD3-TIREGDAT-NDC      PIC X(6).                               
052500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
052600     03   HRAD3-TIREGHH-NDC       PIC X(2).                               
052700     03   FILLER                  PIC X(1)  VALUE ':'.                    
052800     03   HRAD3-TIREGMM-NDC       PIC X(2).                               
052900                                                                          
053000 01  PU-HRAD3-JAP.                                                        
053100     03   HRAD3-BEGMT-RAD1-JAP    PIC X(30).                              
053200     03   HRAD3-PRC-JAP           PIC X(9).                               
053300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
053400     03   HRAD3-LOPNR-JAP         PIC X(6).                               
053500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
053600     03   HRAD3-TEXT-JAP          PIC X(4).                               
053700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
053800     03   FILLER                  PIC X(10) VALUE 'REG DATE: '.           
053900     03   HRAD3-TIREGDAT-JAP      PIC X(6).                               
054000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
054100     03   HRAD3-TIREGHH-JAP       PIC X(2).                               
054200     03   FILLER                  PIC X(1)  VALUE ':'.                    
054300     03   HRAD3-TIREGMM-JAP       PIC X(2).                               
054400                                                                          
054500 01  PU-HRAD3-2.                                                          
054600     03   FILLER                  PIC X(32) VALUE SPACE.                  
054700     03   HRAD3-2-PRC             PIC X(3).                               
054800     03   FILLER                  PIC X(3)  VALUE SPACE.                  
054900     03   HRAD3-2-LOPNR           PIC X(5).                               
055000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
055100     03   PU-HRAD3-2-TEXT         PIC X(4).                               
055200                                                                          
055300 01  PU-HRAD4.                                                            
055400     03   HRAD4-BEGMT-RAD2        PIC X(35).                              
055500     03   HRAD4-IDPRC             PIC X(4).                               
055600     03   FILLER                  PIC X(4)  VALUE SPACE.                  
055700     03   HRAD4-IDLOPNR-PL        PIC ZZ9.                                
055800     03   FILLER                  PIC X(4)  VALUE SPACE.                  
055900     03   HRAD4-IDLOPNR-ORD       PIC Z9.                                 
056000     03   FILLER                  PIC X(5)  VALUE SPACE.                  
056100     03   HRAD4-IDZON             PIC X(2).                               
056200     03   FILLER                  PIC X(4)  VALUE SPACE.                  
056300     03   HRAD4-TIREGDAT          PIC X(6).                               
056400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
056500     03   HRAD4-TIREGHH           PIC X(2).                               
056600     03   FILLER                  PIC X(1)  VALUE ':'.                    
056700     03   HRAD4-TIREGMM           PIC X(2).                               
056800                                                                          
056900 01  PU-HRAD4-NDC.                                                        
057000     03   HRAD4-BEGMT-RAD2-NDC    PIC X(35).                              
057100     03   HRAD4-IDPRC-NDC         PIC X(4).                               
057200     03   FILLER                  PIC X(4)  VALUE SPACE.                  
057300     03   HRAD4-IDLOPNR-PL-NDC    PIC ZZ9.                                
057400     03   FILLER                  PIC X(4)  VALUE SPACE.                  
057500     03   HRAD4-IDLOPNR-ORD-NDC   PIC Z9.                                 
057600     03   FILLER                  PIC X(6)  VALUE SPACE.                  
057700     03   FILLER                  PIC X(7)  VALUE 'PRINT: '.              
057800     03   HRAD4-TIUTSKR-NDC       PIC X(6).                               
057900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
058000     03   HRAD4-TIUTSHH-NDC       PIC X(2).                               
058100     03   FILLER                  PIC X(1)  VALUE ':'.                    
058200     03   HRAD4-TIUTSMM-NDC       PIC X(2).                               
058300                                                                          
058400 01  PU-HRAD4-JAP.                                                        
058500     03   HRAD4-BEGMT-RAD2-JAP    PIC X(35).                              
058600     03   HRAD4-IDPRC-JAP         PIC X(4).                               
058700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
058800     03   HRAD4-IDLOPNR-PL-JAP    PIC ZZ9.                                
058900     03   FILLER                  PIC X(4)  VALUE SPACE.                  
059000     03   HRAD4-IDLOPNR-ORD-JAP   PIC Z9.                                 
059100     03   FILLER                  PIC X(4)  VALUE SPACE.                  
059200     03   FILLER                  PIC X(7)  VALUE 'PRINT: '.              
059300     03   HRAD4-TIUTSKR-JAP       PIC X(6).                               
059400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
059500     03   HRAD4-TIUTSHH-JAP       PIC X(2).                               
059600     03   FILLER                  PIC X(1)  VALUE ':'.                    
059700     03   HRAD4-TIUTSMM-JAP       PIC X(2).                               
059800                                                                          
059900 01  PU-HRAD4-2.                                                          
060000     03   FILLER                  PIC X(35) VALUE SPACE.                  
060100     03   HRAD4-2-IDPRC           PIC X(4).                               
060200     03   FILLER                  PIC X(4)  VALUE SPACE.                  
060300     03   HRAD4-2-IDLOPNR-PL      PIC ZZ9.                                
060400     03   FILLER                  PIC X(4)  VALUE SPACE.                  
060500     03   HRAD4-2-IDLOPNR-ORD     PIC Z9.                                 
060600                                                                          
060700 01  PU-HRAD5.                                                            
060800     03   HRAD5-ADGMT-GATA        PIC X(35).                              
060900     03   FILLER                  PIC X(42) VALUE SPACE.                  
061000                                                                          
061100 01  PU-HRAD5-ITA.                                                        
061200     03   HRAD5-ADGMT-GATA-ITA    PIC X(30).                              
061300     03   FILLER                  PIC X(6)  VALUE 'CODICE'.               
061400     03   FILLER                  PIC X(41) VALUE SPACE.                  
061500                                                                          
061600 01  PU-HRAD6.                                                            
061700     03   HRAD6-ADGMT-PADR        PIC X(35).                              
061800     03   FILLER                  PIC X(2)  VALUE 'FC'.                   
061900     03   FILLER                  PIC X(6)  VALUE SPACE.                  
062000     03   FILLER                  PIC X(9)  VALUE 'TRANSPORT'.            
062100     03   FILLER                  PIC X(11) VALUE SPACE.                  
062200     03   HRAD6-PRINTDATUM        PIC X(15).                              
062300                                                                          
062400 01  PU-HRAD6-NDC.                                                        
062500     03   HRAD6-ADGMT-PADR-NDC    PIC X(35).                              
062600     03   FILLER                  PIC X(2)  VALUE 'FC'.                   
062700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
062800     03   FILLER                PIC X(14) VALUE 'CARRIER     : '.         
062900     03   HRAD6-IDTRP-NDC         PIC X(5)  VALUE SPACE.                  
063000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
063100     03   HRAD6-BETRPFIR-NDC      PIC X(15) VALUE SPACE.                  
063200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
063300                                                                          
063400 01  PU-HRAD6-JAP.                                                        
063500     03   HRAD6-ADGMT-PADR-JAP    PIC X(35).                              
063600     03   FILLER                  PIC X(2)  VALUE 'FC'.                   
063700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
063800     03   FILLER                PIC X(14) VALUE 'CARRIER     : '.         
063900     03   HRAD6-IDTRP-JAP         PIC X(5)  VALUE SPACE.                  
064000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
064100     03   HRAD6-BETRPFIR-JAP      PIC X(15) VALUE SPACE.                  
064200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
064300                                                                          
064400 01  PU-HRAD6-FRA.                                                        
064500     03   HRAD6-ADGMT-PADR-FRA    PIC X(29).                              
064600     03   FILLER                  PIC X(12) VALUE 'CODE DE FRET'.         
064700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
064800     03   FILLER                  PIC X(9)  VALUE 'TRANSPORT'.            
064900     03   FILLER                  PIC X(10) VALUE SPACE.                  
065000     03   HRAD6-PRINTDATUM-FRA    PIC X(15).                              
065100                                                                          
065200 01  PU-HRAD6-ITA.                                                        
065300     03   HRAD6-ADGMT-PADR-ITA    PIC X(29).                              
065400     03   FILLER                  PIC X(7)  VALUE 'TRASP.:'.              
065500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
065600     03   FILLER                  PIC X(10) VALUE 'TRASPORTO:'.           
065700     03   FILLER                  PIC X(14) VALUE SPACE.                  
065800     03   HRAD6-PRINTDATUM-ITA    PIC X(15).                              
065900                                                                          
066000 01  PU-HRAD6-SPA.                                                        
066100     03   HRAD6-ADGMT-PADR-SPA    PIC X(35).                              
066200     03   FILLER                  PIC X(3)  VALUE 'CT:'.                  
066300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
066400     03   FILLER                  PIC X(11) VALUE 'TRANSPORTE:'.          
066500     03   FILLER                  PIC X(13) VALUE SPACE.                  
066600     03   HRAD6-PRINTDATUM-SPA    PIC X(15).                              
066700                                                                          
066800 01  PU-HRAD7.                                                            
066900     03   HRAD7-ADGMT-LAND        PIC X(35).                              
067000     03   HRAD7-KDFRAKT           PIC Z9.                                 
067100     03   FILLER                  PIC X(5)  VALUE SPACE.                  
067200     03   HRAD7-BEFRAKT           PIC X(20).                              
067300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
067400     03   HRAD7-TIUTSKR           PIC X(6).                               
067500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
067600     03   HRAD7-TIUTSHH           PIC X(2).                               
067700     03   FILLER                  PIC X(1)  VALUE ':'.                    
067800     03   HRAD7-TIUTSMM           PIC X(2).                               
067900                                                                          
068000 01  PU-HRAD7-NDC.                                                        
068100     03   HRAD7-ADGMT-LAND-NDC    PIC X(35).                              
068200     03   HRAD7-KDFRAKT-NDC       PIC Z9.                                 
068300     03   HRAD7-TXT-ALT           PIC X(7)  VALUE SPACE.                  
068400     03   HRAD7-TXT-CARRIER       PIC X(9)  VALUE SPACE.                  
068500     03   HRAD7-IDTRP-NDC         PIC X(5)  VALUE SPACE.                  
068600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
068700     03   HRAD7-BETRPFIR-NDC      PIC X(15) VALUE SPACE.                  
068800     03   FILLER                  PIC X(3)  VALUE SPACE.                  
068900                                                                          
069000 01  PU-HRAD7-JAP.                                                        
069100     03   HRAD7-ADGMT-LAND-JAP    PIC X(35).                              
069200     03   HRAD7-KDFRAKT-JAP       PIC ZZ.                                 
069300     03   HRAD7-TXT-ALT-JAP       PIC X(7)  VALUE SPACE.                  
069400     03   HRAD7-TXT-CARRIER-JAP   PIC X(9)  VALUE SPACE.                  
069500     03   HRAD7-IDTRP-JAP         PIC X(5)  VALUE SPACE.                  
069600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
069700     03   HRAD7-BETRPFIR-JAP      PIC X(15) VALUE SPACE.                  
069800     03   FILLER                  PIC X(3)  VALUE SPACE.                  
069900                                                                          
070000 01  PU-CONTACT-INFO-LINE.                                                
070110     03   FILLER                  PIC X(4)  VALUE 'NAME'.                 
070120     03   FILLER                  PIC X(2)  VALUE SPACE.                  
070200     03   CONTACT-NAME-B2C        PIC X(35).                              
070300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
070310     03   FILLER                  PIC X(5)  VALUE 'TEL'.                  
070400     03   CONTACT-BETELNR-B2C     PIC X(20).                              
070600                                                                          
070700 01  PU-CONTACT-MAIL-LINE.                                                
070800     03   FILLER                  PIC X(6)  VALUE 'MAIL'.                 
070900     03   CONTACT-MAIL-INFO       PIC X(60).                              
071000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
071100                                                                          
071110 01  PU-CONTACT-BERADREF.                                                 
071120     03   FILLER                  PIC X(9)  VALUE 'LINEREF'.              
071130     03   CONTACT-BERADREF        PIC X(15).                              
071140     03   FILLER                  PIC X(5)  VALUE SPACE.                  
071150                                                                          
071200 01  PU-HRAD8.                                                            
071300     03   HRAD8-TEKUNDRF          PIC X(10).                              
071400     03   HRAD8-BEKUNDRF          PIC X(15).                              
071500     03   FILLER                  PIC X(4)  VALUE SPACE.                  
071600     03   HRAD8-TEVARREF          PIC X(11).                              
071700     03   HRAD8-BEVARREF          PIC X(10).                              
071800     03   FILLER                  PIC X(13) VALUE SPACE.                  
071900     03   FILLER                  PIC X(3)  VALUE 'RFS'.                  
072000                                                                          
072100 01  PU-HRAD8-FRA.                                                        
072200     03   HRAD8-FRA-TEKUNDRF      PIC X(10).                              
072300     03   HRAD8-FRA-BEKUNDRF      PIC X(15).                              
072400     03   FILLER                  PIC X(2)  VALUE SPACE.                  
072500     03   HRAD8-FRA-TEVARREF      PIC X(11).                              
072600     03   HRAD8-FRA-BEVARREF      PIC X(10).                              
072700     03   FILLER                  PIC X(17) VALUE SPACE.                  
072800     03   FILLER                  PIC X(12) VALUE 'DATE EMBALL.'.         
072900                                                                          
073000 01  PU-HRAD8-ITA.                                                        
073100     03   HRAD8-ITA-TEKUNDRF      PIC X(10).                              
073200     03   HRAD8-ITA-BEKUNDRF      PIC X(15).                              
073300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
073400     03   HRAD8-ITA-TEVARREF      PIC X(11).                              
073500     03   HRAD8-ITA-BEVARREF      PIC X(10).                              
073600     03   FILLER                  PIC X(17) VALUE SPACE.                  
073700     03   FILLER                  PIC X(3)  VALUE 'RFS'.                  
073800                                                                          
073900 01  PU-HRAD8-SPA.                                                        
074000     03   HRAD8-SPA-TEKUNDRF      PIC X(10).                              
074100     03   HRAD8-SPA-BEKUNDRF      PIC X(15).                              
074200     03   FILLER                  PIC X(4)  VALUE SPACE.                  
074300     03   HRAD8-SPA-TEVARREF      PIC X(11).                              
074400     03   HRAD8-SPA-BEVARREF      PIC X(10).                              
074500     03   FILLER                  PIC X(9)  VALUE SPACE.                  
074600     03   FILLER                  PIC X(18) VALUE                         
074700                          'LISTO PARA ENVIAR:'.                           
074800                                                                          
074900 01  PU-HRAD8-NDC.                                                        
075000     03   HRAD8-NDC-TEKUNDRF          PIC X(10).                          
075100     03   HRAD8-NDC-BEKUNDRF          PIC X(15).                          
075200     03   FILLER                  PIC X(4)  VALUE SPACE.                  
075300     03   HRAD8-NDC-TEVARREF          PIC X(11).                          
075400     03   HRAD8-NDC-BEVARREF          PIC X(10).                          
075500     03   FILLER                  PIC X(18) VALUE SPACE.                  
075600                                                                          
075700 01  PU-HRAD8B-NDC.                                                       
075800     03   PU-HRAD8B-NDC-COD-TEXT  PIC X(52).                              
075900     03   FILLER                  PIC X(28)  VALUE SPACE.                 
076000                                                                          
076100 01  PU-HRAD8-JAP.                                                        
076200     03   HRAD8-JAP-TEKUNDRF      PIC X(10) VALUE SPACE.                  
076300     03   HRAD8-JAP-BEKUNDRF      PIC X(15) VALUE SPACE.                  
076400     03   FILLER                  PIC X(4)  VALUE SPACE.                  
076500     03   HRAD8-JAP-TEVARREF      PIC X(11) VALUE SPACE.                  
076600     03   HRAD8-JAP-BEVARREF      PIC X(10) VALUE SPACE.                  
076700     03   FILLER                  PIC X(18) VALUE SPACE.                  
076800                                                                          
076900 01  PU-HRAD8B-JAP.                                                       
077000     03   PU-HRAD8B-JAP-COD-TEXT  PIC X(52).                              
077100     03   FILLER                  PIC X(28)  VALUE SPACE.                 
077200                                                                          
077300 01  PU-HRAD9.                                                            
077400     03   PU-HRAD9-TEXT           PIC X(22).                              
077500     03   HRAD9-BEFDKRAV          PIC X(40).                              
077600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
077700     03   HRAD9-TIRFSDAT          PIC X(6).                               
077800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
077900     03   HRAD9-TIRFSHH           PIC X(2).                               
078000     03   FILLER                  PIC X(1)  VALUE ':'.                    
078100     03   HRAD9-TIRFSMM           PIC X(2).                               
078200                                                                          
078300 01  PU-HRAD9-NDC.                                                        
078400     03   HRAD9-TELAGINS         PIC X(16).                               
078500     03   HRAD9-BELAGINS-DEL1    PIC X(60).                               
078600                                                                          
078700 01  PU-HRAD9-2-NDC.                                                      
078800     03   FILLER                  PIC X(16) VALUE SPACE.                  
078900     03   HRAD9-2-BELAGINS-DEL2   PIC X(60).                              
079000                                                                          
079100 01  PU-HRAD10.                                                           
079200     03   HRAD10-TEGDSMRK         PIC X(16).                              
079300     03   HRAD10-BEGMRK-RAD1      PIC X(30).                              
079400                                                                          
079500 01  PU-HRAD11.                                                           
079600     03   FILLER                  PIC X(16) VALUE SPACE.                  
079700     03   HRAD11-BEGMRK-RAD2      PIC X(30).                              
079800                                                                          
079900 01  PU-HRAD12.                                                           
080000     03   HRAD12-AST1             PIC X(1)  VALUE SPACE.                  
080100     03   HRAD12-TELAGINS         PIC X(16).                              
080200     03   HRAD12-BELAGINS-DEL1    PIC X(60).                              
080300     03   HRAD12-AST2             PIC X(1)  VALUE SPACE.                  
080400                                                                          
080500 01  PU-HRAD12A.                                                          
080600     03   FILLER                  PIC X(78) VALUE ALL '*'.                
080700                                                                          
080800 01  PU-HRAD12-NDC.                                                       
080900     03   PU-HRAD12-TEXT          PIC X(22) VALUE SPACE.                  
081000     03   HRAD12-BEFDKRAV         PIC X(39) VALUE SPACE.                  
081100     03   FILLER                  PIC X(4)  VALUE 'RFS:'.                 
081200     03   HRAD12-TIRFSDAT         PIC X(6)  VALUE SPACE.                  
081300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
081400     03   HRAD12-TIRFSHH          PIC X(2)  VALUE SPACE.                  
081500     03   FILLER                  PIC X(1)  VALUE ':'.                    
081600     03   HRAD12-TIRFSMM          PIC X(2)  VALUE SPACE.                  
081700                                                                          
081800 01  PU-HRAD13.                                                           
081900     03   HRAD13-AST1             PIC X(1)  VALUE SPACE.                  
082000     03   FILLER                  PIC X(16) VALUE SPACE.                  
082100     03   HRAD13-BELAGINS-DEL2    PIC X(60).                              
082200     03   HRAD13-AST2             PIC X(1)  VALUE SPACE.                  
082300                                                                          
082400 01  PU-HRAD14-SVE.                                                       
082500     03   FILLER                  PIC X(5)  VALUE 'O-REF'.                
082600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
082700     03   FILLER                  PIC X(6)  VALUE 'ADRESS'.               
082800     03   FILLER                  PIC X(8)  VALUE SPACE.                  
082900     03   FILLER                  PIC X(7)  VALUE '  ARTNR'.              
083000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
083100     03   FILLER                  PIC X(3)  VALUE 'RAD'.                  
083200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
083300     03   FILLER                  PIC X(3)  VALUE 'ORG'.                  
083400     03   FILLER                  PIC X(3)  VALUE ' 1 '.                  
083500     03   FILLER                  PIC X(9)  VALUE 'BENÄMNING'.            
083600     03   FILLER                  PIC X(5)  VALUE SPACE.                  
083900     03   FILLER                  PIC X(4)  VALUE ' LEV'.                 
084000     03   FILLER                  PIC X(4)  VALUE SPACE.                  
084010     03   FILLER                  PIC X(3)  VALUE ' Q3'.                  
084020     03   FILLER                  PIC X(3)  VALUE SPACE.                  
084100     03   RAD14-TXT-KOLLI-SPEC.                                           
084200       05 RAD14-KOLLI             PIC X(5).                               
084300       05 RAD14-SPACE1            PIC X(1).                               
084400       05 RAD14-SPEC              PIC X(4).                               
084500     03   RAD14-TXT-GISLAVED REDEFINES RAD14-TXT-KOLLI-SPEC.              
084600       05 RAD14-IDGISLAVED        PIC X(10).                              
084700                                                                          
084800 01  PU-HRAD14-ENG.                                                       
084900     03   FILLER                  PIC X(5)  VALUE 'O-REF'.                
085000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
085100     03   FILLER                  PIC X(7)  VALUE 'ADDRESS'.              
085200     03   FILLER                  PIC X(5)  VALUE SPACE.                  
085300     03   FILLER                  PIC X(7)  VALUE 'PART.NO'.              
085400     03   FILLER                  PIC X(4)  VALUE SPACE.                  
085500     03   FILLER                  PIC X(4)  VALUE 'LINE'.                 
085600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
085700     03   FILLER                  PIC X(3)  VALUE 'ORG'.                  
085800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
085900     03   FILLER                  PIC X(9)  VALUE 'PART NAME'.            
086000     03   FILLER                  PIC X(4)  VALUE SPACE.                  
086100     03   FILLER                  PIC X(5)  VALUE 'ORDER'.                
086200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
086300     03   FILLER                  PIC X(5)  VALUE 'DELIV'.                
086400     03   FILLER                  PIC X(3)  VALUE SPACE.                  
086500     03   FILLER                  PIC X(4)  VALUE 'CASE'.                 
086600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
086700     03   FILLER                  PIC X(4)  VALUE 'SPEC'.                 
086800*LDC                                                                      
086900 01  PU-HRAD14-ENG-LDC.                                                   
087000     03   FILLER                  PIC X(8)  VALUE 'VIP REF'.              
087100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
087200     03   FILLER                  PIC X(7)  VALUE 'ADDRESS'.              
087300     03   FILLER                  PIC X(4)  VALUE SPACE.                  
087400     03   FILLER                  PIC X(7)  VALUE 'PART.NO'.              
087500     03   FILLER                  PIC X(3)  VALUE SPACE.                  
087600     03   FILLER                  PIC X(4)  VALUE 'LINE'.                 
087700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
087800     03   FILLER                  PIC X(3)  VALUE 'ORG'.                  
087900     03   FILLER                  PIC X(3)  VALUE ' 1 '.                  
088000     03   FILLER                  PIC X(9)  VALUE 'PART NAME'.            
088100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
088400     03   FILLER                  PIC X(5)  VALUE 'DELIV'.                
088401     03   FILLER                  PIC X(1)  VALUE SPACE.                  
088402     03   FILLER                  PIC X(4)  VALUE ' Q3 '.                 
088500     03   FILLER                  PIC X(4)  VALUE SPACE.                  
088600     03   FILLER                  PIC X(4)  VALUE 'CASE'.                 
088700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
088800     03   FILLER                  PIC X(4)  VALUE 'SPEC'.                 
088900                                                                          
089000 01  PU-HRAD14-FRA.                                                       
089100     03   FILLER                  PIC X(8)  VALUE 'CDE ORG.'.             
089200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
089300     03   FILLER                  PIC X(7)  VALUE 'ADRESSE'.              
089400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
089500     03   FILLER                  PIC X(9)  VALUE 'REFERENCE'.            
089600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
089700     03   FILLER                  PIC X(5)  VALUE 'LIGNE'.                
089800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
089900     03   FILLER                  PIC X(3)  VALUE 'ORG'.                  
090000     03   FILLER                  PIC X(3)  VALUE ' 1 '.                  
090100     03   FILLER                  PIC X(11) VALUE 'DESIGNATION'.          
090200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
090300     03   FILLER                  PIC X(8)  VALUE 'QTE CDEE'.             
090400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
090500     03   FILLER                  PIC X(7)  VALUE 'ALL.'.                 
090600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
090700     03   FILLER                  PIC X(6)  VALUE 'LIVREE'.               
090800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
090900     03   FILLER                  PIC X(4)  VALUE 'SPEC'.                 
091000                                                                          
091100 01  PU-HRAD14-1-ITA.                                                     
091200     03   FILLER                  PIC X(4)  VALUE 'RIF.'.                 
091300     03   FILLER                  PIC X(30) VALUE SPACE.                  
091400     03   FILLER                  PIC X(4)  VALUE 'ORIG'.                 
091500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
091600     03   FILLER                  PIC X(9)  VALUE 'DENOMINA-'.            
091700     03   FILLER                  PIC X(24) VALUE SPACE.                  
091800                                                                          
091900 01  PU-HRAD14-2-ITA.                                                     
092000     03   FILLER                  PIC X(4)  VALUE 'ORD.'.                 
092100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
092200     03   FILLER                  PIC X(9)  VALUE 'INDIRIZZO'.            
092300     03   FILLER                  PIC X(3)  VALUE SPACE.                  
092400     03   FILLER                  PIC X(7)  VALUE 'N§.PART'.              
092500     03   FILLER                  PIC X(3)  VALUE SPACE.                  
092600     03   FILLER                  PIC X(5)  VALUE 'LINEA'.                
092700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
092800     03   FILLER                  PIC X(3)  VALUE 'INE'.                  
092900     03   FILLER                  PIC X(3)  VALUE ' 1 '.                  
093000     03   FILLER                  PIC X(11) VALUE '-ZIONE     '.          
093100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
093200     03   FILLER                  PIC X(6)  VALUE 'DELIV.'.               
093300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
093400     03   FILLER                  PIC X(6)  VALUE 'VENITE'.               
093500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
093600     03   FILLER                  PIC X(6)  VALUE 'CASSA '.               
093700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
093800     03   FILLER                  PIC X(4)  VALUE 'SPEC'.                 
093900                                                                          
094000 01  PU-HRAD14-1-SPA.                                                     
094100     03   FILLER                  PIC X(4)  VALUE 'REF-'.                 
094200     03   FILLER                  PIC X(57) VALUE SPACE.                  
094300     03   FILLER                  PIC X(5)  VALUE 'ENTRE'.                
094400     03   FILLER                  PIC X(9)  VALUE SPACE.                  
094500                                                                          
094600 01  PU-HRAD14-2-SPA.                                                     
094700     03   FILLER                  PIC X(5)  VALUE 'PED  '.                
094800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
094900     03   FILLER                  PIC X(9)  VALUE 'UBICACION'.            
095000     03   FILLER                  PIC X(4)  VALUE SPACE.                  
095100     03   FILLER                  PIC X(7)  VALUE 'NO.REF.'.              
095200     03   FILLER                  PIC X(4)  VALUE SPACE.                  
095300     03   FILLER                  PIC X(5)  VALUE 'LINEA'.                
095400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
095500     03   FILLER                  PIC X(3)  VALUE 'ORI'.                  
095600*ORI = COUNTRY OF ORIGIN                                                  
095700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
095800     03   FILLER                  PIC X(12) VALUE 'DENOMINACION'.         
095900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
096000     03   FILLER                  PIC X(6)  VALUE 'PEDIDO'.               
096100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
096200     03   FILLER                  PIC X(5)  VALUE '-GADO'.                
096300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
096400     03   FILLER                  PIC X(4)  VALUE 'ESP.'.                 
096500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
096600     03   FILLER                  PIC X(5)  VALUE 'COLLI'.                
096700                                                                          
096800 01  PU-HRAD14-OST.                                                       
096900     03   FILLER                  PIC X(5)  VALUE 'O-REF'.                
097000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
097100     03   FILLER                  PIC X(8)  VALUE 'ADDRESSE'.             
097200     03   FILLER                  PIC X(6)  VALUE SPACE.                  
097300     03   FILLER                  PIC X(7)  VALUE 'TEILENR'.              
097400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
097500     03   FILLER                  PIC X(4)  VALUE 'ZEIL'.                 
097600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
097700     03   FILLER                  PIC X(3)  VALUE 'ORG'.                  
097800     03   FILLER                  PIC X(3)  VALUE ' 1 '.                  
097900     03   FILLER                  PIC X(11) VALUE 'BEZEICHNUNG'.          
098000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
098100     03   FILLER                  PIC X(5)  VALUE 'ORDER'.                
098200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
098300     03   FILLER                  PIC X(9)  VALUE 'GELIEFERT'.            
098400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
098500     03   FILLER                  PIC X(5)  VALUE 'KOLLI'.                
098600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
098700     03   FILLER                  PIC X(4)  VALUE 'SPEC'.                 
098800*PURAD                                                                    
098900 01  PU-RAD.                                                              
099000     03   RAD-IDKUNDRF-RO-URS     PIC X(5).                               
099100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
099200     03   RAD-ADRESS-SVE.                                                 
099300       05 RAD-ADLAGOMR-SVE        PIC 99.                                 
099400       05 RAD-ADGANG-SVE          PIC Z9.                                 
099500       05 RAD-FILLER1             PIC X(1).                               
099600       05 RAD-ADPLATS-SVE         PIC X(5).                               
099700       05 RAD-FILLER2             PIC X(1)  VALUE SPACE.                  
099800     03   RAD-ADRESS-ENG REDEFINES RAD-ADRESS-SVE.                        
099900       05 RAD-ADLAGOMR-ENG        PIC 99.                                 
100000       05 RAD-ADGANG-ENG          PIC 99.                                 
100100       05 RAD-PUNKT-ENG           PIC X(1).                               
100200       05 RAD-ADPLATS-ENG         PIC X(5).                               
100300       05 RAD-FILLER3             PIC X(1).                               
100400     03   RAD-ADRESS-GB  REDEFINES RAD-ADRESS-SVE.                        
100500       05 RAD-BERADREF-GB         PIC X(10).                              
100600       05 RAD-FILLER6             PIC X(1).                               
100700     03   RAD-ADRESS-NDC REDEFINES RAD-ADRESS-SVE.                        
100800       05 RAD-ADLAGOMR-NDC        PIC 99.                                 
100900       05 RAD-ADGANG-NDC          PIC 99.                                 
101000       05 RAD-FILLER9             PIC X(1).                               
101100       05 RAD-ADPLATS-NDC         PIC 9(5).                               
101200       05 RAD-FILLER10            PIC X(1).                               
101300     03   RAD-ADRESS-JAP REDEFINES RAD-ADRESS-SVE.                        
101400       05 RAD-ADLAGOMR-JAP        PIC 99.                                 
101500       05 RAD-ADGANG-JAP          PIC 99.                                 
101600       05 RAD-FILLER11            PIC X(1).                               
101700       05 RAD-ADPLATS-JAP         PIC 9(5).                               
101800       05 RAD-FILLER12            PIC X(1).                               
101900     03   RAD-IDARTNR             PIC Z(7)9.                              
102000     03   FILLER                  PIC X(1)  VALUE '-'.                    
102100     03   RAD-REKSIFFR            PIC X(1).                               
102200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
102300     03   RAD-IDPURAD             PIC Z(4).                               
102400     03   FILLER                  PIC X(2)  VALUE SPACE.                  
102500     03   RAD-KDARTURS            PIC X(2).                               
102600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
102700     03   RAD-FLTILLK             PIC X(1).                               
102800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
102900     03   RAD-BEART               PIC X(11).                              
103000     03   RAD-QUANTITY-SVE.                                               
103010       05 RAD-KVAVBART-SVE        PIC Z(5)9.                              
103011       05 RAD-FILLER13            PIC X(1).                               
103030       05 RAD-KVQPACK-3           PIC Z(6).                               
103031       05 RAD-FILLER14            PIC X(1).                               
103303     03   RAD-QUANTITY-US REDEFINES RAD-QUANTITY-SVE.                     
103304       05 RAD-KVBEART-Q           PIC Z(5)9.                              
103305       05 RAD-FILLER15            PIC X(1).                               
103307       05 RAD-KVAVBART            PIC Z(5)9.                              
103308       05 RAD-FILLER16            PIC X(1).                               
103400     03   RAD-KDFARLIG            PIC X(1).                               
103500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
103600     03   RAD-SISTA2-FALT.                                                
103700       05 RAD-FILLER7             PIC X(4).                               
103800       05 RAD-IDSPECEMB           PIC Z(4).                               
103900     03   RAD-SISTA-GISLAVED REDEFINES RAD-SISTA2-FALT.                   
104000       05 RAD-IDGISLAVED          PIC X(8).                               
104100     03   RAD-BERADREF10     REDEFINES RAD-SISTA2-FALT.                   
104200       05 RAD-BERADREF            PIC X(8).                               
104300                                                                          
104400* PURAD-SPA                                                               
104500 01  PU-RAD-SPA.                                                          
104600     03   RAD-IDKUNDRF-RO-URS-SPA PIC X(5).                               
104700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
104800     03   RAD-ADRESS-SPA.                                                 
104900       05 RAD-ADLAGOMR-SPA        PIC 99.                                 
105000       05 RAD-ADGANG-SPA          PIC 99.                                 
105100       05 FILLER                  PIC X(1)  VALUE SPACE.                  
105200       05 RAD-ADPLATS-SPA         PIC X(5).                               
105300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
105400     03   RAD-IDARTNR-SPA         PIC Z(7)9.                              
105500     03   FILLER                  PIC X(1)  VALUE '-'.                    
105600     03   RAD-REKSIFFR-SPA        PIC X(1).                               
105700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
105800     03   RAD-IDPURAD-SPA         PIC Z(4).                               
105900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
106000     03   RAD-KDARTURS-SPA        PIC X(2).                               
106100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
106200     03   RAD-BEART-SPA           PIC X(11).                              
106300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
106400     03   RAD-KVBEART-Q-SPA       PIC Z(5)9.                              
106500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
106600     03   RAD-KVAVBART-SPA        PIC Z(5)9.                              
106700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
106800     03   RAD-KDFARLIG-SPA        PIC X(1).                               
106900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
107000                                                                          
107100* PURAD-NL                                                                
107200 01  PU-RAD-NL.                                                           
107300     03   RAD-IDKUNDRF-RO-URS-NL PIC X(5).                                
107400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
107500     03   RAD-ADRESS-NL.                                                  
107600       05 RAD-ADLAGOMR-NL         PIC 99.                                 
107700       05 RAD-ADGANG-NL           PIC 99.                                 
107800       05 RAD-PUNKT-NL            PIC X(1)  VALUE SPACE.                  
107900       05 RAD-ADPLATS-NL          PIC X(5).                               
108000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
108100     03   RAD-IDARTNR-NL          PIC Z(7)9.                              
108200     03   FILLER                  PIC X(1)  VALUE '-'.                    
108300     03   RAD-REKSIFFR-NL         PIC X(1).                               
108400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
108500     03   RAD-IDPURAD-NL          PIC Z(4).                               
108600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
108700     03   RAD-KDARTURS-NL         PIC X(2).                               
108800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
108900     03   RAD-BEART-NL            PIC X(11).                              
109000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
109100     03   RAD-KVBEART-Q-NL        PIC Z(5)9.                              
109200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
109300     03   RAD-KVAVBART-NL         PIC Z(5)9.                              
109400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
109500     03   RAD-KDFARLIG-NL         PIC X(1).                               
109600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
109700                                                                          
109800*LDC-GB                                                                   
109900 01  PU-RAD-LDC.                                                          
110000     03   RAD-VIPID               PIC X(10).                              
110100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
110200     03   RAD-ADRESS-LDC.                                                 
110300       05 RAD-ADLAGOMR-LDC        PIC 99.                                 
110400       05 RAD-ADGANG-LDC          PIC 99.                                 
110500       05 RAD-PUNKT-LDC           PIC X(1).                               
110600       05 RAD-ADPLATS-LDC         PIC X(5).                               
110700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
110800     03   RAD-IDARTNR-LDC         PIC Z(7)9.                              
110900     03   FILLER                  PIC X(1)  VALUE '-'.                    
111000     03   RAD-REKSIFFR-LDC        PIC X(1).                               
111100     03   RAD-IDPURAD-LDC         PIC Z(4).                               
111200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
111300     03   RAD-KDARTURS-LDC        PIC X(2).                               
111400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
111500     03   RAD-FLTILLK-LDC         PIC X(1).                               
111600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
111700     03   RAD-BEART-LDC           PIC X(11).                              
111900     03   RAD-KVAVBART-LDC        PIC Z(5)9.                              
111910     03   RAD-KVQPACK-3-LDC       PIC Z(5)9.                              
112000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
112100     03   RAD-KDFARLIG-LDC        PIC X(1).                               
112200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
112300     03   RAD-FILLERL             PIC X(4).                               
112400     03   RAD-IDSPECEMB-LDC       PIC Z(4).                               
112500                                                                          
112600 01  PU-RAD-LYNK.                                                         
112700     03   FILLER                  PIC X(18) VALUE SPACE.                  
112800     03   RAD-IDARTNR-LYNK        PIC X(10).                              
112900     03   FILLER                  PIC X(12) VALUE SPACE.                  
113000     03   RAD-BEART-LYNK          PIC X(15).                              
113100     03   FILLER                  PIC X(24) VALUE SPACE.                  
113200 01  PU-RAD-LDC-LYNK.                                                     
113300     03   FILLER                  PIC X(22) VALUE SPACE.                  
113400     03   RAD-IDARTNR-LDC-LYNK    PIC X(10).                              
113500     03   FILLER                  PIC X(11) VALUE SPACE.                  
113600     03   RAD-BEART-LDC-LYNK      PIC X(15).                              
113700     03   FILLER                  PIC X(21) VALUE SPACE.                  
113800 01  PU-TRAD1-SVE.                                                        
113900     03   FILLER                  PIC X(11) VALUE 'NETTOVIKT: '.          
114000     03   TRAD1-SVE-VKORDNTO      PIC Z(5)9.9(1).                         
114100     03   FILLER                  PIC X(6)  VALUE ' KG   '.               
114200     03   FILLER                  PIC X(7)  VALUE 'VOLYM: '.              
114300     03   TRAD1-SVE-VLORDNTO      PIC Z(3)9.9(3).                         
114400     03   FILLER                  PIC X(5)  VALUE ' M3  '.                
114500     03   FILLER                  PIC X(16) VALUE                         
114600                                  '   ANTAL RADER: '.                     
114700     03   TRAD1-SVE-KVRADER       PIC Z(4)9.                              
114800     03   FILLER                  PIC X(3)  VALUE ' ST'.                  
114900                                                                          
115000 01  PU-TRAD1-ENG.                                                        
115100     03   FILLER                  PIC X(11) VALUE 'NETWEIGHT: '.          
115200     03   TRAD1-ENG-VKORDNTO      PIC Z(5)9.9(1).                         
115300     03   FILLER                  PIC X(1)  VALUE ' '.                    
115400     03   TRAD1-ENG-VK            PIC X(3).                               
115500     03   FILLER                  PIC X(9)  VALUE ' VOLUME: '.            
115600     03   TRAD1-ENG-VLORDNTO      PIC Z(3)9.9(3).                         
115700     03   FILLER                  PIC X(1)  VALUE ' '.                    
115800     03   TRAD1-ENG-VL            PIC X(3).                               
115900     03   FILLER                  PIC X(17) VALUE                         
116000                                  'NO. ORDERLINES: '.                     
116100     03   TRAD1-ENG-KVRADER       PIC Z(4)9.                              
116200                                                                          
116300 01  PU-TRAD1-FRA.                                                        
116400     03   FILLER                  PIC X(11) VALUE ' POIDS:    '.          
116500     03   TRAD1-FRA-VKORDNTO      PIC Z(5)9.9(1).                         
116600     03   FILLER                  PIC X(5)  VALUE ' KG  '.                
116700     03   FILLER                  PIC X(8)  VALUE 'VOLUME: '.             
116800     03   TRAD1-FRA-VLORDNTO      PIC Z(3)9.9(3).                         
116900     03   FILLER                  PIC X(4)  VALUE ' M3 '.                 
117000     03   FILLER                  PIC X(17) VALUE                         
117100                                  'NBRE. DE LIGNES: '.                    
117200     03   TRAD1-FRA-KVRADER       PIC Z(4)9.                              
117300                                                                          
117400 01  PU-TRAD1-ITA.                                                        
117500     03   FILLER                  PIC X(11) VALUE '  PESO:    '.          
117600     03   TRAD1-ITA-VKORDNTO      PIC Z(5)9.9(1).                         
117700     03   FILLER                  PIC X(5)  VALUE ' KG  '.                
117800     03   FILLER                  PIC X(8)  VALUE 'VOLUME: '.             
117900     03   TRAD1-ITA-VLORDNTO      PIC Z(3)9.9(3).                         
118000     03   FILLER                  PIC X(4)  VALUE ' M3 '.                 
118100     03   FILLER                  PIC X(17) VALUE                         
118200                                  'NBRE. DE LIGNES: '.                    
118300     03   TRAD1-ITA-KVRADER       PIC Z(4)9.                              
118400                                                                          
118500 01  PU-TRAD1-SPA.                                                        
118600     03   FILLER                  PIC X(11) VALUE 'PESO NETO: '.          
118700     03   TRAD1-SPA-VKORDNTO      PIC Z(5)9.9(1).                         
118800     03   FILLER                  PIC X(5)  VALUE ' KG  '.                
118900     03   FILLER                  PIC X(8)  VALUE 'VOLUMEN:'.             
119000     03   TRAD1-SPA-VLORDNTO      PIC Z(3)9.9(3).                         
119100     03   FILLER                  PIC X(5)  VALUE ' M3  '.                
119200     03   FILLER                  PIC X(16) VALUE                         
119300                                  'NO.LINEAS PEDIDO'.                     
119400     03   TRAD1-SPA-KVRADER       PIC Z(4)9.                              
119500                                                                          
119600 01  PU-TRAD2-SVE.                                                        
119700     03   FILLER                  PIC X(11) VALUE 'TOTALVIKT: '.          
119800     03   TRAD2-SVE-VKORDNTO      PIC Z(5)9.9(1).                         
119900     03   FILLER                  PIC X(6)  VALUE ' KG   '.               
120000     03   FILLER                  PIC X(7)  VALUE 'VOLYM: '.              
120100     03   TRAD2-SVE-VLORDNTO      PIC Z(3)9.9(3).                         
120200     03   FILLER                  PIC X(5)  VALUE ' M3  '.                
120300     03   FILLER                  PIC X(16) VALUE                         
120400                                  '   ANTAL RADER: '.                     
120500     03   TRAD2-SVE-KVRADER       PIC Z(4)9.                              
120600     03   FILLER                  PIC X(3)  VALUE ' ST'.                  
120700                                                                          
120800 01  PU-TRAD2-ENG.                                                        
120900     03   FILLER                  PIC X(11) VALUE 'TOTWEIGHT: '.          
121000     03   TRAD2-ENG-VKORDNTO      PIC Z(5)9.9(1).                         
121100     03   FILLER                  PIC X(1)  VALUE ' '.                    
121200     03   TRAD2-ENG-VK            PIC X(3).                               
121300     03   FILLER                  PIC X(9)  VALUE ' VOLUME: '.            
121400     03   TRAD2-ENG-VLORDNTO      PIC Z(3)9.9(3).                         
121500     03   FILLER                  PIC X(1)  VALUE ' '.                    
121600     03   TRAD2-ENG-VL            PIC X(3).                               
121700     03   FILLER                  PIC X(17) VALUE                         
121800                                  ' NO. ORDERLINES: '.                    
121900     03   TRAD2-ENG-KVRADER       PIC Z(4)9.                              
122000                                                                          
122100 01  PU-TRAD2-FRA.                                                        
122200     03   FILLER                  PIC X(9)  VALUE 'POIDS:   '.            
122300     03   TRAD2-FRA-VKORDNTO      PIC Z(5)9.9(1).                         
122400     03   FILLER                  PIC X(5)  VALUE ' KG  '.                
122500     03   FILLER                  PIC X(8)  VALUE 'VOLUME: '.             
122600     03   TRAD2-FRA-VLORDNTO      PIC Z(3)9.9(3).                         
122700     03   FILLER                  PIC X(4)  VALUE ' M3 '.                 
122800     03   FILLER                  PIC X(17) VALUE                         
122900                                  'NBRE. DE LIGNES: '.                    
123000     03   TRAD2-FRA-KVRADER       PIC Z(4)9.                              
123100                                                                          
123200 01  PU-TRAD2-ITA.                                                        
123300     03   FILLER                  PIC X(12) VALUE 'PESO TOTALE:'.         
123400     03   TRAD2-ITA-VKORDNTO      PIC Z(5)9.9(1).                         
123500     03   FILLER                  PIC X(5)  VALUE ' KG  '.                
123600     03   FILLER                  PIC X(8)  VALUE 'VOLUME: '.             
123700     03   TRAD2-ITA-VLORDNTO      PIC Z(3)9.9(3).                         
123800     03   FILLER                  PIC X(4)  VALUE ' M3 '.                 
123900     03   FILLER                  PIC X(18) VALUE                         
124000                                  'N§ LINEA D ORDINE:'.                   
124100     03   TRAD2-ITA-KVRADER       PIC Z(4)9.                              
124200                                                                          
124300 01  PU-TRAD2-SPA.                                                        
124400     03   FILLER                  PIC X(11) VALUE 'PESO TOTAL:'.          
124500     03   TRAD2-SPA-VKORDNTO      PIC Z(5)9.9(1).                         
124600     03   FILLER                  PIC X(5)  VALUE ' KG  '.                
124700     03   FILLER                  PIC X(8)  VALUE 'VOLUMEN:'.             
124800     03   TRAD2-SPA-VLORDNTO      PIC Z(3)9.9(3).                         
124900     03   FILLER                  PIC X(5)  VALUE ' M3  '.                
125000     03   FILLER                  PIC X(16) VALUE                         
125100                                  'NO.LINEAS PEDIDO'.                     
125200     03   TRAD2-SPA-KVRADER       PIC Z(4)9.                              
125300                                                                          
125400 01  PU-TRAD2-OST.                                                        
125500     03   FILLER                  PIC X(13) VALUE 'NETTOGEWICHT:'.        
125600     03   TRAD2-OST-VKORDNTO      PIC Z(5)9.9(1).                         
125700     03   FILLER                  PIC X(4)  VALUE ' KG '.                 
125800     03   FILLER                  PIC X(8)  VALUE 'VOLUMEN:'.             
125900     03   TRAD2-OST-VLORDNTO      PIC Z(3)9.9(3).                         
126000     03   FILLER                  PIC X(4)  VALUE ' M3 '.                 
126100     03   FILLER                  PIC X(14) VALUE                         
126200                                  'ANZAHL ZEILEN:'.                       
126300     03   TRAD2-OST-KVRADER       PIC Z(4)9.                              
126400                                                                          
126500 01  PU-TRAD3X-FRA.                                                       
126600     03   FILLER                  PIC X(38)                               
126700          VALUE '(CANAL DE PRODUCTION SUPPLEMENTAIRE.)'.                  
126800     03   FILLER                  PIC X(42).                              
126900                                                                          
127000 01  PU-TRAD3X-ITA.                                                       
127100     03   FILLER                  PIC X(39)                               
127200          VALUE '(CANALE DI PRODUZIONE SUPPLEMENTAIRE.)'.                 
127300     03   FILLER                  PIC X(41).                              
127400                                                                          
127500 01  PU-TRAD3X-SPA.                                                       
127600     03   FILLER                  PIC X(39)                               
127700          VALUE '(CANAL DE PRODUCTION SUPPLEMENTAIRE.)'.                  
127800     03   FILLER                  PIC X(41).                              
127900                                                                          
128000 01  PU-TRAD3.                                                            
128100     03   FILLER                  PIC X(9)  VALUE 'PICK-UP: '.            
128200     03   TRAD3-INFO.                                                     
128300       05 TRAD3-IDPRC-1           PIC X(4).                               
128400       05 FILLER                  PIC X(1)  VALUE SPACE.                  
128500       05 TRAD3-VLORDNTO-1        PIC Z(3)9.9(3).                         
128600       05 FILLER                  PIC X(1)  VALUE SPACE.                  
128700       05 TRAD3-KDSORT-1          PIC X(3).                               
128800       05 FILLER                  PIC X(2)  VALUE SPACE.                  
128900       05 TRAD3-IDPRODNR-1        PIC Z(6)9.                              
129000       05 TRAD3-STRECK-1          PIC X(1).                               
129100       05 TRAD3-IDPLKLST-1        PIC 9(3).                               
129200       05 FILLER                  PIC X(09) VALUE SPACE.                  
129300       05 TRAD3-IDPRC-2           PIC X(4).                               
129400       05 FILLER                  PIC X(1)  VALUE SPACE.                  
129500       05 TRAD3-VLORDNTO-2        PIC Z(3)9.9(3).                         
129600       05 FILLER                  PIC X(1)  VALUE SPACE.                  
129700       05 TRAD3-KDSORT-2          PIC X(3).                               
129800       05 FILLER                  PIC X(1)  VALUE SPACE.                  
129900       05 TRAD3-IDPRODNR-2        PIC Z(6)9.                              
130000       05 TRAD3-STRECK-2          PIC X(1).                               
130100       05 TRAD3-IDPLKLST-2        PIC 9(3).                               
130200                                                                          
130300 01  PU-TRAD4-SVE.                                                        
130400     03   FILLER                  PIC X(26) VALUE                         
130500                                 'RADER PÅ ÖVRIGA LAGOMR.'.               
130600     03   TRAD4-SVE-LAGOMRADE.                                            
130700       05 FILLER                  OCCURS 6.                               
130800          07 TRAD4-SVE-ADLAGOMR   PIC ZZB.                                
130900          07 TRAD4-SVE-ANTAL      PIC ZZZ.                                
131000          07 TRAD4-SVE-KOMMA      PIC X(2).                               
131100                                                                          
131200 01  PU-TRAD4-ENG.                                                        
131300     03   FILLER                  PIC X(26) VALUE                         
131400                                 'ORDERLINES AT MISC. AREAS'.             
131500     03   TRAD4-ENG-LAGOMRADE.                                            
131600       05 FILLER                  OCCURS 6.                               
131700          07 TRAD4-ENG-ADLAGOMR   PIC ZZB.                                
131800          07 TRAD4-ENG-ANTAL      PIC ZZZ.                                
131900          07 TRAD4-ENG-KOMMA      PIC X(2).                               
132000                                                                          
132100 01  PU-TRAD4-FRA.                                                        
132200     03   FILLER                  PIC X(37) VALUE                         
132300                         'LIGNES DANS D AUTRES ZONES L ENTREPOT'.         
132400     03   TRAD4-FRA-LAGOMRADE.                                            
132500       05 FILLER                  OCCURS 6.                               
132600          07 TRAD4-FRA-ADLAGOMR   PIC ZZB.                                
132700          07 TRAD4-FRA-ANTAL      PIC ZZZ.                                
132800          07 TRAD4-FRA-KOMMA      PIC X(2).                               
132900                                                                          
133000 01  PU-TRAD4-ITA.                                                        
133100     03   FILLER                  PIC X(37) VALUE                         
133200                         'LINEE D ORDINE DI AREE MISTE.'.                 
133300     03   TRAD4-ITA-LAGOMRADE.                                            
133400       05 FILLER                  OCCURS 6.                               
133500          07 TRAD4-ITA-ADLAGOMR   PIC ZZB.                                
133600          07 TRAD4-ITA-ANTAL      PIC ZZZ.                                
133700          07 TRAD4-ITA-KOMMA      PIC X(2).                               
133800                                                                          
133900 01  PU-TRAD4-SPA.                                                        
134000     03   FILLER                  PIC X(26) VALUE                         
134100                         'LINEAS EN ZONAS DISTINTAS'.                     
134200     03   TRAD4-SPA-LAGOMRADE.                                            
134300       05 FILLER                  OCCURS 6.                               
134400          07 TRAD4-SPA-ADLAGOMR   PIC ZZB.                                
134500          07 TRAD4-SPA-ANTAL      PIC ZZZ.                                
134600          07 TRAD4-SPA-KOMMA      PIC X(2).                               
134700                                                                          
134800 01  PU-TRAD4-OST.                                                        
134900     03   FILLER                  PIC X(35) VALUE                         
135000                      'ZEILEN AUF UBRIGE PRODUKTION CANAL.'.              
135100     03   TRAD4-OST-LAGOMRADE.                                            
135200       05 FILLER                  OCCURS 6.                               
135300          07 TRAD4-OST-ADLAGOMR   PIC ZZB.                                
135400          07 TRAD4-OST-ANTAL      PIC ZZZ.                                
135500          07 TRAD4-OST-KOMMA      PIC X(2).                               
135600                                                                          
135700 01  PU-TRAD5-SVE.                                                        
135800     03   FILLER                  PIC X(50) VALUE                         
135900                                 'DENNA ORDER SAKNAS PÅ HUVUDPRC'.        
136000                                                                          
136100 01  PU-TRAD5-ENG.                                                        
136200     03   FILLER                  PIC X(50) VALUE                         
136300                             'THIS ORDER IS MISSING ON MAIN PRC'.         
136400                                                                          
136500 01  PU-TRAD5-FRA.                                                        
136600     03   FILLER                  PIC X(58) VALUE                         
136700     'CETTE COMMANDE MANGE DANS LE CANAL DE PRODUCTION PRIMAIRE'.         
136800                                                                          
136900 01  PU-TRAD5-ITA.                                                        
137000     03   FILLER                  PIC X(14) VALUE                         
137100                                  'QUESTO ORDINE'.                        
137200                                                                          
137300 01  PU-TRAD5-SPA.                                                        
137400     03   FILLER                  PIC X(50) VALUE                         
137500     'ESTE PEDIDO FALTA EN CANAL DE PRODUCION MAYOR.'.                    
137600                                                                          
137700 01  PU-TRAD5-OST.                                                        
137800     03   FILLER                  PIC X(50) VALUE                         
137900     'DIESE ORDER FEHLEN AUF HAUPT PRC.         '.                        
138000                                                                          
138100 01  PU-TRAD6-SVE.                                                        
138200     03   FILLER                  PIC X(50) VALUE                         
138300                        'PACKAS I EGET KOLLI.'.                           
138400                                                                          
138500 01  PU-TRAD6-ENG.                                                        
138600     03   FILLER                  PIC X(50) VALUE                         
138700                        'PACK IN SEPARATE CASE.'.                         
138800 01  PU-TRAD6-FRA.                                                        
138900     03   FILLER                  PIC X(50) VALUE                         
139000                        'EMBALLER DANS UNE CAISSE UNIQE.'.                
139100                                                                          
139200 01  PU-TRAD6-ITA.                                                        
139300     03   FILLER                  PIC X(50) VALUE                         
139400                        'IMBALLATO IN PIÉ CASSE.'.                        
139500                                                                          
139600 01  PU-TRAD6-SPA.                                                        
139700     03   FILLER                  PIC X(50) VALUE                         
139800                        'EMBAQUETAR EN COLLI SEPARADA.'.                  
139900                                                                          
140000 01  PU-TRAD6-OST.                                                        
140100     03   FILLER                  PIC X(50) VALUE                         
140200                        'PACKEN DARIN EIGEN KOLLI.    '.                  
140300                                                                          
140400 01  PU-PRAD1-SVE.                                                        
140500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
140600     03   FILLER                  PIC X(9)  VALUE 'KOLLINR'.              
140700     03   FILLER                  PIC X(10) VALUE 'KOLLIKOD'.             
140800     03   FILLER                  PIC X(12) VALUE 'BR. VIKT'.             
140900     03   FILLER                  PIC X(14) VALUE 'KOLLIADRESS'.          
141000     03   FILLER                  PIC X(9)  VALUE 'EMB.TYP'.              
141100     03   FILLER                  PIC X(7)  VALUE 'LÄNGD'.                
141200     03   FILLER                  PIC X(7)  VALUE 'BREDD'.                
141300     03   FILLER                  PIC X(7)  VALUE 'HÖJD'.                 
141400                                                                          
141500 01  PU-PRAD1-ENG.                                                        
141600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
141700     03   FILLER                  PIC X(9)  VALUE 'CASENO.'.              
141800     03   FILLER                  PIC X(10) VALUE 'CASECODE'.             
141900     03   FILLER                  PIC X(12) VALUE 'GROSS WEIGHT'.         
142000     03   FILLER                  PIC X(14) VALUE 'CASE ADDRESS'.         
142100     03   FILLER                  PIC X(9)  VALUE SPACE.                  
142200     03   FILLER                  PIC X(7)  VALUE '  L  '.                
142300     03   FILLER                  PIC X(7)  VALUE '  W  '.                
142400     03   FILLER                  PIC X(7)  VALUE '  H '.                 
142500                                                                          
142600 01  PU-PRAD1-FRA.                                                        
142700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
142800     03   FILLER                  PIC X(7)  VALUE 'LIVREE.'.              
142900     03   FILLER                  PIC X(10) VALUE 'LIVREE CDE'.           
143000     03   FILLER                  PIC X(10) VALUE ' POIDS    '.           
143100     03   FILLER                PIC X(14) VALUE 'LIVREE ADRESSE'.         
143200     03   FILLER                  PIC X(9)  VALUE SPACE.                  
143300     03   FILLER                  PIC X(7)  VALUE '  L  '.                
143400     03   FILLER                  PIC X(7)  VALUE '  W  '.                
143500     03   FILLER                  PIC X(7)  VALUE '  H '.                 
143600                                                                          
143700 01  PU-PRAD1-ITA.                                                        
143800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
143900     03   FILLER                  PIC X(9)  VALUE 'CASSA N§ '.            
144000     03   FILLER                 PIC X(13) VALUE ' CODICE CASSA'.         
144100     03   FILLER                  PIC X(10) VALUE ' PESO  '.              
144200     03   FILLER               PIC X(15) VALUE 'CASSA INDIRIZZO'.         
144300     03   FILLER                  PIC X(9)  VALUE SPACE.                  
144400     03   FILLER                  PIC X(7)  VALUE 'LUNGHEZ'.              
144500     03   FILLER                  PIC X(7)  VALUE 'PROFOND'.              
144600     03   FILLER                  PIC X(7)  VALUE 'ALTEZZA'.              
144700                                                                          
144800 01  PU-PRAD1-SPA.                                                        
144900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
145000     03   FILLER                  PIC X(9)  VALUE 'NO COLLI'.             
145100     03   FILLER                  PIC X(10) VALUE 'COLLI CODE'.           
145200     03   FILLER                  PIC X(12) VALUE 'PESO BRUTTO '.         
145300     03   FILLER             PIC X(16) VALUE 'UBAICACION COLLI'.          
145400     03   FILLER                  PIC X(7)  VALUE SPACE.                  
145500     03   FILLER                  PIC X(7)  VALUE 'LARGO'.                
145600     03   FILLER                  PIC X(7)  VALUE 'ANCHO'.                
145700     03   FILLER                  PIC X(7)  VALUE ' ALTO'.                
145800                                                                          
145900 01  PU-PRAD2.                                                            
146000     03   FILLER                  PIC X(10) VALUE SPACE.                  
146100     03   FILLER                  PIC X(1)  VALUE 'I'.                    
146200     03   FILLER                  PIC X(9)  VALUE SPACE.                  
146300     03   FILLER                  PIC X(1)  VALUE 'I'.                    
146400     03   FILLER                  PIC X(11) VALUE SPACE.                  
146500     03   FILLER                  PIC X(1)  VALUE 'I'.                    
146600     03   FILLER                  PIC X(13) VALUE SPACE.                  
146700     03   FILLER                  PIC X(1)  VALUE 'I'.                    
146800     03   FILLER                  PIC X(8)  VALUE SPACE.                  
146900     03   FILLER                  PIC X(1)  VALUE 'I'.                    
147000     03   FILLER                  PIC X(6)  VALUE SPACE.                  
147100     03   FILLER                  PIC X(1)  VALUE 'I'.                    
147200     03   FILLER                  PIC X(6)  VALUE SPACE.                  
147300     03   FILLER                  PIC X(1)  VALUE 'I'.                    
147400     03   FILLER                  PIC X(5)  VALUE SPACE.                  
147500     03   FILLER                  PIC X(1)  VALUE 'I'.                    
147600                                                                          
147700 01  PU-PRAD3-SVE.                                                        
147800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
147900     03   FILLER                  PIC X(4)  VALUE 'RAD '.                 
148000     03   FILLER                  PIC X(6)  VALUE 'ANTAL '.               
148100     03   FILLER                  PIC X(5)  VALUE 'FROM '.                
148200     03   FILLER                  PIC X(5)  VALUE 'TOM  '.                
148300     03   FILLER                  PIC X(6)  VALUE 'ANTAL '.               
148400     03   FILLER                  PIC X(6)  VALUE 'KOLLI '.               
148500     03   FILLER                  PIC X(10) VALUE SPACE.                  
148600     03   FILLER                  PIC X(4)  VALUE 'RAD '.                 
148700     03   FILLER                  PIC X(6)  VALUE 'ANTAL '.               
148800     03   FILLER                  PIC X(5)  VALUE 'FROM '.                
148900     03   FILLER                  PIC X(5)  VALUE 'TOM  '.                
149000     03   FILLER                  PIC X(6)  VALUE 'ANTAL '.               
149100     03   FILLER                  PIC X(6)  VALUE 'KOLLI '.               
149200                                                                          
149300 01  PU-PRAD4-SVE.                                                        
149400     03   FILLER                  PIC X(21) VALUE SPACE.                  
149500     03   FILLER                  PIC X(1)  VALUE 'I'.                    
149600     03   FILLER                  PIC X(5)  VALUE SPACE.                  
149700     03   FILLER                  PIC X(1)  VALUE 'I'.                    
149800     03   FILLER                  PIC X(5)  VALUE SPACE.                  
149900     03   FILLER                  PIC X(1)  VALUE 'I'.                    
150000     03   FILLER                  PIC X(29) VALUE SPACE.                  
150100     03   FILLER                  PIC X(1)  VALUE 'I'.                    
150200     03   FILLER                  PIC X(5)  VALUE SPACE.                  
150300     03   FILLER                  PIC X(1)  VALUE 'I'.                    
150400     03   FILLER                  PIC X(5)  VALUE SPACE.                  
150500     03   FILLER                  PIC X(1)  VALUE 'I'.                    
150600                                                                          
150700*LDC-SE                                                                   
150800                                                                          
150900 01  PU-LDC-SE-RAD-ASTERIX.                                               
151000     03 FILLER                       PIC X(5)   VALUE SPACE.              
151100     03 LDC-SE-RAD-ASTERIXDEL1       PIC X(35)  VALUE                     
151200        '***********************************'.                            
151300     03 LDC-SE-RAD-ASTERIXDEL2       PIC X(34)  VALUE                     
151400        '**********************************'.                             
151500     03 FILLER                       PIC X(1)   VALUE SPACE.              
151600                                                                          
151700 01  PU-LDC-SE-RAD1.                                                      
151800     03 FILLER                       PIC X(5)   VALUE SPACE.              
151900     03 LDC-SE-RAD-TEXT-1            PIC X(39)  VALUE                     
152000        'ORDERN INNEHÅLLER DELAR SOM KOMMER FRÅN'.                        
152100     03 LDC-SE-RAD-TEXT-2            PIC X(30)  VALUE                     
152200        ' DIREKT-LEVERANTÖR ELLER CDC:'.                                  
152300     03 FILLER                       PIC X(7)   VALUE SPACE.              
152400                                                                          
152500 01  PU-LDC-SE-CDC-RAD.                                                   
152600     03 FILLER                       PIC X(10)  VALUE SPACE.              
152700     03 LDC-SE-RAD-CDC-TEXT-1        PIC X(40)  VALUE                     
152800        'OBS! PACKA MED ORDER-RADER FRÅN CDC.    '.                       
152900     03 LDC-SE-RAD-CDC-TEXT-2        PIC X(33)  VALUE                     
153000        '                                 '.                              
153100     03 FILLER                       PIC X(1)   VALUE SPACE.              
153200                                                                          
153300 01  PU-LDC-SE-DIR-RAD.                                                   
153400     03 FILLER                       PIC X(10)  VALUE SPACE.              
153500     03 LDC-SE-RAD-DIR-TEXT          PIC X(49)  VALUE                     
153600        'OBS! PACKA MED ORDER-RAD FRÅN DIREKT LEV:  '.                    
153700     03 LDC-SE-RAD-DIR-LEV-NAMN      PIC X(15).                           
153800     03 FILLER                       PIC X(6)   VALUE SPACE.              
153900                                                                          
154000 01  PU-LDC-SE-TOM-RAD.                                                   
154100     03 FILLER                       PIC X(10)  VALUE SPACE.              
154200     03 LDC-SE-RAD-TOM-RAD-DEL1      PIC X(35)  VALUE                     
154300        '                                   '.                            
154400     03 LDC-SE-RAD-TOM-RAD-DEL2      PIC X(34)  VALUE                     
154500        '                                  '.                             
154600     03 FILLER                       PIC X(1)   VALUE SPACE.              
154700                                                                          
154800*LDC-GB                                                                   
154900 01  PU-LDC-RAD0.                                                         
155000     03 LDC-RAD-0-ASTERIX            PIC X(55) VALUE                      
155100     '****** START OF VIP REFERENCE SUMMARY LIST ************'.           
155200     03 FILLER                       PIC X(77) VALUE SPACE.               
155300                                                                          
155400 01  PU-LDC-RAD1-RUBRIK.                                                  
155500     03 FILLER                       PIC X(6)   VALUE SPACE.              
155600     03 LDC-RAD1-VIPNR               PIC X(10)  VALUE 'VIP REF.'.         
155700     03 FILLER                       PIC X(3)   VALUE SPACE.              
155800     03 LDC-RAD1-LINES               PIC X(5)   VALUE 'LINES'.            
155900     03 FILLER                       PIC X(108) VALUE SPACE.              
156000                                                                          
156100 01  PU-LDC-RUBRIK-STRECK.                                                
156200     03 FILLER                       PIC X(1)   VALUE SPACE.              
156300     03 LDC-RAD1-STRECKAD-RAD        PIC X(53)  VALUE                     
156400      '-----------------------------------------------------'.            
156500     03 FILLER                       PIC X(78)  VALUE SPACE.              
156600                                                                          
156700 01  PU-LDC-DETALJ-RAD.                                                   
156800     03 FILLER                       PIC X(5)   VALUE SPACE.              
156900     03 LDC-RAD-BERADREF             PIC X(10).                           
157000     03 FILLER                       PIC X(2)   VALUE SPACE.              
157100     03 LDC-RAD-ANTAL-ART            PIC Z(4)9.                           
157200     03 FILLER                       PIC X(110) VALUE SPACE.              
157300                                                                          
157400 01  PU-LDC-SISTA-RAD.                                                    
157500     03 LDC-SISTARAD-ASTERIX         PIC X(55)  VALUE                     
157600     '****** END OF VIP REFERENCE SUMMARY LIST **************'.           
157700     03 FILLER                       PIC X(77)  VALUE SPACE.              
157800                                                                          
157900     EJECT                                                                
158000*    --- IMS FUNKTIONSKODER                                               
158100*01  -COPY W0003                                                          
158200     EJECT                                                                
158300*    ---  DLI INPUT-OUTPUT AREA                                           
158400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
158500                                                                          
158600 01  DLI-IO-AREA1.                                                        
158700     03  WL400711.                                                        
158800*        05  -COPY WDGX4008                                               
158900     EJECT                                                                
159000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
159100                                                                          
159200 01  DLI-IO-AREA2.                                                        
159300*    03  -COPY WDQ201                                                     
159400     EJECT                                                                
159500*    03  -COPY WDQ212                                                     
159600     EJECT                                                                
159700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
159800                                                                          
159900 01  DLI-IO-AREA3.                                                        
160000     03  IO-AREA3                PIC X(200)  VALUE SPACE.                 
160100                                                                          
160200     03  WLXXKH01 REDEFINES IO-AREA3.                                     
160300*        05 -COPY WDGX4447                                                
160400     EJECT                                                                
160500     03  WLXXKH11 REDEFINES IO-AREA3.                                     
160600*        05 -COPY WDGX4448                                                
160700     EJECT                                                                
160800     03  WLXXKU01 REDEFINES IO-AREA3.                                     
160900*        05 -COPY WDGX4535                                                
161000     EJECT                                                                
161100     03  WLXXKU11 REDEFINES IO-AREA3.                                     
161200*        05 -COPY WDGX4536                                                
161300     EJECT                                                                
161400     03  WL473201 REDEFINES IO-AREA3.                                     
161500*        05 -COPY WDGX473B                                                
161600     EJECT                                                                
161700     03  WL473211 REDEFINES IO-AREA3.                                     
161800*        05 -COPY WDGX4732                                                
161900     EJECT                                                                
162000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
162100                                                                          
162200 01  DLI-IO-AREA4.                                                        
162300     03  IO-AREA4                PIC X(200)  VALUE SPACE.                 
162400                                                                          
162500     03  WLORQA01 REDEFINES IO-AREA4.                                     
162600*        05 -COPY WDQ301                                                  
162700     EJECT                                                                
162800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
162900                                                                          
163000 01  DLI-IO-AREA5.                                                        
163100     03  WL400721.                                                        
163200*        05 -COPY WDGX4010                                                
163300     EJECT                                                                
163400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
163500                                                                          
163600 01  DLI-IO-AREA6.                                                        
163700     03  IO-AREA6                PIC X(50)   VALUE SPACE.                 
163800                                                                          
163900*    03  WLXXKB01 -COPY WDGX4433   -RED IO-AREA6.                         
164000     EJECT                                                                
164100*    03  WLXXKB11 -COPY WDGX4434   -RED IO-AREA6.                         
164200     EJECT                                                                
164300                                                                          
164400 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA '.        
164500 01  DLI-IO-AREA-3.                                                       
164600     03  WLGMTA01.                                                        
164700*        05  -COPY WDB201                                                 
164800     EJECT                                                                
164900 01  FILLER                      PIC X(16)  VALUE '4017-AREA'.            
165000*01  -COPY WDGX4017                                                       
165100     SKIP2                                                                
165200 01  FILLER                      PIC X(16)  VALUE '4018-AREA'.            
165300*01  -COPY WDGX4018                                                       
165400     EJECT                                                                
165500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
165600 01   DLI-IO-AREA-B601.                                                   
165700*     03  -COPY WDB601                                                    
165800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF502'.                      
165900 01  DLI-IO-WDF502.                                                       
166000*    03  -COPY WDF502                                                     
166100     EJECT                                                                
166200 LINKAGE SECTION.                                                         
166300                                                                          
166400*01  -COPY W0009      -PRE MSG-                                           
166500     EJECT                                                                
166600*01  -COPY W0009      -PRE ALT1-                                          
166700     EJECT                                                                
166800*01  -COPY W0009      -PRE ALT2-                                          
166900     EJECT                                                                
167000*01  -COPY W0009      -PRE ALT3-                                          
167100     EJECT                                                                
167200*01  -COPY W0009      -PRE USEA-                                          
167300     EJECT                                                                
167400*01  -COPY W0008      -PRE LISB-                                          
167500     05  FILLER                  PIC X.                                   
167600     EJECT                                                                
167700*01  -COPY W0008      -PRE 4007-                                          
167800     05  FILLER                  PIC X.                                   
167900     EJECT                                                                
168000*01  -COPY W0008      -PRE ORQI-                                          
168100     05  FILLER                  PIC X.                                   
168200     EJECT                                                                
168300*01  -COPY W0008      -PRE ORQA-                                          
168400     05  FILLER                  PIC X.                                   
168500     EJECT                                                                
168600*01  -COPY W0008      -PRE XXKH-                                          
168700     05  FILLER                  PIC X.                                   
168800     EJECT                                                                
168900*01  -COPY W0008      -PRE XXKU-                                          
169000     05  FILLER                  PIC X.                                   
169100     EJECT                                                                
169200*01  -COPY W0008      -PRE 4732-                                          
169300     05  FILLER                  PIC X.                                   
169400     EJECT                                                                
169500*01  -COPY W0008      -PRE XXKB-                                          
169600     05  FILLER                  PIC X.                                   
169700     EJECT                                                                
169800*01  -COPY W0008      -PRE GMTA-                                          
169900     05  FILLER                  PIC X.                                   
170000     EJECT                                                                
170100*01  -COPY W0008      -PRE 4017-                                          
170200     05  FILLER                  PIC X.                                   
170300     EJECT                                                                
170400*01  -COPY W0008      -PRE WDB6-                                          
170500     05  FILLER                  PIC X.                                   
170600     EJECT                                                                
170700*01  -COPY W0008      -PRE WDF5-                                          
170800     05  FILLER                  PIC X.                                   
170900     EJECT                                                                
171000 PROCEDURE DIVISION  USING MSG-PCB  ALT1-PCB ALT2-PCB ALT3-PCB            
171100                           USEA-PCB LISB-PCB 4007-PCB                     
171200                           ORQI-PCB ORQA-PCB XXKH-PCB XXKU-PCB            
171300                           4732-PCB XXKB-PCB GMTA-PCB 4017-PCB            
171400                           WDB6-PCB WDF5-PCB.                             
171500                                                                          
171600     ENTRY 'DLITCBL' USING MSG-PCB  ALT1-PCB ALT2-PCB ALT3-PCB            
171700                           USEA-PCB LISB-PCB 4007-PCB                     
171800                           ORQI-PCB ORQA-PCB XXKH-PCB XXKU-PCB            
171900                           4732-PCB XXKB-PCB GMTA-PCB 4017-PCB            
172000                           WDB6-PCB WDF5-PCB.                             
172100                                                                          
172200     PERFORM IMS-GU-MSG                                                   
172300     IF SEGMENT-FINNS                                                     
172400        PERFORM A-INIT                                                    
172500        IF ALLT-OK                                                        
172600           PERFORM B-LAES-PLOCKSATS                                       
172700           PERFORM C-BEHANDLA-PURADER                                     
172800           PERFORM D-UPPDAT-PLOCKSATS                                     
172900           PERFORM E-SKICKA-IMSTRANS                                      
173000        END-IF                                                            
173100     END-IF                                                               
173200                                                                          
173300     MOVE ZERO TO RETURN-CODE                                             
173400     GOBACK                                                               
173500     .                                                                    
173600     EJECT                                                                
173700 A-INIT SECTION.                                                          
173800                                                                          
173900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
174000     MOVE '013'             TO MSGI-KDCALL                                
174100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
174200     MOVE '4375'            TO MSGI-IDTRANS                               
174300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
174400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
174500     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
174600     MOVE 1                 TO WS-VIP-LOPNR                               
174700     MOVE ZERO              TO LDC-TOT-ANT-ART                            
174800                                                                          
174900     MOVE JA  TO ALLT-SW                                                  
175000                 FORSTA-LIST-SIDA-SW                                      
175100                 INITIERA-LDC-TABELL                                      
175200     MOVE NEJ TO PU-SW                                                    
175300                 NY-PRINTER-SW                                            
175400                 PLOCKSATS-KLAR-SW                                        
175500                 VIP-ID-HITTAT-SW                                         
175600                 SKRIV-EN-GANG-SW                                         
175700                 SW-SKRIV-INTE-RAD14                                      
175800                                                                          
175900     MOVE SPACE      TO SPAR-PRINTER                                      
176000                        SPAR-KDSS-PU                                      
176100                        RAD-FILLER1                                       
176200                        RAD-FILLER2                                       
176300                        RAD-FILLER3                                       
176400                        RAD-FILLER6                                       
176500                        RAD-FILLER7                                       
176600                        RAD-FILLER9                                       
176700                        RAD-FILLER10                                      
176800                        RAD-FILLER11                                      
176900                        RAD-FILLER12                                      
176910                        RAD-FILLER13                                      
176920                        RAD-FILLER14                                      
176930                        RAD-FILLER15                                      
176940                        RAD-FILLER16                                      
177000                        RAD-IDKUNDRF-RO-URS                               
177100                        RAD-IDKUNDRF-RO-URS-SPA                           
177200                        RAD-IDKUNDRF-RO-URS-NL                            
177300                                                                          
177400     MOVE 999999999  TO SPAR-IDORDER                                      
177500     MOVE +100       TO LDC-TAB-IX-MAX                                    
177600                                                                          
177700     IF MSG-KDTRANS-1  NOT = 'W4T377X '                                   
177800        MOVE NEJ TO ALLT-SW                                               
177900                    ALLT-NOK1                                             
178000     END-IF                                                               
178100                                                                          
178200     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
178300     IF NOT GODK-MID                                                      
178400        MOVE NEJ TO ALLT-SW                                               
178500                    ALLT-NOK2                                             
178600     END-IF                                                               
178700                                                                          
178800     MOVE '999'                          TO W-4010-KDPRT-MAX              
178900                                                                          
179000     IF ALLT-OK                                                           
179100        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I37701                  
179200        MOVE MSG-KDMFSFOR-1              TO PTOP1-KDMFSFOR                
179300                                            PTOP2-KDMFSFOR                
179400                                                                          
179500        MOVE MID-IDPRC                   TO WS-PU-IDPRC                   
179600        MOVE '-'                         TO WS-PU-STRECK                  
179700        MOVE MID-IDLOPNR                 TO WS-PU-IDLOPNR                 
179800     END-IF                                                               
179900     .                                                                    
180000     EJECT                                                                
180100 B-LAES-PLOCKSATS SECTION.                                                
180200     MOVE 'STA B-LAES-PLOCKSATS'         TO PGMPOS                        
180300                                                                          
180400     MOVE MID-IDPRODNR  TO W-4001-IDPRODNR                                
180500     MOVE MID-IDPLKLST  TO W-4001-IDPLKLST                                
180600                                                                          
180700     PERFORM IMS-GHU-4007-WL400711                                        
180800                                                                          
180900     IF 4008-NYCKEL-GRP NOT = LOW-VALUE                                   
181000        MOVE 4008-NYCKEL-GRP     TO W-4010-IDHTYP-X                       
181100        MOVE 4008-IDDC           TO WS-IDDC                               
181200        PERFORM BA-GET-USER                                               
181300        PERFORM IMS-GNP-4007-WL400721-KVAL                                
181400        MOVE 4010-IDORDER        TO SPAR-IDORDER                          
181500        MOVE 4010-IDPRC          TO SPAR-IDPRC                            
181600        MOVE NEJ                 TO FORSTA-SIDA-SW                        
181700        PERFORM S01-BRYT-PRINTER                                          
181800        PERFORM S05-SKAPA-PACKHUVUD                                       
181900        PERFORM S10-EV-INITIERA-LDC-TAB                                   
182000     END-IF                                                               
182100     MOVE 'END B-LAES-PLOCKSATS'         TO PGMPOS                        
182200     .                                                                    
182300     SKIP2                                                                
182400 BA-GET-USER               SECTION.                                       
182500                                                                          
182600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
182700     MOVE '013'             TO MSGI-KDCALL                                
182800     MOVE WS-IDDC           TO IDDC-XX                                    
182900     MOVE IDDC-USER         TO MSGI-IDUSER                                
183000     MOVE '4377'            TO MSGI-IDTRANS                               
183100                                                                          
183200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
183300     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
183400     .                                                                    
183500     SKIP2                                                                
183600 C-BEHANDLA-PURADER SECTION.                                              
183700                                                                          
183800     PERFORM UNTIL SID-IX > MAX-SIDA                                      
183900        PERFORM CA-LAES-PACKRAD                                           
184000                                                                          
184100        IF PLOCKSATS-EJ-KLAR                                              
184200                                                                          
184300           IF SPAR-IDORDER NOT = 4010-IDORDER                             
184400              MOVE JA TO NY-ORDER-SW                                      
184500              PERFORM CC-TOTAL-PRINTER                                    
184600              PERFORM CB-TOTAL-IDORDER                                    
184700              MOVE 4010-KDSS-PU TO SPAR-KDSS-PU                           
184800           ELSE                                                           
184900                                                                          
185000              IF SPAR-PRINTER NOT = 4010-KDPRT                            
185100                 PERFORM CC-TOTAL-PRINTER                                 
185200                 PERFORM S01-BRYT-PRINTER                                 
185300                 MOVE JA TO FORSTA-SIDA-SW                                
185400                 PERFORM S06-SKRIV-PACKHUVUD                              
185500              ELSE                                                        
185600                                                                          
185700                 IF SPAR-KDSS-PU NOT = 4010-KDSS-PU                       
185800                    PERFORM S06-SKRIV-PACKHUVUD                           
185900                    MOVE 4010-KDSS-PU TO SPAR-KDSS-PU                     
186000                 END-IF                                                   
186100              END-IF                                                      
186200           END-IF                                                         
186300           PERFORM CD-REDIGERA-DETALJRAD                                  
186400        ELSE                                                              
186500           IF PU-OPEN                                                     
186600              MOVE JA TO NY-ORDER-SW                                      
186700              PERFORM CC-TOTAL-PRINTER                                    
186800              PERFORM CB-TOTAL-IDORDER                                    
186900           END-IF                                                         
187000*LDC-GB                                                                   
187100           PERFORM CE-SKRIV-LDC-GB-INFO                                   
187200           MOVE 2000 TO SID-IX                                            
187300        END-IF                                                            
187400     END-PERFORM                                                          
187500                                                                          
187600     IF PU-OPEN                                                           
187700        PERFORM S03-CLOSE-PU                                              
187800     END-IF                                                               
187900     .                                                                    
188000     EJECT                                                                
188100 CA-LAES-PACKRAD SECTION.                                                 
188200                                                                          
188300     PERFORM IMS-GNP-4007-WL400721-OKVAL                                  
188400                                                                          
188500     IF SEGMENT-SAKNAS                                                    
188600        MOVE JA TO PLOCKSATS-KLAR-SW                                      
188700     END-IF                                                               
188800     .                                                                    
188900     EJECT                                                                
189000 CB-TOTAL-IDORDER SECTION.                                                
189100                                                                          
189200     IF SPAR-IDORDER = 999999999                                          
189300        PERFORM S01-BRYT-PRINTER                                          
189400     ELSE                                                                 
189500        PERFORM CBA-TOTAL-IDORDER                                         
189600        MOVE JA TO FORSTA-SIDA-SW                                         
189700     END-IF                                                               
189800                                                                          
189900     IF PLOCKSATS-EJ-KLAR                                                 
190000        PERFORM S05-SKAPA-PACKHUVUD                                       
190100        PERFORM S10-EV-INITIERA-LDC-TAB                                   
190200        IF 4010-KDPRT NOT = SPAR-PRINTER                                  
190300           PERFORM S01-BRYT-PRINTER                                       
190400        END-IF                                                            
190500        PERFORM S06-SKRIV-PACKHUVUD                                       
190600        MOVE 4010-IDORDER  TO SPAR-IDORDER                                
190700        MOVE 4010-IDPRC    TO SPAR-IDPRC                                  
190800     END-IF                                                               
190900     .                                                                    
191000     EJECT                                                                
191100 CBA-TOTAL-IDORDER SECTION.                                               
191200     IF SPAR-PRINTER NOT = 4008-KDPRT-PU                                  
191300        PERFORM S04-PURGE-PU                                              
191400        MOVE '4'               TO WS-SYSTDEL                              
191500        MOVE 'PU'              TO WS-LISTTYP                              
191600        MOVE 4008-KDPRT-PU     TO WS-KDPRT                                
191700                                  SPAR-PRINTER                            
191800        MOVE JA                TO FORSTA-SIDA-SW                          
191900        PERFORM S06-SKRIV-PACKHUVUD                                       
192000        MOVE PRT-AFTER-3       TO PRT-RADSKIP                             
192100     ELSE                                                                 
192200        IF RAD-IX > 53                                                    
192300           PERFORM S06-SKRIV-PACKHUVUD                                    
192400           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
192500        ELSE                                                              
192600           MOVE PRT-AFTER-2 TO PRT-RADSKIP                                
192700        END-IF                                                            
192800     END-IF                                                               
192900                                                                          
193000     MOVE 4008-IDDC                TO WS-IDDC                             
193100     IF WS-IDDC NOT = W-IDDC-B6                                           
193200        MOVE WS-IDDC TO W-IDDC-B6                                         
193300        PERFORM IMS-GU-WDB601                                             
193400     END-IF                                                               
193500                                                                          
193600     EVALUATE  TRUE                                                       
193700        WHEN  DCS-CDC                                                     
193800          OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                            
193900          MOVE 4008-VKORDNTO (2)   TO TRAD2-SVE-VKORDNTO                  
194000          MOVE 4008-VLORDNTO (2)   TO TRAD2-SVE-VLORDNTO                  
194100          MOVE 4008-KVRADER (2)    TO TRAD2-SVE-KVRADER                   
194200          MOVE PU-TRAD2-SVE        TO WS-PU-RAD                           
194300        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
194400          MOVE 4008-VKORDNTO (2)   TO TRAD2-ITA-VKORDNTO                  
194500          MOVE 4008-VLORDNTO (2)   TO TRAD2-ITA-VLORDNTO                  
194600          MOVE 4008-KVRADER (2)    TO TRAD2-ITA-KVRADER                   
194700          MOVE PU-TRAD2-ITA        TO WS-PU-RAD                           
194800        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
194900          MOVE 4008-VKORDNTO (2)   TO TRAD2-SPA-VKORDNTO                  
195000          MOVE 4008-VLORDNTO (2)   TO TRAD2-SPA-VLORDNTO                  
195100          MOVE 4008-KVRADER (2)    TO TRAD2-SPA-KVRADER                   
195200          MOVE PU-TRAD2-SPA        TO WS-PU-RAD                           
195300        WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                              
195400          MOVE 4008-VKORDNTO (2)   TO TRAD2-OST-VKORDNTO                  
195500          MOVE 4008-VLORDNTO (2)   TO TRAD2-OST-VLORDNTO                  
195600          MOVE 4008-KVRADER (2)    TO TRAD2-OST-KVRADER                   
195700          MOVE PU-TRAD2-OST        TO WS-PU-RAD                           
195800        WHEN OTHER                                                        
195900          IF US-MATT                                                      
196000             COMPUTE TRAD2-ENG-VKORDNTO = 4008-VKORDNTO (2)               
196100                                       * CONV-KG-TO-LB                    
196200             COMPUTE TRAD2-ENG-VLORDNTO = 4008-VLORDNTO (2)               
196300                                       * CONV-M3-TO-FT3                   
196400             MOVE 'FT3'            TO TRAD2-ENG-VL                        
196500             MOVE 'LBS'            TO TRAD2-ENG-VK                        
196600          ELSE                                                            
196700            MOVE 4008-VKORDNTO (2) TO TRAD2-ENG-VKORDNTO                  
196800            MOVE 4008-VLORDNTO (2) TO TRAD2-ENG-VLORDNTO                  
196900            MOVE 'M3 '             TO TRAD2-ENG-VL                        
197000            MOVE 'KG '             TO TRAD2-ENG-VK                        
197100          END-IF                                                          
197200          MOVE 4008-KVRADER (2)    TO TRAD2-ENG-KVRADER                   
197300          MOVE PU-TRAD2-ENG        TO WS-PU-RAD                           
197400     END-EVALUATE                                                         
197500                                                                          
197600     PERFORM S07-SKRIV-RAD                                                
197700                                                                          
197800       ADD PRT-RADSKIP TO RAD-IX                                          
197900                                                                          
198000     IF W-SPAR-4448-KDPRCTYP = 2                                          
198100        PERFORM CBAA-IDORDER-HUVUD-PRC                                    
198200     ELSE                                                                 
198300        IF W-SPAR-4448-KDPRCTYP = 3                                       
198400           PERFORM CBAB-IDORDER-PICKUP-PRC                                
198500        END-IF                                                            
198600     END-IF                                                               
198700                                                                          
198800     PERFORM CBAC-IDORDER-OVRIGA-LAGOMRADE                                
198900                                                                          
199000     PERFORM CBAE-IDORDER-NOLLA-ACKAR                                     
199100     .                                                                    
199200     EJECT                                                                
199300 CBAA-IDORDER-HUVUD-PRC SECTION.                                          
199400                                                                          
199500     MOVE JA TO PICK-UP-OK-SW                                             
199600                                                                          
199700     MOVE LOW-VALUE   TO W-WDQ301KY-MIN-X                                 
199800     MOVE HIGH-VALUE  TO W-WDQ301KY-MAX-X                                 
199900                                                                          
200000     MOVE WS-IDORDER  TO W-Q301-MIN-IDORDER                               
200100                         W-Q301-MAX-IDORDER                               
200200     MOVE WS-IDDC     TO W-Q301-MIN-IDDC                                  
200300                         W-Q301-MAX-IDDC                                  
200400     MOVE WS-IDPRODNR TO W-Q301-MIN-IDPRODNR                              
200500                         W-Q301-MAX-IDPRODNR                              
200600                                                                          
200700     PERFORM CBAAA-LAGRA-PICK-UP-TABELL                                   
200800                                                                          
200900     IF PICK-UP-OK                                                        
201000        PERFORM CBAAB-SKRIV-PICK-UP                                       
201100     END-IF                                                               
201200     .                                                                    
201300     EJECT                                                                
201400 CBAAA-LAGRA-PICK-UP-TABELL SECTION.                                      
201500                                                                          
201600     PERFORM IMS-GU-ORQA-WLORQA01                                         
201700                                                                          
201800     PERFORM UNTIL SEGMENT-SAKNAS                                         
201900                   OR                                                     
202000                   END-OF-DATA                                            
202100                   OR                                                     
202200                   PICK-UP-EJ-OK                                          
202300        IF ODEL-IDPRC = W-SPAR-4448-IDPRC                                 
202400           AND                                                            
202500           ODEL-IDPLKLST < WS-IDPLKLST                                    
202600           MOVE NEJ TO PICK-UP-OK-SW                                      
202700        ELSE                                                              
202800           IF ODEL-KVRADER > 0                                            
202900              MOVE 1 TO IX1                                               
203000              PERFORM UNTIL IX1 > 10                                      
203100                 IF ODEL-IDPRC = W-SPAR-4448-IDPRC-SUB (IX1)              
203200                    ADD ODEL-VLORDNTO TO TAB-SUVOLYM (IX1)                
203300                    MOVE 1 TO TAB-IX                                      
203400                    PERFORM UNTIL TAB-IX > 100                            
203500                       IF TAB-IX > 99                                     
203600                          MOVE 'PICK-UP-TABELL > 99 I CBAAA-SEC.'         
203700                                         TO FELTEXT                       
203800                          CALL FELLOG                                     
203900                       ELSE                                               
204000                          IF TAB-IDPRC (IX1 TAB-IX) = SPACE               
204100                             MOVE ODEL-IDPRC                              
204200                                    TO TAB-IDPRC    (IX1 TAB-IX)          
204300                             MOVE ODEL-IDPRODNR                           
204400                                    TO TAB-IDPRODNR (IX1 TAB-IX)          
204500                             MOVE ODEL-IDPLKLST                           
204600                                    TO TAB-IDPLKLST (IX1 TAB-IX)          
204700                             MOVE ODEL-VLORDNTO                           
204800                                    TO TAB-VLORDNTO (IX1 TAB-IX)          
204900                             MOVE 100 TO TAB-IX                           
205000                          END-IF                                          
205100                       END-IF                                             
205200                       ADD 1 TO TAB-IX                                    
205300                    END-PERFORM                                           
205400                    MOVE 10 TO IX1                                        
205500                 END-IF                                                   
205600                 ADD 1 TO IX1                                             
205700              END-PERFORM                                                 
205800           END-IF                                                         
205900           PERFORM IMS-GN-ORQA-WLORQA01                                   
206000        END-IF                                                            
206100     END-PERFORM                                                          
206200     .                                                                    
206300     EJECT                                                                
206400 CBAAB-SKRIV-PICK-UP SECTION.                                             
206500                                                                          
206600     MOVE 1 TO IX1                                                        
206700     MOVE 0 TO IX2                                                        
206800     MOVE SPACE TO TRAD3-INFO                                             
206900     MOVE JA    TO BRYT-TOTAL-SW                                          
207000                                                                          
207100     IF WS-IDDC NOT = W-IDDC-B6                                           
207200        MOVE WS-IDDC TO W-IDDC-B6                                         
207300        PERFORM IMS-GU-WDB601                                             
207400     END-IF                                                               
207500                                                                          
207600     PERFORM UNTIL IX1 > 10                                               
207700        IF TAB-SUVOLYM (IX1) <= TAB-VLKOLGR (IX1)                         
207800           MOVE 1 TO TAB-IX                                               
207900           PERFORM UNTIL TAB-IX > 99                                      
208000              IF TAB-IDPRC (IX1 TAB-IX) = SPACE                           
208100                 MOVE 99 TO TAB-IX                                        
208200              ELSE                                                        
208300                 ADD 1   TO IX2                                           
208400                 IF IX2 = 1                                               
208500                    MOVE TAB-IDPRC    (IX1 TAB-IX)                        
208600                                            TO TRAD3-IDPRC-1              
208700                    IF US-MATT                                            
208800                       COMPUTE TRAD3-VLORDNTO-1                           
208900                                   = TAB-VLORDNTO (IX1 TAB-IX)            
209000                                   * CONV-M3-TO-FT3                       
209100                       MOVE 'FT3'           TO TRAD3-KDSORT-1             
209200                    ELSE                                                  
209300                       MOVE TAB-VLORDNTO (IX1 TAB-IX)                     
209400                                            TO TRAD3-VLORDNTO-1           
209500                       MOVE 'M3'            TO TRAD3-KDSORT-1             
209600                    END-IF                                                
209700                    MOVE TAB-VLORDNTO (IX1 TAB-IX)                        
209800                                            TO TRAD3-VLORDNTO-1           
209900                    MOVE 'M3'               TO TRAD3-KDSORT-1             
210000                    MOVE TAB-IDPRODNR (IX1 TAB-IX)                        
210100                                            TO TRAD3-IDPRODNR-1           
210200                    MOVE '-'                TO TRAD3-STRECK-1             
210300                    MOVE TAB-IDPLKLST (IX1 TAB-IX)                        
210400                                            TO TRAD3-IDPLKLST-1           
210500                 ELSE                                                     
210600                    MOVE TAB-IDPRC    (IX1 TAB-IX)                        
210700                                            TO TRAD3-IDPRC-2              
210800                    IF US-MATT                                            
210900                       COMPUTE TRAD3-VLORDNTO-2                           
211000                                   = TAB-VLORDNTO (IX1 TAB-IX)            
211100                                   * CONV-M3-TO-FT3                       
211200                       MOVE 'FT3'           TO TRAD3-KDSORT-2             
211300                    ELSE                                                  
211400                       MOVE TAB-VLORDNTO (IX1 TAB-IX)                     
211500                                            TO TRAD3-VLORDNTO-2           
211600                       MOVE 'M3'            TO TRAD3-KDSORT-2             
211700                    END-IF                                                
211800                    MOVE TAB-IDPRODNR (IX1 TAB-IX)                        
211900                                            TO TRAD3-IDPRODNR-2           
212000                    MOVE '-'                TO TRAD3-STRECK-2             
212100                    MOVE TAB-IDPLKLST (IX1 TAB-IX)                        
212200                                            TO TRAD3-IDPLKLST-2           
212300                                                                          
212400                    IF DCS-CDC AND CDC-DAGORDER                           
212500                      IF RAD-IX > 52                                      
212600                        PERFORM S06-SKRIV-PACKHUVUD                       
212700                        MOVE PRT-AFTER-3 TO PRT-RADSKIP                   
212800                      ELSE                                                
212900                        IF TOTAL-BRYTNING                                 
213000                          MOVE PRT-AFTER-2 TO PRT-RADSKIP                 
213100                        ELSE                                              
213200                          MOVE PRT-AFTER-1 TO PRT-RADSKIP                 
213300                        END-IF                                            
213400                      END-IF                                              
213500                    ELSE                                                  
213600                      IF RAD-IX > 59                                      
213700                       PERFORM S06-SKRIV-PACKHUVUD                        
213800                       MOVE PRT-AFTER-3 TO PRT-RADSKIP                    
213900                      ELSE                                                
214000                        IF TOTAL-BRYTNING                                 
214100                          MOVE PRT-AFTER-2 TO PRT-RADSKIP                 
214200                        ELSE                                              
214300                          MOVE PRT-AFTER-1 TO PRT-RADSKIP                 
214400                        END-IF                                            
214500                      END-IF                                              
214600                    END-IF                                                
214700                                                                          
214800                    EVALUATE TRUE                                         
214900                      WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                
215000                         MOVE PU-TRAD3X-ITA   TO WS-PU-RAD                
215100                      WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                
215200                         MOVE PU-TRAD3X-SPA   TO WS-PU-RAD                
215300                    END-EVALUATE                                          
215400                                                                          
215500                    PERFORM S07-SKRIV-RAD                                 
215600                    ADD PRT-RADSKIP TO RAD-IX                             
215700                                                                          
215800                    MOVE PU-TRAD3       TO WS-PU-RAD                      
215900                    PERFORM S07-SKRIV-RAD                                 
216000                    ADD PRT-RADSKIP TO RAD-IX                             
216100                    MOVE 0 TO IX2                                         
216200                    MOVE SPACE TO TRAD3-INFO                              
216300                    MOVE NEJ   TO BRYT-TOTAL-SW                           
216400                 END-IF                                                   
216500              END-IF                                                      
216600              ADD 1 TO TAB-IX                                             
216700           END-PERFORM                                                    
216800        END-IF                                                            
216900        ADD 1 TO IX1                                                      
217000     END-PERFORM                                                          
217100                                                                          
217200     IF IX2 > 0                                                           
217300       IF DCS-CDC AND CDC-DAGORDER                                        
217400         IF RAD-IX > 52                                                   
217500           PERFORM S06-SKRIV-PACKHUVUD                                    
217600           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
217700         ELSE                                                             
217800           IF TOTAL-BRYTNING                                              
217900             MOVE PRT-AFTER-2 TO PRT-RADSKIP                              
218000           ELSE                                                           
218100             MOVE PRT-AFTER-1 TO PRT-RADSKIP                              
218200           END-IF                                                         
218300         END-IF                                                           
218400       ELSE                                                               
218500         IF RAD-IX > 59                                                   
218600           PERFORM S06-SKRIV-PACKHUVUD                                    
218700           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
218800         ELSE                                                             
218900           IF TOTAL-BRYTNING                                              
219000             MOVE PRT-AFTER-2 TO PRT-RADSKIP                              
219100           ELSE                                                           
219200             MOVE PRT-AFTER-1 TO PRT-RADSKIP                              
219300           END-IF                                                         
219400         END-IF                                                           
219500       END-IF                                                             
219600                                                                          
219700              EVALUATE        TRUE                                        
219800         WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                             
219900           MOVE PU-TRAD3X-ITA               TO WS-PU-RAD                  
220000         WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                             
220100           MOVE PU-TRAD3X-SPA               TO WS-PU-RAD                  
220200       END-EVALUATE                                                       
220300                                                                          
220400       PERFORM S07-SKRIV-RAD                                              
220500       ADD PRT-RADSKIP TO RAD-IX                                          
220600                                                                          
220700       MOVE PU-TRAD3 TO WS-PU-RAD                                         
220800       PERFORM S07-SKRIV-RAD                                              
220900       ADD PRT-RADSKIP TO RAD-IX                                          
221000     END-IF                                                               
221100     .                                                                    
221200     EJECT                                                                
221300 CBAB-IDORDER-PICKUP-PRC SECTION.                                         
221400                                                                          
221500     IF WS-IDDC NOT = W-IDDC-B6                                           
221600        MOVE WS-IDDC TO W-IDDC-B6                                         
221700        PERFORM IMS-GU-WDB601                                             
221800     END-IF                                                               
221900                                                                          
222000     IF DCS-CDC AND CDC-DAGORDER                                          
222100       IF RAD-IX > 52                                                     
222200         PERFORM S06-SKRIV-PACKHUVUD                                      
222300         MOVE PRT-AFTER-3 TO PRT-RADSKIP                                  
222400       ELSE                                                               
222500         MOVE PRT-AFTER-2 TO PRT-RADSKIP                                  
222600       END-IF                                                             
222700     ELSE                                                                 
222800       IF RAD-IX > 59                                                     
222900         PERFORM S06-SKRIV-PACKHUVUD                                      
223000         MOVE PRT-AFTER-3 TO PRT-RADSKIP                                  
223100       ELSE                                                               
223200         MOVE PRT-AFTER-2 TO PRT-RADSKIP                                  
223300       END-IF                                                             
223400     END-IF                                                               
223500                                                                          
223600     IF 4008-VLORDNTO (2) > W-SPAR-4448-VLKOLGR                           
223700        EVALUATE  TRUE                                                    
223800           WHEN  DCS-CDC                                                  
223900             OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                         
224000             MOVE PU-TRAD6-SVE TO WS-PU-RAD                               
224100           WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                           
224200             MOVE PU-TRAD6-ITA TO WS-PU-RAD                               
224300           WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                           
224400             MOVE PU-TRAD6-SPA TO WS-PU-RAD                               
224500           WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                           
224600             MOVE PU-TRAD6-OST TO WS-PU-RAD                               
224700           WHEN OTHER                                                     
224800             MOVE PU-TRAD6-ENG TO WS-PU-RAD                               
224900        END-EVALUATE                                                      
225000        PERFORM S07-SKRIV-RAD                                             
225100        ADD PRT-RADSKIP     TO RAD-IX                                     
225200     ELSE                                                                 
225300        IF 4008-VLORDNTO (2) <= W-SPAR-4448-VLKOLGR                       
225400           MOVE LOW-VALUE    TO W-WDQ301KY-MIN-X                          
225500           MOVE HIGH-VALUE   TO W-WDQ301KY-MAX-X                          
225600           MOVE WS-IDORDER   TO W-Q301-MIN-IDORDER                        
225700                                W-Q301-MAX-IDORDER                        
225800           MOVE WS-IDDC      TO W-Q301-MIN-IDDC                           
225900                                W-Q301-MAX-IDDC                           
226000           PERFORM IMS-GU-ORQA-WLORQA01                                   
226100           MOVE 0 TO IX1                                                  
226200           PERFORM UNTIL IX1 > 0                                          
226300              IF SEGMENT-SAKNAS OR END-OF-DATA                            
226400                 EVALUATE  TRUE                                           
226500                    WHEN  DCS-CDC                                         
226600                      OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                
226700                      MOVE PU-TRAD5-SVE  TO WS-PU-RAD                     
226800                    WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                  
226900                      MOVE PU-TRAD5-ITA  TO WS-PU-RAD                     
227000                    WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                  
227100                      MOVE PU-TRAD5-SPA  TO WS-PU-RAD                     
227200                    WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                  
227300                      MOVE PU-TRAD5-OST  TO WS-PU-RAD                     
227400                    WHEN OTHER                                            
227500                      MOVE PU-TRAD5-ENG  TO WS-PU-RAD                     
227600                 END-EVALUATE                                             
227700                 MOVE 1 TO IX1                                            
227800                 PERFORM S07-SKRIV-RAD                                    
227900                 ADD PRT-RADSKIP       TO RAD-IX                          
228000              END-IF                                                      
228100              IF SEGMENT-FINNS                                            
228200                 IF ODEL-IDPRC = W-SPAR-4448-IDPRC-HUV                    
228300                    MOVE 1 TO IX1                                         
228400                 ELSE                                                     
228500                    PERFORM IMS-GN-ORQA-WLORQA01                          
228600                 END-IF                                                   
228700              END-IF                                                      
228800           END-PERFORM                                                    
228900        END-IF                                                            
229000     END-IF                                                               
229100     .                                                                    
229200     EJECT                                                                
229300 CBAC-IDORDER-OVRIGA-LAGOMRADE SECTION.                                   
229400                                                                          
229500     IF WS-IDDC NOT = W-IDDC-B6                                           
229600        MOVE WS-IDDC TO W-IDDC-B6                                         
229700        PERFORM IMS-GU-WDB601                                             
229800     END-IF                                                               
229900                                                                          
230000     EVALUATE  TRUE                                                       
230100        WHEN  DCS-CDC                                                     
230200          OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                            
230300          MOVE SPACE TO TRAD4-SVE-LAGOMRADE                               
230400        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
230500          MOVE SPACE TO TRAD4-ITA-LAGOMRADE                               
230600        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
230700          MOVE SPACE TO TRAD4-SPA-LAGOMRADE                               
230800        WHEN OTHER                                                        
230900          MOVE SPACE TO TRAD4-ENG-LAGOMRADE                               
231000     END-EVALUATE                                                         
231100                                                                          
231200     MOVE 1  TO IX1                                                       
231300     MOVE 0  TO IX2                                                       
231400     MOVE JA TO BRYT-TOTAL-SW                                             
231500                                                                          
231600     PERFORM UNTIL IX1 > MAX-LAGOMR                                       
231700        IF 4008-KVRADER-GRP (IX1) > 0                                     
231800           ADD 1                  TO IX2                                  
231900           EVALUATE  TRUE                                                 
232000              WHEN  DCS-CDC                                               
232100                OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                      
232200                MOVE IX1         TO TRAD4-SVE-ADLAGOMR (IX2)              
232300                MOVE 4008-KVRADER-GRP (IX1)                               
232400                                 TO TRAD4-SVE-ANTAL    (IX2)              
232500                MOVE ','         TO TRAD4-SVE-KOMMA    (IX2)              
232600              WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                        
232700                MOVE IX1         TO TRAD4-ITA-ADLAGOMR (IX2)              
232800                MOVE 4008-KVRADER-GRP (IX1)                               
232900                                 TO TRAD4-ITA-ANTAL    (IX2)              
233000                MOVE ','         TO TRAD4-ITA-KOMMA    (IX2)              
233100              WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                        
233200                MOVE IX1         TO TRAD4-SPA-ADLAGOMR (IX2)              
233300                MOVE 4008-KVRADER-GRP (IX1)                               
233400                                 TO TRAD4-SPA-ANTAL    (IX2)              
233500                MOVE ','         TO TRAD4-SPA-KOMMA    (IX2)              
233600              WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                        
233700                MOVE IX1         TO TRAD4-OST-ADLAGOMR (IX2)              
233800                MOVE 4008-KVRADER-GRP (IX1)                               
233900                                 TO TRAD4-OST-ANTAL    (IX2)              
234000                MOVE ','         TO TRAD4-OST-KOMMA    (IX2)              
234100              WHEN OTHER                                                  
234200                MOVE IX1         TO TRAD4-ENG-ADLAGOMR (IX2)              
234300                MOVE 4008-KVRADER-GRP (IX1)                               
234400                                 TO TRAD4-ENG-ANTAL    (IX2)              
234500                MOVE ','         TO TRAD4-ENG-KOMMA    (IX2)              
234600           END-EVALUATE                                                   
234700           IF IX2 = 6                                                     
234800             IF DCS-CDC AND CDC-DAGORDER                                  
234900               IF RAD-IX > 52                                             
235000                 PERFORM S06-SKRIV-PACKHUVUD                              
235100                 MOVE PRT-AFTER-3 TO PRT-RADSKIP                          
235200               ELSE                                                       
235300                 IF TOTAL-BRYTNING                                        
235400                   MOVE PRT-AFTER-2 TO PRT-RADSKIP                        
235500                 ELSE                                                     
235600                   MOVE PRT-AFTER-1 TO PRT-RADSKIP                        
235700                 END-IF                                                   
235800               END-IF                                                     
235900             ELSE                                                         
236000               IF RAD-IX > 59                                             
236100                 PERFORM S06-SKRIV-PACKHUVUD                              
236200                 MOVE PRT-AFTER-3 TO PRT-RADSKIP                          
236300               ELSE                                                       
236400                 IF TOTAL-BRYTNING                                        
236500                   MOVE PRT-AFTER-2 TO PRT-RADSKIP                        
236600                 ELSE                                                     
236700                   MOVE PRT-AFTER-1 TO PRT-RADSKIP                        
236800                 END-IF                                                   
236900               END-IF                                                     
237000             END-IF                                                       
237100                                                                          
237200             EVALUATE  TRUE                                               
237300               WHEN  DCS-CDC                                              
237400                OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                      
237500                 MOVE SPACE       TO TRAD4-SVE-KOMMA (IX2)                
237600                 MOVE PU-TRAD4-SVE TO WS-PU-RAD                           
237700                 PERFORM S07-SKRIV-RAD                                    
237800                 MOVE SPACE       TO TRAD4-SVE-LAGOMRADE                  
237900               WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                       
238000                 MOVE SPACE       TO TRAD4-ITA-KOMMA (IX2)                
238100                 MOVE PU-TRAD4-ITA TO WS-PU-RAD                           
238200                 PERFORM S07-SKRIV-RAD                                    
238300                 MOVE SPACE       TO TRAD4-ITA-LAGOMRADE                  
238400               WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                       
238500                 MOVE SPACE       TO TRAD4-SPA-KOMMA (IX2)                
238600                 MOVE PU-TRAD4-SPA TO WS-PU-RAD                           
238700                 PERFORM S07-SKRIV-RAD                                    
238800                 MOVE SPACE       TO TRAD4-SPA-LAGOMRADE                  
238900               WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                       
239000                 MOVE SPACE       TO TRAD4-SPA-KOMMA (IX2)                
239100                 MOVE PU-TRAD4-OST TO WS-PU-RAD                           
239200                 PERFORM S07-SKRIV-RAD                                    
239300                 MOVE SPACE       TO TRAD4-OST-LAGOMRADE                  
239400               WHEN OTHER                                                 
239500                 MOVE SPACE       TO TRAD4-ENG-KOMMA (IX2)                
239600                 MOVE PU-TRAD4-ENG TO WS-PU-RAD                           
239700                 PERFORM S07-SKRIV-RAD                                    
239800                 MOVE SPACE       TO TRAD4-ENG-LAGOMRADE                  
239900             END-EVALUATE                                                 
240000                                                                          
240100              ADD PRT-RADSKIP     TO RAD-IX                               
240200              MOVE 0              TO IX2                                  
240300              MOVE NEJ            TO BRYT-TOTAL-SW                        
240400           END-IF                                                         
240500        END-IF                                                            
240600        ADD 1 TO IX1                                                      
240700     END-PERFORM                                                          
240800                                                                          
240900     IF IX2 > 0                                                           
241000       IF DCS-CDC AND CDC-DAGORDER                                        
241100         IF RAD-IX > 52                                                   
241200           PERFORM S06-SKRIV-PACKHUVUD                                    
241300           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
241400         ELSE                                                             
241500           IF TOTAL-BRYTNING                                              
241600             MOVE PRT-AFTER-2 TO PRT-RADSKIP                              
241700           ELSE                                                           
241800             MOVE PRT-AFTER-1 TO PRT-RADSKIP                              
241900           END-IF                                                         
242000         END-IF                                                           
242100       ELSE                                                               
242200         IF RAD-IX > 59                                                   
242300           PERFORM S06-SKRIV-PACKHUVUD                                    
242400           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
242500         ELSE                                                             
242600           IF TOTAL-BRYTNING                                              
242700             MOVE PRT-AFTER-2 TO PRT-RADSKIP                              
242800           ELSE                                                           
242900             MOVE PRT-AFTER-1 TO PRT-RADSKIP                              
243000           END-IF                                                         
243100         END-IF                                                           
243200       END-IF                                                             
243300                                                                          
243400       EVALUATE  TRUE                                                     
243500         WHEN  DCS-CDC                                                    
243600           OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                           
243700              MOVE SPACE       TO TRAD4-SVE-KOMMA (IX2)                   
243800           MOVE PU-TRAD4-SVE TO WS-PU-RAD                                 
243900           PERFORM S07-SKRIV-RAD                                          
244000           MOVE SPACE       TO TRAD4-SVE-LAGOMRADE                        
244100         WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                             
244200           MOVE SPACE       TO TRAD4-ITA-KOMMA (IX2)                      
244300           MOVE PU-TRAD4-ITA TO WS-PU-RAD                                 
244400           PERFORM S07-SKRIV-RAD                                          
244500           MOVE SPACE       TO TRAD4-ITA-LAGOMRADE                        
244600         WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                             
244700           MOVE SPACE       TO TRAD4-SPA-KOMMA (IX2)                      
244800           MOVE PU-TRAD4-SPA TO WS-PU-RAD                                 
244900           PERFORM S07-SKRIV-RAD                                          
245000           MOVE SPACE       TO TRAD4-ITA-LAGOMRADE                        
245100         WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                             
245200           MOVE SPACE       TO TRAD4-OST-KOMMA (IX2)                      
245300           MOVE PU-TRAD4-OST TO WS-PU-RAD                                 
245400           PERFORM S07-SKRIV-RAD                                          
245500           MOVE SPACE       TO TRAD4-OST-LAGOMRADE                        
245600         WHEN OTHER                                                       
245700           MOVE SPACE       TO TRAD4-ENG-KOMMA (IX2)                      
245800           MOVE PU-TRAD4-ENG TO WS-PU-RAD                                 
245900           PERFORM S07-SKRIV-RAD                                          
246000           MOVE SPACE       TO TRAD4-ENG-LAGOMRADE                        
246100       END-EVALUATE                                                       
246200     END-IF                                                               
246300     .                                                                    
246400     EJECT                                                                
246500 CBAE-IDORDER-NOLLA-ACKAR SECTION.                                        
246600                                                                          
246700     MOVE 0 TO 4008-IDSID                                                 
246800                                                                          
246900     MOVE 1 TO IX1                                                        
247000     PERFORM UNTIL IX1 > 101                                              
247100        IF IX1 < 4                                                        
247200           MOVE 0 TO 4008-KVRADER     (IX1)                               
247300                     4008-VKORDNTO    (IX1)                               
247400                     4008-VLORDNTO    (IX1)                               
247500        END-IF                                                            
247600        MOVE 0    TO 4008-KVRADER-GRP (IX1)                               
247700        ADD 1     TO IX1                                                  
247800     END-PERFORM                                                          
247900     .                                                                    
248000     EJECT                                                                
248100 CC-TOTAL-PRINTER SECTION.                                                
248200                                                                          
248300     IF PU-OPEN                                                           
248400        IF EJ-NY-ORDER                                                    
248500           PERFORM CCA-SKRIV-PRINTER-TOTAL                                
248600        ELSE                                                              
248700           PERFORM CCB-KOLLA-OM-TOTALER                                   
248800           IF SKRIV-TOTAL                                                 
248900              PERFORM CCA-SKRIV-PRINTER-TOTAL                             
249000           END-IF                                                         
249100        END-IF                                                            
249200        PERFORM CCC-PRINTER-NOLLA-ACKAR                                   
249300     END-IF                                                               
249400     MOVE NEJ TO NY-ORDER-SW                                              
249500     .                                                                    
249600     EJECT                                                                
249700 CCA-SKRIV-PRINTER-TOTAL SECTION.                                         
249800                                                                          
249900     IF WS-IDDC NOT = W-IDDC-B6                                           
250000       MOVE WS-IDDC TO W-IDDC-B6                                          
250100       PERFORM IMS-GU-WDB601                                              
250200     END-IF                                                               
250300     IF DCS-CDC AND CDC-DAGORDER                                          
250400       IF RAD-IX > 52                                                     
250500         PERFORM S06-SKRIV-PACKHUVUD                                      
250600         MOVE PRT-AFTER-3 TO PRT-RADSKIP                                  
250700       ELSE                                                               
250800         MOVE PRT-AFTER-2 TO PRT-RADSKIP                                  
250900       END-IF                                                             
251000     ELSE                                                                 
251100       IF RAD-IX > 58                                                     
251200         PERFORM S06-SKRIV-PACKHUVUD                                      
251300         MOVE PRT-AFTER-3 TO PRT-RADSKIP                                  
251400       ELSE                                                               
251500         MOVE PRT-AFTER-2 TO PRT-RADSKIP                                  
251600       END-IF                                                             
251700     END-IF                                                               
251800                                                                          
251900     EVALUATE TRUE                                                        
252000        WHEN  DCS-CDC                                                     
252100          OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                            
252200          MOVE 4008-VKORDNTO (1)   TO TRAD1-SVE-VKORDNTO                  
252300          MOVE 4008-VLORDNTO (1)   TO TRAD1-SVE-VLORDNTO                  
252400          MOVE 4008-KVRADER (1)    TO TRAD1-SVE-KVRADER                   
252500          MOVE PU-TRAD1-SVE        TO WS-PU-RAD                           
252600        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
252700          MOVE 4008-VKORDNTO (1)   TO TRAD1-ITA-VKORDNTO                  
252800          MOVE 4008-VLORDNTO (1)   TO TRAD1-ITA-VLORDNTO                  
252900          MOVE 4008-KVRADER (1)    TO TRAD1-ITA-KVRADER                   
253000          MOVE PU-TRAD1-ITA        TO WS-PU-RAD                           
253100        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
253200          MOVE 4008-VKORDNTO (1)   TO TRAD1-SPA-VKORDNTO                  
253300          MOVE 4008-VLORDNTO (1)   TO TRAD1-SPA-VLORDNTO                  
253400          MOVE 4008-KVRADER (1)    TO TRAD1-SPA-KVRADER                   
253500          MOVE PU-TRAD1-SPA        TO WS-PU-RAD                           
253600        WHEN OTHER                                                        
253700          IF US-MATT                                                      
253800             COMPUTE TRAD1-ENG-VKORDNTO = 4008-VKORDNTO (1)               
253900                                       * CONV-KG-TO-LB                    
254000             COMPUTE TRAD1-ENG-VLORDNTO = 4008-VLORDNTO (1)               
254100                                       * CONV-M3-TO-FT3                   
254200             MOVE 'FT3'            TO TRAD1-ENG-VL                        
254300             MOVE 'LBS'            TO TRAD1-ENG-VK                        
254400          ELSE                                                            
254500            MOVE 4008-VKORDNTO (1) TO TRAD1-ENG-VKORDNTO                  
254600            MOVE 4008-VLORDNTO (1) TO TRAD1-ENG-VLORDNTO                  
254700            MOVE 'M3 '             TO TRAD1-ENG-VL                        
254800            MOVE 'KG '             TO TRAD1-ENG-VK                        
254900          END-IF                                                          
255000          MOVE 4008-KVRADER (1)    TO TRAD1-ENG-KVRADER                   
255100          MOVE PU-TRAD1-ENG        TO WS-PU-RAD                           
255200     END-EVALUATE                                                         
255300                                                                          
255400     PERFORM S07-SKRIV-RAD                                                
255500                                                                          
255600     ADD PRT-RADSKIP  TO RAD-IX                                           
255700     .                                                                    
255800     EJECT                                                                
255900 CCB-KOLLA-OM-TOTALER SECTION.                                            
256000                                                                          
256100     MOVE NEJ TO SKRIV-TOTAL-RAD-SW                                       
256200                                                                          
256300     IF SPAR-PRINTER NOT = 4008-KDPRT-PU                                  
256400        MOVE JA TO SKRIV-TOTAL-RAD-SW                                     
256500     END-IF                                                               
256600     .                                                                    
256700     EJECT                                                                
256800 CCC-PRINTER-NOLLA-ACKAR SECTION.                                         
256900                                                                          
257000     ADD 4008-KVRADER  (1) TO 4008-KVRADER     (2)                        
257100     ADD 4008-VKORDNTO (1) TO 4008-VKORDNTO    (2)                        
257200     ADD 4008-VLORDNTO (1) TO 4008-VLORDNTO    (2)                        
257300                                                                          
257400     MOVE 0                TO 4008-KVRADER     (1)                        
257500                              4008-VKORDNTO    (1)                        
257600                              4008-VLORDNTO    (1)                        
257700     .                                                                    
257800     EJECT                                                                
257900 CD-REDIGERA-DETALJRAD SECTION.                                           
258000                                                                          
258100     IF WS-IDDC NOT = W-IDDC-B6                                           
258200        MOVE WS-IDDC TO W-IDDC-B6                                         
258300        PERFORM IMS-GU-WDB601                                             
258400     END-IF                                                               
258500                                                                          
258600     MOVE OHUV-IDDISTR     TO DIST34-IDDISTR                              
258700                                                                          
258800     PERFORM S12-LASER-PRINTER                                            
258900     IF DCS-CDC AND LASER-SKRIVARE                                        
259000       IF RAD-IX > 59                                                     
259100         PERFORM S06-SKRIV-PACKHUVUD                                      
259200       END-IF                                                             
259300     ELSE                                                                 
259400       IF (DCS-CDC AND CDC-DAGORDER) OR                                   
259500          (DCS-SDC AND DCS-IDLANDX2 NOT = 'SE')                           
259600         IF RAD-IX > 52                                                   
259700           PERFORM S06-SKRIV-PACKHUVUD                                    
259800         END-IF                                                           
259900       ELSE                                                               
260000         IF DCS-NDC-NA OR DCS-NDC-PF                                      
260100           IF RAD-IX > 61                                                 
260200             PERFORM S06-SKRIV-PACKHUVUD                                  
260300           END-IF                                                         
260400         ELSE                                                             
260500           IF RAD-IX > 66                                                 
260600             PERFORM S06-SKRIV-PACKHUVUD                                  
260700           END-IF                                                         
260800         END-IF                                                           
260900       END-IF                                                             
261000     END-IF                                                               
261100                                                                          
261200*LDC-GB                                                                   
261300     IF DCS-SDC AND (DCS-IDLANDX2 = 'GB' OR 'NL' OR 'IT')                 
261400       IF GMT-FLLDCKND = JA  AND                                          
261500         (DIST34-ENGLAND-SDC OR                                           
261600          DIST34-ITALIEN-SDC OR                                           
261700          DIST34-HOLLAND-SDC)                                             
261800       AND (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1 OR                       
261900            OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                         
262000*VIP                                                                      
262100          MOVE 4010-BERADREF         TO RAD-VIPID                         
262200*      INSPECT RAD-IDKUNDRF-RO-URS REPLACING LEADING ZERO BY SPACE        
262300       ELSE                                                               
262400         IF 4010-IDKUNDRF-RO > '0000000   '                               
262500           MOVE 4010-IDKUNDRF-RO (3:5) TO RAD-IDKUNDRF-RO-URS             
262600         ELSE                                                             
262700           MOVE SPACE              TO RAD-IDKUNDRF-RO-URS                 
262800         END-IF                                                           
262900       END-IF                                                             
263000     ELSE                                                                 
263100       IF 4010-IDKUNDRF-RO > '0000000   '                                 
263200         MOVE 4010-IDKUNDRF-RO (3:5) TO RAD-IDKUNDRF-RO-URS               
263300                                        RAD-IDKUNDRF-RO-URS-SPA           
263400                                        RAD-IDKUNDRF-RO-URS-NL            
263500       ELSE                                                               
263600         MOVE SPACE                  TO RAD-IDKUNDRF-RO-URS               
263700                                        RAD-IDKUNDRF-RO-URS-SPA           
263800                                        RAD-IDKUNDRF-RO-URS-NL            
263900       END-IF                                                             
264000     END-IF                                                               
264100                                                                          
264200     IF DCS-NDC-NA OR                                                     
264300       (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU') OR                            
264400       (DCS-SDC AND DCS-IDLANDX2 = 'AT')                                  
264500       MOVE 4010-ADPLATS-ORD TO WS-ADPLATS                                
264600     ELSE                                                                 
264700       PERFORM CDA-JUSTERA-PLATS                                          
264800     END-IF                                                               
264900                                                                          
265000        EVALUATE TRUE                                                     
265100         WHEN DCS-CDC                                                     
265200           OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                           
265300            MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-SVE                    
265400                                      RAD-ADLAGOMR-LDC                    
265500            MOVE 4010-ADGANG    TO RAD-ADGANG-SVE                         
265600                                   RAD-ADGANG-LDC                         
265610            MOVE 4010-KVQPACK-3 TO RAD-KVQPACK-3                          
265620            MOVE 4010-KVAVBART  TO RAD-KVAVBART-SVE                       
265700            MOVE WS-ADPLATS     TO RAD-ADPLATS-SVE                        
265800                                   RAD-ADPLATS-LDC                        
265900                                                                          
266000            IF GMT-FLLDCKND = JA  AND                                     
266100               DIST34-ENGLAND-SDC AND                                     
266200              (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1 OR                    
266300               OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                      
266400                                                                          
266500              MOVE 4010-BERADREF TO RAD-VIPID                             
266600              PERFORM CDB-SPARA-I-LDC-TAB                                 
266700            ELSE                                                          
266800              MOVE 4010-BERADREF TO RAD-VIPID                             
266900            END-IF                                                        
267000          WHEN DCS-SDC AND DCS-IDLANDX2 = 'GB'                            
267100            IF DIST34-ENGLAND-SDC                                         
267200              IF GMT-FLLDCKND = JA                                        
267300                IF (4010-KDORDKL = 0 OR 1 OR 3 OR 4)                      
267400                                                                          
267500                  PERFORM CDB-SPARA-I-LDC-TAB                             
267600                                                                          
267700                  MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-LDC              
267800                  MOVE 4010-ADGANG TO RAD-ADGANG-LDC                      
267900                  MOVE '.'        TO RAD-PUNKT-LDC                        
268000                  MOVE WS-ADPLATS TO RAD-ADPLATS-LDC                      
268100                ELSE                                                      
268200                  MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-ENG              
268300                  MOVE 4010-ADGANG TO RAD-ADGANG-ENG                      
268400                  MOVE '.'        TO RAD-PUNKT-ENG                        
268500                  MOVE WS-ADPLATS TO RAD-ADPLATS-ENG                      
268600                END-IF                                                    
268700              ELSE                                                        
268800                MOVE 4010-BERADREF TO RAD-BERADREF-GB                     
268900              END-IF                                                      
269000            ELSE                                                          
269100              MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-ENG                  
269200              MOVE 4010-ADGANG    TO RAD-ADGANG-ENG                       
269300              MOVE '.'            TO RAD-PUNKT-ENG                        
269400              MOVE WS-ADPLATS     TO RAD-ADPLATS-ENG                      
269500              IF DCS-SDC AND DCS-IDLANDX2 = 'AT'                          
269600                INSPECT RAD-ADPLATS-ENG                                   
269700                REPLACING LEADING SPACE BY ZERO                           
269800              END-IF                                                      
269900            END-IF                                                        
270000                                                                          
270100          WHEN (DIST34-HOLLAND-SDC OR                                     
270200                DIST34-ITALIEN-SDC)                                       
270300           AND GMT-FLLDCKND = JA                                          
270400                                                                          
270500              IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                    
270600              OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                    
270700                                                                          
270800                MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-LDC                
270900                MOVE 4010-ADGANG TO RAD-ADGANG-LDC                        
271000                MOVE '.'          TO RAD-PUNKT-LDC                        
271100                MOVE WS-ADPLATS TO RAD-ADPLATS-LDC                        
271200              ELSE                                                        
271300                MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-ENG                
271400                MOVE 4010-ADGANG TO RAD-ADGANG-ENG                        
271500                MOVE '.'          TO RAD-PUNKT-ENG                        
271600                MOVE WS-ADPLATS TO RAD-ADPLATS-ENG                        
271700              END-IF                                                      
271800          WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                            
271900            MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-SPA                    
272000            MOVE 4010-ADGANG       TO RAD-ADGANG-SPA                      
272100            MOVE WS-ADPLATS        TO RAD-ADPLATS-SPA                     
272200          WHEN DCS-SDC AND DCS-IDLANDX2 = 'NL'                            
272300            MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-NL                     
272400            MOVE 4010-ADGANG       TO RAD-ADGANG-NL                       
272500            MOVE '.'               TO RAD-PUNKT-NL                        
272600            MOVE WS-ADPLATS        TO RAD-ADPLATS-NL                      
272700          WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')         
272800            MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-NDC                    
272900            MOVE 4010-ADGANG    TO RAD-ADGANG-NDC                         
273000            MOVE WS-ADPLATS     TO RAD-ADPLATS-NDC                        
273010            MOVE 4010-KVBEART-Q TO RAD-KVBEART-Q                          
273020            MOVE 4010-KVAVBART  TO RAD-KVAVBART                           
273100          WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                         
273200            MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-JAP                    
273300            MOVE 4010-ADGANG    TO RAD-ADGANG-JAP                         
273400            MOVE WS-ADPLATS     TO RAD-ADPLATS-JAP                        
273410            MOVE 4010-KVBEART-Q TO RAD-KVBEART-Q                          
273420            MOVE 4010-KVAVBART  TO RAD-KVAVBART                           
273500          WHEN OTHER                                                      
273600            MOVE 4010-ADLAGOMR-ORD TO RAD-ADLAGOMR-ENG                    
273700            MOVE 4010-ADGANG    TO RAD-ADGANG-ENG                         
273800            MOVE '.'            TO RAD-PUNKT-ENG                          
273900            MOVE WS-ADPLATS     TO RAD-ADPLATS-ENG                        
274000        END-EVALUATE                                                      
274100                                                                          
274200     MOVE 4010-IDARTNR          TO RAD-IDARTNR                            
274300                                   RAD-IDARTNR-LDC                        
274400                                   RAD-IDARTNR-SPA                        
274500                                   RAD-IDARTNR-NL                         
274600     MOVE 4010-REKSIFFR         TO RAD-REKSIFFR                           
274700                                   RAD-REKSIFFR-LDC                       
274800                                   RAD-REKSIFFR-SPA                       
274900                                   RAD-REKSIFFR-NL                        
275000     MOVE 4010-IDPURAD          TO RAD-IDPURAD                            
275100                                   RAD-IDPURAD-LDC                        
275200                                   RAD-IDPURAD-SPA                        
275300                                   RAD-IDPURAD-NL                         
275400                                                                          
275500     IF DIST08-URSP-RAPP OR DIST08-URSP-SPX OR                            
275600       (DCS-CDC  AND DIST08-URSP-RAPP-CDC)                                
275700        MOVE 4010-KDARTURS      TO RAD-KDARTURS                           
275800                                   RAD-KDARTURS-LDC                       
275900                                   RAD-KDARTURS-SPA                       
276000                                   RAD-KDARTURS-NL                        
276100     ELSE                                                                 
276200        MOVE SPACE              TO RAD-KDARTURS                           
276300                                   RAD-KDARTURS-LDC                       
276400                                   RAD-KDARTURS-SPA                       
276500                                   RAD-KDARTURS-NL                        
276600     END-IF                                                               
276700                                                                          
276800     IF 4010-FLTILLK = JA                                                 
276900        MOVE '*'                TO RAD-FLTILLK                            
277000                                   RAD-FLTILLK-LDC                        
277100     ELSE                                                                 
277200        MOVE SPACE              TO RAD-FLTILLK                            
277300                                   RAD-FLTILLK-LDC                        
277400     END-IF                                                               
277500                                                                          
277600     MOVE 4010-BEART            TO RAD-BEART                              
277700                                   RAD-BEART-LDC                          
277800                                   RAD-BEART-SPA                          
277900                                   RAD-BEART-NL                           
278000     MOVE 4010-KVQPACK-3        TO RAD-KVQPACK-3-LDC                      
278100                                                                          
278200     MOVE 4010-KVBEART-Q        TO RAD-KVBEART-Q-SPA                      
278300                                   RAD-KVBEART-Q-NL                       
278400     MOVE 4010-KVAVBART         TO RAD-KVAVBART-NL                        
278500                                   RAD-KVAVBART-LDC                       
278600                                   RAD-KVAVBART-SPA                       
278800                                                                          
278900     EVALUATE TRUE                                                        
279000        WHEN DCS-NDC-NA                                                   
279100          IF 4010-IDPSN > 099 AND < 400                                   
279200          OR 4010-IDPSN = 903                                             
279300            MOVE 'H'            TO RAD-KDFARLIG                           
279400*               H = HAZARDOUS GOODS.                                      
279500          ELSE                                                            
279600            MOVE SPACE          TO RAD-KDFARLIG                           
279700          END-IF                                                          
279800        WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'                           
279900          IF 4010-IDPSN > 600 AND < 699                                   
280000            MOVE 'H'            TO RAD-KDFARLIG                           
280100*               H = HAZARDOUS GOODS.                                      
280200          ELSE                                                            
280300            MOVE SPACE          TO RAD-KDFARLIG                           
280400          END-IF                                                          
280500        WHEN OTHER                                                        
280600          IF 4010-KDFARLIG = 4 OR 6 OR 7                                  
280700            EVALUATE TRUE                                                 
280800               WHEN  DCS-CDC                                              
280900                 OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                     
281000                 MOVE 'F'       TO RAD-KDFARLIG                           
281100*                      F = FARLIGT GODS.                                  
281200               WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                       
281300                 MOVE 'P'       TO RAD-KDFARLIG-SPA                       
281400*                      P = MERCANICA PELIGROSO.                           
281500               WHEN OTHER                                                 
281600                 MOVE 'D'       TO RAD-KDFARLIG                           
281700                                   RAD-KDFARLIG-LDC                       
281800                                   RAD-KDFARLIG-NL                        
281900*                      D = DANGEROUS GOODS.                               
282000            END-EVALUATE                                                  
282100          ELSE                                                            
282200            MOVE SPACE          TO RAD-KDFARLIG                           
282300                                   RAD-KDFARLIG-LDC                       
282400          END-IF                                                          
282500     END-EVALUATE                                                         
282600                                                                          
282700*LDC-SE                                                                   
282800     MOVE OHUV-IDDISTR     TO DIST34-IDDISTR                              
282900     MOVE OHUV-IDDISTR     TO DIST03-IDDISTR                              
283000                                                                          
283100     IF (DCS-SDC AND DCS-IDLANDX2 = 'SE') OR                              
283200        (DCS-CDC AND                                                      
283300        GMT-FLLDCKND = JA AND                                             
283400        DIST03-SVERIGE)                                                   
283500                                                                          
283600       MOVE 4010-BERADREF(1:8)  TO RAD-BERADREF                           
283700     END-IF                                                               
283800                                                                          
283900     IF (DCS-CDC                                                          
284000     AND DIST-SVERIGE-778                                                 
284100     AND KUND-GISLAVED-765)                                               
284200     OR                                                                   
284300     (DCS-CDC                                                             
284400     AND DIST-SVERIGE-76                                                  
284500     AND KUND-GISLAVED-37015)                                             
284600       MOVE 4010-BERADREF(1:8)  TO RAD-IDGISLAVED                         
284700     ELSE                                                                 
284800       MOVE '....'              TO RAD-FILLER7                            
284900                                   RAD-FILLERL                            
285000       MOVE 4010-IDSPECEMB      TO RAD-IDSPECEMB                          
285100                                   RAD-IDSPECEMB-LDC                      
285200     END-IF                                                               
285300                                                                          
285400     MOVE PRT-AFTER-2           TO PRT-RADSKIP                            
285500*LDC-GB                                                                   
285600     IF GMT-FLLDCKND = JA   AND                                           
285700       (DIST34-ENGLAND-SDC OR                                             
285800        DIST34-ITALIEN-SDC OR                                             
285900        DIST34-HOLLAND-SDC) AND                                           
286000       (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1 OR                           
286100        OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                             
286200                                                                          
286300       MOVE SPACE        TO FILLERL1                                      
286400       MOVE SPACE        TO FILLERL2                                      
286500       MOVE PU-RAD-LDC TO WS-PU-RAD-LDC                                   
286600       PERFORM S08-SKRIV-RAD                                              
286700                                                                          
286800*LK ADD LYNK PART NO- *                                                   
286900       IF 4010-IDSYSTEM (1:3) = 'LYN'                                     
287000          MOVE 4010-IDARTNR      TO W-IDARTNR                             
287100          PERFORM IMS-GU-WDF502                                           
287200           IF SEGMENT-FINNS                                               
287300             MOVE XLEV-IDLEVART     TO RAD-IDARTNR-LDC-LYNK               
287400             MOVE 'LYNK&CO PART NO' TO RAD-BEART-LDC-LYNK                 
287500           END-IF                                                         
287600          MOVE PRT-AFTER-1         TO PRT-RADSKIP                         
287700          MOVE PU-RAD-LDC-LYNK     TO WS-PU-RAD                           
287800          PERFORM S07-SKRIV-RAD                                           
287900          ADD 1                    TO RAD-IX                              
288000       END-IF                                                             
288100*                                                                         
288200     ELSE                                                                 
288300       IF DCS-SDC AND DCS-IDLANDX2 = 'ES'                                 
288400         MOVE PU-RAD-SPA TO WS-PU-RAD                                     
288500       ELSE                                                               
288600         IF DCS-SDC AND DCS-IDLANDX2 = 'NL'                               
288700           MOVE PU-RAD-NL TO WS-PU-RAD                                    
288800         ELSE                                                             
288900           MOVE PU-RAD  TO WS-PU-RAD                                      
289000         END-IF                                                           
289100       END-IF                                                             
289200       PERFORM S07-SKRIV-RAD                                              
289300                                                                          
289400*LK ADD LYNK PART NO- *                                                   
289500       IF 4010-IDSYSTEM (1:3) = 'LYN'                                     
289600          MOVE 4010-IDARTNR      TO W-IDARTNR                             
289700          PERFORM IMS-GU-WDF502                                           
289800           IF SEGMENT-FINNS                                               
289900             MOVE XLEV-IDLEVART     TO RAD-IDARTNR-LYNK                   
290000             MOVE 'LYNK&CO PART NO' TO RAD-BEART-LYNK                     
290100           END-IF                                                         
290200          MOVE PRT-AFTER-1         TO PRT-RADSKIP                         
290300          MOVE PU-RAD-LYNK         TO WS-PU-RAD                           
290400          PERFORM S07-SKRIV-RAD                                           
290500          ADD 1                    TO RAD-IX                              
290600       END-IF                                                             
290700*                                                                         
290800     END-IF                                                               
290900     ADD 2                      TO RAD-IX                                 
291000     ADD 1                      TO 4008-KVRADER-GRP (100)                 
291100     ADD 1                      TO 4008-KVRADER     (1)                   
291200                                                                          
291300*** 4008-KVRADER (3) ANVÄNDS FÖR ATT KUNNA LAGRA ANTAL                    
291400*** AVBOKADE ARTKLAR PER RAD.                                             
291500*** VID LÄMPLIGT TILLFÄLLE BÖR SEGMENT 4464 KOMPLETTERAS                  
291600*** MED DATAELEMENT KVAVBART.                                             
291700                                                                          
291800     ADD 4010-KVAVBART          TO 4008-KVRADER     (3)                   
291900                                                                          
292000     ADD 4010-VKORDNTO          TO 4008-VKORDNTO    (1)                   
292100     ADD 4010-VLORDNTO          TO 4008-VLORDNTO    (1)                   
292200                                                                          
292300     IF 4010-KDPRT NOT = 4008-KDPRT-PU                                    
292400        IF 4010-ADLAGOMR-ORD > 0 AND < 100                                
292500           ADD 1 TO 4008-KVRADER-GRP (4010-ADLAGOMR-ORD)                  
292600        END-IF                                                            
292700     END-IF                                                               
292800     .                                                                    
292900     EJECT                                                                
293000 CDA-JUSTERA-PLATS SECTION.                                               
293100                                                                          
293200     MOVE 4010-ADPLATS-ORD TO WS-ADPLATS                                  
293300     INSPECT WS-ADPLATS REPLACING LEADING ZERO BY SPACE                   
293400                                                                          
293500     IF WS-ADPLATS = SPACE                                                
293600        MOVE '0    ' TO WS-ADPLATS                                        
293700     ELSE                                                                 
293800        IF WS-ADPLATS (1:4) = SPACE                                       
293900           MOVE WS-ADPLATS (5:1) TO WS-ADPLATS (1:1)                      
294000           MOVE SPACE            TO WS-ADPLATS (2:4)                      
294100        ELSE                                                              
294200           IF WS-ADPLATS (1:3) = SPACE                                    
294300              MOVE WS-ADPLATS (4:2) TO WS-ADPLATS (1:2)                   
294400              MOVE SPACE            TO WS-ADPLATS (3:3)                   
294500           ELSE                                                           
294600              IF WS-ADPLATS (1:2) = SPACE                                 
294700                 MOVE WS-ADPLATS (3:3) TO WS-ADPLATS (1:3)                
294800                 MOVE SPACE            TO WS-ADPLATS (4:2)                
294900              ELSE                                                        
295000                 IF WS-ADPLATS (1:1) = SPACE                              
295100                    MOVE WS-ADPLATS (2:4) TO WS-ADPLATS (1:4)             
295200                    MOVE SPACE            TO WS-ADPLATS (5:1)             
295300                 END-IF                                                   
295400              END-IF                                                      
295500           END-IF                                                         
295600        END-IF                                                            
295700     END-IF                                                               
295800     .                                                                    
295900     EJECT                                                                
296000 CDB-SPARA-I-LDC-TAB          SECTION.                                    
296100*LDC-GB                                                                   
296200                                                                          
296300     IF WS-IDDC NOT = W-IDDC-B6                                           
296400        MOVE WS-IDDC TO W-IDDC-B6                                         
296500        PERFORM IMS-GU-WDB601                                             
296600     END-IF                                                               
296700                                                                          
296800     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
296900     IF DCS-SDC AND DCS-IDLANDX2 = 'GB'                                   
297000       IF DIST34-ENGLAND-SDC                                              
297100       IF GMT-FLLDCKND = JA                                               
297200         IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                         
297300         OR  OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                        
297400                                                                          
297500          MOVE +1      TO LDC-TAB-IX                                      
297600          MOVE +100    TO LDC-TAB-IX-MAX                                  
297700                                                                          
297800          PERFORM UNTIL 4010-BERADREF = LDC-TAB-VIPID (LDC-TAB-IX)        
297900                     OR LDC-TAB-VIPID (LDC-TAB-IX) = HIGH-VALUE           
298000                     OR LDC-TAB-IX = LDC-TAB-IX-MAX                       
298100              ADD 1    TO LDC-TAB-IX                                      
298200          END-PERFORM                                                     
298300                                                                          
298400           IF LDC-TAB-VIPID (LDC-TAB-IX) = HIGH-VALUE                     
298500                                                                          
298600             MOVE 4010-BERADREF TO LDC-TAB-VIPID     (LDC-TAB-IX)         
298700             MOVE +1            TO LDC-TAB-ANTAL-ART (LDC-TAB-IX)         
298800                                                                          
298900             IF 4010-BERADREF(4:7) > SPACE                                
299000               ADD +1           TO LDC-TOT-ANT-ART                        
299100             END-IF                                                       
299200           ELSE                                                           
299300             ADD +1             TO LDC-TAB-ANTAL-ART (LDC-TAB-IX)         
299400           END-IF                                                         
299500         END-IF                                                           
299600       END-IF                                                             
299700       END-IF                                                             
299800     END-IF                                                               
299900     .                                                                    
300000     EJECT                                                                
300100 CE-SKRIV-LDC-GB-INFO   SECTION.                                          
300200*LDC-GB                                                                   
300300     IF WS-IDDC NOT = W-IDDC-B6                                           
300400        MOVE WS-IDDC TO W-IDDC-B6                                         
300500        PERFORM IMS-GU-WDB601                                             
300600     END-IF                                                               
300700                                                                          
300800     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
300900                                                                          
301000     IF LDC-TOT-ANT-ART > 0                                               
301100     IF DCS-SDC AND DCS-IDLANDX2 = 'GB'                                   
301200       IF DIST34-ENGLAND-SDC AND                                          
301300          GMT-FLLDCKND = JA                                               
301400         IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                         
301500         OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                         
301600                                                                          
301700             MOVE JA                TO SW-SKRIV-INTE-RAD14                
301800             PERFORM S06-SKRIV-PACKHUVUD                                  
301900                                                                          
302000             MOVE PRT-AFTER-1       TO PRT-RADSKIP                        
302100             MOVE PU-LDC-RAD0       TO WS-PU-RAD                          
302200             PERFORM S07-SKRIV-RAD                                        
302300                                                                          
302400             MOVE PRT-AFTER-1       TO PRT-RADSKIP                        
302500             MOVE PU-LDC-RAD1-RUBRIK TO WS-PU-RAD                         
302600             PERFORM S07-SKRIV-RAD                                        
302700                                                                          
302800             MOVE PRT-AFTER-1       TO PRT-RADSKIP                        
302900             MOVE PU-LDC-RUBRIK-STRECK TO WS-PU-RAD                       
303000             PERFORM S07-SKRIV-RAD                                        
303100                                                                          
303200             MOVE 5                 TO RAD-IX                             
303300                                                                          
303400             MOVE 1   TO  LDC-TAB-IX                                      
303500             PERFORM UNTIL LDC-TAB-VIPID(LDC-TAB-IX) = HIGH-VALUE         
303600                        OR LDC-TAB-IX = LDC-TAB-IX-MAX                    
303700                                                                          
303800               MOVE LDC-TAB-VIPID (LDC-TAB-IX) TO LDC-RAD-BERADREF        
303900         INSPECT LDC-RAD-BERADREF REPLACING LEADING ZERO BY SPACE         
304000         MOVE LDC-TAB-ANTAL-ART (LDC-TAB-IX) TO LDC-RAD-ANTAL-ART         
304100               MOVE PRT-AFTER-1     TO PRT-RADSKIP                        
304200               MOVE PU-LDC-DETALJ-RAD TO WS-PU-RAD                        
304300               PERFORM S07-SKRIV-RAD                                      
304400               ADD 1 TO LDC-TAB-IX                                        
304500               ADD 1 TO RAD-IX                                            
304600                                                                          
304700               IF RAD-IX > 49                                             
304800                 PERFORM S06-SKRIV-PACKHUVUD                              
304900*                        RAD-IX INITIERAS I S06-                          
305000                 MOVE PRT-AFTER-1   TO PRT-RADSKIP                        
305100                 MOVE PU-LDC-RAD1-RUBRIK TO WS-PU-RAD                     
305200                 PERFORM S07-SKRIV-RAD                                    
305300                                                                          
305400                 MOVE PRT-AFTER-1   TO PRT-RADSKIP                        
305500                 MOVE PU-LDC-RUBRIK-STRECK TO WS-PU-RAD                   
305600                 PERFORM S07-SKRIV-RAD                                    
305700                 ADD 3             TO RAD-IX                              
305800               END-IF                                                     
305900             END-PERFORM                                                  
306000                                                                          
306100             MOVE PRT-AFTER-2       TO PRT-RADSKIP                        
306200             MOVE PU-LDC-SISTA-RAD  TO WS-PU-RAD                          
306300             PERFORM S07-SKRIV-RAD                                        
306400         END-IF                                                           
306500       END-IF                                                             
306600     END-IF                                                               
306700     END-IF                                                               
306800     MOVE ZERO              TO LDC-TOT-ANT-ART                            
306900     .                                                                    
307000     EJECT                                                                
307100 D-UPPDAT-PLOCKSATS SECTION.                                              
307200                                                                          
307300     IF PLOCKSATS-EJ-KLAR                                                 
307400        MOVE 4010-IDORDER   TO 4008-IDORDER                               
307500        MOVE 4010-KDPRT     TO 4008-KDPRT                                 
307600        MOVE 4010-KDSS-PU   TO 4008-KDSS                                  
307700        MOVE 4010-ADLAGOMR  TO 4008-ADLAGOMR                              
307800        MOVE 4010-ADGANG    TO 4008-ADGANG                                
307900        MOVE 4010-ADPLATS   TO 4008-ADPLATS                               
308000        MOVE 4010-IDARTNR   TO 4008-IDARTNR                               
308100        MOVE 4010-IDLOPNR   TO 4008-IDLOPNR                               
308200        MOVE WL400711       TO W-PU-PLOCKSATS                             
308300        PERFORM IMS-GHU-4007-WL400711                                     
308400        MOVE W-PU-PLOCKSATS TO WL400711                                   
308500        PERFORM IMS-REPL-4007-WL400711                                    
308600     END-IF                                                               
308700     .                                                                    
308800     EJECT                                                                
308900 E-SKICKA-IMSTRANS SECTION.                                               
309000     MOVE 'STA E-SKICKA-IMSTRANS'        TO PGMPOS                        
309100                                                                          
309200     IF WS-IDDC NOT = W-IDDC-B6                                           
309300        MOVE WS-IDDC TO W-IDDC-B6                                         
309400        PERFORM IMS-GU-WDB601                                             
309500     END-IF                                                               
309600                                                                          
309700     IF PLOCKSATS-KLAR                                                    
309800        MOVE MID-W4I37701 TO PTOP2-MID-W4I37801                           
309900        PERFORM IMS-ISRT-MSG-ALT2                                         
310000                                                                          
310100        IF DIST34-ENGLAND-SDC AND                                         
310200           GMT-FLLDCKND = JA                                              
310300          IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                        
310400          OR  OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                       
310500                                                                          
310600            MOVE 'POS E2-SKICKA-IMSTRANS' TO PGMPOS                       
310700            MOVE '4017'       TO W-4017-IDPRODNR                          
310800            MOVE MID-IDPRODNR TO W-4017-IDPRODNR                          
310900            MOVE MID-IDPLKLST TO W-4017-IDPLKLST                          
311000            MOVE LOW-VALUE    TO W-4017-LOW-VALUE                         
311100            PERFORM IMS-GHU-WDGX4017                                      
311200            IF SEGMENT-FINNS                                              
311300                                                                          
311400              PERFORM IMS-DLET-WDGX4017                                   
311500            END-IF                                                        
311600          END-IF                                                          
311700        END-IF                                                            
311800     ELSE                                                                 
311900*LDC-GB                                                                   
312000       IF DCS-SDC AND DCS-IDLANDX2 = 'GB'                                 
312100         IF DIST34-ENGLAND-SDC AND                                        
312200            GMT-FLLDCKND = JA                                             
312300           IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                       
312400           OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                       
312500                                                                          
312600             MOVE MID-IDPRODNR TO W-4017-IDPRODNR                         
312700             MOVE MID-IDPLKLST TO W-4017-IDPLKLST                         
312800             PERFORM IMS-GHU-WDGX4017                                     
312900                                                                          
313000             IF SEGMENT-SAKNAS                                            
313100               PERFORM IMS-ISRT-WDGX4017                                  
313200             END-IF                                                       
313300                                                                          
313400             PERFORM IMS-GHU-WDGX4018                                     
313500             MOVE +1 TO LDC-TAB-IX                                        
313600             PERFORM UNTIL LDC-TAB-IX = LDC-TAB-IX-MAX                    
313700                                                                          
313800               MOVE LDC-TAB-VIPID (LDC-TAB-IX)                            
313900                 TO 4018-BERADREF (LDC-TAB-IX)                            
314000               MOVE ZERO      TO 4018-KVANTART (LDC-TAB-IX)               
314100               ADD +1         TO LDC-TAB-IX                               
314200             END-PERFORM                                                  
314300                                                                          
314400             IF SEGMENT-FINNS                                             
314500               PERFORM IMS-REPL-WDGX4018                                  
314600             ELSE                                                         
314700               PERFORM IMS-ISRT-WDGX4018                                  
314800             END-IF                                                       
314900           END-IF                                                         
315000         END-IF                                                           
315100         MOVE MID-W4I37701 TO PTOP1-MID-W4I37701                          
315200         PERFORM IMS-ISRT-MSG-ALT1                                        
315300       END-IF                                                             
315400     END-IF                                                               
315500     MOVE 'END E-SKICKA-IMSTRANS'        TO PGMPOS                        
315600     .                                                                    
315700     EJECT                                                                
315800 S01-BRYT-PRINTER SECTION.                                                
315900                                                                          
316000     IF WS-IDDC NOT = W-IDDC-B6                                           
316100        MOVE WS-IDDC TO W-IDDC-B6                                         
316200        PERFORM IMS-GU-WDB601                                             
316300     END-IF                                                               
316400                                                                          
316500     IF DCS-NDC-PF AND DCS-IDLANDX2 = 'JP' AND                            
316600        4008-KDPRT-PU = 'JP1'                                             
316700       IF PU-CLOSE                                                        
316800         PERFORM S04-PURGE-PU                                             
316900         MOVE 'W'              TO WS-SYSTDEL                              
317000         MOVE '00'             TO WS-LISTTYP                              
317100         MOVE '002'            TO WS-KDPRT                                
317200         MOVE '1'              TO FILLER-2                                
317300         MOVE 4008-KDPRT-PU    TO SPAR-PRINTER                            
317400       ELSE                                                               
317500          PERFORM S04-PURGE-PU                                            
317600          MOVE 'W'             TO WS-SYSTDEL                              
317700          MOVE '00'            TO WS-LISTTYP                              
317800          MOVE '002'           TO WS-KDPRT                                
317900          MOVE '1'             TO FILLER-2                                
318000       END-IF                                                             
318100     ELSE                                                                 
318200       IF PU-CLOSE                                                        
318300          MOVE '4'             TO WS-SYSTDEL                              
318400          MOVE 'PU'            TO WS-LISTTYP                              
318500          MOVE 4010-KDPRT      TO WS-KDPRT                                
318600          PERFORM S02-OPEN-PU                                             
318700          MOVE JA TO PU-SW                                                
318800       ELSE                                                               
318900          PERFORM S04-PURGE-PU                                            
319000          MOVE '4'             TO WS-SYSTDEL                              
319100          MOVE 'PU'            TO WS-LISTTYP                              
319200          MOVE 4010-KDPRT      TO WS-KDPRT                                
319300       END-IF                                                             
319400     END-IF                                                               
319500                                                                          
319600     MOVE 4010-KDPRT    TO SPAR-PRINTER                                   
319700     MOVE 4010-KDSS-PU  TO SPAR-KDSS-PU                                   
319800     .                                                                    
319900     EJECT                                                                
320000 S02-OPEN-PU SECTION.                                                     
320100                                                                          
320200     PERFORM S12-LASER-PRINTER                                            
320300                                                                          
320400     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
320500                         PRT-OPEN                                         
320600                         WS-IDPRTLST                                      
320700                         ALT3-PCB                                         
320800                         LISB-PCB                                         
320900                         WS-PU-LISTID                                     
321000                         WS-DUMMY                                         
321100                         WS-DUMMY                                         
321200                                                                          
321300     MOVE JA TO NY-PRINTER-SW                                             
321400     .                                                                    
321500     EJECT                                                                
321600 S03-CLOSE-PU SECTION.                                                    
321700                                                                          
321800     IF WS-IDDC NOT = W-IDDC-B6                                           
321900        MOVE WS-IDDC TO W-IDDC-B6                                         
322000        PERFORM IMS-GU-WDB601                                             
322100     END-IF                                                               
322200                                                                          
322300** LÄGG TILL ALIAS FÖR PRINTER OM SIDBRYTNING                             
322400     IF DCS-SDC AND (DCS-IDLANDX2 = 'GB' OR 'NL' OR 'ES')                 
322500     OR (DCS-CDC AND (WS-KDPRT = 'PU1' OR 'PU3' OR                        
322600                                 'TY1' OR 'LR1' OR 'FB1' OR               
322700                                 'PU9' OR 'SV5' OR 'EU1'))                
322800                                                                          
322900       CALL W006PRR1 USING PRT-SPOOL-A4S                                  
323000                           PRT-WRITE                                      
323100                           WS-IDPRTLST                                    
323200                           ALT3-PCB                                       
323300                           LISB-PCB                                       
323400                           WS-PU-LISTID                                   
323500                           PRT-NYSIDA-RAD1                                
323600                           WS-DUMMY                                       
323700     END-IF                                                               
323800                                                                          
323900     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
324000                         PRT-CLOSE                                        
324100                         WS-IDPRTLST                                      
324200                         ALT3-PCB                                         
324300                         LISB-PCB                                         
324400                         WS-PU-LISTID                                     
324500                         WS-DUMMY                                         
324600                         WS-DUMMY                                         
324700     .                                                                    
324800     EJECT                                                                
324900 S04-PURGE-PU SECTION.                                                    
325000                                                                          
325100     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
325200                         PRT-PURGE                                        
325300                         WS-IDPRTLST                                      
325400                         ALT3-PCB                                         
325500                         LISB-PCB                                         
325600                         WS-PU-LISTID                                     
325700                         WS-DUMMY                                         
325800                         WS-DUMMY                                         
325900     .                                                                    
326000     EJECT                                                                
326100 S05-SKAPA-PACKHUVUD SECTION.                                             
326200                                                                          
326300     PERFORM S05A-LAES-ORDERHUVUD                                         
326400     PERFORM S11-DIST-KUND-LDC                                            
326500     PERFORM S05B-LAES-ORDERDEL                                           
326600     PERFORM S05C-LAES-FRAKTTEXT                                          
326700     PERFORM S05D-LAES-PRC                                                
326800     PERFORM S05F-LAES-TRANSP                                             
326900     PERFORM S05E-REDIGERA-PACKHUVUD                                      
327000     .                                                                    
327100     EJECT                                                                
327200 S05A-LAES-ORDERHUVUD SECTION.                                            
327300                                                                          
327400     MOVE 4010-IDORDER  TO W-IDORDER                                      
327500     MOVE 4010-IDDC     TO W-IDDC                                         
327600                           WS-IDDC                                        
327700     PERFORM IMS-GU-ORQI-WLORQI01-WLORQI12                                
327800     MOVE OHUV-IDDISTR  TO TEST-IDDISTR                                   
327900                                                                          
328000     MOVE OHUV-IDDISTR  TO SW-DIST-SVE-GISLAVED                           
328100     MOVE OHUV-IDKUNDNR TO SW-KUND-GISLAVED                               
328200     .                                                                    
328300     EJECT                                                                
328400 S05B-LAES-ORDERDEL SECTION.                                              
328500                                                                          
328600     IF WS-IDDC NOT = W-IDDC-B6                                           
328700        MOVE WS-IDDC TO W-IDDC-B6                                         
328800        PERFORM IMS-GU-WDB601                                             
328900     END-IF                                                               
329000                                                                          
329100     MOVE 4010-IDORDER  TO W-Q301-MIN-IDORDER                             
329200                           W-Q301-MAX-IDORDER                             
329300     MOVE 4010-IDDC     TO W-Q301-MIN-IDDC                                
329400                           W-Q301-MAX-IDDC                                
329500     MOVE 4010-IDPRODNR TO W-Q301-MIN-IDPRODNR                            
329600                           W-Q301-MAX-IDPRODNR                            
329700     MOVE 4010-IDPLKLST TO W-Q301-MIN-IDPLKLST                            
329800                           W-Q301-MAX-IDPLKLST                            
329900     IF  DCS-CDC                                                          
330000     OR (DCS-SDC  AND DCS-IDLANDX2 = 'SE')                                
330100        MOVE 1          TO DC-INDX                                        
330200     ELSE                                                                 
330300        MOVE 2          TO DC-INDX                                        
330400     END-IF                                                               
330500                                                                          
330600     PERFORM IMS-GU-ORQA-WLORQA01                                         
330700     IF SEGMENT-SAKNAS                                                    
330800        MOVE 'HITTAR EJ ORDERDEL I S05B-SECTIONEN'                        
330900                           TO FELTEXT                                     
331000        CALL FELLOG                                                       
331100     ELSE                                                                 
331200        MOVE ODEL-KDPRODKL TO WS-KDPRODKL                                 
331300        MOVE ODEL-IDORDER  TO WS-IDORDER                                  
331400        MOVE ODEL-IDPRODNR TO WS-IDPRODNR                                 
331500        MOVE ODEL-IDPLKLST TO WS-IDPLKLST                                 
331600     END-IF                                                               
331700     .                                                                    
331800     EJECT                                                                
331900 S05C-LAES-FRAKTTEXT SECTION.                                             
332000                                                                          
332100     IF WS-IDDC NOT = W-IDDC-B6                                           
332200        MOVE WS-IDDC TO W-IDDC-B6                                         
332300        PERFORM IMS-GU-WDB601                                             
332400     END-IF                                                               
332500                                                                          
332600     MOVE ARB-KDFRAKT   TO W-4732-KDFRAKT                                 
332700     PERFORM IMS-GU-4732-WL473211                                         
332800     IF SEGMENT-FINNS                                                     
332900        EVALUATE TRUE                                                     
333000          WHEN  DCS-CDC                                                   
333100            OR (DCS-SDC  AND DCS-IDLANDX2 = 'SE')                         
333200            MOVE FRAKT-BEFRAKT (1)            TO HRAD7-BEFRAKT            
333300          WHEN DCS-SDC  AND DCS-IDLANDX2 = 'ES'                           
333400            MOVE FRAKT-BEFRAKT (4)            TO HRAD7-BEFRAKT            
333500          WHEN DCS-SDC  AND DCS-IDLANDX2 = 'IT'                           
333600            MOVE FRAKT-BEFRAKT (6)            TO HRAD7-BEFRAKT            
333700          WHEN OTHER                                                      
333800            MOVE FRAKT-BEFRAKT (2)            TO HRAD7-BEFRAKT            
333900        END-EVALUATE                                                      
334000     ELSE                                                                 
334100        EVALUATE TRUE                                                     
334200          WHEN  DCS-CDC                                                   
334300            OR (DCS-SDC  AND DCS-IDLANDX2 = 'SE')                         
334400            MOVE 'FRAKTTEXT SAKNAS'           TO HRAD7-BEFRAKT            
334500          WHEN DCS-SDC  AND DCS-IDLANDX2 = 'IT'                           
334600            MOVE 'TEXT DI TRASPORT.SMARRITO' TO  HRAD7-BEFRAKT            
334700          WHEN DCS-SDC  AND DCS-IDLANDX2 = 'ES'                           
334800            MOVE 'NO FETHARO DESCRIPPCCION.' TO  HRAD7-BEFRAKT            
334900          WHEN OTHER                                                      
335000            MOVE 'NO FREIGHT-DESC.'           TO HRAD7-BEFRAKT            
335100        END-EVALUATE                                                      
335200     END-IF                                                               
335300     .                                                                    
335400     EJECT                                                                
335500 S05D-LAES-PRC SECTION.                                                   
335600                                                                          
335700     MOVE 1        TO IX1                                                 
335800     PERFORM UNTIL IX1 > 10                                               
335900        MOVE 0     TO TAB-VLKOLGR  (IX1)                                  
336000                      TAB-SUVOLYM  (IX1)                                  
336100        MOVE 1     TO IX2                                                 
336200        PERFORM UNTIL IX2 > 99                                            
336300           MOVE SPACE    TO TAB-IDPRC    (IX1 IX2)                        
336400           MOVE 0        TO TAB-IDPRODNR (IX1 IX2)                        
336500                            TAB-IDPLKLST (IX1 IX2)                        
336600                            TAB-VLORDNTO (IX1 IX2)                        
336700           ADD 1      TO IX2                                              
336800        END-PERFORM                                                       
336900        ADD 1      TO IX1                                                 
337000     END-PERFORM                                                          
337100                                                                          
337200     MOVE 4010-IDDC     TO W-4447-IDDC                                    
337300     MOVE 4010-IDPRC    TO W-4448-IDPRC-KEY                               
337400     PERFORM IMS-GU-XXKH-WLXXKH11                                         
337500     IF SEGMENT-FINNS                                                     
337600        MOVE WLXXKH11 TO W-SPAR-IDPRC                                     
337700        IF W-SPAR-4448-KDPRCTYP = 2                                       
337800           MOVE 1 TO IX1                                                  
337900           PERFORM UNTIL IX1 > 10                                         
338000              IF W-SPAR-4448-IDPRC-SUB (IX1) NOT = SPACE                  
338100                 MOVE W-SPAR-4448-IDPRC-SUB (IX1)                         
338200                                         TO W-4448-IDPRC-KEY              
338300                 PERFORM IMS-GU-XXKH-WLXXKH11                             
338400                 IF SEGMENT-FINNS                                         
338500                    MOVE 4448-VLKOLGR TO TAB-VLKOLGR (IX1)                
338600                 END-IF                                                   
338700              END-IF                                                      
338800              ADD 1 TO IX1                                                
338900           END-PERFORM                                                    
339000        END-IF                                                            
339100     END-IF                                                               
339200     .                                                                    
339300     EJECT                                                                
339400 S05F-LAES-TRANSP SECTION.                                                
339500                                                                          
339600     MOVE 4010-IDDC        TO W-4433-IDDC                                 
339700     MOVE ARB-IDTRP        TO W-4434-IDTRP                                
339800*    MOVE ZERO             TO W-4434-IDTRP (4:2)                          
339900     PERFORM IMS-GU-WLXXKB01                                              
340000     IF SEGMENT-FINNS                                                     
340100        PERFORM IMS-GNP-WLXXKB11                                          
340200        IF SEGMENT-FINNS AND                                              
340300           W-4434-IDTRP = 4434-IDTRP                                      
340400           MOVE 4434-BETRPFIR TO WS-BETRPFIR                              
340500        ELSE                                                              
340600           MOVE SPACE         TO WS-BETRPFIR                              
340700        END-IF                                                            
340800     ELSE                                                                 
340900        MOVE SPACE            TO WS-BETRPFIR                              
341000     END-IF                                                               
341100                                                                          
341200     MOVE ARB-IDTRP-ALT    TO W-4434-IDTRP                                
341300*    MOVE ZERO             TO W-4434-IDTRP (4:2)                          
341400     PERFORM IMS-GU-WLXXKB01                                              
341500     IF SEGMENT-FINNS                                                     
341600        PERFORM IMS-GNP-WLXXKB11                                          
341700        IF SEGMENT-FINNS AND                                              
341800           W-4434-IDTRP = 4434-IDTRP                                      
341900           MOVE 4434-BETRPFIR TO WS-BETRPFIR-ALT                          
342000        ELSE                                                              
342100           MOVE SPACE         TO WS-BETRPFIR-ALT                          
342200        END-IF                                                            
342300     ELSE                                                                 
342400        MOVE SPACE            TO WS-BETRPFIR-ALT                          
342500     END-IF                                                               
342600     .                                                                    
342700     EJECT                                                                
342800 S05E-REDIGERA-PACKHUVUD SECTION.                                         
342900                                                                          
343000     IF WS-IDDC NOT = W-IDDC-B6                                           
343100        MOVE WS-IDDC TO W-IDDC-B6                                         
343200        PERFORM IMS-GU-WDB601                                             
343300     END-IF                                                               
343400                                                                          
343500     IF DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                                
343600       MOVE OHUV-IDDISTR           TO W-IDDISTR-WDB2                      
343700       MOVE OHUV-IDKUNDNR          TO W-IDKUNDNR-WDB2                     
343800       PERFORM IMS-GU-WDB201                                              
343900       IF SEGMENT-FINNS                                                   
344000         IF GMT-BEGMT-OVR-RAD1 = SPACE                                    
344100           MOVE OHUV-BEGMT-RAD1 TO WS-GMT-BEGMT-OVR-RAD1                  
344200           MOVE OHUV-BEGMT-RAD2 TO WS-GMT-BEGMT-OVR-RAD2                  
344300           MOVE OHUV-ADGMT-GATA TO WS-GMT-ADGMT-OVR-GATA                  
344400           MOVE OHUV-ADGMT-PADR TO WS-GMT-ADGMT-OVR-PADR                  
344500           MOVE OHUV-ADGMT-LAND TO WS-GMT-ADGMT-OVR-LAND                  
344600         ELSE                                                             
344700           MOVE GMT-BEGMT-OVR-RAD1 TO WS-GMT-BEGMT-OVR-RAD1               
344800           MOVE GMT-BEGMT-OVR-RAD2 TO WS-GMT-BEGMT-OVR-RAD2               
344900           MOVE GMT-ADGMT-OVR-GATA TO WS-GMT-ADGMT-OVR-GATA               
345000           MOVE GMT-ADGMT-OVR-PADR TO WS-GMT-ADGMT-OVR-PADR               
345100           MOVE GMT-ADGMT-OVR-LAND TO WS-GMT-ADGMT-OVR-LAND               
345200         END-IF                                                           
345300       ELSE                                                               
345400         MOVE 'ADDRESS IS MISSING' TO WS-GMT-BEGMT-OVR-RAD1               
345500         MOVE 'FROM VIPS-SYSTEM'   TO WS-GMT-BEGMT-OVR-RAD2               
345600         MOVE SPACE                TO WS-GMT-ADGMT-OVR-GATA               
345700                                      WS-GMT-ADGMT-OVR-PADR               
345800                                      WS-GMT-ADGMT-OVR-LAND               
345900       END-IF                                                             
346000     END-IF                                                               
346100                                                                          
346200     MOVE OHUV-IDDISTR           TO W-IDDISTR-WDB2                        
346300     MOVE OHUV-IDKUNDNR          TO W-IDKUNDNR-WDB2                       
346400     PERFORM IMS-GU-WDB201                                                
346500     IF SEGMENT-FINNS                                                     
346600        PERFORM S05EA-REDIGERA-HRAD0-HRAD3                                
346700        PERFORM S05EB-REDIGERA-HRAD4                                      
346800     ELSE                                                                 
346900        MOVE 'ADDRESS IS MISSING' TO WS-GMT-BEGMT-OVR-RAD1                
347000        MOVE 'FROM VIPS-SYSTEM'   TO WS-GMT-BEGMT-OVR-RAD2                
347100        MOVE SPACE                TO WS-GMT-ADGMT-OVR-GATA                
347200                                     WS-GMT-ADGMT-OVR-PADR                
347300                                     WS-GMT-ADGMT-OVR-LAND                
347400     END-IF                                                               
347500                                                                          
347600     MOVE OHUV-IDDISTR     TO DIST03-IDDISTR                              
347700     EVALUATE TRUE                                                        
347800     WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                              
347900       MOVE WS-GMT-ADGMT-OVR-GATA TO HRAD5-ADGMT-GATA                     
348000     WHEN OTHER                                                           
348100       IF GMT-FLLDCKND = YES                                              
348200         MOVE OHUV-ADGMT-GATA   TO HRAD5-ADGMT-GATA                       
348300       ELSE                                                               
348400         MOVE OHUV-ADGMT-GATA   TO HRAD5-ADGMT-GATA                       
348500                                   HRAD5-ADGMT-GATA-ITA                   
348600       END-IF                                                             
348700     END-EVALUATE                                                         
348800                                                                          
348900     EVALUATE TRUE                                                        
349000     WHEN  DCS-CDC                                                        
349100       OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                               
349200        MOVE 'UTSKRIFTS DATUM' TO HRAD6-PRINTDATUM                        
349300     WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                                 
349400        MOVE 'DATA STAMPA    ' TO HRAD6-PRINTDATUM-ITA                    
349500     WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                                 
349600        MOVE 'FECHA IMPR.    ' TO HRAD6-PRINTDATUM-SPA                    
349700     WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                              
349800        MOVE WS-GMT-ADGMT-OVR-PADR TO HRAD6-ADGMT-PADR-JAP                
349900        MOVE ARB-IDTRP      TO HRAD6-IDTRP-JAP                            
350000        MOVE WS-BETRPFIR    TO HRAD6-BETRPFIR-JAP                         
350100     WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')              
350200        MOVE ARB-IDTRP      TO HRAD6-IDTRP-NDC                            
350300        MOVE WS-BETRPFIR    TO HRAD6-BETRPFIR-NDC                         
350400     WHEN OTHER                                                           
350500        MOVE 'PRINT DATE     ' TO HRAD6-PRINTDATUM                        
350600     END-EVALUATE                                                         
350700                                                                          
350800     MOVE OHUV-IDDISTR     TO DIST03-IDDISTR                              
350900     MOVE OHUV-ADGMT-PADR  TO HRAD6-ADGMT-PADR                            
351000                              HRAD6-ADGMT-PADR-FRA                        
351100                              HRAD6-ADGMT-PADR-ITA                        
351200                              HRAD6-ADGMT-PADR-NDC                        
351300                                                                          
351400     PERFORM S05EC-REDIGERA-HRAD7                                         
351500     PERFORM S05ED-REDIGERA-HRAD8                                         
351600     PERFORM S05EE-REDIGERA-HRAD9                                         
351700     PERFORM S05EF-REDIGERA-HRAD10-HRAD11                                 
351800     PERFORM S05EG-REDIGERA-HRAD12-HRAD13                                 
351900     .                                                                    
352000     EJECT                                                                
352100 S05EA-REDIGERA-HRAD0-HRAD3 SECTION.                                      
352200                                                                          
352300     IF WS-IDDC NOT = W-IDDC-B6                                           
352400        MOVE WS-IDDC TO W-IDDC-B6                                         
352500        PERFORM IMS-GU-WDB601                                             
352600     END-IF                                                               
352700                                                                          
352800     MOVE '*COPY*'             TO HRAD0-COPY                              
352900     MOVE SPACE TO HRAD1-TEXT                                             
353000                                                                          
353100     IF OHUV-FLFORBI = JA OR                                              
353200        OHUV-FLFORBI = SPEC-FORBI                                         
353300        EVALUATE TRUE                                                     
353400          WHEN  DCS-CDC                                                   
353500            OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                          
353600            MOVE '** F Ö R B I O R D E R **'  TO HRAD1-TEXT               
353700          WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                            
353800            MOVE '*O R D I N E   DI A T T E S A*' TO HRAD1-TEXT           
353900          WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                            
354000            MOVE '*O R D I N E   DI A T T E S A*' TO HRAD1-TEXT           
354100          WHEN OTHER                                                      
354200            MOVE '* B Y P A S S   O R D E R *' TO HRAD1-TEXT              
354300        END-EVALUATE                                                      
354400     END-IF                                                               
354500                                                                          
354600     MOVE '  '                 TO HRAD2-PV-LV                             
354700     MOVE OHUV-IDDISTR         TO HRAD2-IDDISTR                           
354800     MOVE OHUV-IDKUNDNR        TO HRAD2-IDKUNDNR                          
354900     MOVE 4010-IDKUNDRF        TO HRAD2-IDKUNDRF                          
355000     INSPECT HRAD2-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
355100     IF HRAD2-IDKUNDRF = SPACE                                            
355200        MOVE '      0' TO HRAD2-IDKUNDRF                                  
355300     END-IF                                                               
355400     MOVE OHUV-KDORDKL         TO HRAD2-KDORDKL                           
355500     MOVE 4010-IDBORD          TO HRAD2-IDBORD                            
355600     MOVE 4010-IDUSER          TO HRAD2-IDUSER                            
355700     INSPECT HRAD2-IDUSER REPLACING LEADING ZERO BY SPACE                 
355800     MOVE 4010-IDPRODNR        TO HRAD2-IDPRODNR                          
355900     MOVE ODEL-IDPLKLST        TO HRAD2-IDPLKLST                          
356000                                                                          
356100     MOVE OHUV-IDDISTR     TO DIST03-IDDISTR                              
356200     EVALUATE TRUE                                                        
356300        WHEN  DCS-CDC                                                     
356400          OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                            
356500          MOVE OHUV-BEGMT-RAD1 TO HRAD3-BEGMT-RAD1                        
356600          MOVE '      PRC'     TO HRAD3-PRC                               
356700                                  HRAD3-2-PRC                             
356800          MOVE 'LÖPNR '        TO HRAD3-LOPNR                             
356900                                  HRAD3-2-LOPNR                           
357000          MOVE 'REG.DATUM   '  TO HRAD3-REGDATUM                          
357100          MOVE SPACE           TO HRAD3-ZON                               
357200        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
357300          MOVE OHUV-BEGMT-RAD1 TO HRAD3-BEGMT-RAD1-ITA                    
357400          MOVE 'CAN.PROD.'     TO HRAD3-PRC-ITA                           
357500                                  HRAD3-2-PRC                             
357600          MOVE 'P.UNIT'        TO HRAD3-LOPNR-ITA                         
357700                                  HRAD3-2-LOPNR                           
357800          MOVE 'DATA REG.   '  TO HRAD3-REGDATUM-ITA                      
357900        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
358000          MOVE OHUV-BEGMT-RAD1 TO HRAD3-BEGMT-RAD1-SPA                    
358100          MOVE 'CAN.PROD.'     TO HRAD3-PRC-SPA                           
358200                                  HRAD3-2-PRC                             
358300          MOVE 'NO SER.'       TO HRAD3-LOPNR-SPA                         
358400                                  HRAD3-2-LOPNR                           
358500          MOVE 'FECHA REG.  '  TO HRAD3-REGDATUM-SPA                      
358600        WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                              
358700          MOVE OHUV-BEGMT-RAD1 TO HRAD3-BEGMT-RAD1-OST                    
358800          MOVE '      PRC'     TO HRAD3-PRC-OST                           
358900                                  HRAD3-2-PRC                             
359000          MOVE 'LAUFENDES NR'  TO HRAD3-LOPNR-OST                         
359100                                  HRAD3-2-LOPNR                           
359200          MOVE 'REG.DATUM   '  TO HRAD3-REGDATUM-OST                      
359300                                                                          
359400        WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                           
359500          MOVE WS-GMT-BEGMT-OVR-RAD1 TO HRAD3-BEGMT-RAD1-JAP              
359600          MOVE '      PRC'        TO HRAD3-PRC-JAP                        
359700          MOVE '   PU'            TO HRAD3-LOPNR-JAP                      
359800          MOVE 4010-TIREGDAT      TO WS-DATUM                             
359900          MOVE 4010-TIREGTID      TO WS-TID                               
360000          MOVE WS-DATUM           TO HRAD3-TIREGDAT-JAP                   
360100          MOVE WS-TID (1:2)       TO HRAD3-TIREGHH-JAP                    
360200          MOVE WS-TID (3:2)       TO HRAD3-TIREGMM-JAP                    
360300        WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')           
360400          MOVE OHUV-BEGMT-RAD1 TO HRAD3-BEGMT-RAD1-NDC                    
360500          MOVE '      PRC'     TO HRAD3-PRC-NDC                           
360600          MOVE '   PU'         TO HRAD3-LOPNR-NDC                         
360700          MOVE 4010-TIREGDAT   TO WS-DATUM                                
360800          MOVE 4010-TIREGTID   TO WS-TID                                  
360900          MOVE WS-DATUM        TO HRAD3-TIREGDAT-NDC                      
361000          MOVE WS-TID (1:2)    TO HRAD3-TIREGHH-NDC                       
361100          MOVE WS-TID (3:2)    TO HRAD3-TIREGMM-NDC                       
361200        WHEN OTHER                                                        
361300          MOVE OHUV-BEGMT-RAD1 TO HRAD3-BEGMT-RAD1                        
361400          MOVE '      PRC'     TO HRAD3-PRC                               
361500                                  HRAD3-2-PRC                             
361600          MOVE '   PU'         TO HRAD3-LOPNR                             
361700                                  HRAD3-2-LOPNR                           
361800          MOVE 'REG.DATE    '  TO HRAD3-REGDATUM                          
361900          IF DCS-SDC AND DCS-IDLANDX2 = 'GB'                              
362000            MOVE 'ZONE'        TO HRAD3-ZON                               
362100          ELSE                                                            
362200            MOVE SPACE         TO HRAD3-ZON                               
362300          END-IF                                                          
362400     END-EVALUATE                                                         
362500                                                                          
362600     .                                                                    
362700     EJECT                                                                
362800 S05EB-REDIGERA-HRAD4 SECTION.                                            
362900                                                                          
363000     IF WS-IDDC NOT = W-IDDC-B6                                           
363100        MOVE WS-IDDC TO W-IDDC-B6                                         
363200        PERFORM IMS-GU-WDB601                                             
363300     END-IF                                                               
363400                                                                          
363500     MOVE OHUV-IDDISTR     TO DIST03-IDDISTR                              
363600     EVALUATE TRUE                                                        
363700     WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                              
363800       MOVE WS-GMT-BEGMT-OVR-RAD2 TO HRAD4-BEGMT-RAD2-JAP                 
363900       MOVE 4010-IDPRC         TO HRAD4-IDPRC-JAP                         
364000       MOVE 4010-IDLOPNR-PL    TO HRAD4-IDLOPNR-PL-JAP                    
364100       MOVE 4010-IDLOPNR-ORD   TO HRAD4-IDLOPNR-ORD-JAP                   
364200                                                                          
364300       MOVE ODEL-DAUTSKR (3:6) TO WS-DATUM                                
364400       MOVE ODEL-TIUTSTID      TO WS-TID                                  
364500       MOVE WS-DATUM           TO HRAD4-TIUTSKR-JAP                       
364600       MOVE WS-TID (1:2)       TO HRAD4-TIUTSHH-JAP                       
364700       MOVE WS-TID (3:2)       TO HRAD4-TIUTSMM-JAP                       
364800     WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')              
364900       MOVE OHUV-BEGMT-RAD2    TO HRAD4-BEGMT-RAD2-NDC                    
365000       MOVE 4010-IDPRC         TO HRAD4-IDPRC-NDC                         
365100       MOVE 4010-IDLOPNR-PL    TO HRAD4-IDLOPNR-PL-NDC                    
365200       MOVE 4010-IDLOPNR-ORD   TO HRAD4-IDLOPNR-ORD-NDC                   
365300                                                                          
365400       MOVE ODEL-DAUTSKR (3:6) TO WS-DATUM                                
365500       MOVE ODEL-TIUTSTID      TO WS-TID                                  
365600       MOVE WS-DATUM           TO HRAD4-TIUTSKR-NDC                       
365700       MOVE WS-TID (1:2)       TO HRAD4-TIUTSHH-NDC                       
365800       MOVE WS-TID (3:2)       TO HRAD4-TIUTSMM-NDC                       
365900     WHEN OTHER                                                           
366000       MOVE OHUV-BEGMT-RAD2    TO HRAD4-BEGMT-RAD2                        
366100       MOVE 4010-IDPRC         TO HRAD4-IDPRC                             
366200                                  HRAD4-2-IDPRC                           
366300       MOVE 4010-IDLOPNR-PL    TO HRAD4-IDLOPNR-PL                        
366400                                  HRAD4-2-IDLOPNR-PL                      
366500       MOVE 4010-IDLOPNR-ORD   TO HRAD4-IDLOPNR-ORD                       
366600       MOVE 4010-IDZON         TO HRAD4-IDZON                             
366700                                                                          
366800       MOVE 4010-TIREGDAT      TO WS-DATUM                                
366900       MOVE 4010-TIREGTID      TO WS-TID                                  
367000       MOVE WS-DATUM           TO HRAD4-TIREGDAT                          
367100       MOVE WS-TID (1:2)       TO HRAD4-TIREGHH                           
367200       MOVE WS-TID (3:2)       TO HRAD4-TIREGMM                           
367300     END-EVALUATE                                                         
367400                                                                          
367500     .                                                                    
367600     EJECT                                                                
367700 S05EC-REDIGERA-HRAD7 SECTION.                                            
367800                                                                          
367900     IF WS-IDDC NOT = W-IDDC-B6                                           
368000        MOVE WS-IDDC TO W-IDDC-B6                                         
368100        PERFORM IMS-GU-WDB601                                             
368200     END-IF                                                               
368300                                                                          
368400     MOVE OHUV-IDDISTR     TO DIST03-IDDISTR                              
368500     EVALUATE TRUE                                                        
368600     WHEN DCS-SDC AND DCS-IDLANDX2 = 'SE'                                 
368700       MOVE 4010-BERADREF        TO HRAD7-ADGMT-LAND                      
368800       MOVE ARB-KDFRAKT          TO HRAD7-KDFRAKT                         
368900                                                                          
369000       MOVE ODEL-DAUTSKR (3:6)   TO WS-DATUM                              
369100       MOVE ODEL-TIUTSTID        TO WS-TID                                
369200       MOVE WS-DATUM             TO HRAD7-TIUTSKR                         
369300                                                                          
369400       MOVE WS-TID (1:2)         TO HRAD7-TIUTSHH                         
369500       MOVE WS-TID (3:2)         TO HRAD7-TIUTSMM                         
369600     WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                              
369700       MOVE WS-GMT-ADGMT-OVR-LAND TO HRAD7-ADGMT-LAND-JAP                 
369800       MOVE ARB-KDFRAKT          TO HRAD7-KDFRAKT-JAP                     
369900                                                                          
370000       MOVE SPACE                TO HRAD7-IDTRP-JAP                       
370100                                    HRAD7-TXT-ALT-JAP                     
370200                                    HRAD7-TXT-CARRIER-JAP                 
370300                                    HRAD7-IDTRP-JAP                       
370400       MOVE WS-BETRPFIR-ALT      TO HRAD7-BETRPFIR-JAP                    
370500     WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')              
370600       MOVE OHUV-ADGMT-LAND      TO HRAD7-ADGMT-LAND-NDC                  
370700       MOVE ARB-KDFRAKT          TO HRAD7-KDFRAKT-NDC                     
370800                                                                          
370900       IF OHUV-KDORDKL = 0 OR 1                                           
371000       AND DCS-NDC-NA AND DCS-IDLANDX2 = 'US'                             
371100         MOVE SPACE              TO HRAD7-IDTRP-NDC                       
371200       ELSE                                                               
371300         MOVE '  ALT. '          TO HRAD7-TXT-ALT                         
371400         MOVE 'CARRIER: '        TO HRAD7-TXT-CARRIER                     
371500         MOVE ARB-IDTRP-ALT      TO HRAD7-IDTRP-NDC                       
371600       END-IF                                                             
371700       MOVE WS-BETRPFIR-ALT      TO HRAD7-BETRPFIR-NDC                    
371800     WHEN OTHER                                                           
371900       IF GMT-FLLDCKND = YES                                              
372000         MOVE 4010-BERADREF     TO HRAD7-ADGMT-LAND                       
372100       ELSE                                                               
372200         MOVE OHUV-ADGMT-LAND   TO HRAD7-ADGMT-LAND                       
372300       END-IF                                                             
372400       MOVE ARB-KDFRAKT         TO HRAD7-KDFRAKT                          
372500                                                                          
372600       MOVE ODEL-DAUTSKR (3:6)  TO WS-DATUM                               
372700       MOVE ODEL-TIUTSTID       TO WS-TID                                 
372800       MOVE WS-DATUM            TO HRAD7-TIUTSKR                          
372900                                                                          
373000       MOVE WS-TID (1:2)        TO HRAD7-TIUTSHH                          
373100       MOVE WS-TID (3:2)        TO HRAD7-TIUTSMM                          
373200     END-EVALUATE                                                         
373300     .                                                                    
373400     EJECT                                                                
373500 S05ED-REDIGERA-HRAD8 SECTION.                                            
373600                                                                          
373700     IF WS-IDDC NOT = W-IDDC-B6                                           
373800        MOVE WS-IDDC TO W-IDDC-B6                                         
373900        PERFORM IMS-GU-WDB601                                             
374000     END-IF                                                               
374100                                                                          
374200     IF OHUV-BEKUNDRF = SPACE                                             
374300        MOVE SPACE             TO HRAD8-TEKUNDRF                          
374400                                  HRAD8-BEKUNDRF                          
374500     ELSE                                                                 
374600        EVALUATE TRUE                                                     
374700        WHEN  DCS-CDC                                                     
374800          OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                            
374900           MOVE 'KUND REF: '   TO HRAD8-TEKUNDRF                          
375000           MOVE OHUV-BEKUNDRF  TO HRAD8-BEKUNDRF                          
375100        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
375200           MOVE 'RIF. CL. :'   TO HRAD8-ITA-TEKUNDRF                      
375300           MOVE OHUV-BEKUNDRF  TO HRAD8-ITA-BEKUNDRF                      
375400        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
375500           MOVE 'REF.CLIENT'   TO HRAD8-SPA-TEKUNDRF                      
375600           MOVE OHUV-BEKUNDRF  TO HRAD8-SPA-BEKUNDRF                      
375700        WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')           
375800           MOVE 'CUST REF: '   TO HRAD8-NDC-TEKUNDRF                      
375900           MOVE OHUV-BEKUNDRF  TO HRAD8-NDC-BEKUNDRF                      
376000        WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                           
376100           MOVE 'CUST REF: '   TO HRAD8-JAP-TEKUNDRF                      
376200           MOVE OHUV-BEKUNDRF  TO HRAD8-JAP-BEKUNDRF                      
376300        WHEN OTHER                                                        
376400           MOVE 'CUST REF: '   TO HRAD8-TEKUNDRF                          
376500           MOVE OHUV-BEKUNDRF  TO HRAD8-BEKUNDRF                          
376600        END-EVALUATE                                                      
376700     END-IF                                                               
376800                                                                          
376900     IF OHUV-BEVARREF = SPACE                                             
377000        MOVE SPACE             TO HRAD8-TEVARREF                          
377100                                  HRAD8-BEVARREF                          
377200     ELSE                                                                 
377300        EVALUATE TRUE                                                     
377400        WHEN  DCS-CDC                                                     
377500          OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                            
377600           MOVE 'KUND REF: '   TO HRAD8-TEKUNDRF                          
377700           MOVE OHUV-BEVARREF  TO HRAD8-BEVARREF                          
377800           MOVE OHUV-BEVARREF  TO HRAD8-FRA-BEVARREF                      
377900        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
378000           MOVE 'VOLVO RIF: '  TO HRAD8-ITA-TEVARREF                      
378100           MOVE OHUV-BEVARREF  TO HRAD8-ITA-BEVARREF                      
378200        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
378300           MOVE 'REF.CLIENT'   TO HRAD8-SPA-TEKUNDRF                      
378400           MOVE OHUV-BEVARREF  TO HRAD8-SPA-BEVARREF                      
378500        WHEN DCS-NDC-PF OR DCS-NDC-PF                                     
378600           MOVE 'CUST REF: '   TO HRAD8-JAP-TEKUNDRF                      
378700           MOVE OHUV-BEVARREF  TO HRAD8-JAP-BEVARREF                      
378800        WHEN OTHER                                                        
378900           MOVE 'VOLVO REF: '  TO HRAD8-TEVARREF                          
379000           MOVE OHUV-BEVARREF  TO HRAD8-BEVARREF                          
379100        END-EVALUATE                                                      
379200     END-IF                                                               
379300     .                                                                    
379400     EJECT                                                                
379500 S05EE-REDIGERA-HRAD9 SECTION.                                            
379600                                                                          
379700     IF WS-IDDC NOT = W-IDDC-B6                                           
379800        MOVE WS-IDDC TO W-IDDC-B6                                         
379900        PERFORM IMS-GU-WDB601                                             
380000     END-IF                                                               
380100                                                                          
380200     MOVE ODEL-KDFDKRAV        TO W-4535-KDFDKRAV                         
380300     EVALUATE TRUE                                                        
380400       WHEN  DCS-CDC                                                      
380500         OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                             
380600          MOVE 'S  '           TO W-4536-IDSKYLT                          
380700          MOVE 'SAKNAS'        TO HRAD9-BEFDKRAV                          
380800       WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                               
380900          MOVE 'I  '           TO W-4536-IDSKYLT                          
381000          MOVE 'SMARRITO'      TO HRAD9-BEFDKRAV                          
381100       WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                               
381200          MOVE 'E  '           TO W-4536-IDSKYLT                          
381300          MOVE ' FALTA  '      TO HRAD9-BEFDKRAV                          
381400       WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')            
381500          MOVE 'TRANSPORTEMBALLAGE:  ' TO PU-HRAD12-TEXT                  
381600          MOVE 'GB '           TO W-4536-IDSKYLT                          
381700          MOVE 'MISSING'       TO HRAD12-BEFDKRAV                         
381800       WHEN OTHER                                                         
381900          MOVE 'GB '           TO W-4536-IDSKYLT                          
382000          MOVE 'MISSING'       TO HRAD9-BEFDKRAV                          
382100     END-EVALUATE                                                         
382200     PERFORM IMS-GU-XXKU-WLXXKU11                                         
382300     IF SEGMENT-FINNS                                                     
382400        EVALUATE TRUE                                                     
382500          WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')         
382600            MOVE 4536-BEFDKRAV TO HRAD12-BEFDKRAV                         
382700          WHEN OTHER                                                      
382800            MOVE 4536-BEFDKRAV TO HRAD9-BEFDKRAV                          
382900       END-EVALUATE                                                       
383000     END-IF                                                               
383100     MOVE 4010-TIRFS           TO WS-DATUM-TID                            
383200     EVALUATE TRUE                                                        
383300       WHEN DCS-NDC-NA OR DCS-NDC-PF                                      
383400         MOVE WS-DATUM-TID (2:6) TO HRAD12-TIRFSDAT                       
383500         MOVE WS-DATUM-TID (8:2) TO HRAD12-TIRFSHH                        
383600         MOVE WS-DATUM-TID (10:2) TO HRAD12-TIRFSMM                       
383700       WHEN OTHER                                                         
383800         MOVE WS-DATUM-TID (2:6) TO HRAD9-TIRFSDAT                        
383900         MOVE WS-DATUM-TID (8:2) TO HRAD9-TIRFSHH                         
384000         MOVE WS-DATUM-TID (10:2) TO HRAD9-TIRFSMM                        
384100     END-EVALUATE                                                         
384200     .                                                                    
384300     EJECT                                                                
384400 S05EF-REDIGERA-HRAD10-HRAD11 SECTION.                                    
384500                                                                          
384600     IF WS-IDDC NOT = W-IDDC-B6                                           
384700        MOVE WS-IDDC TO W-IDDC-B6                                         
384800        PERFORM IMS-GU-WDB601                                             
384900     END-IF                                                               
385000                                                                          
385100     MOVE SPACE TO HRAD10-TEGDSMRK                                        
385200                   HRAD10-BEGMRK-RAD1                                     
385300                   HRAD11-BEGMRK-RAD2                                     
385400                                                                          
385500     IF ARB-BEGMRK NOT = SPACE                                            
385600        EVALUATE TRUE                                                     
385700        WHEN  DCS-CDC                                                     
385800          OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                            
385900           MOVE 'GODSMÄRKNING:'     TO HRAD10-TEGDSMRK                    
386000        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
386100           MOVE 'NOTE CLIENTE  :'   TO HRAD10-TEGDSMRK                    
386200        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
386300           MOVE 'IDENTI.MERCANCIA'  TO HRAD10-TEGDSMRK                    
386400        WHEN OTHER                                                        
386500           MOVE 'GOODSMARKING:'     TO HRAD10-TEGDSMRK                    
386600        END-EVALUATE                                                      
386700        MOVE ARB-BEGMRK-RAD1        TO HRAD10-BEGMRK-RAD1                 
386800        MOVE ARB-BEGMRK-RAD2        TO HRAD11-BEGMRK-RAD2                 
386900     END-IF                                                               
387000     .                                                                    
387100     EJECT                                                                
387200 S05EG-REDIGERA-HRAD12-HRAD13 SECTION.                                    
387300                                                                          
387400     IF WS-IDDC NOT = W-IDDC-B6                                           
387500        MOVE WS-IDDC TO W-IDDC-B6                                         
387600        PERFORM IMS-GU-WDB601                                             
387700     END-IF                                                               
387800                                                                          
387900     MOVE SPACE TO HRAD12-TELAGINS                                        
388000                   HRAD12-BELAGINS-DEL1                                   
388100                   HRAD13-BELAGINS-DEL2                                   
388200                                                                          
388300     IF OHUV-BELAGINS-GRP NOT = SPACE                                     
388400                                                                          
388500        EVALUATE  TRUE                                                    
388600                                                                          
388700          WHEN  DCS-CDC                                                   
388800            OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                          
388900             MOVE 'LAGERINSTRUKT:'      TO HRAD12-TELAGINS                
389000          WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                            
389100             MOVE 'MAN.DIMAGAZZINO:'    TO HRAD12-TELAGINS                
389200          WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                            
389300             MOVE 'INST. ALMACEN:'    TO HRAD12-TELAGINS                  
389400          WHEN OTHER                                                      
389500             MOVE 'WAREHOUSE INSTR:'    TO HRAD12-TELAGINS                
389600                                                                          
389700        END-EVALUATE                                                      
389800                                                                          
389900        IF OHUV-BELAGINS-DEL1 = SPACE                                     
390000           MOVE OHUV-BELAGINS-DEL2    TO HRAD12-BELAGINS-DEL1             
390100        ELSE                                                              
390200           MOVE OHUV-BELAGINS-DEL1    TO HRAD12-BELAGINS-DEL1             
390300           IF OHUV-BELAGINS-DEL2 NOT = SPACE                              
390400                                                                          
390500             MOVE OHUV-BELAGINS-DEL2 TO HRAD13-BELAGINS-DEL2              
390600           END-IF                                                         
390700        END-IF                                                            
390800     END-IF                                                               
390900     .                                                                    
391000     EJECT                                                                
391100 S06-SKRIV-PACKHUVUD SECTION.                                             
391200                                                                          
391300     IF WS-IDDC NOT = W-IDDC-B6                                           
391400        MOVE WS-IDDC TO W-IDDC-B6                                         
391500        PERFORM IMS-GU-WDB601                                             
391600     END-IF                                                               
391700                                                                          
391800     ADD 1                  TO 4008-IDSID                                 
391900     MOVE 4008-IDSID        TO HRAD1-IDSID                                
392000     IF DCS-CDC                                                           
392100     AND (WS-KDPRT = '002'                                                
392200     OR   WS-KDPRT = 'PUB'                                                
392300     OR   WS-KDPRT = 'PU2'                                                
392400     OR   WS-KDPRT = 'PU8'                                                
392500     OR   WS-KDPRT = 'TY1'                                                
392600     OR   WS-KDPRT = 'LR1')                                               
392700     AND FORSTA-LIST-SIDA                                                 
392800       MOVE PRT-AFTER-3     TO PRT-RADSKIP                                
392900       MOVE NEJ             TO FORSTA-LIST-SIDA-SW                        
393000     ELSE                                                                 
393100                                                                          
393200       IF DC99-LDC-GB                                                     
393300       AND FORSTA-LIST-SIDA                                               
393400         MOVE PRT-AFTER-3   TO PRT-RADSKIP                                
393500         MOVE NEJ           TO FORSTA-LIST-SIDA-SW                        
393600       ELSE                                                               
393700         IF (DCS-SDC AND DCS-IDLANDX2 = 'SE')                             
393800         AND (WS-KDPRT = 'EN1'                                            
393900         OR WS-KDPRT = '1A1')                                             
394000         AND FORSTA-LIST-SIDA                                             
394100           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
394200           MOVE NEJ         TO FORSTA-LIST-SIDA-SW                        
394300         ELSE                                                             
394400           MOVE PRT-NYSIDA-RAD3 TO PRT-RADSKIP                            
394500         END-IF                                                           
394600       END-IF                                                             
394700     END-IF                                                               
394800     MOVE PU-HRAD0          TO WS-PU-RAD                                  
394900     PERFORM S07-SKRIV-RAD                                                
395000                                                                          
395100     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
395200     MOVE PU-HRAD1          TO WS-PU-RAD                                  
395300     PERFORM S07-SKRIV-RAD                                                
395400                                                                          
395500     MOVE PRT-AFTER-3       TO PRT-RADSKIP                                
395600     MOVE PU-HRAD2          TO WS-PU-RAD                                  
395700     PERFORM S07-SKRIV-RAD                                                
395800                                                                          
395900     MOVE 6                 TO RAD-IX                                     
396000                                                                          
396100     IF FORSTA-SIDA                                                       
396200        PERFORM S06A-OVRIGA-HUVUDRADER                                    
396300        MOVE NEJ TO FORSTA-SIDA-SW                                        
396400     ELSE                                                                 
396500        IF DCS-CDC OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                   
396600           PERFORM S06B-SIDA2-HUVUDRADER                                  
396700           ADD 6  TO RAD-IX                                               
396800        ELSE                                                              
396900           PERFORM S06C-SIDA2-HUVUDRADER                                  
397000           ADD 8  TO RAD-IX                                               
397100           IF DCS-SDC AND DCS-IDLANDX2 = 'GB'                             
397200             ADD 2  TO RAD-IX                                             
397300           END-IF                                                         
397400        END-IF                                                            
397500     END-IF                                                               
397600                                                                          
397700     ADD 1     TO SID-IX                                                  
397800     .                                                                    
397900     EJECT                                                                
398000 S06A-OVRIGA-HUVUDRADER SECTION.                                          
398100                                                                          
398200     IF WS-IDDC NOT = W-IDDC-B6                                           
398300        MOVE WS-IDDC TO W-IDDC-B6                                         
398400        PERFORM IMS-GU-WDB601                                             
398500     END-IF                                                               
398600                                                                          
398700     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
398800                                                                          
398900      EVALUATE  TRUE                                                      
399000        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
399100           MOVE ' N§ '     TO PU-HRAD3-TEXT-ITA                           
399200           MOVE PU-HRAD3-ITA TO WS-PU-RAD                                 
399300        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
399400           MOVE ' NO.'     TO PU-HRAD3-TEXT-SPA                           
399500           MOVE PU-HRAD3-SPA TO WS-PU-RAD                                 
399600        WHEN DCS-SDC AND DCS-IDLANDX2 = 'GB'                              
399700           MOVE ' NO.'     TO PU-HRAD3-TEXT                               
399800           MOVE PU-HRAD3   TO WS-PU-RAD                                   
399900        WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                              
400000           MOVE ' NR.'     TO PU-HRAD3-TEXT-OST                           
400100           MOVE PU-HRAD3-OST TO WS-PU-RAD                                 
400200        WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                           
400300           MOVE ' NO.'     TO PU-HRAD3-TEXT                               
400400           MOVE PU-HRAD3-JAP  TO WS-PU-RAD                                
400500        WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')           
400600           MOVE ' NO.'     TO PU-HRAD3-TEXT                               
400700           MOVE PU-HRAD3-NDC  TO WS-PU-RAD                                
400800        WHEN OTHER                                                        
400900           MOVE ' NR '     TO PU-HRAD3-TEXT                               
401000           MOVE PU-HRAD3   TO WS-PU-RAD                                   
401100      END-EVALUATE                                                        
401200                                                                          
401300     PERFORM S07-SKRIV-RAD                                                
401400                                                                          
401500     IF PU-HRAD4 = SPACE                                                  
401600        MOVE PRT-AFTER-2  TO PRT-RADSKIP                                  
401700        EVALUATE  TRUE                                                    
401800          WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                            
401900             MOVE PU-HRAD5-ITA TO WS-PU-RAD                               
402000          WHEN OTHER                                                      
402100             MOVE PU-HRAD5     TO WS-PU-RAD                               
402200        END-EVALUATE                                                      
402300        PERFORM S07-SKRIV-RAD                                             
402400     ELSE                                                                 
402500        MOVE PRT-AFTER-1  TO PRT-RADSKIP                                  
402600        EVALUATE  TRUE                                                    
402700          WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                         
402800             MOVE PU-HRAD4-JAP TO WS-PU-RAD                               
402900          WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')         
403000             MOVE PU-HRAD4-NDC TO WS-PU-RAD                               
403100          WHEN OTHER                                                      
403200             MOVE PU-HRAD4     TO WS-PU-RAD                               
403300        END-EVALUATE                                                      
403400        PERFORM S07-SKRIV-RAD                                             
403500                                                                          
403600        MOVE PRT-AFTER-1  TO PRT-RADSKIP                                  
403700        EVALUATE  TRUE                                                    
403800          WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                            
403900             MOVE PU-HRAD5-ITA TO WS-PU-RAD                               
404000          WHEN OTHER                                                      
404100             MOVE PU-HRAD5     TO WS-PU-RAD                               
404200        END-EVALUATE                                                      
404300        PERFORM S07-SKRIV-RAD                                             
404400                                                                          
404500        MOVE PRT-AFTER-1  TO PRT-RADSKIP                                  
404600        EVALUATE  TRUE                                                    
404700          WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                            
404800             MOVE PU-HRAD6-ITA TO WS-PU-RAD                               
404900          WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                            
405000             MOVE PU-HRAD6-SPA TO WS-PU-RAD                               
405100          WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                         
405200             MOVE PU-HRAD6-JAP TO WS-PU-RAD                               
405300          WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')         
405400             MOVE PU-HRAD6-NDC TO WS-PU-RAD                               
405500          WHEN OTHER                                                      
405600             MOVE PU-HRAD6     TO WS-PU-RAD                               
405700        END-EVALUATE                                                      
405800        PERFORM S07-SKRIV-RAD                                             
405900     END-IF                                                               
406000                                                                          
406100     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
406200     EVALUATE  TRUE                                                       
406300       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
406400          MOVE PU-HRAD7-JAP TO WS-PU-RAD                                  
406500       WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')            
406600          MOVE PU-HRAD7-NDC TO WS-PU-RAD                                  
406700       WHEN OTHER                                                         
406800          MOVE PU-HRAD7     TO WS-PU-RAD                                  
406900     END-EVALUATE                                                         
407000     PERFORM S07-SKRIV-RAD                                                
407100                                                                          
407300     IF OHUV-IDSYSTEM (1:3) = 'LYN'                                       
407400     OR OHUV-IDSYSTEM (1:3) = 'POL'                                       
407500*PRINT CONTACT PERSON + TELNR + MAILADDRESS                               
407600       IF OHUV-BEBETRAD-1 > SPACE                                         
407700       OR OHUV-BETELNR    > SPACE                                         
407800                                                                          
407900         MOVE PRT-AFTER-2   TO PRT-RADSKIP                                
408000         MOVE OHUV-BEBETRAD-1   TO CONTACT-NAME-B2C                       
408100         MOVE OHUV-BETELNR      TO CONTACT-BETELNR-B2C                    
408200         MOVE PU-CONTACT-INFO-LINE   TO WS-PU-RAD                         
408300         PERFORM S07-SKRIV-RAD                                            
408400         ADD  1             TO RAD-IX                                     
408500       END-IF                                                             
408600*                                                                         
408700       IF OHUV-IDMAIL     > SPACE                                         
408800         MOVE PRT-AFTER-1       TO PRT-RADSKIP                            
408900         MOVE OHUV-IDMAIL       TO CONTACT-MAIL-INFO                      
409000         MOVE PU-CONTACT-MAIL-LINE   TO WS-PU-RAD                         
409100         PERFORM S07-SKRIV-RAD                                            
409200                                                                          
409300         ADD  1             TO RAD-IX                                     
409400       END-IF                                                             
409401*                                                                         
409410       IF 4010-BERADREF   > SPACE                                         
409420         MOVE PRT-AFTER-1       TO PRT-RADSKIP                            
409430         MOVE 4010-BERADREF     TO CONTACT-BERADREF                       
409440         MOVE PU-CONTACT-BERADREF    TO WS-PU-RAD                         
409450         PERFORM S07-SKRIV-RAD                                            
409460                                                                          
409470         ADD  1             TO RAD-IX                                     
409480       END-IF                                                             
409500     END-IF                                                               
409600                                                                          
409700     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
409800     EVALUATE  TRUE                                                       
409900       WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                               
410000          MOVE PU-HRAD8-ITA TO WS-PU-RAD                                  
410100       WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                               
410200          MOVE PU-HRAD8-SPA TO WS-PU-RAD                                  
410300       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
410400          MOVE PU-HRAD8-JAP TO WS-PU-RAD                                  
410500       WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')            
410600          MOVE PU-HRAD8-NDC TO WS-PU-RAD                                  
410700       WHEN OTHER                                                         
410800          MOVE PU-HRAD8     TO WS-PU-RAD                                  
410900     END-EVALUATE                                                         
411000     PERFORM S07-SKRIV-RAD                                                
411100*   CASH ON DELIVERY KUND                                                 
411200     EVALUATE  TRUE                                                       
411300       WHEN DCS-NDC-NA AND DCS-IDLANDX2 = 'US'                            
411400        AND 4010-FLCOD = JA                                               
411500          MOVE PRT-AFTER-1               TO PRT-RADSKIP                   
411600          MOVE '* * * * * * * * CASH ON DELIVERY * * * * * * * *'         
411700          TO PU-HRAD8B-NDC-COD-TEXT                                       
411800          MOVE PU-HRAD8B-NDC-COD-TEXT    TO WS-PU-RAD                     
411900          PERFORM S07-SKRIV-RAD                                           
412000       WHEN OTHER                                                         
412100          CONTINUE                                                        
412200     END-EVALUATE                                                         
412300                                                                          
412400     MOVE PRT-AFTER-1                    TO PRT-RADSKIP                   
412500     EVALUATE  TRUE                                                       
412600       WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                               
412700         MOVE 'IMBALLO DI TRASPORTO:  '  TO PU-HRAD9-TEXT                 
412800       WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                               
412900         MOVE 'EMBALAJE DE TRANSPORTE:'  TO PU-HRAD9-TEXT                 
413000       WHEN DCS-SDC AND DCS-IDLANDX2 = 'GB'                               
413100         MOVE 'TRANSPORT PACKING:   '    TO PU-HRAD9-TEXT                 
413200       WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                               
413300         MOVE 'TRANSPORT PACKING:   '    TO PU-HRAD9-TEXT                 
413400       WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')            
413500         MOVE SPACE TO HRAD9-TELAGINS                                     
413600                       HRAD9-BELAGINS-DEL1                                
413700                       HRAD9-2-BELAGINS-DEL2                              
413800         IF OHUV-BELAGINS-GRP NOT = SPACE                                 
413900            MOVE 'WAREHOUSE INSTR:' TO HRAD9-TELAGINS                     
414000            IF OHUV-BELAGINS-DEL1 = SPACE                                 
414100               MOVE OHUV-BELAGINS-DEL2 TO HRAD9-BELAGINS-DEL1             
414200            ELSE                                                          
414300               MOVE OHUV-BELAGINS-DEL1 TO HRAD9-BELAGINS-DEL1             
414400               IF OHUV-BELAGINS-DEL2 NOT = SPACE                          
414500                 MOVE OHUV-BELAGINS-DEL2 TO HRAD9-2-BELAGINS-DEL2         
414600               END-IF                                                     
414700            END-IF                                                        
414800         END-IF                                                           
414900       WHEN OTHER                                                         
415000         MOVE 'TRANSPORTEMBALLAGE:  '    TO PU-HRAD9-TEXT                 
415100     END-EVALUATE                                                         
415200                                                                          
415300     EVALUATE TRUE                                                        
415400       WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')            
415500         MOVE PU-HRAD9-NDC               TO WS-PU-RAD                     
415600         PERFORM S07-SKRIV-RAD                                            
415700                                                                          
415800         MOVE PRT-AFTER-1                TO PRT-RADSKIP                   
415900         MOVE PU-HRAD9-2-NDC             TO WS-PU-RAD                     
416000         PERFORM S07-SKRIV-RAD                                            
416100         ADD 1                           TO RAD-IX                        
416200       WHEN OTHER                                                         
416300         MOVE PU-HRAD9                   TO WS-PU-RAD                     
416400         PERFORM S07-SKRIV-RAD                                            
416500     END-EVALUATE                                                         
416600                                                                          
416700     ADD 9             TO RAD-IX                                          
416800                                                                          
416900     IF PU-HRAD10 NOT = SPACE                                             
417000        MOVE PRT-AFTER-2  TO PRT-RADSKIP                                  
417100        MOVE PU-HRAD10    TO WS-PU-RAD                                    
417200        PERFORM S07-SKRIV-RAD                                             
417300        ADD 2             TO RAD-IX                                       
417400        IF PU-HRAD11 NOT = SPACE                                          
417500           MOVE PRT-AFTER-1  TO PRT-RADSKIP                               
417600           MOVE PU-HRAD11    TO WS-PU-RAD                                 
417700           PERFORM S07-SKRIV-RAD                                          
417800           ADD 1             TO RAD-IX                                    
417900        END-IF                                                            
418000     END-IF                                                               
418100                                                                          
418200     IF PU-HRAD12 NOT = SPACE                                             
418300        MOVE PRT-AFTER-2  TO PRT-RADSKIP                                  
418400        IF DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')             
418500          MOVE PU-HRAD12-NDC  TO WS-PU-RAD                                
418600        ELSE                                                              
418700          IF DCS-CDC                                                      
418800             MOVE PU-HRAD12A   TO WS-PU-RAD                               
418900             PERFORM S07-SKRIV-RAD                                        
419000             ADD 2             TO RAD-IX                                  
419100             MOVE '*'          TO HRAD12-AST1                             
419200                                  HRAD12-AST2                             
419300             MOVE PRT-AFTER-1  TO PRT-RADSKIP                             
419400          END-IF                                                          
419500          MOVE PU-HRAD12  TO WS-PU-RAD                                    
419600        END-IF                                                            
419700        PERFORM S07-SKRIV-RAD                                             
419800        IF DCS-CDC                                                        
419900          ADD 1             TO RAD-IX                                     
420000        ELSE                                                              
420100          ADD 2             TO RAD-IX                                     
420200        END-IF                                                            
420300        IF PU-HRAD13 NOT = SPACE                                          
420400           IF NOT (DCS-NDC-NA OR                                          
420500                  (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'))                   
420600             IF DCS-CDC                                                   
420700               MOVE '*'        TO HRAD13-AST1                             
420800                                  HRAD13-AST2                             
420900             END-IF                                                       
421000             MOVE PRT-AFTER-1  TO PRT-RADSKIP                             
421100             MOVE PU-HRAD13    TO WS-PU-RAD                               
421200             PERFORM S07-SKRIV-RAD                                        
421300             ADD 1             TO RAD-IX                                  
421400           END-IF                                                         
421500        END-IF                                                            
421600        IF HRAD12-AST1 = '*'                                              
421700           MOVE SPACE          TO HRAD12-AST1                             
421800                                  HRAD12-AST2                             
421900                                  HRAD13-AST1                             
422000                                  HRAD13-AST2                             
422100           MOVE PRT-AFTER-1    TO PRT-RADSKIP                             
422200           MOVE PU-HRAD12A     TO WS-PU-RAD                               
422300           PERFORM S07-SKRIV-RAD                                          
422400           ADD 1               TO RAD-IX                                  
422500        END-IF                                                            
422600     END-IF                                                               
422700                                                                          
422800     IF PU-HRAD13 = SPACE AND                                             
422900       (DCS-SDC AND DCS-IDLANDX2 NOT = 'SE')                              
423000        MOVE PRT-AFTER-1  TO PRT-RADSKIP                                  
423100        MOVE PU-HRAD13    TO WS-PU-RAD                                    
423200        PERFORM S07-SKRIV-RAD                                             
423300        ADD 1             TO RAD-IX                                       
423400     END-IF                                                               
423500                                                                          
423600     MOVE PRT-AFTER-3     TO PRT-RADSKIP                                  
423700     EVALUATE TRUE                                                        
423800       WHEN DCS-CDC                                                       
423900          IF (DIST-SVERIGE-778 AND                                        
424000              KUND-GISLAVED-765)                                          
424300            MOVE 'IDGISLAVED' TO RAD14-IDGISLAVED                         
424400          ELSE                                                            
424500            MOVE 'KOLLI'     TO RAD14-KOLLI                               
424600            MOVE ' '         TO RAD14-SPACE1                              
424700            MOVE 'SPEC'      TO RAD14-SPEC                                
424800          END-IF                                                          
424900          IF GMT-FLLDCKND = JA   AND                                      
425000            (DIST34-ENGLAND-SDC OR                                        
425100             DIST34-ITALIEN-SDC OR                                        
425200             DIST34-HOLLAND-SDC) AND                                      
425300            (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1 OR                      
425400             OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                        
425500                                                                          
425600            MOVE PU-HRAD14-ENG-LDC TO WS-PU-RAD                           
425700          ELSE                                                            
425800            MOVE PU-HRAD14-SVE TO WS-PU-RAD                               
425900          END-IF                                                          
426000       WHEN DCS-SDC AND DCS-IDLANDX2 = 'SE'                               
426100          MOVE 'KOLLI'       TO RAD14-KOLLI                               
426200          MOVE ' '           TO RAD14-SPACE1                              
426300          MOVE 'SPEC'        TO RAD14-SPEC                                
426400          MOVE PU-HRAD14-SVE TO WS-PU-RAD                                 
426500       WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                               
426600          MOVE PU-HRAD14-1-SPA TO WS-PU-RAD                               
426700          PERFORM S07-SKRIV-RAD                                           
426800          MOVE PRT-AFTER-1     TO PRT-RADSKIP                             
426900          MOVE PU-HRAD14-2-SPA TO WS-PU-RAD                               
427000       WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                               
427100          MOVE PU-HRAD14-OST TO WS-PU-RAD                                 
427200       WHEN DCS-SDC AND (DCS-IDLANDX2 = 'GB' OR 'NL' OR 'IT')             
427300          IF GMT-FLLDCKND = JA AND                                        
427400            (DIST34-ENGLAND-SDC OR                                        
427500             DIST34-ITALIEN-SDC OR                                        
427600             DIST34-HOLLAND-SDC) AND                                      
427700            (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1 OR                      
427800             OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                        
427900                                                                          
428000             MOVE JA                TO SW-SKRIV-INTE-RAD14                
428100            MOVE PU-HRAD14-ENG-LDC TO WS-PU-RAD                           
428200            PERFORM S08-SKRIV-RAD                                         
428300          ELSE                                                            
428400            MOVE PU-HRAD14-ENG    TO WS-PU-RAD                            
428500          END-IF                                                          
428600       WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                               
428700          MOVE PU-HRAD14-1-ITA TO WS-PU-RAD                               
428800          PERFORM S07-SKRIV-RAD                                           
428900          MOVE PRT-AFTER-1     TO PRT-RADSKIP                             
429000          MOVE PU-HRAD14-2-ITA TO WS-PU-RAD                               
429100       WHEN OTHER                                                         
429200          MOVE PU-HRAD14-ENG TO WS-PU-RAD                                 
429300     END-EVALUATE                                                         
429400     IF SW-SKRIV-INTE-RAD14 = JA                                          
429500       MOVE SPACE              TO WS-PU-RAD                               
429600     ELSE                                                                 
429700       PERFORM S07-SKRIV-RAD                                              
429800     END-IF                                                               
429900                                                                          
430000     ADD 3             TO RAD-IX                                          
430100     .                                                                    
430200     EJECT                                                                
430300 S06B-SIDA2-HUVUDRADER     SECTION.                                       
430400                                                                          
430500     IF WS-IDDC NOT = W-IDDC-B6                                           
430600        MOVE WS-IDDC TO W-IDDC-B6                                         
430700        PERFORM IMS-GU-WDB601                                             
430800     END-IF                                                               
430900                                                                          
431000     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
431100                                                                          
431200      EVALUATE  TRUE                                                      
431300        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
431400           MOVE ' N§ '     TO PU-HRAD3-TEXT-ITA                           
431500        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
431600           MOVE ' NO.'     TO PU-HRAD3-TEXT-ITA                           
431700        WHEN DCS-SDC AND DCS-IDLANDX2 = 'GB'                              
431800           MOVE ' NO.'     TO PU-HRAD3-TEXT                               
431900        WHEN DCS-NDC-NA OR DCS-NDC-PF                                     
432000           MOVE ' NO.'     TO PU-HRAD3-TEXT                               
432100        WHEN OTHER                                                        
432200           MOVE ' NR '     TO PU-HRAD3-TEXT                               
432300      END-EVALUATE                                                        
432400                                                                          
432500     MOVE PU-HRAD3-2   TO WS-PU-RAD                                       
432600     PERFORM S07-SKRIV-RAD                                                
432700                                                                          
432800     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
432900     MOVE PU-HRAD4-2   TO WS-PU-RAD                                       
433000     PERFORM S07-SKRIV-RAD                                                
433100                                                                          
433200     MOVE PRT-AFTER-3  TO PRT-RADSKIP                                     
433300     MOVE PU-HRAD14-SVE TO WS-PU-RAD                                      
433400     PERFORM S07-SKRIV-RAD                                                
433500     .                                                                    
433600     EJECT                                                                
433700 S06C-SIDA2-HUVUDRADER     SECTION.                                       
433800                                                                          
433900     IF WS-IDDC NOT = W-IDDC-B6                                           
434000        MOVE WS-IDDC TO W-IDDC-B6                                         
434100        PERFORM IMS-GU-WDB601                                             
434200     END-IF                                                               
434300                                                                          
434400     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
434500                                                                          
434600      EVALUATE  TRUE                                                      
434700        WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                              
434800           MOVE ' N§ '     TO PU-HRAD3-TEXT-ITA                           
434900           MOVE PU-HRAD3-ITA TO WS-PU-RAD                                 
435000        WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                              
435100           MOVE ' NO.'     TO PU-HRAD3-TEXT-SPA                           
435200           MOVE PU-HRAD3-SPA TO WS-PU-RAD                                 
435300        WHEN DCS-SDC AND DCS-IDLANDX2 = 'GB'                              
435400           MOVE ' NO.'     TO PU-HRAD3-TEXT                               
435500           MOVE PU-HRAD3   TO WS-PU-RAD                                   
435600        WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                           
435700           MOVE WS-GMT-BEGMT-OVR-RAD1 TO HRAD3-BEGMT-RAD1-JAP             
435800           MOVE '      PRC'     TO HRAD3-PRC-JAP                          
435900           MOVE '   PU'         TO HRAD3-LOPNR-JAP                        
436000           MOVE ' NO.'          TO HRAD3-TEXT-JAP                         
436100           MOVE 4010-TIREGDAT   TO WS-DATUM                               
436200           MOVE 4010-TIREGTID   TO WS-TID                                 
436300           MOVE WS-DATUM        TO HRAD3-TIREGDAT-JAP                     
436400           MOVE WS-TID (1:2)    TO HRAD3-TIREGHH-JAP                      
436500           MOVE WS-TID (3:2)    TO HRAD3-TIREGMM-JAP                      
436600           MOVE PU-HRAD3-JAP    TO WS-PU-RAD                              
436700        WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')           
436800           MOVE OHUV-BEGMT-RAD1 TO HRAD3-BEGMT-RAD1-NDC                   
436900           MOVE '      PRC'     TO HRAD3-PRC-NDC                          
437000           MOVE '   PU'         TO HRAD3-LOPNR-NDC                        
437100           MOVE ' NO.'          TO HRAD3-TEXT-NDC                         
437200           MOVE 4010-TIREGDAT   TO WS-DATUM                               
437300           MOVE 4010-TIREGTID   TO WS-TID                                 
437400           MOVE WS-DATUM        TO HRAD3-TIREGDAT-NDC                     
437500           MOVE WS-TID (1:2)    TO HRAD3-TIREGHH-NDC                      
437600           MOVE WS-TID (3:2)    TO HRAD3-TIREGMM-NDC                      
437700           MOVE PU-HRAD3-NDC    TO WS-PU-RAD                              
437800        WHEN OTHER                                                        
437900           MOVE ' NR '     TO PU-HRAD3-TEXT                               
438000           MOVE PU-HRAD3   TO WS-PU-RAD                                   
438100      END-EVALUATE                                                        
438200                                                                          
438300     PERFORM S07-SKRIV-RAD                                                
438400                                                                          
438500     IF PU-HRAD4 = SPACE                                                  
438600        MOVE PRT-AFTER-2  TO PRT-RADSKIP                                  
438700        EVALUATE  TRUE                                                    
438800          WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                            
438900               MOVE PU-HRAD5-ITA TO WS-PU-RAD                             
439000          WHEN OTHER                                                      
439100               MOVE PU-HRAD5   TO WS-PU-RAD                               
439200        END-EVALUATE                                                      
439300        PERFORM S07-SKRIV-RAD                                             
439400     ELSE                                                                 
439500        MOVE PRT-AFTER-1  TO PRT-RADSKIP                                  
439600        EVALUATE  TRUE                                                    
439700          WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')         
439800             MOVE PU-HRAD4-NDC TO WS-PU-RAD                               
439900          WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                         
440000             MOVE PU-HRAD4-JAP TO WS-PU-RAD                               
440100          WHEN OTHER                                                      
440200             MOVE PU-HRAD4     TO WS-PU-RAD                               
440300        END-EVALUATE                                                      
440400        PERFORM S07-SKRIV-RAD                                             
440500                                                                          
440600        MOVE PRT-AFTER-1  TO PRT-RADSKIP                                  
440700        EVALUATE  TRUE                                                    
440800          WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                            
440900             MOVE PU-HRAD5-ITA TO WS-PU-RAD                               
441000          WHEN OTHER                                                      
441100             MOVE PU-HRAD5     TO WS-PU-RAD                               
441200        END-EVALUATE                                                      
441300        PERFORM S07-SKRIV-RAD                                             
441400        MOVE PRT-AFTER-1  TO PRT-RADSKIP                                  
441500        EVALUATE  TRUE                                                    
441600          WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                            
441700             MOVE PU-HRAD6-ITA TO WS-PU-RAD                               
441800          WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                            
441900             MOVE PU-HRAD6-SPA TO WS-PU-RAD                               
442000          WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                         
442100             MOVE PU-HRAD6-JAP TO WS-PU-RAD                               
442200          WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')         
442300             MOVE PU-HRAD6-NDC TO WS-PU-RAD                               
442400          WHEN OTHER                                                      
442500             MOVE PU-HRAD6   TO WS-PU-RAD                                 
442600        END-EVALUATE                                                      
442700        PERFORM S07-SKRIV-RAD                                             
442800     END-IF                                                               
442900                                                                          
443000     EVALUATE  TRUE                                                       
443100       WHEN DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'                            
443200         MOVE PRT-AFTER-1  TO PRT-RADSKIP                                 
443300         MOVE PU-HRAD7-JAP TO WS-PU-RAD                                   
443400         PERFORM S07-SKRIV-RAD                                            
443500       WHEN DCS-NDC-NA OR (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU')            
443600         MOVE PRT-AFTER-1  TO PRT-RADSKIP                                 
443700         MOVE PU-HRAD7-NDC TO WS-PU-RAD                                   
443800         PERFORM S07-SKRIV-RAD                                            
443900       WHEN OTHER                                                         
444000         MOVE PRT-AFTER-1  TO PRT-RADSKIP                                 
444100         MOVE PU-HRAD7     TO WS-PU-RAD                                   
444200         PERFORM S07-SKRIV-RAD                                            
444300     END-EVALUATE                                                         
444400                                                                          
444500     IF DCS-SDC AND DCS-IDLANDX2 = 'GB'                                   
444600       MOVE PRT-AFTER-2  TO PRT-RADSKIP                                   
444700       MOVE PU-HRAD8     TO WS-PU-RAD                                     
444800       PERFORM S07-SKRIV-RAD                                              
444900     END-IF                                                               
445000                                                                          
445100     MOVE PRT-AFTER-2  TO PRT-RADSKIP                                     
445200     EVALUATE TRUE                                                        
445300       WHEN DCS-CDC                                                       
445400          IF (DIST-SVERIGE-778 AND                                        
445500              KUND-GISLAVED-765)                                          
445800            MOVE 'IDGISLAVED'  TO RAD14-IDGISLAVED                        
445900          ELSE                                                            
446000            MOVE 'KOLLI'     TO RAD14-KOLLI                               
446100            MOVE ' '         TO RAD14-SPACE1                              
446200            MOVE 'SPEC'      TO RAD14-SPEC                                
446300          END-IF                                                          
446400          MOVE PU-HRAD14-SVE TO WS-PU-RAD                                 
446500       WHEN DCS-SDC AND DCS-IDLANDX2 = 'SE'                               
446600          MOVE 'KOLLI'       TO RAD14-KOLLI                               
446700          MOVE ' '           TO RAD14-SPACE1                              
446800          MOVE 'SPEC'        TO RAD14-SPEC                                
446900          MOVE PU-HRAD14-SVE TO WS-PU-RAD                                 
447000       WHEN DCS-SDC AND DCS-IDLANDX2 = 'ES'                               
447100          MOVE PU-HRAD14-1-SPA TO WS-PU-RAD                               
447200          PERFORM S07-SKRIV-RAD                                           
447300          MOVE PU-HRAD14-2-SPA TO WS-PU-RAD                               
447400       WHEN DCS-SDC AND DCS-IDLANDX2 = 'AT'                               
447500          MOVE PU-HRAD14-OST TO WS-PU-RAD                                 
447600       WHEN DCS-SDC AND (DCS-IDLANDX2 = 'GB' OR 'NL' OR 'IT')             
447700          IF GMT-FLLDCKND = JA   AND                                      
447800            (DIST34-ENGLAND-SDC OR                                        
447900             DIST34-ITALIEN-SDC OR                                        
448000             DIST34-HOLLAND-SDC) AND                                      
448100            (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1 OR                      
448200             OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                        
448300                                                                          
448400            MOVE PU-HRAD14-ENG-LDC TO WS-PU-RAD                           
448500          ELSE                                                            
448600            MOVE PU-HRAD14-ENG    TO WS-PU-RAD                            
448700          END-IF                                                          
448800       WHEN DCS-SDC AND DCS-IDLANDX2 = 'IT'                               
448900          MOVE PU-HRAD14-1-ITA TO WS-PU-RAD                               
449000          PERFORM S07-SKRIV-RAD                                           
449100          MOVE PU-HRAD14-2-ITA TO WS-PU-RAD                               
449200       WHEN OTHER                                                         
449300          MOVE PU-HRAD14-ENG TO WS-PU-RAD                                 
449400     END-EVALUATE                                                         
449500     IF SW-SKRIV-INTE-RAD14 = JA                                          
449600       MOVE SPACE              TO WS-PU-RAD                               
449700     ELSE                                                                 
449800       PERFORM S07-SKRIV-RAD                                              
449900     END-IF                                                               
450000     .                                                                    
450100     EJECT                                                                
450200 S07-SKRIV-RAD SECTION.                                                   
450300                                                                          
450400     INSPECT WS-PU-RAD REPLACING ALL '¤' BY 'U'                           
450500                                                                          
450600     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
450700                         PRT-WRITE                                        
450800                         WS-IDPRTLST                                      
450900                         ALT3-PCB                                         
451000                         LISB-PCB                                         
451100                         WS-PU-LISTID                                     
451200                         PRT-RADSKIP                                      
451300                         WS-PU-LISTRAD                                    
451400     .                                                                    
451500     SKIP2                                                                
451600 S08-SKRIV-RAD SECTION.                                                   
451700                                                                          
451800     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
451900                         PRT-WRITE                                        
452000                         WS-IDPRTLST                                      
452100                         ALT3-PCB                                         
452200                         LISB-PCB                                         
452300                         WS-PU-LISTID                                     
452400                         PRT-RADSKIP                                      
452500                         WS-PU-LISTRAD-LDC                                
452600     .                                                                    
452700     EJECT                                                                
452800 S10-EV-INITIERA-LDC-TAB  SECTION.                                        
452900                                                                          
453000     IF WS-IDDC NOT = W-IDDC-B6                                           
453100        MOVE WS-IDDC TO W-IDDC-B6                                         
453200        PERFORM IMS-GU-WDB601                                             
453300     END-IF                                                               
453400                                                                          
453500     MOVE 'STA S10-EV-INIT'         TO PGMPOS                             
453600                                                                          
453700*LDC-GB                                                                   
453800     IF INITIERA-LDC-TABELL = JA                                          
453900       IF (DCS-SDC AND (DCS-IDLANDX2 = 'GB' OR 'NL' OR 'IT'))             
454000         MOVE OHUV-IDDISTR    TO DIST34-IDDISTR                           
454100         MOVE NEJ             TO INITIERA-LDC-TABELL                      
454200                                                                          
454300         IF GMT-FLLDCKND = JA   AND                                       
454400           (DIST34-ENGLAND-SDC OR                                         
454500            DIST34-ITALIEN-SDC OR                                         
454600            DIST34-HOLLAND-SDC)                                           
454700           IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                       
454800           OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                       
454900                                                                          
455000             MOVE +1 TO LDC-TAB-IX                                        
455100             PERFORM UNTIL LDC-TAB-IX = LDC-TAB-IX-MAX                    
455200               MOVE ZERO TO LDC-TAB-ANTAL-ART(LDC-TAB-IX)                 
455300               MOVE HIGH-VALUE TO LDC-TAB-VIPID (LDC-TAB-IX)              
455400               ADD +1 TO LDC-TAB-IX                                       
455500             END-PERFORM                                                  
455600                                                                          
455700           END-IF                                                         
455800         END-IF                                                           
455900       END-IF                                                             
456000     END-IF                                                               
456100     MOVE 'END S10-EV-INIT'         TO PGMPOS                             
456200     .                                                                    
456300     EJECT                                                                
456400 S11-DIST-KUND-LDC SECTION.                                               
456500                                                                          
456600     MOVE OHUV-IDDISTR     TO  W-IDDISTR-WDB2                             
456700     MOVE OHUV-IDKUNDNR    TO  W-IDKUNDNR-WDB2                            
456800                                                                          
456900     PERFORM IMS-GU-WDB201                                                
457000                                                                          
457100     IF SEGMENT-SAKNAS                                                    
457200        MOVE NEJ              TO GMT-FLLDCKND                             
457300     END-IF                                                               
457400     .                                                                    
457500     EJECT                                                                
457600 S12-LASER-PRINTER SECTION.                                               
457700                                                                          
457800     MOVE '4'                   TO WS-SYSTDEL                             
457900     MOVE 'PU'                  TO WS-LISTTYP                             
458000     MOVE 4008-KDPRT-PU         TO WS-KDPRT                               
458100                                                                          
458200     MOVE 001                   TO PRT-KDCALL                             
458300     MOVE WS-IDPRTLST           TO PRT-IDPRTLST                           
458400                                                                          
458500     CALL W006PRT USING PRT-W006PRT                                       
458600                                                                          
458700     IF PRT-BEPRTLST (1:3) = 'IBM'                                        
458800       IF WS-IDPRTLST = '4PUPUJ  '                                        
458900       OR WS-IDPRTLST = '4PUPUI  '                                        
459000       OR WS-IDPRTLST = '4PUFA3  '                                        
459100       OR WS-IDPRTLST = '4PUSV3  '                                        
459110       OR WS-IDPRTLST = '4PUPUW  '                                        
459120       OR WS-IDPRTLST = '4PUPUK  '                                        
459130       OR WS-IDPRTLST = '4PUPUL  '                                        
459200         MOVE 'W40379' TO PRT-PFDEF-A4S                                   
459300       ELSE                                                               
459400         MOVE 'W40377' TO PRT-PFDEF-A4S                                   
459500       END-IF                                                             
459600       MOVE JA        TO WS-SKRIVARTYP-SW                                 
459700     ELSE                                                                 
459800       MOVE NEJ       TO WS-SKRIVARTYP-SW                                 
459900     END-IF                                                               
460000     .                                                                    
460100     EJECT                                                                
460200                                                                          
460300* --- IMS SEKTIONER ---                                                   
460400                                                                          
460500 IMS-GU-MSG SECTION.                                                      
460600                                                                          
460700     MOVE '  QC' TO GODK-STATUSKODER                                      
460800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
460900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
461000     PERFORM IMS-STATUSKONTROLL                                           
461100     .                                                                    
461200                                                                          
461300 IMS-ISRT-MSG-ALT1 SECTION.                                               
461400                                                                          
461500     MOVE SPACE TO GODK-STATUSKODER                                       
461600     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
461700     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
461800     PERFORM IMS-STATUSKONTROLL                                           
461900     .                                                                    
462000                                                                          
462100 IMS-ISRT-MSG-ALT2 SECTION.                                               
462200                                                                          
462300     MOVE SPACE TO GODK-STATUSKODER                                       
462400     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
462500     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
462600     PERFORM IMS-STATUSKONTROLL                                           
462700     .                                                                    
462800                                                                          
462900 IMS-GHU-4007-WL400711 SECTION.                                           
463000                                                                          
463100     STRING 'WL400701(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
463200          DELIMITED BY SIZE INTO SSA1                                     
463300     MOVE 'WL400711 ' TO SSA2                                             
463400     MOVE '    ' TO GODK-STATUSKODER                                      
463500     CALL CBLTDLI USING GHU 4007-PCB DLI-IO-AREA1 SSA1 SSA2               
463600     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
463700     PERFORM IMS-STATUSKONTROLL                                           
463800     .                                                                    
463900                                                                          
464000 IMS-REPL-4007-WL400711 SECTION.                                          
464100                                                                          
464200     MOVE '    ' TO GODK-STATUSKODER                                      
464300     CALL CBLTDLI USING REPL 4007-PCB DLI-IO-AREA1                        
464400     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
464500     PERFORM IMS-STATUSKONTROLL                                           
464600     .                                                                    
464700     EJECT                                                                
464800 IMS-GNP-4007-WL400721-KVAL SECTION.                                      
464900                                                                          
465000     STRING 'WL400721(KY4010   =' W-4010-IDHTYP-X ')'                     
465100          DELIMITED BY SIZE INTO SSA1                                     
465200     MOVE '    ' TO GODK-STATUSKODER                                      
465300     CALL CBLTDLI USING GNP 4007-PCB DLI-IO-AREA5 SSA1                    
465400     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
465500     PERFORM IMS-STATUSKONTROLL                                           
465600     .                                                                    
465700                                                                          
465800 IMS-GNP-4007-WL400721-OKVAL SECTION.                                     
465900                                                                          
466000     STRING 'WL400721(KDPRT    <' W-4010-KDPRT-MAX-X ')'                  
466100          DELIMITED BY SIZE INTO SSA1                                     
466200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
466300     CALL CBLTDLI USING GNP 4007-PCB DLI-IO-AREA5 SSA1                    
466400     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
466500     PERFORM IMS-STATUSKONTROLL                                           
466600     .                                                                    
466700     EJECT                                                                
466800 IMS-GHU-WDGX4017 SECTION.                                                
466900                                                                          
467000     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
467100          DELIMITED BY SIZE INTO SSA1                                     
467200     MOVE '  GE' TO GODK-STATUSKODER                                      
467300     CALL CBLTDLI USING GHU 4017-PCB 4017-WDGX4017 SSA1                   
467400     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
467500     PERFORM IMS-STATUSKONTROLL                                           
467600     .                                                                    
467700                                                                          
467800 IMS-GHU-WDGX4018 SECTION.                                                
467900                                                                          
468000     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
468100          DELIMITED BY SIZE INTO SSA1                                     
468200     STRING 'WDGX4018(KDSEGKEY =' W-4018-KDSEGKEY-X ')'                   
468300          DELIMITED BY SIZE INTO SSA2                                     
468400     MOVE '  GE' TO GODK-STATUSKODER                                      
468500     CALL CBLTDLI USING GHU 4017-PCB 4018-WDGX4018 SSA1 SSA2              
468600     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
468700     PERFORM IMS-STATUSKONTROLL                                           
468800     .                                                                    
468900                                                                          
469000 IMS-REPL-WDGX4018 SECTION.                                               
469100                                                                          
469200     MOVE '    ' TO GODK-STATUSKODER                                      
469300     CALL CBLTDLI USING REPL 4017-PCB 4018-WDGX4018                       
469400     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
469500     PERFORM IMS-STATUSKONTROLL                                           
469600     .                                                                    
469700                                                                          
469800 IMS-DLET-WDGX4017 SECTION.                                               
469900                                                                          
470000     MOVE '    ' TO GODK-STATUSKODER                                      
470100     CALL CBLTDLI USING DLET 4017-PCB 4017-WDGX4017                       
470200     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
470300     PERFORM IMS-STATUSKONTROLL                                           
470400     .                                                                    
470500                                                                          
470600 IMS-ISRT-WDGX4017 SECTION.                                               
470700                                                                          
470800     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
470900          DELIMITED BY SIZE INTO SSA1                                     
471000     MOVE '    ' TO GODK-STATUSKODER                                      
471100     CALL CBLTDLI USING ISRT 4017-PCB 4017-WDGX4017 SSA1                  
471200     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
471300     PERFORM IMS-STATUSKONTROLL                                           
471400     .                                                                    
471500                                                                          
471600 IMS-ISRT-WDGX4018 SECTION.                                               
471700                                                                          
471800     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
471900          DELIMITED BY SIZE INTO SSA1                                     
472000     MOVE 'WDGX4018 ' TO SSA2                                             
472100     MOVE '    ' TO GODK-STATUSKODER                                      
472200     CALL CBLTDLI USING ISRT 4017-PCB 4018-WDGX4018 SSA1 SSA2             
472300     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
472400     PERFORM IMS-STATUSKONTROLL                                           
472500     .                                                                    
472600                                                                          
472700 IMS-GU-ORQI-WLORQI01-WLORQI12 SECTION.                                   
472800                                                                          
472900     STRING 'WLORQI01*D(IDORDER  =' W-IDORDER-X ')'                       
473000          DELIMITED BY SIZE INTO SSA1                                     
473100     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
473200          DELIMITED BY SIZE INTO SSA2                                     
473300     MOVE '    ' TO GODK-STATUSKODER                                      
473400     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA2 SSA1 SSA2                
473500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
473600     PERFORM IMS-STATUSKONTROLL                                           
473700     .                                                                    
473800     EJECT                                                                
473900 IMS-GU-ORQA-WLORQA01 SECTION.                                            
474000                                                                          
474100     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
474200                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
474300          DELIMITED BY SIZE INTO SSA1                                     
474400     MOVE '  GE' TO GODK-STATUSKODER                                      
474500     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA4 SSA1                     
474600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
474700     PERFORM IMS-STATUSKONTROLL                                           
474800     .                                                                    
474900                                                                          
475000                                                                          
475100 IMS-GN-ORQA-WLORQA01 SECTION.                                            
475200                                                                          
475300     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
475400                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
475500          DELIMITED BY SIZE INTO SSA1                                     
475600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
475700     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA4 SSA1                     
475800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
475900     PERFORM IMS-STATUSKONTROLL                                           
476000     .                                                                    
476100     EJECT                                                                
476200 IMS-GU-XXKH-WLXXKH11 SECTION.                                            
476300                                                                          
476400     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
476500          DELIMITED BY SIZE INTO SSA1                                     
476600     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
476700          DELIMITED BY SIZE INTO SSA2                                     
476800     MOVE '  GE' TO GODK-STATUSKODER                                      
476900     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA3 SSA1 SSA2                
477000     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
477100     PERFORM IMS-STATUSKONTROLL                                           
477200     .                                                                    
477300     EJECT                                                                
477400 IMS-GU-XXKU-WLXXKU11 SECTION.                                            
477500                                                                          
477600     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
477700          DELIMITED BY SIZE INTO SSA1                                     
477800     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
477900          DELIMITED BY SIZE INTO SSA2                                     
478000     MOVE '  GE' TO GODK-STATUSKODER                                      
478100     CALL CBLTDLI USING GU XXKU-PCB DLI-IO-AREA3 SSA1 SSA2                
478200     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
478300     PERFORM IMS-STATUSKONTROLL                                           
478400     .                                                                    
478500     EJECT                                                                
478600 IMS-GU-4732-WL473211 SECTION.                                            
478700                                                                          
478800     STRING 'WL473201(WDGXKEY  =' W-4732-IDHTYP-X ')'                     
478900          DELIMITED BY SIZE INTO SSA1                                     
479000     MOVE   'WL473211 ' TO SSA2                                           
479100     MOVE '  GE' TO GODK-STATUSKODER                                      
479200     CALL CBLTDLI USING GU 4732-PCB DLI-IO-AREA3 SSA1 SSA2                
479300     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
479400     PERFORM IMS-STATUSKONTROLL                                           
479500     .                                                                    
479600     EJECT                                                                
479700 IMS-GU-WLXXKB01 SECTION.                                                 
479800                                                                          
479900     STRING 'WLXXKB01(WDGXKEY  =' W-WDGXKEY-4433-X ')'                    
480000          DELIMITED BY SIZE INTO SSA1                                     
480100     MOVE '  GE' TO GODK-STATUSKODER                                      
480200     CALL CBLTDLI USING GU XXKB-PCB DLI-IO-AREA6 SSA1                     
480300     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
480400     PERFORM IMS-STATUSKONTROLL                                           
480500     .                                                                    
480600     SKIP2                                                                
480700 IMS-GNP-WLXXKB11 SECTION.                                                
480800                                                                          
480900     STRING 'WLXXKB11(WDGXKEY =>' W-WDGXKEY-4434-X ')'                    
481000          DELIMITED BY SIZE INTO SSA1                                     
481100     MOVE '  GE' TO GODK-STATUSKODER                                      
481200     CALL CBLTDLI USING GNP XXKB-PCB DLI-IO-AREA6 SSA1                    
481300     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
481400     PERFORM IMS-STATUSKONTROLL                                           
481500     .                                                                    
481600     EJECT                                                                
481700 IMS-GU-WDB201 SECTION.                                                   
481800                                                                          
481900     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
482000          DELIMITED BY SIZE INTO SSA1                                     
482100     MOVE '  GE' TO GODK-STATUSKODER                                      
482200     CALL CBLTDLI USING GU GMTA-PCB GMT-WDB201 SSA1                       
482300     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
482400     PERFORM IMS-STATUSKONTROLL                                           
482500     .                                                                    
482600     SKIP3                                                                
482700 IMS-GU-WDB601    SECTION.                                                
482800                                                                          
482900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
483000          DELIMITED BY SIZE INTO SSA1                                     
483100     MOVE '  GE' TO GODK-STATUSKODER                                      
483200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
483300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
483400     PERFORM IMS-STATUSKONTROLL                                           
483500     .                                                                    
483600     EJECT                                                                
483700 IMS-GU-WDF502 SECTION.                                                   
483800                                                                          
483900     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
484000            DELIMITED BY SIZE INTO SSA1                                   
484100     MOVE 'WDF502  '       TO SSA2                                        
484200     MOVE '  GE'           TO GODK-STATUSKODER                            
484300     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
484400     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
484500     PERFORM IMS-STATUSKONTROLL                                           
484600     .                                                                    
484700                                                                          
484800 IMS-STATUSKONTROLL SECTION.                                              
484900                                                                          
485000     SET STATUS-IX TO 1                                                   
485100     SEARCH GODK-STATUS                                                   
485200       AT END CALL FELLOG                                                 
485300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
485400     END-SEARCH                                                           
486000     .                                                                    
