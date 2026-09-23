000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1055200.                                                
000300 AUTHOR.         SUSANNE ENEGARD.                                         
000400 DATE-WRITTEN.   APRIL 1985                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION.                                                            
000900*        PROGRAMMET FÖRMEDLAR LÅN AV AVSNITT, RADER OCH                   
001000*        KOLUMNER MELLAN OLIKA KATALOGER/AVSNITT.                         
001100*                                                                         
001200*    AMENDMENTS.                                                          
001300*        INLAGT IMS-ROLLBACK I Q-UPPDATERA FÖR ATT UNDVIKA                
001400*        S0C7 (S0C4) NÄR TAB-IX GÅR ÖVER 25.                              
001500*        IMS BACKAR UR ALLA UPPDATERINGAR OCH FELMEDDELANDE               
001600*        LÄGGS UT. /C.E. FEB-89                                           
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W1T552                                              
002000*        MID:         W1I55201                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        MOD:         W1O55201                                            
002400     SKIP2                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000*    -- CHECKED BY WY2000                                                 
003100 77   PROGRAM-NAMN               PIC X(8) VALUE 'W1055200'.               
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  INDX                        PIC S9(9)   VALUE +1   COMP SYNC.        
003500 77  KOL-IX                      PIC S9(9)   VALUE +1   COMP SYNC.        
003600 77  KOL-TO-IX                   PIC S9(9)   VALUE +1   COMP SYNC.        
003700 77  TAB-IX                      PIC S9(9)   VALUE +1   COMP SYNC.        
003800 77  TAB-IX-MAX                  PIC S9(9)   VALUE +25  COMP SYNC.        
003900 77  SPRAAK-IX                   PIC S9(9)   VALUE +1   COMP SYNC.        
004000 77  TEST-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
004100 77  PER-IX                      PIC S9(9)   VALUE ZERO COMP-3.           
004200 77  PER-IX-MAX                  PIC S9(9)   VALUE +12  COMP-3.           
004300 77  KOLL-IX                     PIC S9(9)   VALUE ZERO COMP-3.           
004400 77  KOLL-IX-MAX                 PIC S9(9)   VALUE +10  COMP-3.           
004500 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
004600 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +200 COMP SYNC.        
004700*                                                                         
004800 77  INDATA-FEL                  PIC X(1)    VALUE 'N'.                   
004900 77  ILLU-FEL                    PIC X(1)    VALUE 'N'.                   
005000 77  UPPDAT-FEL                  PIC X(1)    VALUE 'N'.                   
005100 77  RAD-TO-FEL                  PIC X(1)    VALUE 'N'.                   
005200*                                                                         
005300 77  KOLUMN-IFYLLD               PIC X(1)    VALUE 'N'.                   
005400 77  NASTA-POS-SAKNAS            PIC X(1)    VALUE 'N'.                   
005500 77  NY-POSITION                 PIC X(1)    VALUE 'N'.                   
005600 77  POS-UPD                     PIC X(1)    VALUE 'N'.                   
005700 77  RAD-FINNS                   PIC X(1)    VALUE 'N'.                   
005800 77  TO-BAS-FINNS                PIC X(1)    VALUE 'N'.                   
005900 77  ROT-FROM-LAEST              PIC X(1)    VALUE 'N'.                   
006000 77  SPAR-ART-FINNS              PIC X(1)    VALUE 'N'.                   
006100 77  BAS-FROM-SLUT               PIC X(1)    VALUE 'N'.                   
006200 77  POSITIONER-FINNS            PIC X(1)    VALUE 'N'.                   
006300 77  IDCATPOS-TILL-FINNS         PIC X(1)    VALUE 'N'.                   
006400 77  LINE-BEFORE                 PIC X(1)    VALUE 'N'.                   
006500 77  LAAN-OK                     PIC X(1)    VALUE 'N'.                   
006600 77  MITT-RADER                  PIC X(1)    VALUE 'N'.                   
006700 77  NAESTA-TO-RAD               PIC S9(5)   VALUE ZERO  COMP-3.          
006800 77  SISTA-OK-RAD                PIC S9(5)   VALUE ZERO  COMP-3.          
006900 77  SPAR-W-IDCATRAD-TO          PIC S9(5)   VALUE ZERO  COMP-3.          
007000 77  SPAR-W-IDCATRAD-TO2         PIC S9(5)   VALUE ZERO  COMP-3.          
007100 77  SPAR-W-KDCATPUB-TO2         PIC X(6)    VALUE LOW-VALUE.             
007200 77  WS-MITT-RAD-MAX             PIC S9(5)   VALUE ZERO  COMP-3.          
007300 77  BAS-FROM-IDCATPOS           PIC X(3)    VALUE SPACE.                 
007400 77  SPAR-IDCATPOS               PIC X(3)    VALUE SPACE.                 
007500 77  NAESTA-TO-POS               PIC X(3)    VALUE SPACE.                 
007600 77  SISTA-FROM-POS              PIC X(3)    VALUE SPACE.                 
007700 77  SPAR-KVKOL                  PIC X(3)    VALUE SPACE.                 
007800 77  KDCATPUB-FOM-OK             PIC X       VALUE 'N'.                   
007900 77  KDCATPUB-TOM-OK             PIC X       VALUE 'N'.                   
008000 77  SPAR-KDCATPUB               PIC X(6)    VALUE SPACE.                 
008100 77  WS-KDCATPUB-TO-FOM          PIC X(6)    VALUE SPACE.                 
008200 77  WS-KDCATPUB-TO-TOM          PIC X(6)    VALUE SPACE.                 
008300 77  NY-KDCATPUB-FOM             PIC X(6)    VALUE SPACE.                 
008400 77  NY-KDCATPUB-TOM             PIC X(6)    VALUE SPACE.                 
008500 77  GALLANDE-FINNS              PIC X       VALUE 'N'.                   
008600 77  AKTUELL-RAD                 PIC X       VALUE 'N'.                   
008700 77  SPAR-IFYLLT                 PIC X       VALUE 'N'.                   
008800 77  NY-RAD-FINNS                PIC X       VALUE 'N'.                   
008900 77  NY-PUB-OK                   PIC X       VALUE 'J'.                   
009000                                                                          
009100******** TEST POS                                                         
009200 01 SW-NUM1                      PIC X       VALUE 'N'.                   
009300 01 SW-NUM2                      PIC X       VALUE 'N'.                   
009400 01 SW-NUM3                      PIC X       VALUE 'N'.                   
009500 01 SW-ALFA                      PIC X       VALUE 'N'.                   
009600 01 KOLL-POSX                    PIC X(3)    VALUE SPACE.                 
009700 01 KOLL-POS-NUM1                PIC 9       VALUE ZERO.                  
009800 01 KOLL-POS-NUM2                PIC 9(2)    VALUE ZERO.                  
009900 01 KOLL-POS-NUM3                PIC 9(3)    VALUE ZERO.                  
010000 01 KOLL-POS-ALFA                PIC X(3)    VALUE SPACE.                 
010100 01 TEST-POS-ALFA                PIC X(3)    VALUE SPACE.                 
010200 01 TEST-POS-ALFA1               PIC X(3)    VALUE SPACE.                 
010300 01 TEST-POS-ALFA2               PIC X(3)    VALUE SPACE.                 
010400 01 TEST-POS-ALFA3               PIC X(3)    VALUE SPACE.                 
010500 01 TEST-POS-ALFA4               PIC X(3)    VALUE SPACE.                 
010600 01 TEST-POS-NUM                 PIC 9(3)    VALUE ZERO.                  
010700 01 TEST-POS-NUM1                PIC 9(3)    VALUE ZERO.                  
010800 01 TEST-POS-NUM2                PIC 9(3)    VALUE ZERO.                  
010900 01 TEST-POS-NUM3                PIC 9(3)    VALUE ZERO.                  
011000 01 TEST-POS-NUM4                PIC 9(3)    VALUE ZERO.                  
011100                                                                          
011200 01 KOLL-POS                     PIC X(3).                                
011300 01 FILLER REDEFINES KOLL-POS.                                            
011400    03 KOLL-POS1                 PIC X.                                   
011500    03 KOLL-POS2                 PIC X.                                   
011600    03 KOLL-POS3                 PIC X.                                   
011700******** TEST POS                                                         
011800                                                                          
011900 01  KOLL-TABELL.                                                         
012000     03  KOLL-RAD OCCURS 10.                                              
012100         05 KOLL-IDCATRAD        PIC 9(4).                                
012200         05 KOLL-KDCATPUB-FOM    PIC X(6).                                
012300         05 KOLL-KDCATPUB-TOM    PIC X(6).                                
012400                                                                          
012500 01  FILLER                  PIC X(16) VALUE 'DYNAMISKA SUBPGM'.          
012600 01  DYNAMISKA-SUBPROGRAM.                                                
012700     03 CBLTDLI                  PIC X(8)   VALUE 'CBLTDLI'.              
012800     03 FELLOG                   PIC X(8)   VALUE 'FELLOG '.              
012900     03 WDATKONV                 PIC X(8)   VALUE 'WDATKONV'.             
013000     EJECT                                                                
013100* - - - - - - - - - - - - - - - - - - -  VARIABLER                        
013200 01  FILLER                      PIC X(16)  VALUE 'VARIABLER'.            
013300 01  VARIABLER.                                                           
013400     03  DAGENS-DATUM            PIC 9(6).                                
013500     03  DAGENS-AAR              PIC 9(4)    VALUE ZERO.                  
013600                                                                          
013700     03  DAGENS-AAR-VECKA        PIC 9(4).                                
013800     03  FILLER REDEFINES DAGENS-AAR-VECKA.                               
013900         05  FILLER              PIC 9.                                   
014000         05  DAGENS-VECKA        PIC 9(3).                                
014100                                                                          
014200     03  IN-IDCATPOS             PIC X(3).                                
014300     03  FILLER      REDEFINES  IN-IDCATPOS.                              
014400         05  IN-IDCATPOS-NUM     PIC X(2).                                
014500         05  IN-SPACE            PIC X(1).                                
014600                                                                          
014700     03  WS-FULLT-MEDDELANDE     PIC X(61).                               
014800     03  FILLER      REDEFINES  WS-FULLT-MEDDELANDE.                      
014900         05  FULL-IDCATRAD       PIC Z(4)9.                               
015000         05  FULL-TEXT           PIC X(56).                               
015100     03  WS-FULL-TEXT            PIC X(56)                                
015200                     VALUE ' ÄR FÖRSTA RAD SOM INTE FÅR PLATS'.           
015300                                                                          
015400     03  WS-POS-MEDDELANDE       PIC X(60).                               
015500     03  FILLER      REDEFINES  WS-POS-MEDDELANDE.                        
015600         05  POS-IDCATPOS        PIC X(3).                                
015700         05  POS-TEXT            PIC X(57).                               
015800     03  WS-POS-TEXT             PIC X(57)                                
015900                     VALUE ' ÄR DEN POSITION DÄR LÅNET AVBRUTITS'.        
016000                                                                          
016100     03  WS-MAX                  PIC S9(5)   VALUE ZERO  COMP-3.          
016200     03  WS-RAKNARE              PIC S9(5)   VALUE ZERO  COMP-3.          
016300     03  WS-INTERVALL            PIC S9(5)   VALUE ZERO  COMP-3.          
016400     03  WS-RAD-FROM             PIC S9(5)   VALUE ZERO  COMP-3.          
016500     03  WS-RAD-TO               PIC S9(5)   VALUE ZERO  COMP-3.          
016600     03  WS-RAD-START            PIC S9(5)   VALUE ZERO  COMP-3.          
016700     03  WS-MAX-RAD-TO           PIC S9(5)   VALUE ZERO  COMP-3.          
016800     03  WS-IDCATPOS-FROM        PIC X(3)    VALUE SPACE.                 
016900     03  WS-IDCATPOS-TO          PIC X(3)    VALUE SPACE.                 
017000     03  WS-IDKOL-FROM           PIC X(1)    VALUE ' '.                   
017100     03  WS-IDKOL-TO             PIC X(1)    VALUE ' '.                   
017200     03  WS-KDCATPUB-R-AVV       PIC X(3)    VALUE SPACE.                 
017300     03  WS-KDCATPUB-AAAAVV      PIC X(6)    VALUE SPACE.                 
017400     03  WS-GILTIGA-AAR.                                                  
017500       05  WS-TIAAAA             PIC 9(4)    VALUE ZERO                   
017600                                 OCCURS 4.                                
017700                                                                          
017800 01  WS-IDCATPOS                 PIC X(3)    VALUE SPACE.                 
017900 01  FILLER REDEFINES WS-IDCATPOS.                                        
018000     03  WS-IDCATPOS-POS1        PIC X.                                   
018100     03  WS-IDCATPOS-POS2        PIC X.                                   
018200     03  WS-IDCATPOS-POS3        PIC X.                                   
018300     EJECT                                                                
018400*01    -COPY WDATAREA                                                     
018500     EJECT                                                                
018600* - - - - - - - - - - - - - - - - - - -  SPAR-ART2-AREA                   
018700 01  FILLER                  PIC X(16)  VALUE 'SPAR-ART2-AREA'.           
018800 01  SPAR-ART2-AREA.                                                      
018900*    03  SPAR-ART2 -COPY WDN521     -PRE SPAR-                            
019000     EJECT                                                                
019100* - - - - - - - - - - - - - - - - - - -  IN-SOEK-AREA                     
019200 01  FILLER                      PIC X(16)  VALUE 'IN-SOEK-AREA'.         
019300 01  IN-AREA.                                                             
019400*    03  IN-AREA1  -COPY W10552     -PRE IN-                              
019500     EJECT                                                                
019600* - - - - - - - - - - - - - - - - - - -  JAMF-AREA                        
019700 01  FILLER                    PIC X(16)  VALUE 'JAMF-SOEK-AREA'.         
019800 01  JAMF-AREA.                                                           
019900     03  FILLER         OCCURS 25.                                        
020000*        05  JAMF-AREA1  -COPY W10552                                     
020100         05  JAMF-AREA2.                                                  
020200             07  SOEK-KDFBX      PIC X(1).                                
020300             07  SOEK-KVKOL      PIC X(3).                                
020400             07  SOEK-FLRUBTYP   PIC X(1).                                
020500             07  SOEK-IDCATGRP   PIC S9(3)   COMP-3.                      
020600             07  SOEK-IDCATAVS   PIC S9(5)   COMP-3.                      
020700             07  SOEK-IDCATRAD-H PIC S9(5)   COMP-3.                      
020800             07  SOEK-TENOTE     PIC X(40).                               
020900             07  SPARAD-FRAN-RAD  PIC 9(4).                               
021000             07  SOEK-IDCATPOS    PIC X(3).                               
021100     EJECT                                                                
021200* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
021300 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-T-DLI'.        
021400 01  NYCKLAR-TILL-DLI.                                                    
021500   03  W-IDCATNR-X.                                                       
021600       05  W-IDCATNR-KAT         PIC 9(5)   VALUE ZERO.                   
021700   03  W-WDN501-FROM-X.                                                   
021800       05  W-IDCATNR-FROM        PIC 9(5)   VALUE ZERO.                   
021900       05  W-IDCATGRP-FROM       PIC 9(2)   VALUE ZERO.                   
022000       05  W-IDCATAVS-FROM       PIC 9(4)   VALUE ZERO.                   
022100   03  W-WDN501-TO-X.                                                     
022200       05  W-IDCATNR-TO          PIC 9(5)   VALUE ZERO.                   
022300       05  W-IDCATGRP-TO         PIC 9(2)   VALUE ZERO.                   
022400       05  W-IDCATAVS-TO         PIC 9(4)   VALUE ZERO.                   
022500                                                                          
022600   03  W-WDN512KY-FROM-X.                                                 
022700       05  W-IDCATRAD-FROM2      PIC 9(4)   VALUE ZERO.                   
022800       05  W-KDCATPUB-FROM2      PIC X(6)   VALUE SPACE.                  
022900   03  W-WDN512KY-FROM3-X.                                                
023000       05  W-IDCATRAD-FROM3      PIC 9(4)   VALUE ZERO.                   
023100       05  W-KDCATPUB-FROM3      PIC X(6)   VALUE SPACE.                  
023200   03  W-WDN512KY-TO-X.                                                   
023300       05  W-IDCATRAD-TO2        PIC 9(4)   VALUE ZERO.                   
023400       05  W-KDCATPUB-TO2        PIC X(6)   VALUE SPACE.                  
023500   03  W-WDN512KY-FROM-F-X.                                               
023600       05  W-IDCATRAD-FROM-F     PIC 9(4)   VALUE ZERO.                   
023700       05  W-KDCATPUB-FROM-F     PIC X(6)   VALUE SPACE.                  
023800   03  W-WDN512KY-FROM-MIN-F-X.                                           
023900       05  W-IDCATRAD-FROM-MIN-F  PIC 9(4)   VALUE ZERO.                  
024000       05  W-KDCATPUB-FROM-MIN-F  PIC X(6)   VALUE SPACE.                 
024100   03  W-WDN512KY-FROM-MAX-F-X.                                           
024200       05  W-IDCATRAD-FROM-MAX-F  PIC 9(4)   VALUE ZERO.                  
024300       05  W-KDCATPUB-FROM-MAX-F  PIC X(6)   VALUE SPACE.                 
024400   03  W-WDN512KY-TOM-MIN-X.                                              
024500       05  W-IDCATRAD-TOM-MIN    PIC 9(4)   VALUE ZERO.                   
024600       05  W-KDCATPUB-TOM-MIN    PIC X(6)   VALUE SPACE.                  
024700                                                                          
024800   03  W-IDCATRAD-FROM-X.                                                 
024900       05  W-IDCATRAD-FROM       PIC 9(4)   VALUE ZERO.                   
025000   03  W-IDCATRAD-FROM-MIN-X.                                             
025100       05  W-IDCATRAD-FROM-MIN   PIC 9(4)   VALUE ZERO.                   
025200   03  W-IDCATRAD-FROM-MAX-X.                                             
025300       05  W-IDCATRAD-FROM-MAX   PIC 9(4)   VALUE ZERO.                   
025400   03  W-IDCATRAD-FROM2-MIN-X.                                            
025500       05  W-IDCATRAD-FROM2-MIN  PIC 9(4)   VALUE ZERO.                   
025600   03  W-IDCATRAD-FROM2-MAX-X.                                            
025700       05  W-IDCATRAD-FROM2-MAX  PIC 9(4)   VALUE ZERO.                   
025800                                                                          
025900   03  W-IDCATRAD-TO-X.                                                   
026000       05  W-IDCATRAD-TO         PIC 9(4)   VALUE ZERO.                   
026100   03  W-IDCATRAD-MIN-X.                                                  
026200       05  W-IDCATRAD-MIN        PIC 9(4)   VALUE ZERO.                   
026300   03  W-IDCATRAD-MAX-X.                                                  
026400       05  W-IDCATRAD-MAX        PIC 9(4)   VALUE ZERO.                   
026500                                                                          
026600   03  W-KDCATPUB-FROM-X.                                                 
026700       05  W-KDCATPUB-FROM       PIC X(6)   VALUE SPACE.                  
026800   03  W-IDSEGMNR-X.                                                      
026900       05  W-IDSEGMNR            PIC S9(1)   VALUE ZERO.                  
027000   03  W-IDCATPOS-FROM-X.                                                 
027100       05  W-IDCATPOS-FROM       PIC X(3)   VALUE SPACE.                  
027200   03  W-IDCATPOS-TO-X.                                                   
027300       05  W-IDCATPOS-TO         PIC X(3)    VALUE SPACE.                 
027400     EJECT                                                                
027500* - - - - - - - - - - - - - - - - - - -  MEDDELANDEN                      
027600 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
027700 01  MEDDELANDEN.                                                         
027800     03 FILLER-1.                                                         
027900          05 FILLER              PIC X(40)                                
028000              VALUE '    ILLUSTRATIONSNR STÄMMER EJ          '.           
028100          05 FILLER              PIC X(40)                                
028200              VALUE '    ILLUSTRATION-NO NOT CORRECT         '.           
028300     03 FILLER REDEFINES FILLER-1.                                        
028400          05 FEL-1   OCCURS 2    PIC X(40).                               
028500     03 FILLER-2.                                                         
028600          05 FILLER              PIC X(40)                                
028700              VALUE '    UPPLYSTA FÄLT FEL                   '.           
028800          05 FILLER              PIC X(40)                                
028900              VALUE '    HILIGHTED FIELDS WRONG              '.           
029000     03 FILLER REDEFINES FILLER-2.                                        
029100          05 FEL-2   OCCURS 2    PIC X(40).                               
029200     03 FILLER-3.                                                         
029300          05 FILLER              PIC X(40)                                
029400              VALUE '    UPPDATERING ENBART FRÅN 1 5 5 2     '.           
029500          05 FILLER              PIC X(40)                                
029600              VALUE 'UPDATING IS ONLY OK FROM SCREEN 1 5 5 2 '.           
029700     03 FILLER REDEFINES FILLER-3.                                        
029800          05 FEL-3   OCCURS 2    PIC X(40).                               
029900     03 FILLER-4.                                                         
030000          05 FILLER              PIC X(40)                                
030100              VALUE '    TRYCK PFK11 FÖR UPPDATERING         '.           
030200          05 FILLER              PIC X(40)                                
030300              VALUE '    PRESS PFK11 TO UPDATE               '.           
030400     03 FILLER REDEFINES FILLER-4.                                        
030500          05 FEL-4   OCCURS 2    PIC X(40).                               
030600     03 FILLER-5.                                                         
030700          05 FILLER              PIC X(40)                                
030800              VALUE '    EJ TILLÅTEN ATT LÅNA FRÅN       '.               
030900          05 FILLER              PIC X(40)                                
031000              VALUE '    NOT ALLOWED TO COPY_FROM        '.               
031100     03 FILLER REDEFINES FILLER-5.                                        
031200          05 FEL-5   OCCURS 2    PIC X(40).                               
031300     03 FILLER-6.                                                         
031400          05 FILLER              PIC X(40)                                
031500              VALUE  ' AVBRUTET LÅN, MER ÄN 25 ART/POS       '.           
031600          05 FILLER              PIC X(40)                                
031700              VALUE  ' COPY_INTERRUPTED, MORE THAN 25 PNO/FIG'.           
031800     03 FILLER REDEFINES FILLER-6.                                        
031900          05 FEL-6   OCCURS 2    PIC X(40).                               
032000     03 FILLER-7.                                                         
032100          05 FILLER              PIC X(40)                                
032200              VALUE  ' ANGE EXAKT RADNUMMER-TILL             '.           
032300          05 FILLER              PIC X(40)                                
032400              VALUE  ' DEFINE AN ACCURATE LINE-NO TO         '.           
032500     03 FILLER REDEFINES FILLER-7.                                        
032600          05 FEL-7   OCCURS 2    PIC X(40).                               
032700     03 FILLER-11.                                                        
032800          05 FILLER              PIC X(29)                                
032900              VALUE '    UPPDATERING GJORD        '.                      
033000          05 FILLER              PIC X(29)                                
033100              VALUE '    DATABASE HAS BEEN UPDATED'.                      
033200     03 FILLER REDEFINES FILLER-11.                                       
033300          05 MED-1   OCCURS 2    PIC X(29).                               
033400     03 FILLER-12.                                                        
033500          05 FILLER              PIC X(35)                                
033600              VALUE '    INGEN UPPDATERING GJORD        '.                
033700          05 FILLER              PIC X(35)                                
033800              VALUE '    DATABASE HAS  NOT  BEEN UPDATED'.                
033900     03 FILLER REDEFINES FILLER-12.                                       
034000          05 MED-2   OCCURS 2    PIC X(35).                               
034100     03 FILLER-13.                                                        
034200          05 FILLER              PIC X(37)                                
034300              VALUE '    INGEN RAD SOM UPPFYLLER VILLKORET'.              
034400          05 FILLER              PIC X(37)                                
034500              VALUE '    NO LINE THAT MATCHES THE ARGUMENT'.              
034600     03 FILLER REDEFINES FILLER-13.                                       
034700          05 MED-3   OCCURS 2    PIC X(37).                               
034800     EJECT                                                                
034900* - - - - - - - - - - - - - - - - - - - - MOD-MID-AREA                    
035000 01  FILLER                      PIC X(16)  VALUE 'MOD-MID-AREA'.         
035100 01  W-PROG-TO-PROG-SW.                                                   
035200     03 M-SW-LL                 PIC S9(4)  VALUE +75  COMP SYNC.          
035300     03 M-SW-Z1-Z2              PIC X(2)   VALUE LOW-VALUE.               
035400     03 M-SW-KDTRANS            PIC X(8)   VALUE 'W1T552U '.              
035500     03 M-SW-IDTRANS            PIC X(4)   VALUE '1552'.                  
035600     03 M-SW-KDMFSFOR           PIC X(1)   VALUE '1'.                     
035700*    03  -COPY W1I55201     -PRE MOD-.                                    
035800     EJECT                                                                
035900* - - - - - - - - - - - - - - - - - - - - MID-AREA                        
036000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
036100*01  -COPY W1I55201.                                                      
036200     EJECT                                                                
036300* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
036400 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
036500*01  -COPY WMSGAREA                                                       
036600     EJECT                                                                
036700*    03  POST  -COPY W1O55201 -RED MSG-AREA.                              
036800     EJECT                                                                
036900* - - - - - - - - - - - - - - - - - - -  MFS-AREA                         
037000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
037100*01  -COPY WMFSAREA                                                       
037200     EJECT                                                                
037300* - - - - - - - - - - - - - - - - - - -  IMS-WS                           
037400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
037500 01  IMS-WS.                                                              
037600*                        **** STATUS-KOD FRÅN IMS                         
037700   03  STATUS-WS                 PIC XX.                                  
037800     88  SEGMENT-FINNS                       VALUE '  '.                  
037900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
038000     88  BASEN-SLUT                          VALUE 'GB'.                  
038100     SKIP2                                                                
038200   03  GODK-STATUSKODER.                                                  
038300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038400     SKIP2                                                                
038500 01  SSA1                        PIC X(64).                               
038600 01  SSA2                        PIC X(64).                               
038700 01  SSA3                        PIC X(64).                               
038800     EJECT                                                                
038900*                            IMS FUNKTIONSKODER                           
039000*01    -COPY W0003                                                        
039100     EJECT                                                                
039200* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA                      
039300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
039400 01  DLI-IO-AREA.                                                         
039500     03  IO-AREA-1               PIC X(70)  VALUE SPACE.                  
039600     SKIP3                                                                
039700*    03  WLKATH21 -COPY WDN521   -RED IO-AREA-1.                          
039800     EJECT                                                                
039900*    03  WLKATH22 -COPY WDN522   -RED IO-AREA-1.                          
040000     EJECT                                                                
040100*    03  WLKATH23 -COPY WDN523   -RED IO-AREA-1.                          
040200     EJECT                                                                
040300*    03  WLKATH24 -COPY WDN524   -RED IO-AREA-1.                          
040400     EJECT                                                                
040500*    03  WLKATH01 -COPY WDN501   -RED IO-AREA-1.                          
040600     EJECT                                                                
040700*    03  WLKATH11 -COPY WDN511   -RED IO-AREA-1.                          
040800     EJECT                                                                
040900*    03  WLKATH12 -COPY WDN512   -RED IO-AREA-1.                          
041000     EJECT                                                                
041100*    03  WLKATH25 -COPY WDN525   -RED IO-AREA-1.                          
041200     EJECT                                                                
041300*    03  WLKATH26 -COPY WDN526   -RED IO-AREA-1.                          
041400     EJECT                                                                
041500*    03  WLKATH27 -COPY WDN527   -RED IO-AREA-1.                          
041600     EJECT                                                                
041700     03  IO-AREA-2           PIC X(480)  VALUE SPACE.                     
041800     SKIP3                                                                
041900*    03  WLKATM01 -COPY WDN101   -RED IO-AREA-2.                          
042000     EJECT                                                                
042100*    03  WLKATM11 -COPY WDN111   -RED IO-AREA-2.                          
042200     EJECT                                                                
042300*    03  WLKATH01 -COPY WDN501  -PRE TO-  -RED IO-AREA-2.                 
042400     EJECT                                                                
042500*    03  WLKATH11 -COPY WDN511  -PRE TO-  -RED IO-AREA-2.                 
042600     EJECT                                                                
042700*    03  WLKATH12 -COPY WDN512  -PRE TO-  -RED IO-AREA-2.                 
042800     EJECT                                                                
042900*    03  WLKATH21 -COPY WDN521  -PRE TO-  -RED IO-AREA-2.                 
043000     EJECT                                                                
043100*    03  WLKATH22 -COPY WDN522  -PRE TO-  -RED IO-AREA-2.                 
043200     EJECT                                                                
043300*    03  WLKATH23 -COPY WDN523  -PRE TO-  -RED IO-AREA-2.                 
043400     EJECT                                                                
043500*    03  WLKATH24 -COPY WDN524  -PRE TO-  -RED IO-AREA-2.                 
043600     EJECT                                                                
043700*    03  WLKATH25 -COPY WDN525  -PRE TO-  -RED IO-AREA-2.                 
043800     EJECT                                                                
043900*    03  WLKATH26 -COPY WDN526  -PRE TO-  -RED IO-AREA-2.                 
044000     EJECT                                                                
044100     03  IO-AREA-3           PIC X(40)       VALUE SPACE.                 
044200     SKIP3                                                                
044300*    03  WLKATH01 -COPY WDN501  -PRE KOLL-  -RED IO-AREA-3.               
044400     EJECT                                                                
044500*    03  WLKATH12 -COPY WDN512  -PRE KOLL-  -RED IO-AREA-3.               
044600     EJECT                                                                
044700*    03  WLKATH21 -COPY WDN521  -PRE KOLL-  -RED IO-AREA-3.               
044800     EJECT                                                                
044900 LINKAGE SECTION.                                                         
045000     SKIP2                                                                
045100*01  -COPY W0009    -PRE MSG-                                             
045200     EJECT                                                                
045300*01  -COPY W0008     -PRE ALT-                                            
045400     05  FILLER                  PIC X.                                   
045500     EJECT                                                                
045600*01  -COPY W0008     -PRE AVS1-                                           
045700     05  AVS1-WDN501KY.                                                   
045800         07  AVS1-IDCATNR           PIC 9(5).                             
045900         07  AVS1-IDCATGRP          PIC 9(2).                             
046000         07  AVS1-IDCATAVS          PIC 9(4).                             
046100     05  AVS1-IDCATRAD              PIC 9(4).                             
046200     05  AVS1-KDCATPUB-FOM          PIC X(6).                             
046300     EJECT                                                                
046400*01  -COPY W0008     -PRE AVS2-                                           
046500     05  AVS2-WDN501KY.                                                   
046600         07  AVS2-IDCATNR           PIC 9(5).                             
046700         07  AVS2-IDCATGRP          PIC 9(2).                             
046800         07  AVS2-IDCATAVS          PIC 9(4).                             
046900     05  AVS2-IDCATRAD              PIC 9(4).                             
047000     05  AVS2-KDCATPUB-FOM          PIC X(6).                             
047100     EJECT                                                                
047200*01  -COPY W0008     -PRE KAT-                                            
047300     05  FILLER                  PIC X.                                   
047400     EJECT                                                                
047500*01  -COPY W0008     -PRE AVS3-                                           
047600     05  FILLER                  PIC X.                                   
047700     EJECT                                                                
047800 PROCEDURE DIVISION USING MSG-PCB ALT-PCB AVS1-PCB                        
047900                    AVS2-PCB KAT-PCB AVS3-PCB.                            
048000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB AVS1-PCB                       
048100                    AVS2-PCB KAT-PCB AVS3-PCB.                            
048200     SKIP2                                                                
048300     PERFORM IMS-GET-MSG                                                  
048400                                                                          
048500     IF SEGMENT-FINNS                                                     
048600        PERFORM A-INIT                                                    
048700        IF INDATA-FEL = NEJ                                               
048800           IF MFS-UPDATE                                                  
048900              PERFORM B-KOLLA-INDATA                                      
049000              IF INDATA-FEL = JA                                          
049100                 IF ILLU-FEL = JA                                         
049200                    MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                
049300                 ELSE                                                     
049400                    IF LAAN-OK = NEJ                                      
049500                       MOVE FEL-5 (SPRAAK-IX) TO MOD-TEMFSFEL             
049600                    ELSE                                                  
049700                       IF RAD-TO-FEL = JA                                 
049800                          MOVE FEL-7 (SPRAAK-IX) TO MOD-TEMFSFEL          
049900                       ELSE                                               
050000                          MOVE FEL-2 (SPRAAK-IX) TO MOD-TEMFSFEL          
050100                       END-IF                                             
050200                    END-IF                                                
050300                 END-IF                                                   
050400                 PERFORM G-VISA-BILD-IGEN                                 
050500                 MOVE MAX-MOD-LAENGD TO MSG-KVLL                          
050600                 PERFORM IMS-INSERT-MSG                                   
050700              ELSE                                                        
050800                 IF KOLUMN-IFYLLD = NEJ                                   
050900                    PERFORM C-LAANA-RADER                                 
051000                 ELSE                                                     
051100                    IF POS-UPD = JA                                       
051200                       PERFORM D-LAANA-KOLUMN-POS                         
051300                    ELSE                                                  
051400                       PERFORM E-LAANA-KOLUMN-RADER                       
051500                    END-IF                                                
051600                 END-IF                                                   
051700              END-IF                                                      
051800           ELSE                                                           
051900              MOVE FEL-4 (SPRAAK-IX) TO MOD-TEMFSFEL                      
052000              PERFORM G-VISA-BILD-IGEN                                    
052100              PERFORM X-GRUND-FORMAT                                      
052200              MOVE MAX-MOD-LAENGD TO MSG-KVLL                             
052300              PERFORM IMS-INSERT-MSG                                      
052400           END-IF                                                         
052500        ELSE                                                              
052600           MOVE FEL-3 (SPRAAK-IX) TO MOD-TEMFSFEL                         
052700           PERFORM H-RENSA-BILD                                           
052800           PERFORM X-GRUND-FORMAT                                         
052900           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
053000           PERFORM IMS-INSERT-MSG                                         
053100        END-IF                                                            
053200     END-IF                                                               
053300                                                                          
053400     MOVE ZERO TO RETURN-CODE                                             
053500     GOBACK                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 A-INIT SECTION.                                                          
053900     SKIP2                                                                
054000     MOVE NEJ TO INDATA-FEL                                               
054100                                                                          
054200     IF MSG-DUBBLA-TRANSKODER                                             
054300        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I55201                
054400        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
054500        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
054600     ELSE                                                                 
054700        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I55201                 
054800        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
054900        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
055000     END-IF                                                               
055100                                                                          
055200     IF MFS-IDTRANS = '1552'                                              
055300        MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                          
055400     ELSE                                                                 
055500        MOVE JA TO INDATA-FEL                                             
055600     END-IF                                                               
055700                                                                          
055800     MOVE LOW-VALUE TO MSG-AREA                                           
055900     MOVE 'W1O55201' TO MFS-IDMOD                                         
056000     MOVE '1552' TO MOD-IDTRANS                                           
056100                                                                          
056200     IF ENGLISH-TEXT                                                      
056300        MOVE +2 TO SPRAAK-IX                                              
056400     ELSE                                                                 
056500        MOVE +1 TO SPRAAK-IX                                              
056600     END-IF                                                               
056700                                                                          
056800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
056900                             MOD-TEMFSINF                                 
057000                                                                          
057100     ACCEPT DAGENS-DATUM FROM DATE                                        
057200     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
057300     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
057400     CALL WDATKONV USING DAT-KDDATFORM                                    
057500                         DAT-I-TIDATUM                                    
057600                         DAT-O-TIDATUM                                    
057700                         DAT-KDSVAR                                       
057800     IF DAT-KDSVAR-OK                                                     
057900        MOVE DAT-TIAAVV-GRP TO DAGENS-AAR-VECKA                           
058000        MOVE DAT-TISEKEL    TO DAGENS-AAR(1:2)                            
058100     END-IF                                                               
058200                                                                          
058300     MOVE DAGENS-DATUM(1:2) TO DAGENS-AAR(3:2)                            
058400                                                                          
058500     COMPUTE WS-TIAAAA(1) = DAGENS-AAR - 1                                
058600     COMPUTE WS-TIAAAA(2) = DAGENS-AAR                                    
058700     COMPUTE WS-TIAAAA(3) = DAGENS-AAR + 1                                
058800     COMPUTE WS-TIAAAA(4) = DAGENS-AAR + 2                                
058900                                                                          
059000     MOVE SPACE TO W-IDCATPOS-FROM                                        
059100                   W-IDCATPOS-TO                                          
059200     .                                                                    
059300     EJECT                                                                
059400 B-KOLLA-INDATA SECTION.                                                  
059500******************************************************************        
059600* FRÅN-KDCATPUB: GÄLLANDE RAD LÄSES                                       
059700* TILL-KDCATPUB: KOLLAS MOT WDN1                                          
059800* OM FRÅN-KDCATPUB EJ IFYLLD LÅNAS SAMTLIGA RADER                         
059900*                  TILL-KDCATPUB FÅR DÅ INTE VARA IFYLLD                  
060000******************************************************************        
060100     MOVE NEJ TO INDATA-FEL                                               
060200                 KOLUMN-IFYLLD                                            
060300                 POS-UPD                                                  
060400                 ILLU-FEL                                                 
060500                 MITT-RADER                                               
060600                 SPAR-IFYLLT                                              
060700     MOVE JA  TO LAAN-OK                                                  
060800     MOVE SPACE TO IN-IDCATPOS-NUM                                        
060900     MOVE ZERO TO  WS-MITT-RAD-MAX                                        
061000     MOVE SPACE TO IN-SPACE                                               
061100                                                                          
061200     IF MID-IDCATNR-FROM = ALL '+'                                        
061300        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-FROM-ATTR                   
061400        MOVE JA TO INDATA-FEL                                             
061500     ELSE                                                                 
061600        IF MID-IDCATNR-FROM NUMERIC                                       
061700           IF MID-IDCATNR-FROM = ZERO                                     
061800              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-FROM-ATTR             
061900              MOVE JA TO INDATA-FEL                                       
062000           ELSE                                                           
062100              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATNR-FROM-ATTR           
062200           END-IF                                                         
062300        ELSE                                                              
062400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-FROM-ATTR                
062500           MOVE JA TO INDATA-FEL                                          
062600        END-IF                                                            
062700     END-IF                                                               
062800                                                                          
062900     IF MID-IDCATNR-TO = ALL '+'                                          
063000        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-TO-ATTR                     
063100        MOVE JA TO INDATA-FEL                                             
063200     ELSE                                                                 
063300        IF MID-IDCATNR-TO NUMERIC                                         
063400           IF MID-IDCATNR-TO = ZERO                                       
063500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-TO-ATTR               
063600              MOVE JA TO INDATA-FEL                                       
063700           ELSE                                                           
063800              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATNR-TO-ATTR             
063900           END-IF                                                         
064000        ELSE                                                              
064100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-TO-ATTR                  
064200           MOVE JA TO INDATA-FEL                                          
064300        END-IF                                                            
064400     END-IF                                                               
064500                                                                          
064600     IF MID-IDCATGRP-FROM = ALL '+'                                       
064700        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-FROM-ATTR                  
064800        MOVE JA TO INDATA-FEL                                             
064900     ELSE                                                                 
065000        IF MID-IDCATGRP-FROM NUMERIC                                      
065100           IF MID-IDCATGRP-FROM = ZERO                                    
065200              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-FROM-ATTR            
065300              MOVE JA TO INDATA-FEL                                       
065400           ELSE                                                           
065500              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATGRP-FROM-ATTR          
065600           END-IF                                                         
065700        ELSE                                                              
065800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-FROM-ATTR               
065900           MOVE JA TO INDATA-FEL                                          
066000        END-IF                                                            
066100     END-IF                                                               
066200                                                                          
066300     IF MID-IDCATGRP-TO = ALL '+'                                         
066400        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-TO-ATTR                    
066500        MOVE JA TO INDATA-FEL                                             
066600     ELSE                                                                 
066700        IF MID-IDCATGRP-TO NUMERIC                                        
066800           IF MID-IDCATGRP-TO = ZERO                                      
066900              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-TO-ATTR              
067000              MOVE JA TO INDATA-FEL                                       
067100           ELSE                                                           
067200              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATGRP-TO-ATTR            
067300           END-IF                                                         
067400        ELSE                                                              
067500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-TO-ATTR                 
067600           MOVE JA TO INDATA-FEL                                          
067700        END-IF                                                            
067800     END-IF                                                               
067900                                                                          
068000     IF MID-IDCATAVS-FROM = ALL '+'                                       
068100        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-FROM-ATTR                  
068200        MOVE JA TO INDATA-FEL                                             
068300     ELSE                                                                 
068400        IF MID-IDCATAVS-FROM NUMERIC                                      
068500           IF MID-IDCATAVS-FROM = ZERO                                    
068600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-FROM-ATTR            
068700              MOVE JA TO INDATA-FEL                                       
068800           ELSE                                                           
068900              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATAVS-FROM-ATTR          
069000           END-IF                                                         
069100        ELSE                                                              
069200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-FROM-ATTR               
069300           MOVE JA TO INDATA-FEL                                          
069400        END-IF                                                            
069500     END-IF                                                               
069600                                                                          
069700     IF MID-IDCATAVS-TO = ALL '+'                                         
069800        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-TO-ATTR                    
069900        MOVE JA TO INDATA-FEL                                             
070000     ELSE                                                                 
070100        IF MID-IDCATAVS-TO NUMERIC                                        
070200           IF MID-IDCATAVS-TO = ZERO                                      
070300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-TO-ATTR              
070400              MOVE JA TO INDATA-FEL                                       
070500           ELSE                                                           
070600              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATAVS-TO-ATTR            
070700           END-IF                                                         
070800        ELSE                                                              
070900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-TO-ATTR                 
071000           MOVE JA TO INDATA-FEL                                          
071100        END-IF                                                            
071200     END-IF                                                               
071300                                                                          
071400     IF MID-IDKOL-FROM = ALL '+'                                          
071500        CONTINUE                                                          
071600     ELSE                                                                 
071700        MOVE JA TO KOLUMN-IFYLLD                                          
071800        IF MID-IDKOL-FROM = 'A' OR 'B' OR 'C' OR 'D' OR 'E'               
071900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKOL-FROM-ATTR               
072000           MOVE MID-IDKOL-FROM TO WS-IDKOL-FROM                           
072100        ELSE                                                              
072200           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKOL-FROM-ATTR                 
072300           MOVE JA TO INDATA-FEL                                          
072400        END-IF                                                            
072500     END-IF                                                               
072600                                                                          
072700     IF MID-IDKOL-TO = ALL '+'                                            
072800        CONTINUE                                                          
072900     ELSE                                                                 
073000        MOVE JA TO KOLUMN-IFYLLD                                          
073100        IF MID-IDKOL-TO = 'A' OR 'B' OR 'C' OR 'D' OR 'E'                 
073200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKOL-TO-ATTR                 
073300           MOVE MID-IDKOL-TO TO WS-IDKOL-TO                               
073400        ELSE                                                              
073500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKOL-TO-ATTR                   
073600           MOVE JA TO INDATA-FEL                                          
073700        END-IF                                                            
073800     END-IF                                                               
073900                                                                          
074000     IF MID-IDCATRAD-SPAR NUMERIC                                         
074100        CONTINUE                                                          
074200     ELSE                                                                 
074300        MOVE ZERO TO MID-IDCATRAD-SPAR                                    
074400     END-IF                                                               
074500                                                                          
074600     IF MID-KDCATPUB-SPAR = LOW-VALUE                                     
074700        MOVE NEJ TO SPAR-IFYLLT                                           
074800     ELSE                                                                 
074900        MOVE JA TO SPAR-IFYLLT                                            
075000     END-IF                                                               
075100                                                                          
075200     IF MID-KDCATPUB-SPAR = SPACE                                         
075300        MOVE LOW-VALUE TO MID-KDCATPUB-SPAR                               
075400     END-IF                                                               
075500                                                                          
075600     IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                                 
075700        IF MID-KDCATPUB-R-TO-FOM = ALL '+'                                
075800          MOVE LOW-VALUE TO WS-KDCATPUB-TO-FOM                            
075900        ELSE                                                              
076000          MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TO-FOM-ATTR           
076100          MOVE JA TO INDATA-FEL                                           
076200        END-IF                                                            
076300        IF MID-KDCATPUB-R-TO-TOM = ALL '+'                                
076400          MOVE HIGH-VALUE TO WS-KDCATPUB-TO-TOM                           
076500        ELSE                                                              
076600          MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TO-TOM-ATTR           
076700          MOVE JA TO INDATA-FEL                                           
076800        END-IF                                                            
076900     ELSE                                                                 
077000        IF MID-KDCATPUB-R-FROM-FOM NUMERIC                                
077100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-FROM-FOM-ATTR        
077200        ELSE                                                              
077300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-FROM-FOM-ATTR        
077400           MOVE JA TO INDATA-FEL                                          
077500        END-IF                                                            
077600        IF MID-KDCATPUB-R-TO-FOM = ALL '+'                                
077700           MOVE LOW-VALUE TO WS-KDCATPUB-TO-FOM                           
077800        ELSE                                                              
077900           IF MID-KDCATPUB-R-TO-FOM NUMERIC                               
078000              MOVE MFS-ALFA-FAELT-RAETT                                   
078100                                 TO MOD-KDCATPUB-R-TO-FOM-ATTR            
078200              MOVE MID-KDCATPUB-R-TO-FOM                                  
078300                                   TO WS-KDCATPUB-R-AVV                   
078400              PERFORM S50-Y2K-KDCATPUB-R                                  
078500              MOVE WS-KDCATPUB-AAAAVV                                     
078600                                 TO WS-KDCATPUB-TO-FOM                    
078700           ELSE                                                           
078800            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TO-FOM-ATTR         
078900              MOVE JA TO INDATA-FEL                                       
079000           END-IF                                                         
079100        END-IF                                                            
079200        IF MID-KDCATPUB-R-TO-TOM = ALL '+'                                
079300           MOVE HIGH-VALUE TO WS-KDCATPUB-TO-TOM                          
079400        ELSE                                                              
079500          IF MID-KDCATPUB-R-TO-TOM NUMERIC                                
079600             MOVE MFS-ALFA-FAELT-RAETT                                    
079700                                TO MOD-KDCATPUB-R-TO-TOM-ATTR             
079800             MOVE MID-KDCATPUB-R-TO-TOM                                   
079900                                   TO WS-KDCATPUB-R-AVV                   
080000              PERFORM S50-Y2K-KDCATPUB-R                                  
080100              MOVE WS-KDCATPUB-AAAAVV                                     
080200                                   TO WS-KDCATPUB-TO-TOM                  
080300          ELSE                                                            
080400             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TO-TOM-ATTR        
080500             MOVE JA TO INDATA-FEL                                        
080600          END-IF                                                          
080700        END-IF                                                            
080800     END-IF                                                               
080900                                                                          
081000     IF WS-KDCATPUB-TO-TOM > WS-KDCATPUB-TO-FOM                           
081100        CONTINUE                                                          
081200     ELSE                                                                 
081300        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TO-TOM-ATTR             
081400                                   MOD-KDCATPUB-R-TO-FOM-ATTR             
081500        MOVE JA TO INDATA-FEL                                             
081600     END-IF                                                               
081700                                                                          
081800     IF KOLUMN-IFYLLD = JA                                                
081900        IF MID-IDKOL-FROM = ALL '+'                                       
082000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKOL-FROM-ATTR                 
082100           MOVE JA TO INDATA-FEL                                          
082200        END-IF                                                            
082300        IF MID-IDKOL-TO = ALL '+'                                         
082400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKOL-TO-ATTR                   
082500           MOVE JA TO INDATA-FEL                                          
082600        END-IF                                                            
082700        PERFORM BA-KOLLA-RADER-KOL                                        
082800     ELSE                                                                 
082900        PERFORM BB-KOLLA-RADER                                            
083000     END-IF                                                               
083100                                                                          
083200     IF INDATA-FEL = NEJ                                                  
083300        PERFORM BC-KOLLA-LAAN-OK                                          
083400        PERFORM BL-KOLLA-PUBKODER                                         
083500        IF INDATA-FEL = NEJ                                               
083600*************** MID-IDCATRAD-SPAR > 0 VID TO-RAD 9999                     
083700          IF MID-IDCATRAD-SPAR > ZERO                                     
083800             MOVE MID-IDCATRAD-SPAR TO WS-RAD-FROM                        
083900                                       W-IDCATRAD-FROM                    
084000                                       WS-RAD-START                       
084100                                       W-IDCATRAD-TO                      
084200                                       W-IDCATRAD-TO2                     
084300             MOVE MID-KDCATPUB-SPAR TO W-KDCATPUB-TO2                     
084400                                                                          
084500             MOVE MID-IDCATNR-FROM  TO W-IDCATNR-FROM                     
084600             MOVE MID-IDCATGRP-FROM TO W-IDCATGRP-FROM                    
084700             MOVE MID-IDCATAVS-FROM TO W-IDCATAVS-FROM                    
084800                                                                          
084900             MOVE MID-IDCATNR-TO    TO W-IDCATNR-TO                       
085000             MOVE MID-IDCATGRP-TO   TO W-IDCATGRP-TO                      
085100             MOVE MID-IDCATAVS-TO   TO W-IDCATAVS-TO                      
085200                                                                          
085300             IF KOLUMN-IFYLLD = NEJ                                       
085400*********** KOLLAR OM TILL FINNS                                          
085500                PERFORM IMS-GU-AVS2-TO                                    
085600                PERFORM IMS-GET-AVS2-RAD-TO-NASTA                         
085700                   IF TO-RAD-IDCATRAD = WS-RAD-START                      
085800                      AND TO-RAD-KDCATPUB-FOM = MID-KDCATPUB-SPAR         
085900                      MOVE MFS-NUM-FAELT-FEL                              
086000                             TO MOD-IDCATRAD-START-ATTR                   
086100                      MOVE JA TO INDATA-FEL                               
086200                   ELSE                                                   
086300                      MOVE 9999 TO WS-MAX-RAD-TO                          
086400                      MOVE +1 TO WS-INTERVALL                             
086500                  END-IF                                                  
086600             ELSE                                                         
086700*********** KOLLAR OM TILL-KOLUMN-FINNS                                   
086800                PERFORM IMS-GU-AVS2-TO                                    
086900                IF POS-UPD = NEJ                                          
087000                  PERFORM IMS-GET-AVS2-RAD-TO-NASTA                       
087100                    IF TO-RAD-IDCATRAD = WS-RAD-START                     
087200                       AND TO-RAD-KDCATPUB-FOM = MID-KDCATPUB-SPAR        
087300                        MOVE MFS-NUM-FAELT-FEL                            
087400                            TO MOD-IDCATAVS-TO-ATTR                       
087500                        MOVE JA TO INDATA-FEL                             
087600                    ELSE                                                  
087700                       MOVE 9999 TO WS-MAX-RAD-TO                         
087800                       MOVE +1 TO WS-INTERVALL                            
087900                    END-IF                                                
088000                END-IF                                                    
088100             END-IF                                                       
088200*********KOLLAR OM RAD-START-FINNS                                        
088300             PERFORM IMS-GU-AVS2-TO                                       
088400             PERFORM IMS-GET-AVS2-RAD-TO-NASTA                            
088500             IF SEGMENT-FINNS                                             
088600                MOVE JA TO MITT-RADER                                     
088700                MOVE TO-RAD-IDCATRAD TO WS-MITT-RAD-MAX                   
088800                IF WS-RAD-START = TO-RAD-IDCATRAD                         
088900                   AND MID-KDCATPUB-SPAR = TO-RAD-KDCATPUB-FOM            
089000                    MOVE MFS-NUM-FAELT-FEL                                
089100                                TO MOD-IDCATRAD-START-ATTR                
089200                    MOVE JA TO INDATA-FEL                                 
089300                END-IF                                                    
089400             END-IF                                                       
089500                                                                          
089600          ELSE                                                            
089700           IF KOLUMN-IFYLLD = NEJ                                         
089800              PERFORM BD-KOLLA-FROM-FINNS                                 
089900              IF INDATA-FEL = NEJ                                         
090000                 PERFORM BE-KOLLA-TILL-FINNS                              
090100                 IF INDATA-FEL = NEJ                                      
090200                    PERFORM BF-KOLLA-LIKA                                 
090300                 END-IF                                                   
090400              END-IF                                                      
090500           ELSE                                                           
090600              PERFORM BG-KOLLA-FROM-KOL-FINNS                             
090700              IF INDATA-FEL = NEJ                                         
090800                 PERFORM BH-KOLLA-TILL-KOL-FINNS                          
090900                 IF INDATA-FEL = NEJ                                      
091000                    PERFORM BI-KOLLA-LIKA-KOL                             
091100                 END-IF                                                   
091200             END-IF                                                       
091300          END-IF                                                          
091400          IF INDATA-FEL = NEJ AND                                         
091500             KOLUMN-IFYLLD = JA                                           
091600*************** KONTROLL AV ILLU BORTTAGEN                                
091700             CONTINUE                                                     
091800*            PERFORM BJ-KOLLA-LIKA-ILLU                                   
091900          END-IF                                                          
092000          IF INDATA-FEL = NEJ AND POS-UPD = NEJ                           
092100             PERFORM BK-KOLLA-RAD-START-FINNS                             
092200          END-IF                                                          
092300         END-IF                                                           
092400       END-IF                                                             
092500     END-IF                                                               
092600     .                                                                    
092700     EJECT                                                                
092800 BA-KOLLA-RADER-KOL SECTION.                                              
092900     SKIP2                                                                
093000     IF MID-IDCATRAD-FROM = ALL '+' AND                                   
093100        MID-IDCATRAD-TO = ALL '+' AND                                     
093200        MID-IDCATRAD-START = ALL '+'                                      
093300                                                                          
093400        PERFORM BAA-KOL-POS                                               
093500     ELSE                                                                 
093600        PERFORM BAB-KOL-RAD                                               
093700     END-IF                                                               
093800     .                                                                    
093900     EJECT                                                                
094000 BAA-KOL-POS SECTION.                                                     
094100                                                                          
094200     MOVE +20    TO WS-RAD-FROM                                           
094300     MOVE +9999  TO WS-RAD-TO                                             
094400     MOVE SPACE TO WS-IDCATPOS                                            
094500     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDCATPOS-FROM-ATTR                  
094600                                  MOD-IDCATPOS-TO-ATTR                    
094700                                                                          
094800     IF MID-IDCATPOS-FROM = ALL '+'                                       
094900        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-FROM-ATTR                 
095000        MOVE JA TO INDATA-FEL                                             
095100     ELSE                                                                 
095200        MOVE JA TO POS-UPD                                                
095300        MOVE MID-IDCATPOS-FROM TO WS-IDCATPOS                             
095400                                  WS-IDCATPOS-FROM                        
095500        IF WS-IDCATPOS = SPACE                                            
095600           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-FROM-ATTR              
095700           MOVE JA TO INDATA-FEL                                          
095800        ELSE                                                              
095900           IF WS-IDCATPOS-POS1 = SPACE                                    
096000              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-FROM-ATTR           
096100              MOVE JA TO INDATA-FEL                                       
096200           ELSE                                                           
096300              IF WS-IDCATPOS-POS1 = ZERO                                  
096400                 MOVE MFS-ALFA-FAELT-FEL                                  
096500                          TO MOD-IDCATPOS-FROM-ATTR                       
096600                 MOVE JA TO INDATA-FEL                                    
096700              ELSE                                                        
096800                IF WS-IDCATPOS-POS1 NUMERIC                               
096900                   IF WS-IDCATPOS-POS2 = SPACE                            
097000                      IF WS-IDCATPOS-POS3 = SPACE                         
097100                         MOVE MFS-ALFA-FAELT-RAETT                        
097200                                 TO MOD-IDCATPOS-FROM-ATTR                
097300                         MOVE WS-IDCATPOS TO WS-IDCATPOS-FROM             
097400                      ELSE                                                
097500                         MOVE MFS-ALFA-FAELT-FEL                          
097600                                 TO MOD-IDCATPOS-FROM-ATTR                
097700                         MOVE JA TO INDATA-FEL                            
097800                      END-IF                                              
097900                   ELSE                                                   
098000                      IF WS-IDCATPOS-POS2 NUMERIC                         
098100                         MOVE MFS-ALFA-FAELT-RAETT                        
098200                                 TO MOD-IDCATPOS-FROM-ATTR                
098300                         MOVE WS-IDCATPOS TO WS-IDCATPOS-FROM             
098400                      ELSE                                                
098500                         IF WS-IDCATPOS-POS3 NOT = SPACE                  
098600                            MOVE MFS-ALFA-FAELT-FEL                       
098700                                     TO MOD-IDCATPOS-FROM-ATTR            
098800                            MOVE JA TO INDATA-FEL                         
098900                         END-IF                                           
099000                      END-IF                                              
099100                   END-IF                                                 
099200                ELSE                                                      
099300                   MOVE MFS-ALFA-FAELT-FEL                                
099400                             TO MOD-IDCATPOS-FROM-ATTR                    
099500                   MOVE JA TO INDATA-FEL                                  
099600                END-IF                                                    
099700             END-IF                                                       
099800          END-IF                                                          
099900        END-IF                                                            
100000     END-IF                                                               
100100                                                                          
100200     MOVE SPACE TO WS-IDCATPOS                                            
100300     IF MID-IDCATPOS-TO = ALL '+'                                         
100400        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-TO-ATTR                   
100500        MOVE JA TO INDATA-FEL                                             
100600     ELSE                                                                 
100700        MOVE MID-IDCATPOS-TO TO WS-IDCATPOS                               
100800                                WS-IDCATPOS-TO                            
100900        IF WS-IDCATPOS = SPACE                                            
101000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-TO-ATTR                
101100           MOVE JA TO INDATA-FEL                                          
101200        ELSE                                                              
101300           IF WS-IDCATPOS-POS1 = SPACE                                    
101400              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-TO-ATTR             
101500              MOVE JA TO INDATA-FEL                                       
101600           ELSE                                                           
101700              IF WS-IDCATPOS-POS1 = ZERO                                  
101800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-TO-ATTR          
101900                 MOVE JA TO INDATA-FEL                                    
102000              ELSE                                                        
102100                IF WS-IDCATPOS-POS1 NUMERIC                               
102200                   IF WS-IDCATPOS-POS2 = SPACE                            
102300                      IF WS-IDCATPOS-POS3 = SPACE                         
102400                         MOVE MFS-ALFA-FAELT-RAETT                        
102500                                 TO MOD-IDCATPOS-TO-ATTR                  
102600                         MOVE WS-IDCATPOS TO WS-IDCATPOS-TO               
102700                      ELSE                                                
102800                         MOVE MFS-ALFA-FAELT-FEL                          
102900                                  TO MOD-IDCATPOS-TO-ATTR                 
103000                         MOVE JA TO INDATA-FEL                            
103100                      END-IF                                              
103200                   ELSE                                                   
103300                      IF WS-IDCATPOS-POS2 NUMERIC                         
103400                         MOVE MFS-ALFA-FAELT-RAETT                        
103500                                   TO MOD-IDCATPOS-TO-ATTR                
103600                         MOVE WS-IDCATPOS TO WS-IDCATPOS-TO               
103700                      ELSE                                                
103800                         IF WS-IDCATPOS-POS3 NOT = SPACE                  
103900                            MOVE MFS-ALFA-FAELT-FEL                       
104000                                     TO MOD-IDCATPOS-TO-ATTR              
104100                            MOVE JA TO INDATA-FEL                         
104200                         END-IF                                           
104300                      END-IF                                              
104400                   END-IF                                                 
104500                ELSE                                                      
104600                   MOVE MFS-ALFA-FAELT-FEL                                
104700                             TO MOD-IDCATPOS-TO-ATTR                      
104800                   MOVE JA TO INDATA-FEL                                  
104900                END-IF                                                    
105000             END-IF                                                       
105100          END-IF                                                          
105200        END-IF                                                            
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 BAB-KOL-RAD SECTION.                                                     
105700     SKIP2                                                                
105800     MOVE +20    TO WS-RAD-FROM                                           
105900     MOVE +9999  TO WS-RAD-TO                                             
106000     MOVE 1      TO IN-IDCATPOS-NUM                                       
106100                                                                          
106200     IF MID-IDCATRAD-FROM = ALL '+'                                       
106300        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR                  
106400        MOVE JA TO INDATA-FEL                                             
106500     ELSE                                                                 
106600        IF MID-IDCATRAD-FROM NUMERIC                                      
106700           IF MID-IDCATRAD-FROM = ZERO OR < 20                            
106800              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR            
106900              MOVE JA TO INDATA-FEL                                       
107000           ELSE                                                           
107100              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-FROM-ATTR          
107200              MOVE MID-IDCATRAD-FROM TO WS-RAD-FROM                       
107300           END-IF                                                         
107400        ELSE                                                              
107500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR               
107600           MOVE JA TO INDATA-FEL                                          
107700        END-IF                                                            
107800     END-IF                                                               
107900                                                                          
108000     IF MID-IDCATRAD-TO = ALL '+'                                         
108100        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR                    
108200        MOVE JA TO INDATA-FEL                                             
108300     ELSE                                                                 
108400        IF MID-IDCATRAD-TO NUMERIC                                        
108500           IF MID-IDCATRAD-TO = ZERO OR < 20                              
108600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR              
108700              MOVE JA TO INDATA-FEL                                       
108800           ELSE                                                           
108900              IF MID-IDCATRAD-TO < WS-RAD-FROM                            
109000                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR           
109100                 MOVE JA TO INDATA-FEL                                    
109200              ELSE                                                        
109300                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-TO-ATTR         
109400                 MOVE MID-IDCATRAD-TO TO WS-RAD-TO                        
109500              END-IF                                                      
109600           END-IF                                                         
109700        ELSE                                                              
109800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR                 
109900           MOVE JA TO INDATA-FEL                                          
110000        END-IF                                                            
110100     END-IF                                                               
110200                                                                          
110300     IF MID-IDCATRAD-START = ALL '+'                                      
110400        MOVE +20 TO WS-RAD-START                                          
110500        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-START-ATTR               
110600     ELSE                                                                 
110700        IF MID-IDCATRAD-START NUMERIC                                     
110800           IF MID-IDCATRAD-START = ZERO                                   
110900              MOVE +20                 TO WS-RAD-START                    
111000              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-START-ATTR         
111100           ELSE                                                           
111200              IF WS-RAD-TO = +9999                                        
111300****************************************************************          
111400*    MAN FÅR INTE ANGE TILL-RAD 9999 VID LÅN AV KOLUMN TILL    *          
111500*    ETT BEFINTLIGT AVSNITTS KOLUMN OCH RAD-START.             *          
111600****************************************************************          
111700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR           
111800                 MOVE JA TO INDATA-FEL                                    
111900                            RAD-TO-FEL                                    
112000              ELSE                                                        
112100                 MOVE MID-IDCATRAD-START TO WS-RAD-START                  
112200                 MOVE MFS-NUM-FAELT-RAETT TO                              
112300                                    MOD-IDCATRAD-START-ATTR               
112400              END-IF                                                      
112500           END-IF                                                         
112600        ELSE                                                              
112700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-START-ATTR              
112800           MOVE JA TO INDATA-FEL                                          
112900        END-IF                                                            
113000     END-IF                                                               
113100                                                                          
113200     IF MID-IDCATPOS-FROM = ALL '+'                                       
113300        CONTINUE                                                          
113400     ELSE                                                                 
113500        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-FROM-ATTR                 
113600        MOVE JA TO INDATA-FEL                                             
113700     END-IF                                                               
113800                                                                          
113900     IF MID-IDCATPOS-TO = ALL '+'                                         
114000        CONTINUE                                                          
114100     ELSE                                                                 
114200        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-TO-ATTR                   
114300        MOVE JA TO INDATA-FEL                                             
114400     END-IF                                                               
114500     .                                                                    
114600     EJECT                                                                
114700 BB-KOLLA-RADER SECTION.                                                  
114800     SKIP2                                                                
114900     IF MID-IDCATRAD-FROM = ALL '+'                                       
115000        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR                  
115100        MOVE JA TO INDATA-FEL                                             
115200     ELSE                                                                 
115300        IF MID-IDCATRAD-FROM NUMERIC                                      
115400           IF MID-IDCATRAD-FROM = ZERO OR < 20                            
115500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR            
115600              MOVE JA TO INDATA-FEL                                       
115700           ELSE                                                           
115800              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-FROM-ATTR          
115900              MOVE MID-IDCATRAD-FROM TO WS-RAD-FROM                       
116000           END-IF                                                         
116100        ELSE                                                              
116200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR               
116300           MOVE JA TO INDATA-FEL                                          
116400        END-IF                                                            
116500     END-IF                                                               
116600                                                                          
116700     IF MID-IDCATRAD-TO = ALL '+'                                         
116800        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR                    
116900        MOVE JA TO INDATA-FEL                                             
117000     ELSE                                                                 
117100        IF MID-IDCATRAD-TO NUMERIC                                        
117200           IF MID-IDCATRAD-TO = ZERO OR < 20                              
117300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR              
117400              MOVE JA TO INDATA-FEL                                       
117500           ELSE                                                           
117600              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-TO-ATTR            
117700              MOVE MID-IDCATRAD-TO TO WS-RAD-TO                           
117800           END-IF                                                         
117900        ELSE                                                              
118000           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR                 
118100           MOVE JA TO INDATA-FEL                                          
118200        END-IF                                                            
118300     END-IF                                                               
118400                                                                          
118500     IF MID-IDCATRAD-START = ALL '+'                                      
118600        MOVE +20 TO WS-RAD-START                                          
118700        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-START-ATTR               
118800     ELSE                                                                 
118900        IF MID-IDCATRAD-START NUMERIC                                     
119000           IF MID-IDCATRAD-START = ZERO                                   
119100              MOVE +20 TO WS-RAD-START                                    
119200              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-START-ATTR         
119300           ELSE                                                           
119400              IF MID-IDCATRAD-START = 20                                  
119500                 MOVE MID-IDCATRAD-START TO WS-RAD-START                  
119600                 MOVE MFS-NUM-FAELT-RAETT TO                              
119700                      MOD-IDCATRAD-START-ATTR                             
119800              ELSE                                                        
119900                 IF WS-RAD-TO = 9999                                      
120000                    MOVE MFS-NUM-FAELT-FEL TO                             
120100                         MOD-IDCATRAD-START-ATTR                          
120200                    MOVE JA TO INDATA-FEL                                 
120300                 ELSE                                                     
120400                    MOVE MID-IDCATRAD-START TO WS-RAD-START               
120500                    MOVE MFS-NUM-FAELT-RAETT TO                           
120600                         MOD-IDCATRAD-START-ATTR                          
120700                 END-IF                                                   
120800              END-IF                                                      
120900           END-IF                                                         
121000        ELSE                                                              
121100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-START-ATTR              
121200           MOVE JA TO INDATA-FEL                                          
121300        END-IF                                                            
121400     END-IF                                                               
121500                                                                          
121600     IF MID-IDCATPOS-FROM = ALL '+'                                       
121700        CONTINUE                                                          
121800     ELSE                                                                 
121900        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-FROM-ATTR                 
122000        MOVE JA TO INDATA-FEL                                             
122100     END-IF                                                               
122200                                                                          
122300     IF MID-IDCATPOS-TO = ALL '+'                                         
122400        CONTINUE                                                          
122500     ELSE                                                                 
122600        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-TO-ATTR                   
122700        MOVE JA TO INDATA-FEL                                             
122800     END-IF                                                               
122900     .                                                                    
123000     EJECT                                                                
123100 BC-KOLLA-LAAN-OK SECTION.                                                
123200     SKIP2                                                                
123300     MOVE MID-IDCATNR-FROM  TO W-IDCATNR-KAT                              
123400     PERFORM IMS-GU-KAT                                                   
123500     IF SEGMENT-FINNS                                                     
123600        IF KAT-FLKOPIE = 'J'                                              
123700           CONTINUE                                                       
123800        ELSE                                                              
123900           MOVE NEJ TO LAAN-OK                                            
124000           MOVE JA TO INDATA-FEL                                          
124100        END-IF                                                            
124200     ELSE                                                                 
124300        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-FROM-ATTR                   
124400        MOVE JA TO INDATA-FEL                                             
124500     END-IF                                                               
124600     .                                                                    
124700     EJECT                                                                
124800 BD-KOLLA-FROM-FINNS SECTION.                                             
124900                                                                          
125000     MOVE MID-IDCATNR-FROM   TO W-IDCATNR-FROM                            
125100     MOVE MID-IDCATGRP-FROM  TO W-IDCATGRP-FROM                           
125200     MOVE MID-IDCATAVS-FROM  TO W-IDCATAVS-FROM                           
125300     MOVE NEJ TO GALLANDE-FINNS                                           
125400                                                                          
125500     PERFORM IMS-GU-AVS1-FROM                                             
125600                                                                          
125700     IF SEGMENT-FINNS                                                     
125800        MOVE MID-IDCATRAD-FROM TO W-IDCATRAD-FROM                         
125900                                  W-IDCATRAD-FROM2                        
126000        IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                              
126100           PERFORM IMS-GNP-AVS1-RAD-FROM                                  
126200        ELSE                                                              
126300           MOVE MID-KDCATPUB-R-FROM-FOM                                   
126400                                   TO WS-KDCATPUB-R-AVV                   
126500           PERFORM S50-Y2K-KDCATPUB-R                                     
126600           MOVE WS-KDCATPUB-AAAAVV                                        
126700                                   TO W-KDCATPUB-FROM2                    
126800           PERFORM IMS-GET-AVS1-RAD-FROM                                  
126900           IF SEGMENT-FINNS                                               
127000              MOVE JA TO GALLANDE-FINNS                                   
127100              MOVE MID-KDCATPUB-R-FROM-FOM                                
127200                                   TO WS-KDCATPUB-R-AVV                   
127300              PERFORM S50-Y2K-KDCATPUB-R                                  
127400              MOVE WS-KDCATPUB-AAAAVV                                     
127500                                   TO SPAR-KDCATPUB                       
127600           ELSE                                                           
127700              PERFORM BDA-SOEK-GALLANDE                                   
127800           END-IF                                                         
127900        END-IF                                                            
128000                                                                          
128100        IF SEGMENT-FINNS                                                  
128200           IF WS-RAD-TO = +9999                                           
128300              CONTINUE                                                    
128400           ELSE                                                           
128500              MOVE NEJ TO GALLANDE-FINNS                                  
128600              PERFORM IMS-GU-AVS1-FROM                                    
128700              MOVE WS-RAD-TO TO W-IDCATRAD-FROM                           
128800                                W-IDCATRAD-FROM2                          
128900              IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                        
129000                 PERFORM IMS-GNP-AVS1-RAD-FROM                            
129100              ELSE                                                        
129200                 MOVE MID-KDCATPUB-R-FROM-FOM                             
129300                                   TO WS-KDCATPUB-R-AVV                   
129400                 PERFORM S50-Y2K-KDCATPUB-R                               
129500                 MOVE WS-KDCATPUB-AAAAVV                                  
129600                                   TO W-KDCATPUB-FROM2                    
129700                 PERFORM IMS-GET-AVS1-RAD-FROM                            
129800                 IF SEGMENT-FINNS                                         
129900                    MOVE JA TO GALLANDE-FINNS                             
130000                    MOVE MID-KDCATPUB-R-FROM-FOM                          
130100                                   TO WS-KDCATPUB-R-AVV                   
130200                    PERFORM S50-Y2K-KDCATPUB-R                            
130300                    MOVE WS-KDCATPUB-AAAAVV                               
130400                                   TO SPAR-KDCATPUB                       
130500                 ELSE                                                     
130600                    PERFORM BDA-SOEK-GALLANDE                             
130700                 END-IF                                                   
130800              END-IF                                                      
130900              IF SEGMENT-SAKNAS                                           
131000                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-TO-ATTR           
131100                 MOVE JA TO INDATA-FEL                                    
131200              ELSE                                                        
131300                 MOVE MID-IDCATRAD-TO TO WS-RAD-TO                        
131400              END-IF                                                      
131500           END-IF                                                         
131600        ELSE                                                              
131700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR               
131800           MOVE JA TO INDATA-FEL                                          
131900        END-IF                                                            
132000     ELSE                                                                 
132100        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-FROM-ATTR                   
132200                                  MOD-IDCATGRP-FROM-ATTR                  
132300                                  MOD-IDCATAVS-FROM-ATTR                  
132400        MOVE JA TO INDATA-FEL                                             
132500     END-IF                                                               
132600                                                                          
132700     MOVE ZERO TO W-IDCATRAD-FROM2                                        
132800     .                                                                    
132900     EJECT                                                                
133000 BDA-SOEK-GALLANDE SECTION.                                               
133100                                                                          
133200     MOVE LOW-VALUE TO SPAR-KDCATPUB                                      
133300     MOVE NEJ TO GALLANDE-FINNS                                           
133400                                                                          
133500     PERFORM IMS-GET-AVS1-RAD-FROM-FIRST                                  
133600     PERFORM UNTIL SEGMENT-SAKNAS OR GALLANDE-FINNS = JA                  
133700        MOVE MID-KDCATPUB-R-FROM-FOM                                      
133800                             TO WS-KDCATPUB-R-AVV                         
133900        PERFORM S50-Y2K-KDCATPUB-R                                        
134000        IF RAD-KDCATPUB-FOM <= WS-KDCATPUB-AAAAVV                         
134100           IF RAD-KDCATPUB-TOM >= WS-KDCATPUB-AAAAVV                      
134200              IF RAD-KDCATPUB-FOM >= SPAR-KDCATPUB                        
134300                 MOVE RAD-KDCATPUB-FOM TO SPAR-KDCATPUB                   
134400                 MOVE JA TO GALLANDE-FINNS                                
134500              END-IF                                                      
134600           END-IF                                                         
134700        END-IF                                                            
134800        IF GALLANDE-FINNS = NEJ                                           
134900           PERFORM IMS-GNP-AVS1-RAD-FROM                                  
135000        END-IF                                                            
135100     END-PERFORM                                                          
135200     IF GALLANDE-FINNS = JA                                               
135300        MOVE SPACE TO STATUS-WS                                           
135400     END-IF                                                               
135500     .                                                                    
135600     EJECT                                                                
135700 BE-KOLLA-TILL-FINNS SECTION.                                             
135800                                                                          
135900     MOVE MID-IDCATNR-TO     TO W-IDCATNR-TO                              
136000     MOVE MID-IDCATGRP-TO    TO W-IDCATGRP-TO                             
136100     MOVE MID-IDCATAVS-TO    TO W-IDCATAVS-TO                             
136200                                                                          
136300     PERFORM IMS-GU-AVS2-TO                                               
136400                                                                          
136500     IF SEGMENT-FINNS                                                     
136600        MOVE WS-RAD-START TO W-IDCATRAD-TO                                
136700                             W-IDCATRAD-TO2                               
136800        IF SPAR-IFYLLT = JA                                               
136900           MOVE MID-KDCATPUB-SPAR TO W-KDCATPUB-TO2                       
137000           PERFORM IMS-GET-AVS2-RAD-TO-NASTA                              
137100        ELSE                                                              
137200           PERFORM IMS-GNP-AVS2-RAD-TO                                    
137300        END-IF                                                            
137400        IF SPAR-IFYLLT = JA                                               
137500           IF SEGMENT-FINNS                                               
137600              IF TO-RAD-IDCATRAD = WS-RAD-START AND                       
137700                (TO-RAD-KDCATPUB-FOM = MID-KDCATPUB-SPAR)                 
137800                    MOVE MFS-ALFA-FAELT-FEL TO                            
137900                               MOD-IDCATRAD-START-ATTR                    
138000                 MOVE JA TO INDATA-FEL                                    
138100              ELSE                                                        
138200                 MOVE TO-RAD-IDCATRAD TO WS-MAX-RAD-TO                    
138300                 MOVE +1 TO WS-INTERVALL                                  
138400              END-IF                                                      
138500           ELSE                                                           
138600              MOVE +9999 TO WS-MAX-RAD-TO                                 
138700              MOVE +10 TO WS-INTERVALL                                    
138800           END-IF                                                         
138900         ELSE                                                             
139000            IF SEGMENT-FINNS                                              
139100              IF TO-RAD-IDCATRAD = WS-RAD-START                           
139200                 MOVE MFS-NUM-FAELT-FEL TO                                
139300                                MOD-IDCATRAD-START-ATTR                   
139400                 MOVE JA TO INDATA-FEL                                    
139500              ELSE                                                        
139600                 IF WS-RAD-TO = +9999                                     
139700                    MOVE MFS-NUM-FAELT-FEL                                
139800                                     TO MOD-IDCATRAD-TO-ATTR              
139900                    MOVE JA TO INDATA-FEL                                 
140000                 ELSE                                                     
140100                    MOVE TO-RAD-IDCATRAD TO WS-MAX-RAD-TO                 
140200                    MOVE +1 TO WS-INTERVALL                               
140300                 END-IF                                                   
140400              END-IF                                                      
140500            ELSE                                                          
140600               MOVE +9999 TO WS-MAX-RAD-TO                                
140700               MOVE +10 TO WS-INTERVALL                                   
140800            END-IF                                                        
140900       END-IF                                                             
141000     ELSE                                                                 
141100        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-TO-ATTR                     
141200                                  MOD-IDCATGRP-TO-ATTR                    
141300                                  MOD-IDCATAVS-TO-ATTR                    
141400        MOVE JA TO INDATA-FEL                                             
141500     END-IF                                                               
141600     .                                                                    
141700     EJECT                                                                
141800 BF-KOLLA-LIKA SECTION.                                                   
141900     SKIP2                                                                
142000     IF (W-IDCATNR-FROM = W-IDCATNR-TO) AND                               
142100        (W-IDCATGRP-FROM = W-IDCATGRP-TO) AND                             
142200        (W-IDCATAVS-FROM = W-IDCATAVS-TO)                                 
142300                                                                          
142400        IF WS-RAD-TO = +9999                                              
142500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-START-ATTR              
142600           MOVE JA TO INDATA-FEL                                          
142700        ELSE                                                              
142800           IF WS-RAD-START < WS-RAD-FROM                                  
142900              IF WS-RAD-START < +20                                       
143000                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-START-ATTR        
143100                 MOVE JA TO INDATA-FEL                                    
143200              END-IF                                                      
143300           ELSE                                                           
143400              IF WS-RAD-START > WS-RAD-TO                                 
143500                 CONTINUE                                                 
143600              ELSE                                                        
143700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-START-ATTR        
143800                 MOVE JA TO INDATA-FEL                                    
143900              END-IF                                                      
144000           END-IF                                                         
144100        END-IF                                                            
144200     END-IF                                                               
144300     .                                                                    
144400     EJECT                                                                
144500 BG-KOLLA-FROM-KOL-FINNS SECTION.                                         
144600     SKIP2                                                                
144700     MOVE MID-IDCATNR-FROM   TO W-IDCATNR-FROM                            
144800     MOVE MID-IDCATGRP-FROM  TO W-IDCATGRP-FROM                           
144900     MOVE MID-IDCATAVS-FROM  TO W-IDCATAVS-FROM                           
145000     MOVE NEJ TO GALLANDE-FINNS                                           
145100                                                                          
145200     PERFORM IMS-GU-AVS1-FROM                                             
145300                                                                          
145400     IF SEGMENT-FINNS                                                     
145500        IF POS-UPD = NEJ                                                  
145600           MOVE MID-IDCATRAD-FROM TO W-IDCATRAD-FROM                      
145700                                     W-IDCATRAD-FROM2                     
145800           IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                           
145900              PERFORM IMS-GNP-AVS1-RAD-FROM                               
146000           ELSE                                                           
146100              MOVE MID-KDCATPUB-R-FROM-FOM                                
146200                             TO WS-KDCATPUB-R-AVV                         
146300              PERFORM S50-Y2K-KDCATPUB-R                                  
146400              MOVE WS-KDCATPUB-AAAAVV                                     
146500                             TO W-KDCATPUB-FROM2                          
146600              PERFORM IMS-GET-AVS1-RAD-FROM                               
146700              IF SEGMENT-FINNS                                            
146800                 MOVE JA TO GALLANDE-FINNS                                
146900                 MOVE MID-KDCATPUB-R-FROM-FOM                             
147000                             TO WS-KDCATPUB-R-AVV                         
147100                PERFORM S50-Y2K-KDCATPUB-R                                
147200                MOVE WS-KDCATPUB-AAAAVV                                   
147300                             TO SPAR-KDCATPUB                             
147400              ELSE                                                        
147500                 PERFORM BDA-SOEK-GALLANDE                                
147600              END-IF                                                      
147700           END-IF                                                         
147800                                                                          
147900           IF SEGMENT-FINNS                                               
148000              IF WS-RAD-TO = +9999                                        
148100                 CONTINUE                                                 
148200              ELSE                                                        
148300                 PERFORM IMS-GU-AVS1-FROM                                 
148400                 MOVE WS-RAD-TO TO W-IDCATRAD-FROM                        
148500                                   W-IDCATRAD-FROM2                       
148600                 IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                     
148700                    PERFORM IMS-GNP-AVS1-RAD-FROM                         
148800                 ELSE                                                     
148900                    MOVE NEJ TO GALLANDE-FINNS                            
149000                    MOVE MID-KDCATPUB-R-FROM-FOM                          
149100                             TO WS-KDCATPUB-R-AVV                         
149200                    PERFORM S50-Y2K-KDCATPUB-R                            
149300                    MOVE WS-KDCATPUB-AAAAVV                               
149400                             TO W-KDCATPUB-FROM2                          
149500                    PERFORM IMS-GET-AVS1-RAD-FROM                         
149600                    IF SEGMENT-FINNS                                      
149700                       MOVE JA TO GALLANDE-FINNS                          
149800                       MOVE MID-KDCATPUB-R-FROM-FOM                       
149900                             TO WS-KDCATPUB-R-AVV                         
150000                       PERFORM S50-Y2K-KDCATPUB-R                         
150100                       MOVE WS-KDCATPUB-AAAAVV                            
150200                             TO SPAR-KDCATPUB                             
150300                    ELSE                                                  
150400                       PERFORM BDA-SOEK-GALLANDE                          
150500                    END-IF                                                
150600                 END-IF                                                   
150700                 IF SEGMENT-SAKNAS                                        
150800                    MOVE MFS-NUM-FAELT-FEL TO                             
150900                         MOD-IDCATRAD-TO-ATTR                             
151000                    MOVE JA TO INDATA-FEL                                 
151100                 ELSE                                                     
151200                    MOVE MID-IDCATRAD-TO TO WS-RAD-TO                     
151300                 END-IF                                                   
151400              END-IF                                                      
151500           ELSE                                                           
151600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR            
151700              MOVE JA TO INDATA-FEL                                       
151800           END-IF                                                         
151900        END-IF                                                            
152000     ELSE                                                                 
152100        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-FROM-ATTR                   
152200                                  MOD-IDCATGRP-FROM-ATTR                  
152300                                  MOD-IDCATAVS-FROM-ATTR                  
152400        MOVE JA TO INDATA-FEL                                             
152500     END-IF                                                               
152600     .                                                                    
152700     EJECT                                                                
152800 BH-KOLLA-TILL-KOL-FINNS SECTION.                                         
152900     SKIP2                                                                
153000     MOVE MID-IDCATNR-TO     TO W-IDCATNR-TO                              
153100     MOVE MID-IDCATGRP-TO    TO W-IDCATGRP-TO                             
153200     MOVE MID-IDCATAVS-TO    TO W-IDCATAVS-TO                             
153300                                                                          
153400     PERFORM IMS-GU-AVS2-TO                                               
153500                                                                          
153600     IF SEGMENT-FINNS                                                     
153700        MOVE WS-RAD-START TO W-IDCATRAD-TO                                
153800                             W-IDCATRAD-TO2                               
153900        IF POS-UPD = NEJ                                                  
154000           IF SPAR-IFYLLT = JA                                            
154100              MOVE MID-KDCATPUB-SPAR TO W-KDCATPUB-TO2                    
154200              PERFORM IMS-GET-AVS2-RAD-TO-NASTA                           
154300           ELSE                                                           
154400              PERFORM IMS-GNP-AVS2-RAD-TO                                 
154500           END-IF                                                         
154600           IF SPAR-IFYLLT = JA                                            
154700              IF SEGMENT-FINNS                                            
154800                 IF TO-RAD-IDCATRAD = WS-RAD-START AND                    
154900                    (TO-RAD-KDCATPUB-FOM = MID-KDCATPUB-SPAR)             
155000                      MOVE MFS-NUM-FAELT-FEL                              
155100                            TO MOD-IDCATAVS-TO-ATTR                       
155200                      MOVE JA TO INDATA-FEL                               
155300                 ELSE                                                     
155400                    MOVE TO-RAD-IDCATRAD TO WS-MAX-RAD-TO                 
155500                    MOVE +1 TO WS-INTERVALL                               
155600                 END-IF                                                   
155700              ELSE                                                        
155800                 MOVE 9999 TO WS-MAX-RAD-TO                               
155900                 MOVE +10 TO WS-INTERVALL                                 
156000              END-IF                                                      
156100           ELSE                                                           
156200              IF SEGMENT-FINNS                                            
156300                 IF TO-RAD-IDCATRAD = WS-RAD-START                        
156400                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-TO-ATTR        
156500                    MOVE JA TO INDATA-FEL                                 
156600                 ELSE                                                     
156700                    IF WS-RAD-TO = +9999                                  
156800                       MOVE MFS-NUM-FAELT-FEL TO                          
156900                            MOD-IDCATRAD-TO-ATTR                          
157000                       MOVE JA TO INDATA-FEL                              
157100                    ELSE                                                  
157200                       MOVE TO-RAD-IDCATRAD TO WS-MAX-RAD-TO              
157300                       MOVE +1 TO WS-INTERVALL                            
157400                    END-IF                                                
157500                 END-IF                                                   
157600              ELSE                                                        
157700                 MOVE +9999 TO WS-MAX-RAD-TO                              
157800                 MOVE +10 TO WS-INTERVALL                                 
157900              END-IF                                                      
158000           END-IF                                                         
158100        END-IF                                                            
158200     ELSE                                                                 
158300        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-TO-ATTR                     
158400                                  MOD-IDCATGRP-TO-ATTR                    
158500                                  MOD-IDCATAVS-TO-ATTR                    
158600        MOVE JA TO INDATA-FEL                                             
158700     END-IF                                                               
158800     .                                                                    
158900     EJECT                                                                
159000 BI-KOLLA-LIKA-KOL SECTION.                                               
159100     SKIP2                                                                
159200     IF (W-IDCATNR-FROM = W-IDCATNR-TO) AND                               
159300        (W-IDCATGRP-FROM = W-IDCATGRP-TO) AND                             
159400        (W-IDCATAVS-FROM = W-IDCATAVS-TO)                                 
159500                                                                          
159600        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-TO-ATTR                     
159700                                  MOD-IDCATGRP-TO-ATTR                    
159800                                  MOD-IDCATAVS-TO-ATTR                    
159900        MOVE JA TO INDATA-FEL                                             
160000     END-IF                                                               
160100     .                                                                    
160200     EJECT                                                                
160300 BJ-KOLLA-LIKA-ILLU SECTION.                                              
160400                                                                          
160500     IF POS-UPD = NEJ                                                     
160600        CONTINUE                                                          
160700     ELSE                                                                 
160800        PERFORM IMS-GNP-AVS1-ILLU                                         
160900        IF SEGMENT-SAKNAS                                                 
161000           MOVE JA TO INDATA-FEL                                          
161100                      ILLU-FEL                                            
161200        END-IF                                                            
161300                                                                          
161400        PERFORM IMS-GNP-AVS2-ILLU                                         
161500        IF SEGMENT-SAKNAS                                                 
161600           MOVE JA TO INDATA-FEL                                          
161700                      ILLU-FEL                                            
161800        END-IF                                                            
161900                                                                          
162000        IF ILLU-FEL = NEJ                                                 
162100           IF ILLU-IDILLU = TO-ILLU-IDILLU                                
162200              CONTINUE                                                    
162300           ELSE                                                           
162400              MOVE JA TO INDATA-FEL  ILLU-FEL                             
162500           END-IF                                                         
162600        END-IF                                                            
162700     END-IF                                                               
162800     .                                                                    
162900     EJECT                                                                
163000 BK-KOLLA-RAD-START-FINNS SECTION.                                        
163100                                                                          
163200     PERFORM IMS-GU-AVS2-TO                                               
163300     IF MID-IDCATRAD-START = ALL '+'                                      
163400        PERFORM IMS-GNP-AVS2-RAD-TO                                       
163500        IF SEGMENT-FINNS                                                  
163600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-FROM-ATTR               
163700           MOVE JA TO INDATA-FEL                                          
163800        END-IF                                                            
163900     ELSE                                                                 
164000        MOVE MID-IDCATRAD-START TO W-IDCATRAD-TO                          
164100                                   W-IDCATRAD-TO2                         
164200        IF SPAR-IFYLLT = JA                                               
164300           MOVE MID-KDCATPUB-SPAR TO W-KDCATPUB-TO2                       
164400           PERFORM IMS-GET-AVS2-RAD-TO-NASTA                              
164500        ELSE                                                              
164600           PERFORM IMS-GNP-AVS2-RAD-TO                                    
164700        END-IF                                                            
164800                                                                          
164900        IF SPAR-IFYLLT = JA                                               
165000           IF SEGMENT-FINNS                                               
165100              IF TO-RAD-IDCATRAD = MID-IDCATRAD-START AND                 
165200                (TO-RAD-KDCATPUB-FOM = MID-KDCATPUB-SPAR)                 
165300                    MOVE MFS-ALFA-FAELT-FEL TO                            
165400                               MOD-IDCATRAD-START-ATTR                    
165500                 MOVE JA TO INDATA-FEL                                    
165600              ELSE                                                        
165700                 MOVE JA TO MITT-RADER                                    
165800                 MOVE TO-RAD-IDCATRAD TO WS-MITT-RAD-MAX                  
165900              END-IF                                                      
166000           END-IF                                                         
166100        ELSE                                                              
166200           PERFORM IMS-GNP-AVS2-RAD-TO                                    
166300           IF SEGMENT-FINNS                                               
166400              MOVE JA TO MITT-RADER                                       
166500              MOVE TO-RAD-IDCATRAD TO WS-MITT-RAD-MAX                     
166600              IF MID-IDCATRAD-START = TO-RAD-IDCATRAD                     
166700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-START-ATTR        
166800                 MOVE JA TO INDATA-FEL                                    
166900              END-IF                                                      
167000           END-IF                                                         
167100        END-IF                                                            
167200     END-IF                                                               
167300     .                                                                    
167400     EJECT                                                                
167500 BL-KOLLA-PUBKODER SECTION.                                               
167600                                                                          
167700     MOVE NEJ TO KDCATPUB-FOM-OK                                          
167800                 KDCATPUB-TOM-OK                                          
167900     MOVE MID-IDCATNR-TO TO W-IDCATNR-KAT                                 
168000                                                                          
168100     IF MID-KDCATPUB-R-TO-FOM = ALL '+' AND                               
168200        MID-KDCATPUB-R-TO-TOM = ALL '+'                                   
168300        MOVE JA TO KDCATPUB-FOM-OK                                        
168400                   KDCATPUB-TOM-OK                                        
168500     ELSE                                                                 
168600        PERFORM IMS-GU-KAT                                                
168700        IF SEGMENT-FINNS                                                  
168800           IF MID-KDCATPUB-R-TO-FOM = ALL '+'                             
168900              MOVE JA TO KDCATPUB-FOM-OK                                  
169000           ELSE                                                           
169100              PERFORM IMS-GNP-KATM11                                      
169200              PERFORM UNTIL SEGMENT-SAKNAS                                
169300                 MOVE +1 TO PER-IX                                        
169400                 PERFORM UNTIL PER-IX > PER-IX-MAX                        
169500                    MOVE MID-KDCATPUB-R-TO-FOM                            
169600                             TO WS-KDCATPUB-R-AVV                         
169700                    PERFORM S50-Y2K-KDCATPUB-R                            
169800                    IF TAB-KDCATPUB-FOM(PER-IX) =                         
169900                       WS-KDCATPUB-AAAAVV                                 
170000                       MOVE JA TO KDCATPUB-FOM-OK                         
170100                    END-IF                                                
170200                    ADD +1 TO PER-IX                                      
170300                 END-PERFORM                                              
170400                 PERFORM IMS-GNP-KATM11                                   
170500              END-PERFORM                                                 
170600           END-IF                                                         
170700           IF KDCATPUB-FOM-OK = NEJ                                       
170800            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TO-FOM-ATTR         
170900              MOVE JA TO INDATA-FEL                                       
171000           END-IF                                                         
171100                                                                          
171200           IF MID-KDCATPUB-R-TO-TOM = ALL '+'                             
171300              MOVE JA TO KDCATPUB-TOM-OK                                  
171400           ELSE                                                           
171500              PERFORM IMS-GNP-KATM11-FIRST                                
171600              PERFORM UNTIL SEGMENT-SAKNAS                                
171700                 MOVE +1 TO PER-IX                                        
171800                 PERFORM UNTIL PER-IX > PER-IX-MAX                        
171900                    MOVE MID-KDCATPUB-R-TO-TOM                            
172000                             TO WS-KDCATPUB-R-AVV                         
172100                    PERFORM S50-Y2K-KDCATPUB-R                            
172200                    IF TAB-KDCATPUB-TOM(PER-IX) =                         
172300                       WS-KDCATPUB-AAAAVV                                 
172400                       MOVE JA TO KDCATPUB-TOM-OK                         
172500                    END-IF                                                
172600                    ADD +1 TO PER-IX                                      
172700                 END-PERFORM                                              
172800                 PERFORM IMS-GNP-KATM11                                   
172900              END-PERFORM                                                 
173000           END-IF                                                         
173100           IF KDCATPUB-TOM-OK = NEJ                                       
173200            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TO-TOM-ATTR         
173300              MOVE JA TO INDATA-FEL                                       
173400           END-IF                                                         
173500        ELSE                                                              
173600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TO-FOM-ATTR          
173700                                      MOD-KDCATPUB-R-TO-TOM-ATTR          
173800           MOVE JA TO INDATA-FEL                                          
173900        END-IF                                                            
174000     END-IF                                                               
174100     .                                                                    
174200     EJECT                                                                
174300 C-LAANA-RADER SECTION.                                                   
174400                                                                          
174500******************************************************************        
174600* VID FRÅN-PUBKOD EJ IFYLLD LÅNAS ALLA ARTIKEL-RADER                      
174700* VID FRÅN-PUBKOD IFYLLD    LÅNAS GÄLLANDE ARTIKEL-RADER                  
174800******************************************************************        
174900                                                                          
175000     PERFORM CA-ISRT-RUBRIKER                                             
175100                                                                          
175200     MOVE +100 TO WS-MAX                                                  
175300     MOVE +1 TO WS-RAKNARE                                                
175400                                                                          
175500     PERFORM IMS-GU-AVS1-FROM                                             
175600     PERFORM IMS-GU-AVS2-TO                                               
175700                                                                          
175800     MOVE WS-RAD-FROM TO W-IDCATRAD-FROM-MIN-F                            
175900     MOVE WS-RAD-TO   TO W-IDCATRAD-FROM-MAX-F                            
176000     MOVE LOW-VALUE   TO W-KDCATPUB-FROM-MIN-F                            
176100     MOVE HIGH-VALUE  TO W-KDCATPUB-FROM-MAX-F                            
176200     IF MID-IDCATRAD-SPAR > ZERO                                          
176300        MOVE MID-KDCATPUB-SPAR TO W-KDCATPUB-FROM-MIN-F                   
176400     END-IF                                                               
176500     IF SPAR-IFYLLT = JA                                                  
176600        MOVE MID-KDCATPUB-SPAR TO W-KDCATPUB-FROM-MIN-F                   
176700     END-IF                                                               
176800     IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                                 
176900        PERFORM IMS-GET-AVS1-RAD                                          
177000     ELSE                                                                 
177100        PERFORM S99-SOEK-AKTUELL-RAD                                      
177200     END-IF                                                               
177300                                                                          
177400     IF MITT-RADER = JA                                                   
177500        PERFORM CB-LAANA-MITT                                             
177600     ELSE                                                                 
177700        PERFORM CC-LAANA-VANLIGA                                          
177800     END-IF                                                               
177900                                                                          
178000     IF WS-RAKNARE = WS-MAX                                               
178100        IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                              
178200           IF NY-RAD-FINNS = JA                                           
178300              PERFORM S30-FLYTTA-TO-PROG                                  
178400              PERFORM IMS-INSERT-ALT-MSG                                  
178500           ELSE                                                           
178600              MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                       
178700              PERFORM G-VISA-BILD-IGEN                                    
178800              PERFORM X-GRUND-FORMAT                                      
178900              MOVE MAX-MOD-LAENGD TO MSG-KVLL                             
179000              PERFORM IMS-INSERT-MSG                                      
179100           END-IF                                                         
179200        ELSE                                                              
179300           IF AKTUELL-RAD = JA                                            
179400              PERFORM S30-FLYTTA-TO-PROG                                  
179500              PERFORM IMS-INSERT-ALT-MSG                                  
179600           ELSE                                                           
179700              MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                       
179800              PERFORM G-VISA-BILD-IGEN                                    
179900              PERFORM X-GRUND-FORMAT                                      
180000              MOVE MAX-MOD-LAENGD TO MSG-KVLL                             
180100              PERFORM IMS-INSERT-MSG                                      
180200           END-IF                                                         
180300        END-IF                                                            
180400     ELSE                                                                 
180500        IF WS-RAKNARE < WS-MAX                                            
180600           IF INDATA-FEL = JA                                             
180700              CONTINUE                                                    
180800           ELSE                                                           
180900              IF WS-RAKNARE = 1                                           
181000                 MOVE MED-2(SPRAAK-IX) TO MOD-TEMFSINF                    
181100              ELSE                                                        
181200                 MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                    
181300              END-IF                                                      
181400           END-IF                                                         
181500           PERFORM G-VISA-BILD-IGEN                                       
181600           PERFORM X-GRUND-FORMAT                                         
181700           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
181800           PERFORM IMS-INSERT-MSG                                         
181900        END-IF                                                            
182000     END-IF                                                               
182100     .                                                                    
182200     EJECT                                                                
182300 CA-ISRT-RUBRIKER SECTION.                                                
182400                                                                          
182500******************************************************************        
182600* OM TILL-AVSNITTET REDAN HAR RUBRIKRADER INSERTAS INGA NYA               
182700* RUBRIKER. DETTA FÖR ATT UNDVIKA KONTROLL OCH UPPDATERING AV             
182800* START OCH STOPPDATUM AV BEF. RUBRIKRADER. DETTA GÖRS I 1514.            
182900*                                                                         
183000* VID FRÅN-PUBKOD EJ IFYLLD LÅNAS ALLA RUBRIKRADER                        
183100* VID FRÅN-PUBKOD IFYLLD    LÅNAS GÄLLANDE RUBRIKRADER                    
183200******************************************************************        
183300                                                                          
183400     PERFORM IMS-GU-AVS1-FROM                                             
183500     PERFORM IMS-GU-AVS2-TO                                               
183600                                                                          
183700     MOVE +1  TO W-IDCATRAD-FROM-MIN                                      
183800                 W-IDCATRAD-FROM2-MIN                                     
183900     MOVE +19 TO W-IDCATRAD-FROM-MAX                                      
184000                 W-IDCATRAD-FROM2-MAX                                     
184100     MOVE NEJ TO GALLANDE-FINNS                                           
184200                                                                          
184300     PERFORM IMS-GET-AVS2-RAD                                             
184400     IF SEGMENT-FINNS                                                     
184500        CONTINUE                                                          
184600     ELSE                                                                 
184700        IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                              
184800           PERFORM IMS-GHNP-AVS1-RAD                                      
184900           PERFORM UNTIL STATUS-WS NOT = SPACE                            
185000              PERFORM CAA-ISRT-RUB-RAD                                    
185100              PERFORM IMS-GHNP-AVS1-RAD                                   
185200           END-PERFORM                                                    
185300        ELSE                                                              
185400           PERFORM CAB-SOEK-GALLANDE-RUBRIKRADER                          
185500           IF GALLANDE-FINNS = JA                                         
185600              PERFORM IMS-GU-AVS1-FROM                                    
185700              MOVE SPAR-KDCATPUB TO W-KDCATPUB-FROM2                      
185800              MOVE +1 TO W-IDCATRAD-FROM2                                 
185900              PERFORM IMS-GET-AVS1-RAD-FROM                               
186000              PERFORM UNTIL W-IDCATRAD-FROM2 > 14                         
186100                 IF SEGMENT-FINNS                                         
186200                    PERFORM CAA-ISRT-RUB-RAD                              
186300                 END-IF                                                   
186400                 ADD +1 TO W-IDCATRAD-FROM2                               
186500                 PERFORM IMS-GET-AVS1-RAD-FROM                            
186600              END-PERFORM                                                 
186700           END-IF                                                         
186800        END-IF                                                            
186900     END-IF                                                               
187000     .                                                                    
187100     EJECT                                                                
187200 CAA-ISRT-RUB-RAD SECTION.                                                
187300                                                                          
187400******   VID INSERTERNA GODKÄNNS 'II' SOM STATUS-KOD                      
187500                                                                          
187600     IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                                 
187700        MOVE RAD-IDCATRAD     TO W-IDCATRAD-FROM2                         
187800        MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                         
187900                                 W-KDCATPUB-TO2                           
188000     ELSE                                                                 
188100        MOVE RAD-IDCATRAD  TO W-IDCATRAD-FROM2                            
188200        MOVE SPAR-KDCATPUB TO W-KDCATPUB-FROM2                            
188300                              W-KDCATPUB-TO2                              
188400        IF MID-KDCATPUB-R-TO-FOM NOT = ALL '+'                            
188500           MOVE WS-KDCATPUB-TO-FOM TO RAD-KDCATPUB-FOM                    
188600                                      W-KDCATPUB-TO2                      
188700        END-IF                                                            
188800        IF MID-KDCATPUB-R-TO-TOM NOT = ALL '+'                            
188900           MOVE WS-KDCATPUB-TO-TOM TO RAD-KDCATPUB-TOM                    
189000        END-IF                                                            
189100     END-IF                                                               
189200     MOVE RAD-IDCATRAD TO W-IDCATRAD-TO                                   
189300                          W-IDCATRAD-TO2                                  
189400     MOVE DAGENS-DATUM TO RAD-TIUPPDAT                                    
189500     MOVE 'L'          TO RAD-KDRADST                                     
189600     PERFORM IMS-ISRT-AVS2-RUBRIKER                                       
189700                                                                          
189800     PERFORM IMS-GET-AVS1-TEXT                                            
189900                                                                          
190000     IF SEGMENT-FINNS AND TEXT-TEKOL NOT = SPACE                          
190100        PERFORM IMS-ISRT-AVS2-RUB-TEXT-IO1                                
190200     END-IF                                                               
190300     .                                                                    
190400     EJECT                                                                
190500 CAB-SOEK-GALLANDE-RUBRIKRADER SECTION.                                   
190600                                                                          
190700     MOVE LOW-VALUE TO SPAR-KDCATPUB                                      
190800                                                                          
190900     PERFORM IMS-GHNP-AVS1-RAD                                            
191000     PERFORM UNTIL SEGMENT-SAKNAS                                         
191100        MOVE MID-KDCATPUB-R-FROM-FOM                                      
191200                             TO WS-KDCATPUB-R-AVV                         
191300        PERFORM S50-Y2K-KDCATPUB-R                                        
191400        IF RAD-KDCATPUB-FOM <= WS-KDCATPUB-AAAAVV                         
191500           IF RAD-KDCATPUB-TOM >= WS-KDCATPUB-AAAAVV                      
191600              IF RAD-KDCATPUB-FOM >= SPAR-KDCATPUB                        
191700                 MOVE RAD-KDCATPUB-FOM TO SPAR-KDCATPUB                   
191800                 MOVE JA TO GALLANDE-FINNS                                
191900              END-IF                                                      
192000           END-IF                                                         
192100        END-IF                                                            
192200        PERFORM IMS-GHNP-AVS1-RAD                                         
192300     END-PERFORM                                                          
192400     .                                                                    
192500     EJECT                                                                
192600 CB-LAANA-MITT SECTION.                                                   
192700     SKIP2                                                                
192800     PERFORM UNTIL STATUS-WS NOT = SPACE                                  
192900     OR INDATA-FEL = JA                                                   
193000        IF RAD-IDCATRAD NOT > WS-RAD-TO                                   
193100           IF WS-RAD-START < WS-MITT-RAD-MAX                              
193200              PERFORM S98-KOLLA-TIDSINTERVALL                             
193300              IF NY-PUB-OK = JA                                           
193400                 PERFORM S07-SPARA-RAD                                    
193500                 PERFORM S01-FLYTTA-STATUS-DATUM                          
193600                 PERFORM IMS-ISRT-AVS2-RAD                                
193700                                                                          
193800                 MOVE RAD-IDCATRAD     TO W-IDCATRAD-FROM2                
193900                 MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                
194000                 PERFORM CD-NYUPPLAGG-RAD                                 
194100              END-IF                                                      
194200              PERFORM S06-ADDERA-RAD-START                                
194300           ELSE                                                           
194400              MOVE JA TO INDATA-FEL                                       
194500              PERFORM S25-PLATS-FINNS-EJ                                  
194600           END-IF                                                         
194700        ELSE                                                              
194800           MOVE JA TO INDATA-FEL                                          
194900           IF WS-RAKNARE = 1                                              
195000              MOVE MED-2 (SPRAAK-IX) TO MOD-TEMFSINF                      
195100           ELSE                                                           
195200              MOVE MED-1 (SPRAAK-IX) TO MOD-TEMFSINF                      
195300           END-IF                                                         
195400        END-IF                                                            
195500     END-PERFORM                                                          
195600     .                                                                    
195700     EJECT                                                                
195800 CC-LAANA-VANLIGA SECTION.                                                
195900                                                                          
196000     PERFORM UNTIL STATUS-WS NOT = SPACE                                  
196100     OR (WS-RAKNARE NOT < WS-MAX)                                         
196200     OR (INDATA-FEL = JA)                                                 
196300                                                                          
196400        IF WS-RAD-START < WS-MAX-RAD-TO                                   
196500           PERFORM S98-KOLLA-TIDSINTERVALL                                
196600           IF NY-PUB-OK = JA                                              
196700              PERFORM S07-SPARA-RAD                                       
196800              PERFORM S01-FLYTTA-STATUS-DATUM                             
196900              PERFORM IMS-ISRT-AVS2-RAD                                   
197000                                                                          
197100              MOVE RAD-IDCATRAD     TO W-IDCATRAD-FROM2                   
197200              MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                   
197300              PERFORM CD-NYUPPLAGG-RAD                                    
197400           END-IF                                                         
197500           PERFORM S06-ADDERA-RAD-START                                   
197600        ELSE                                                              
197700           MOVE JA TO INDATA-FEL                                          
197800           PERFORM S25-PLATS-FINNS-EJ                                     
197900        END-IF                                                            
198000     END-PERFORM                                                          
198100     .                                                                    
198200     EJECT                                                                
198300 CD-NYUPPLAGG-RAD SECTION.                                                
198400     SKIP2                                                                
198500     PERFORM CDA-AVS-ART                                                  
198600     PERFORM S20-AVS-UPPDATERING                                          
198700     PERFORM S21-AVS-UPD-HAEN                                             
198800     .                                                                    
198900     EJECT                                                                
199000 CDA-AVS-ART SECTION.                                                     
199100     SKIP2                                                                
199200     PERFORM IMS-GET-AVS1-ART-FIRST                                       
199300     IF SEGMENT-FINNS                                                     
199400        PERFORM IMS-ISRT-AVS2-ART                                         
199500     END-IF                                                               
199600     .                                                                    
199700     EJECT                                                                
199800 D-LAANA-KOLUMN-POS SECTION.                                              
199900                                                                          
200000     MOVE NEJ TO INDATA-FEL                                               
200100                 UPPDAT-FEL                                               
200200                 RAD-FINNS                                                
200300                 BAS-FROM-SLUT                                            
200400                 SPAR-ART-FINNS                                           
200500     MOVE JA TO TO-BAS-FINNS                                              
200600     MOVE WS-IDCATPOS-FROM TO W-IDCATPOS-FROM                             
200700                              W-IDCATPOS-TO                               
200800***  MOVE WS-IDCATPOS-TO   TO W-IDCATPOS-TO                               
200900     MOVE SPACE            TO NAESTA-TO-POS                               
201000     PERFORM S04-RAETT-FROM-KOLUMN                                        
201100     PERFORM S05-RAETT-TO-KOLUMN                                          
201200                                                                          
201300     PERFORM DA-ISRT-TEKOL                                                
201400                                                                          
201500     MOVE +20 TO W-IDCATRAD-FROM                                          
201600                 W-IDCATRAD-TO                                            
201700     MOVE +9999 TO NAESTA-TO-RAD                                          
201800                                                                          
201900     PERFORM IMS-GU-AVS1-FROM                                             
202000     PERFORM IMS-GU-AVS2-TO                                               
202100                                                                          
202200     IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                                 
202300        PERFORM IMS-GNP-AVS1-RAD-ART-FROM                                 
202400     ELSE                                                                 
202500        PERFORM S97-SOEK-AKTUELL-RAD-POS                                  
202600     END-IF                                                               
202700                                                                          
202800     IF SEGMENT-SAKNAS                                                    
202900        MOVE MED-3(SPRAAK-IX) TO MOD-TEMFSINF                             
203000        PERFORM G-VISA-BILD-IGEN                                          
203100     ELSE                                                                 
203200        MOVE JA TO RAD-FINNS                                              
203300        MOVE AVS1-IDCATRAD     TO W-IDCATRAD-FROM                         
203400                                  W-IDCATRAD-FROM2                        
203500        MOVE AVS1-KDCATPUB-FOM TO W-KDCATPUB-FROM                         
203600                                  W-KDCATPUB-FROM2                        
203700        PERFORM DC-NOLLSTALL-TABELL                                       
203800        PERFORM DD-LAS-RADER                                              
203900                                                                          
204000        PERFORM UNTIL (RAD-FINNS = NEJ)                                   
204100           OR (INDATA-FEL = JA)                                           
204200           OR (UPPDAT-FEL = JA)                                           
204300           OR (NASTA-POS-SAKNAS = JA)                                     
204400                                                                          
204500           PERFORM DE-UPPDATERA-TILL                                      
204600           IF BAS-FROM-SLUT = NEJ                                         
204700              IF INDATA-FEL = NEJ                                         
204800                 PERFORM DC-NOLLSTALL-TABELL                              
204900***** KONTROLL POS                                                        
205000                 MOVE ART-IDCATPOS TO KOLL-POSX                           
205100                 PERFORM S95-KOLLA-POS                                    
205200                 MOVE TEST-POS-NUM  TO TEST-POS-NUM1                      
205300                 MOVE TEST-POS-ALFA TO TEST-POS-ALFA1                     
205400                                                                          
205500                 MOVE WS-IDCATPOS-TO TO KOLL-POSX                         
205600                 PERFORM S95-KOLLA-POS                                    
205700                 MOVE TEST-POS-NUM  TO TEST-POS-NUM2                      
205800                 MOVE TEST-POS-ALFA TO TEST-POS-ALFA2                     
205900                                                                          
206000                 MOVE W-IDCATPOS-TO TO KOLL-POSX                          
206100                 PERFORM S95-KOLLA-POS                                    
206200                 MOVE TEST-POS-NUM  TO TEST-POS-NUM3                      
206300                 MOVE TEST-POS-ALFA TO TEST-POS-ALFA3                     
206400***** KONTROLL POS                                                        
206500                                                                          
206600*****            IF ART-IDCATPOS > WS-IDCATPOS-TO                         
206700*****               OR ART-IDCATPOS < W-IDCATPOS-TO                       
206800                 IF TEST-POS-NUM1 > TEST-POS-NUM2                         
206900                    OR TEST-POS-NUM1 < TEST-POS-NUM3                      
207000                    MOVE NEJ TO RAD-FINNS                                 
207100                 ELSE                                                     
207200                    IF TEST-POS-NUM1 = TEST-POS-NUM2                      
207300                       IF TEST-POS-ALFA1 > TEST-POS-ALFA2                 
207400                        OR TEST-POS-ALFA1 < TEST-POS-ALFA3                
207500                          MOVE NEJ TO RAD-FINNS                           
207600                       ELSE                                               
207700                          PERFORM DD-LAS-RADER                            
207800                       END-IF                                             
207900                    ELSE                                                  
208000                       PERFORM DD-LAS-RADER                               
208100                    END-IF                                                
208200                 END-IF                                                   
208300              END-IF                                                      
208400           END-IF                                                         
208500        END-PERFORM                                                       
208600                                                                          
208700        IF UPPDAT-FEL = NEJ                                               
208800           IF BAS-FROM-SLUT = JA                                          
208900              MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                       
209000           ELSE                                                           
209100              IF INDATA-FEL = JA                                          
209200                 MOVE SPAR-IDCATPOS TO POS-IDCATPOS                       
209300                 MOVE WS-POS-TEXT TO POS-TEXT                             
209400                 MOVE WS-POS-MEDDELANDE TO MOD-TEMFSINF                   
209500              ELSE                                                        
209600                 MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                    
209700                 IF RAD-FINNS = NEJ                                       
209800***** KONTROLL POS                                                        
209900                    MOVE ART-IDCATPOS TO KOLL-POSX                        
210000                    PERFORM S95-KOLLA-POS                                 
210100                    MOVE TEST-POS-NUM  TO TEST-POS-NUM1                   
210200                    MOVE TEST-POS-ALFA TO TEST-POS-ALFA1                  
210300                                                                          
210400                    MOVE WS-IDCATPOS-TO TO KOLL-POSX                      
210500                    PERFORM S95-KOLLA-POS                                 
210600                    MOVE TEST-POS-NUM  TO TEST-POS-NUM2                   
210700                    MOVE TEST-POS-ALFA TO TEST-POS-ALFA2                  
210800                                                                          
210900********            IF ART-IDCATPOS > WS-IDCATPOS-TO                      
211000                    IF TEST-POS-NUM1 > TEST-POS-NUM2                      
211100                       MOVE NEJ TO RAD-FINNS                              
211200                    ELSE                                                  
211300                       IF (TEST-POS-NUM1 = TEST-POS-NUM2)                 
211400                          AND (TEST-POS-ALFA1 > TEST-POS-ALFA2)           
211500                             MOVE NEJ TO RAD-FINNS                        
211600                       ELSE                                               
211700                          PERFORM DF-KOLLA-SISTA-RAD                      
211800                       END-IF                                             
211900                    END-IF                                                
212000                 END-IF                                                   
212100              END-IF                                                      
212200           END-IF                                                         
212300        END-IF                                                            
212400        PERFORM G-VISA-BILD-IGEN                                          
212500        PERFORM X-GRUND-FORMAT                                            
212600     END-IF                                                               
212700                                                                          
212800     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
212900     PERFORM IMS-INSERT-MSG                                               
213000     .                                                                    
213100     EJECT                                                                
213200 DA-ISRT-TEKOL SECTION.                                                   
213300******************************************************************        
213400* OM TILL-AVSNITTET REDAN HAR RUBRIKRADER INSERTAS INGA NYA               
213500* RUBRIKER. DETTA FÖR ATT UNDVIKA KONTROLL OCH UPPDATERING AV             
213600* START OCH STOPPDATUM AV BEF. RUBRIKRADER. DETTA GÖRS I 1514.            
213700*                                                                         
213800* VID FRÅN-PUBKOD EJ IFYLLD LÅNAS ALLA PUBKODER                           
213900* VID FRÅN-PUBKOD IFYLLD    LÅNAS GÄLLANDE PUBKOD                         
214000******************************************************************        
214100                                                                          
214200     PERFORM IMS-GU-AVS1-FROM                                             
214300     PERFORM IMS-GU-AVS2-TO                                               
214400                                                                          
214500     MOVE +1  TO W-IDCATRAD-FROM2-MIN                                     
214600     MOVE +19 TO W-IDCATRAD-FROM2-MAX                                     
214700     PERFORM IMS-GET-AVS2-RAD                                             
214800     IF SEGMENT-FINNS                                                     
214900        CONTINUE                                                          
215000     ELSE                                                                 
215100        IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                              
215200           PERFORM IMS-GNP-AVS1-RAD-FROM                                  
215300           PERFORM UNTIL SEGMENT-SAKNAS                                   
215400              MOVE W-IDCATRAD-TO2 TO TO-RAD-IDCATRAD                      
215500              MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                   
215600                                       W-KDCATPUB-TO2                     
215700                                       TO-RAD-KDCATPUB-FOM                
215800              MOVE RAD-KDCATPUB-TOM TO TO-RAD-KDCATPUB-TOM                
215900              MOVE SPACE            TO TO-RAD-IDUSER                      
216000              MOVE DAGENS-DATUM     TO TO-RAD-TIUPPDAT                    
216100              MOVE 'L'              TO TO-RAD-KDRADST                     
216200              PERFORM IMS-GET-AVS1-TEXT                                   
216300              IF SEGMENT-FINNS AND TEXT-TEKOL NOT = SPACE                 
216400                 MOVE W-IDCATRAD-TO TO TO-RAD-IDCATRAD                    
216500                 PERFORM IMS-ISRT-AVS2-RAD                                
216600                 PERFORM IMS-ISRT-AVS2-TEXT-IO1                           
216700              END-IF                                                      
216800              PERFORM IMS-GNP-AVS1-RAD-FROM                               
216900           END-PERFORM                                                    
217000        ELSE                                                              
217100           PERFORM BDA-SOEK-GALLANDE                                      
217200           IF SEGMENT-FINNS                                               
217300              MOVE SPAR-KDCATPUB TO W-KDCATPUB-FROM2                      
217400              PERFORM IMS-GET-AVS1-RAD-FROM                               
217500              MOVE W-IDCATRAD-TO2   TO TO-RAD-IDCATRAD                    
217600              MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                   
217700              MOVE WS-KDCATPUB-TO-FOM TO W-KDCATPUB-TO2                   
217800                                         TO-RAD-KDCATPUB-FOM              
217900              MOVE WS-KDCATPUB-TO-TOM TO TO-RAD-KDCATPUB-TOM              
218000              MOVE SPACE        TO TO-RAD-IDUSER                          
218100              MOVE DAGENS-DATUM TO TO-RAD-TIUPPDAT                        
218200              MOVE 'L'          TO TO-RAD-KDRADST                         
218300              PERFORM IMS-GET-AVS1-TEXT                                   
218400              IF SEGMENT-FINNS AND TEXT-TEKOL NOT = SPACE                 
218500                 PERFORM IMS-ISRT-AVS2-RAD                                
218600                 PERFORM IMS-ISRT-AVS2-TEXT-IO1                           
218700              END-IF                                                      
218800           END-IF                                                         
218900        END-IF                                                            
219000     END-IF                                                               
219100     .                                                                    
219200     EJECT                                                                
219300 DC-NOLLSTALL-TABELL SECTION.                                             
219400     SKIP2                                                                
219500     MOVE +1 TO TAB-IX                                                    
219600     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
219700        MOVE ZERO   TO SOEK-IDCATRAD(TAB-IX)                              
219800                       SOEK-IDARTNR(TAB-IX)                               
219900                       SOEK-KVPUNKT(TAB-IX)                               
220000                       SOEK-IDTTEXNR(TAB-IX)                              
220100                       SOEK-KDHOM(TAB-IX)                                 
220200                       SOEK-IDRUBNR(TAB-IX, 1)                            
220300                       SOEK-IDRUBNR(TAB-IX, 2)                            
220400                       SOEK-IDRUBNR(TAB-IX, 3)                            
220500                       SOEK-IDFOTNR(TAB-IX, 1)                            
220600                       SOEK-IDFOTNR(TAB-IX, 2)                            
220700                       SOEK-IDFOTNR(TAB-IX, 3)                            
220800        MOVE SPACE  TO SOEK-KDPS(TAB-IX)                                  
220900                       SOEK-TEKATANM(TAB-IX)                              
221000                       SOEK-BEART(TAB-IX)                                 
221100                       SOEK-KDCATPUB-FOM(TAB-IX)                          
221200                       SOEK-KDCATPUB-TOM(TAB-IX)                          
221300        MOVE ZERO   TO SOEK-IDCATGRP(TAB-IX)                              
221400                       SOEK-IDCATAVS(TAB-IX)                              
221500                       SOEK-IDCATRAD-H(TAB-IX)                            
221600        MOVE SPACE  TO SOEK-KDFBX(TAB-IX)                                 
221700                       SOEK-FLRUBTYP(TAB-IX)                              
221800                       SOEK-KVKOL(TAB-IX)                                 
221900                       SOEK-TENOTE(TAB-IX)                                
222000        ADD +1 TO TAB-IX                                                  
222100     END-PERFORM                                                          
222200     .                                                                    
222300     EJECT                                                                
222400 DD-LAS-RADER SECTION.                                                    
222500     SKIP2                                                                
222600     MOVE SPACE TO BAS-FROM-IDCATPOS                                      
222700                   SPAR-IDCATPOS                                          
222800                   SISTA-FROM-POS                                         
222900                                                                          
223000     MOVE ART-IDCATPOS TO SPAR-IDCATPOS                                   
223100     MOVE NEJ TO LINE-BEFORE                                              
223200                                                                          
223300     IF ART-KVKOL(KOL-IX) = SPACE                                         
223400                                                                          
223500        IF ART-KDFBX = 'F'                                                
223600           PERFORM DDA-LAS-FIRST-VILLKOR                                  
223700        ELSE                                                              
223800           IF ART-KVKOL(1) = SPACE AND                                    
223900              ART-KVKOL(2) = SPACE AND                                    
224000              ART-KVKOL(3) = SPACE AND                                    
224100              ART-KVKOL(4) = SPACE AND                                    
224200              ART-KVKOL(5) = SPACE                                        
224300              PERFORM Q-UPPDATERA-TABELL                                  
224400           ELSE                                                           
224500              IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                        
224600                 PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM                       
224700              ELSE                                                        
224800                 PERFORM S96-SOEK-AKTUELL-RAD                             
224900              END-IF                                                      
225000              PERFORM S03-FLYTTA-FROM-RAD                                 
225100                                                                          
225200              PERFORM UNTIL (STATUS-WS NOT = SPACE)                       
225300                 OR (BAS-FROM-IDCATPOS NOT = SPACE)                       
225400                 OR (NASTA-POS-SAKNAS = JA)                               
225500                 PERFORM DDA-LAS-FIRST-VILLKOR                            
225600                                                                          
225700                 IF UPPDAT-FEL = NEJ                                      
225800                    IF NASTA-POS-SAKNAS = JA                              
225900                       CONTINUE                                           
226000                    ELSE                                                  
226100                       IF BAS-FROM-IDCATPOS = SPACE                       
226200                          IF MID-KDCATPUB-R-FROM-FOM = ALL '+'            
226300                             PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM           
226400                          ELSE                                            
226500                             PERFORM S96-SOEK-AKTUELL-RAD                 
226600                          END-IF                                          
226700                          PERFORM S03-FLYTTA-FROM-RAD                     
226800                       END-IF                                             
226900                    END-IF                                                
227000                 END-IF                                                   
227100              END-PERFORM                                                 
227200                                                                          
227300              IF BAS-FROM-IDCATPOS = SPACE                                
227400                 MOVE NEJ TO RAD-FINNS                                    
227500              END-IF                                                      
227600           END-IF                                                         
227700        END-IF                                                            
227800     ELSE                                                                 
227900        PERFORM Q-UPPDATERA-TABELL                                        
228000     END-IF                                                               
228100     .                                                                    
228200     EJECT                                                                
228300 DDA-LAS-FIRST-VILLKOR SECTION.                                           
228400                                                                          
228500     MOVE NEJ TO NASTA-POS-SAKNAS                                         
228600     MOVE RAD-IDCATRAD     TO W-IDCATRAD-FROM2                            
228700     MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                            
228800     PERFORM IMS-GET-AVS1-ART                                             
228900     IF SEGMENT-FINNS                                                     
229000        IF ART-IDCATPOS NOT = SPACE                                       
229100***** KONTROLL POS                                                        
229200           MOVE ART-IDCATPOS TO KOLL-POSX                                 
229300           PERFORM S95-KOLLA-POS                                          
229400           MOVE TEST-POS-NUM  TO TEST-POS-NUM1                            
229500           MOVE TEST-POS-ALFA TO TEST-POS-ALFA1                           
229600                                                                          
229700           MOVE WS-IDCATPOS-TO TO KOLL-POSX                               
229800           PERFORM S95-KOLLA-POS                                          
229900           MOVE TEST-POS-NUM  TO TEST-POS-NUM2                            
230000           MOVE TEST-POS-ALFA TO TEST-POS-ALFA2                           
230100                                                                          
230200           MOVE W-IDCATPOS-TO TO KOLL-POSX                                
230300           PERFORM S95-KOLLA-POS                                          
230400           MOVE TEST-POS-NUM  TO TEST-POS-NUM3                            
230500           MOVE TEST-POS-ALFA TO TEST-POS-ALFA3                           
230600***** KONTROLL POS                                                        
230700                                                                          
230800*****      IF ART-IDCATPOS > WS-IDCATPOS-TO                               
230900*****           OR ART-IDCATPOS < W-IDCATPOS-TO                           
231000           IF TEST-POS-NUM1 > TEST-POS-NUM2                               
231100                OR TEST-POS-NUM1 < TEST-POS-NUM3                          
231200              MOVE JA TO NASTA-POS-SAKNAS                                 
231300           ELSE                                                           
231400              MOVE NEJ TO LINE-BEFORE                                     
231500              MOVE ART-IDCATPOS TO SPAR-IDCATPOS                          
231600              IF ART-KVKOL(KOL-IX) = SPACE                                
231700                 PERFORM DDAA-KOLLA-TRAEFF                                
231800              ELSE                                                        
231900                 PERFORM Q-UPPDATERA-TABELL                               
232000                 MOVE '999' TO BAS-FROM-IDCATPOS                          
232100              END-IF                                                      
232200           END-IF                                                         
232300        ELSE                                                              
232400           IF ART-KVKOL(KOL-IX) = SPACE                                   
232500              PERFORM DDAA-KOLLA-TRAEFF                                   
232600*          ELSE                                                           
232700*             IF SPAR-IDCATPOS NOT = SPACE                                
232800*                PERFORM Q-UPPDATERA-TABELL                               
232900*                MOVE '999' TO BAS-FROM-IDCATPOS                          
233000*             END-IF                                                      
233100           END-IF                                                         
233200        END-IF                                                            
233300     END-IF                                                               
233400     .                                                                    
233500     EJECT                                                                
233600 DDAA-KOLLA-TRAEFF SECTION.                                               
233700     SKIP2                                                                
233800     IF ART-KVKOL(1) = SPACE AND                                          
233900        ART-KVKOL(2) = SPACE AND                                          
234000        ART-KVKOL(3) = SPACE AND                                          
234100        ART-KVKOL(4) = SPACE AND                                          
234200        ART-KVKOL(5) = SPACE                                              
234300        IF ART-KDFBX = 'F'                                                
234400           CONTINUE                                                       
234500        ELSE                                                              
234600           PERFORM Q-UPPDATERA-TABELL                                     
234700           MOVE '999' TO BAS-FROM-IDCATPOS                                
234800        END-IF                                                            
234900     END-IF                                                               
235000     .                                                                    
235100     EJECT                                                                
235200 DE-UPPDATERA-TILL SECTION.                                               
235300     SKIP2                                                                
235400     MOVE SPAR-IDCATPOS TO W-IDCATPOS-TO                                  
235500     MOVE +20 TO SPAR-W-IDCATRAD-TO                                       
235600                                                                          
235700     PERFORM S08-BLANKA-IN-SOEK                                           
235800                                                                          
235900     IF SPAR-ART-FINNS = JA                                               
236000        PERFORM S10-FLYTTA-ART2                                           
236100        MOVE NEJ TO SPAR-ART-FINNS                                        
236200     END-IF                                                               
236300                                                                          
236400     IF TO-BAS-FINNS = JA                                                 
236500        IF NAESTA-TO-POS = SPAR-IDCATPOS                                  
236600           PERFORM DEA-KOLLA-UPPDAT                                       
236700        ELSE                                                              
236800           MOVE NAESTA-TO-POS TO KOLL-POSX                                
236900           PERFORM S95-KOLLA-POS                                          
237000           MOVE TEST-POS-NUM  TO TEST-POS-NUM1                            
237100           MOVE TEST-POS-ALFA TO TEST-POS-ALFA1                           
237200                                                                          
237300           MOVE SPAR-IDCATPOS TO KOLL-POSX                                
237400           PERFORM S95-KOLLA-POS                                          
237500           MOVE TEST-POS-NUM  TO TEST-POS-NUM2                            
237600           MOVE TEST-POS-ALFA TO TEST-POS-ALFA2                           
237700                                                                          
237800******     IF NAESTA-TO-POS < SPAR-IDCATPOS                               
237900           IF (TEST-POS-NUM1 < TEST-POS-NUM2) OR                          
238000              (TEST-POS-NUM1 = TEST-POS-NUM2 AND TEST-POS-ALFA1           
238100               < TEST-POS-ALFA2)                                          
238200              PERFORM IMS-GNP-AVS2-RAD-ART-TO                             
238300              IF SEGMENT-FINNS                                            
238400                 MOVE AVS2-IDCATRAD     TO W-IDCATRAD-TO2                 
238500                 MOVE AVS2-KDCATPUB-FOM TO W-KDCATPUB-TO2                 
238600                 PERFORM DEA-KOLLA-UPPDAT                                 
238700              ELSE                                                        
238800                 MOVE JA TO INDATA-FEL                                    
238900              END-IF                                                      
239000           ELSE                                                           
239100              MOVE JA TO INDATA-FEL                                       
239200           END-IF                                                         
239300        END-IF                                                            
239400     ELSE                                                                 
239500        MOVE JA TO INDATA-FEL                                             
239600     END-IF                                                               
239700     .                                                                    
239800     EJECT                                                                
239900 DEA-KOLLA-UPPDAT SECTION.                                                
240000     SKIP2                                                                
240100     MOVE JA TO TO-BAS-FINNS                                              
240200     MOVE NEJ TO NY-POSITION                                              
240300     MOVE +9999 TO NAESTA-TO-RAD                                          
240400     MOVE W-IDCATRAD-TO2 TO SISTA-OK-RAD                                  
240500**** MOVE SPACE TO NAESTA-TO-POS                                          
240600                                                                          
240700     PERFORM DEAA-FLYTTA-IN-DATA                                          
240800     PERFORM DEAB-KOLLA-LIKA                                              
240900                                                                          
241000     PERFORM IMS-GNP-AVS2-RAD-TO                                          
241100                                                                          
241200     PERFORM DEAC-KOLLA-SEG                                               
241300                                                                          
241400     PERFORM UNTIL (STATUS-WS NOT = SPACE)                                
241500        OR (NY-POSITION = JA)                                             
241600        MOVE TO-RAD-IDCATRAD     TO W-IDCATRAD-TO                         
241700                                    W-IDCATRAD-TO2                        
241800        MOVE TO-RAD-KDCATPUB-FOM TO W-KDCATPUB-TO2                        
241900        PERFORM IMS-GET-AVS2-ART                                          
242000                                                                          
242100        IF SEGMENT-FINNS                                                  
242200           PERFORM S09-FLYTTA-IN-ART2                                     
242300           MOVE JA TO SPAR-ART-FINNS                                      
242400                                                                          
242500           MOVE TO-ART-IDCATPOS TO KOLL-POSX                              
242600           PERFORM S95-KOLLA-POS                                          
242700           MOVE TEST-POS-NUM  TO TEST-POS-NUM1                            
242800           MOVE TEST-POS-ALFA TO TEST-POS-ALFA1                           
242900                                                                          
243000           MOVE SPAR-IDCATPOS TO KOLL-POSX                                
243100           PERFORM S95-KOLLA-POS                                          
243200           MOVE TEST-POS-NUM  TO TEST-POS-NUM2                            
243300           MOVE TEST-POS-ALFA TO TEST-POS-ALFA2                           
243400******     IF TO-ART-IDCATPOS NOT > SPAR-IDCATPOS                         
243500           IF TEST-POS-NUM1 NOT > TEST-POS-NUM2                           
243600              IF TEST-POS-NUM1 = TEST-POS-NUM2                            
243700                 IF TEST-POS-ALFA1 NOT > TEST-POS-ALFA2                   
243800                    PERFORM DEAA-FLYTTA-IN-DATA                           
243900                    PERFORM DEAB-KOLLA-LIKA                               
244000                    MOVE W-IDCATRAD-TO TO SISTA-OK-RAD                    
244100                    PERFORM IMS-GNP-AVS2-RAD-TO                           
244200                    PERFORM DEAC-KOLLA-SEG                                
244300                 ELSE                                                     
244400                    MOVE JA TO NY-POSITION                                
244500                    MOVE W-IDCATRAD-TO TO NAESTA-TO-RAD                   
244600                    MOVE TO-ART-IDCATPOS TO NAESTA-TO-POS                 
244700                 END-IF                                                   
244800              ELSE                                                        
244900                 PERFORM DEAA-FLYTTA-IN-DATA                              
245000                 PERFORM DEAB-KOLLA-LIKA                                  
245100                 MOVE W-IDCATRAD-TO TO SISTA-OK-RAD                       
245200                 PERFORM IMS-GNP-AVS2-RAD-TO                              
245300                 PERFORM DEAC-KOLLA-SEG                                   
245400              END-IF                                                      
245500           ELSE                                                           
245600              MOVE JA TO NY-POSITION                                      
245700              MOVE W-IDCATRAD-TO TO NAESTA-TO-RAD                         
245800              MOVE TO-ART-IDCATPOS TO NAESTA-TO-POS                       
245900           END-IF                                                         
246000        ELSE                                                              
246100           MOVE NEJ TO SPAR-ART-FINNS                                     
246200           MOVE W-IDCATRAD-TO TO SISTA-OK-RAD                             
246300           PERFORM IMS-GNP-AVS2-RAD-TO                                    
246400           PERFORM DEAC-KOLLA-SEG                                         
246500        END-IF                                                            
246600     END-PERFORM                                                          
246700                                                                          
246800     IF NY-POSITION = NEJ                                                 
246900        MOVE +9999 TO NAESTA-TO-RAD                                       
247000     END-IF                                                               
247100                                                                          
247200     PERFORM DEAD-NUMRERA-RADER                                           
247300     PERFORM DEAE-ISRT-AVS2-RAD-ART                                       
247400     .                                                                    
247500     EJECT                                                                
247600 DEAA-FLYTTA-IN-DATA SECTION.                                             
247700     SKIP2                                                                
247800     MOVE ZERO                TO IN-SOEK-IDCATRAD                         
247900     MOVE TO-ART-IDARTNR      TO IN-SOEK-IDARTNR                          
248000     MOVE TO-ART-KDPS         TO IN-SOEK-KDPS                             
248100     MOVE TO-ART-KVPUNKT      TO IN-SOEK-KVPUNKT                          
248200     MOVE TO-ART-IDTTEXNR     TO IN-SOEK-IDTTEXNR                         
248300     PERFORM IMS-GET-AVS2-RAD-FIRST                                       
248400     MOVE TO-RAD-KDCATPUB-FOM TO IN-SOEK-KDCATPUB-FOM                     
248500     MOVE TO-RAD-KDCATPUB-TOM TO IN-SOEK-KDCATPUB-TOM                     
248600     PERFORM DEAAA-FLYTTA-TEXT                                            
248700     PERFORM DEAAB-FLYTTA-BEN                                             
248800     PERFORM DEAAC-FLYTTA-RUBNR                                           
248900     PERFORM DEAAD-FLYTTA-FOTNR                                           
249000     .                                                                    
249100     EJECT                                                                
249200 DEAAA-FLYTTA-TEXT SECTION.                                               
249300     SKIP2                                                                
249400     PERFORM IMS-GET-AVS2-TEXT                                            
249500     IF SEGMENT-FINNS                                                     
249600        MOVE TO-TEXT-TEKATANM TO IN-SOEK-TEKATANM                         
249700     ELSE                                                                 
249800        MOVE SPACE            TO IN-SOEK-TEKATANM                         
249900     END-IF                                                               
250000     .                                                                    
250100     EJECT                                                                
250200 DEAAB-FLYTTA-BEN SECTION.                                                
250300     SKIP2                                                                
250400     PERFORM IMS-GET-AVS2-BEN                                             
250500     IF SEGMENT-FINNS                                                     
250600        MOVE TO-BEN-KDHOM     TO IN-SOEK-KDHOM                            
250700        MOVE TO-BEN-BEART     TO IN-SOEK-BEART                            
250800     ELSE                                                                 
250900        MOVE ZERO             TO IN-SOEK-KDHOM                            
251000        MOVE SPACE            TO IN-SOEK-BEART                            
251100     END-IF                                                               
251200     .                                                                    
251300     EJECT                                                                
251400 DEAAC-FLYTTA-RUBNR SECTION.                                              
251500     SKIP2                                                                
251600     MOVE +1 TO INDX                                                      
251700     MOVE ZERO TO IN-SOEK-IDRUBNR(1)                                      
251800                  IN-SOEK-IDRUBNR(2)                                      
251900                  IN-SOEK-IDRUBNR(3)                                      
252000                                                                          
252100     PERFORM IMS-GET-AVS2-RUB                                             
252200                                                                          
252300     PERFORM UNTIL (STATUS-WS NOT = SPACE)                                
252400        OR (INDX NOT < +4)                                                
252500        MOVE TO-RUB-IDRUBNR TO                                            
252600             IN-SOEK-IDRUBNR(TO-RUB-IDSEGMNR)                             
252700        PERFORM IMS-GET-AVS2-RUB                                          
252800        ADD +1 TO INDX                                                    
252900     END-PERFORM                                                          
253000     .                                                                    
253100     EJECT                                                                
253200 DEAAD-FLYTTA-FOTNR SECTION.                                              
253300     SKIP2                                                                
253400     MOVE +1 TO INDX                                                      
253500     MOVE ZERO TO IN-SOEK-IDFOTNR(1)                                      
253600                  IN-SOEK-IDFOTNR(2)                                      
253700                  IN-SOEK-IDFOTNR(3)                                      
253800                                                                          
253900     PERFORM IMS-GET-AVS2-FOT                                             
254000                                                                          
254100     PERFORM UNTIL (STATUS-WS NOT = SPACE)                                
254200        OR (INDX NOT < +4)                                                
254300        MOVE TO-FOT-IDFOTNR TO                                            
254400             IN-SOEK-IDFOTNR(TO-FOT-IDSEGMNR)                             
254500        PERFORM IMS-GET-AVS2-FOT                                          
254600        ADD +1 TO INDX                                                    
254700     END-PERFORM                                                          
254800     .                                                                    
254900     EJECT                                                                
255000 DEAB-KOLLA-LIKA SECTION.                                                 
255100     SKIP2                                                                
255200     MOVE +1 TO TAB-IX                                                    
255300     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
255400        IF MID-KDCATPUB-R-TO-FOM = ALL '+'                                
255500           CONTINUE                                                       
255600        ELSE                                                              
255700           MOVE MID-KDCATPUB-R-TO-FOM                                     
255800                             TO WS-KDCATPUB-R-AVV                         
255900           PERFORM S50-Y2K-KDCATPUB-R                                     
256000           MOVE WS-KDCATPUB-AAAAVV                                        
256100                             TO IN-SOEK-KDCATPUB-FOM                      
256200        END-IF                                                            
256300        IF MID-KDCATPUB-R-TO-TOM = ALL '+'                                
256400           CONTINUE                                                       
256500        ELSE                                                              
256600           MOVE MID-KDCATPUB-R-TO-TOM                                     
256700                             TO WS-KDCATPUB-R-AVV                         
256800           PERFORM S50-Y2K-KDCATPUB-R                                     
256900           MOVE WS-KDCATPUB-AAAAVV                                        
257000                             TO IN-SOEK-KDCATPUB-TOM                      
257100        END-IF                                                            
257200        IF (SOEK-IDARTNR(TAB-IX)       = IN-SOEK-IDARTNR) AND             
257300           (SOEK-KDPS(TAB-IX)          = IN-SOEK-KDPS) AND                
257400           (SOEK-KVPUNKT(TAB-IX)       = IN-SOEK-KVPUNKT) AND             
257500           (SOEK-IDTTEXNR(TAB-IX)      = IN-SOEK-IDTTEXNR) AND            
257600           (SOEK-KDHOM(TAB-IX)         = IN-SOEK-KDHOM) AND               
257700           (SOEK-BEART(TAB-IX)         = IN-SOEK-BEART) AND               
257800           (SOEK-IDRUBNR(TAB-IX, 1)    = IN-SOEK-IDRUBNR(1)) AND          
257900           (SOEK-IDRUBNR(TAB-IX, 2)    = IN-SOEK-IDRUBNR(2)) AND          
258000           (SOEK-IDRUBNR(TAB-IX, 3)    = IN-SOEK-IDRUBNR(3)) AND          
258100           (SOEK-IDFOTNR(TAB-IX, 1)    = IN-SOEK-IDFOTNR(1)) AND          
258200           (SOEK-IDFOTNR(TAB-IX, 2)    = IN-SOEK-IDFOTNR(2)) AND          
258300           (SOEK-IDFOTNR(TAB-IX, 3)    = IN-SOEK-IDFOTNR(3)) AND          
258400           (SOEK-KDCATPUB-FOM(TAB-IX)  = IN-SOEK-KDCATPUB-FOM) AND        
258500           (SOEK-KDCATPUB-TOM(TAB-IX)  = IN-SOEK-KDCATPUB-TOM)            
258600               PERFORM DEABA-REPL-AVS2-ART                                
258700               ADD TAB-IX-MAX TO TAB-IX                                   
258800        END-IF                                                            
258900        ADD +1 TO TAB-IX                                                  
259000     END-PERFORM                                                          
259100     .                                                                    
259200     EJECT                                                                
259300 DEABA-REPL-AVS2-ART SECTION.                                             
259400     SKIP2                                                                
259500     PERFORM IMS-GET-AVS2-RAD-FIRST                                       
259600     MOVE DAGENS-DATUM TO TO-RAD-TIUPPDAT                                 
259700     MOVE 'L'          TO TO-RAD-KDRADST                                  
259800     PERFORM IMS-REPL-AVS2                                                
259900                                                                          
260000     PERFORM IMS-GET-AVS2-ART                                             
260100     IF SEGMENT-FINNS                                                     
260200        MOVE SOEK-KVKOL(TAB-IX) TO TO-ART-KVKOL(KOL-TO-IX)                
260300        PERFORM IMS-REPL-AVS2                                             
260400     END-IF                                                               
260500     PERFORM S02-BLANKA-SOEK-RAD                                          
260600     .                                                                    
260700     EJECT                                                                
260800 DEAC-KOLLA-SEG SECTION.                                                  
260900     SKIP2                                                                
261000     IF SEGMENT-SAKNAS                                                    
261100        MOVE +9999 TO NAESTA-TO-RAD                                       
261200        MOVE NEJ TO TO-BAS-FINNS                                          
261300     ELSE                                                                 
261400        MOVE TO-RAD-IDCATRAD TO NAESTA-TO-RAD                             
261500     END-IF                                                               
261600     .                                                                    
261700     EJECT                                                                
261800 DEAD-NUMRERA-RADER SECTION.                                              
261900     SKIP2                                                                
262000     MOVE +1 TO TAB-IX                                                    
262100     PERFORM UNTIL (TAB-IX > TAB-IX-MAX)                                  
262200        OR (INDATA-FEL = JA)                                              
262300        IF (SOEK-IDARTNR(TAB-IX) = ZERO) AND                              
262400           (SOEK-KDPS(TAB-IX) = SPACE) AND                                
262500           (SOEK-KVPUNKT(TAB-IX) = ZERO) AND                              
262600           (SOEK-IDTTEXNR(TAB-IX) = ZERO) AND                             
262700           (SOEK-KDHOM(TAB-IX) = ZERO) AND                                
262800           (SOEK-BEART(TAB-IX) = SPACE) AND                               
262900           (SOEK-IDRUBNR(TAB-IX, 1) = ZERO) AND                           
263000           (SOEK-IDRUBNR(TAB-IX, 2) = ZERO) AND                           
263100           (SOEK-IDRUBNR(TAB-IX, 3) = ZERO) AND                           
263200           (SOEK-IDFOTNR(TAB-IX, 1) = ZERO) AND                           
263300           (SOEK-IDFOTNR(TAB-IX, 2) = ZERO) AND                           
263400           (SOEK-IDFOTNR(TAB-IX, 3) = ZERO) AND                           
263500           (SOEK-KDFBX(TAB-IX) = SPACE) AND                               
263600           (SOEK-KVKOL(TAB-IX) = SPACE)                                   
263700               CONTINUE                                                   
263800        ELSE                                                              
263900           IF TAB-IX = 1                                                  
264000              ADD +1 TO SISTA-OK-RAD                                      
264100              IF SISTA-OK-RAD < NAESTA-TO-RAD                             
264200                 MOVE SISTA-OK-RAD TO SOEK-IDCATRAD(TAB-IX)               
264300              ELSE                                                        
264400                 MOVE JA TO INDATA-FEL                                    
264500              END-IF                                                      
264600           ELSE                                                           
264700             IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                         
264800                MOVE TAB-IX TO TEST-IX                                    
264900                ADD -1 TO TEST-IX                                         
265000                IF SPARAD-FRAN-RAD(TEST-IX) =                             
265100                   SPARAD-FRAN-RAD(TAB-IX)                                
265200                   MOVE SISTA-OK-RAD TO SOEK-IDCATRAD(TAB-IX)             
265300                ELSE                                                      
265400                   ADD +1 TO SISTA-OK-RAD                                 
265500                   IF SISTA-OK-RAD < NAESTA-TO-RAD                        
265600                      MOVE SISTA-OK-RAD TO SOEK-IDCATRAD(TAB-IX)          
265700                   ELSE                                                   
265800                      MOVE JA TO INDATA-FEL                               
265900                   END-IF                                                 
266000                END-IF                                                    
266100             ELSE                                                         
266200                ADD +1 TO SISTA-OK-RAD                                    
266300                IF SISTA-OK-RAD < NAESTA-TO-RAD                           
266400                   MOVE SISTA-OK-RAD TO SOEK-IDCATRAD(TAB-IX)             
266500                ELSE                                                      
266600                   MOVE JA TO INDATA-FEL                                  
266700                END-IF                                                    
266800             END-IF                                                       
266900           END-IF                                                         
267000        END-IF                                                            
267100        ADD +1 TO TAB-IX                                                  
267200     END-PERFORM                                                          
267300     .                                                                    
267400     EJECT                                                                
267500 DEAE-ISRT-AVS2-RAD-ART SECTION.                                          
267600     SKIP2                                                                
267700**- - - - - - - - DENNA FLYTTNING AV NYCKEL GÖRS FÖR ATT SPARA            
267800**- - - - - - - - FÖR FORTSATT LÄSNING                                    
267900     MOVE W-IDCATRAD-TO  TO SPAR-W-IDCATRAD-TO                            
268000     MOVE W-IDCATRAD-TO2 TO SPAR-W-IDCATRAD-TO2                           
268100     MOVE W-KDCATPUB-TO2 TO SPAR-W-KDCATPUB-TO2                           
268200     MOVE +1 TO TAB-IX                                                    
268300     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
268400        IF SOEK-IDCATRAD(TAB-IX) = ZERO                                   
268500           CONTINUE                                                       
268600        ELSE                                                              
268700**- - - - - - - - MAN FLYTTAR IN NYCKEL FÖR INSERT                        
268800           MOVE SOEK-IDCATRAD(TAB-IX) TO W-IDCATRAD-TO                    
268900                                         W-IDCATRAD-TO2                   
269000                                         TO-RAD-IDCATRAD                  
269100*          PERFORM S98-KOLLA-TIDSINTERVALL                                
269200*          IF NY-PUB-OK = JA                                              
269300              MOVE W-IDCATRAD-TO      TO TO-RAD-IDCATRAD                  
269400              IF MID-KDCATPUB-R-TO-FOM = ALL '+'                          
269500                 MOVE SOEK-KDCATPUB-FOM(TAB-IX)                           
269600                                      TO TO-RAD-KDCATPUB-FOM              
269700                                         W-KDCATPUB-TO2                   
269800              ELSE                                                        
269900                 MOVE WS-KDCATPUB-TO-FOM                                  
270000                                      TO TO-RAD-KDCATPUB-FOM              
270100                                         W-KDCATPUB-TO2                   
270200              END-IF                                                      
270300              IF MID-KDCATPUB-R-TO-TOM = ALL '+'                          
270400                 MOVE SOEK-KDCATPUB-TOM(TAB-IX)                           
270500                                      TO TO-RAD-KDCATPUB-TOM              
270600              ELSE                                                        
270700                 MOVE WS-KDCATPUB-TO-TOM                                  
270800                                      TO TO-RAD-KDCATPUB-TOM              
270900              END-IF                                                      
271000              MOVE SPACE              TO TO-RAD-IDUSER                    
271100              MOVE DAGENS-DATUM       TO TO-RAD-TIUPPDAT                  
271200              MOVE 'L'                TO TO-RAD-KDRADST                   
271300              PERFORM IMS-ISRT-AVS2-RAD                                   
271400              PERFORM DEAEA-ISRT-ART                                      
271500              PERFORM DEAEB-ISRT-TEXT                                     
271600              PERFORM DEAEC-ISRT-BEN                                      
271700              PERFORM DEAED-ISRT-NOT                                      
271800              PERFORM DEAEE-ISRT-RUB                                      
271900              PERFORM DEAEF-ISRT-FOT                                      
272000*          END-IF                                                         
272100           PERFORM S02-BLANKA-SOEK-RAD                                    
272200        END-IF                                                            
272300        ADD +1 TO TAB-IX                                                  
272400     END-PERFORM                                                          
272500**- - - - - - - - DENNA FLYTTNING AV NYCKEL GÖRS FÖR ATT FÅ               
272600**- - - - - - - - TILLBAKS POSITION FÖR FORTSATT LÄSNING                  
272700     MOVE SPAR-W-IDCATRAD-TO  TO W-IDCATRAD-TO                            
272800     MOVE SPAR-W-IDCATRAD-TO2 TO W-IDCATRAD-TO2                           
272900     MOVE SPAR-W-KDCATPUB-TO2 TO W-KDCATPUB-TO2                           
273000     .                                                                    
273100     EJECT                                                                
273200 DEAEA-ISRT-ART SECTION.                                                  
273300     SKIP2                                                                
273400     MOVE '1'                     TO TO-ART-KDSEGKEY                      
273500     MOVE SOEK-KDFBX(TAB-IX)      TO TO-ART-KDFBX                         
273600     MOVE SOEK-IDCATPOS(TAB-IX)   TO TO-ART-IDCATPOS                      
273700     MOVE SOEK-IDARTNR(TAB-IX)    TO TO-ART-IDARTNR                       
273800     MOVE SPACE                   TO TO-ART-KVKOL(1)                      
273900                                     TO-ART-KVKOL(2)                      
274000                                     TO-ART-KVKOL(3)                      
274100                                     TO-ART-KVKOL(4)                      
274200                                     TO-ART-KVKOL(5)                      
274300     MOVE SOEK-KVKOL(TAB-IX)      TO TO-ART-KVKOL(KOL-TO-IX)              
274400     MOVE SOEK-KDPS(TAB-IX)       TO TO-ART-KDPS                          
274500     MOVE SOEK-KVPUNKT(TAB-IX)    TO TO-ART-KVPUNKT                       
274600     MOVE SOEK-IDTTEXNR(TAB-IX)   TO TO-ART-IDTTEXNR                      
274700     PERFORM IMS-ISRT-AVS2-ART-KOL                                        
274800     .                                                                    
274900     EJECT                                                                
275000 DEAEB-ISRT-TEXT SECTION.                                                 
275100     SKIP2                                                                
275200     IF SOEK-TEKATANM(TAB-IX) = SPACE                                     
275300        CONTINUE                                                          
275400     ELSE                                                                 
275500        MOVE '1'                   TO TO-TEXT-KDSEGKEY                    
275600        MOVE SOEK-TEKATANM(TAB-IX) TO TO-TEXT-TEKATANM                    
275700        PERFORM IMS-ISRT-AVS2-TEXT-IO2                                    
275800     END-IF                                                               
275900     .                                                                    
276000     EJECT                                                                
276100 DEAEC-ISRT-BEN SECTION.                                                  
276200     SKIP2                                                                
276300     IF SOEK-KDHOM(TAB-IX) = ZERO AND                                     
276400        SOEK-BEART(TAB-IX) = SPACE                                        
276500           CONTINUE                                                       
276600     ELSE                                                                 
276700        MOVE '1'                TO TO-BEN-KDSEGKEY                        
276800        MOVE SOEK-KDHOM(TAB-IX) TO TO-BEN-KDHOM                           
276900        MOVE SOEK-BEART(TAB-IX) TO TO-BEN-BEART                           
277000        PERFORM IMS-ISRT-AVS2-BEN-IO2                                     
277100     END-IF                                                               
277200     .                                                                    
277300     EJECT                                                                
277400 DEAED-ISRT-NOT SECTION.                                                  
277500     SKIP2                                                                
277600     IF SOEK-TENOTE(TAB-IX) = SPACE                                       
277700        CONTINUE                                                          
277800     ELSE                                                                 
277900        MOVE SOEK-TENOTE(TAB-IX) TO TO-NOT-TENOTE                         
278000        MOVE +1                  TO TO-NOT-IDSEGMNR                       
278100        PERFORM IMS-ISRT-AVS2-NOT-IO2                                     
278200     END-IF                                                               
278300                                                                          
278400     IF SOEK-IDCATGRP(TAB-IX) = ZERO AND                                  
278500        SOEK-IDCATAVS(TAB-IX) = ZERO AND                                  
278600        SOEK-IDCATRAD-H(TAB-IX) = ZERO                                    
278700           CONTINUE                                                       
278800     ELSE                                                                 
278900        MOVE SOEK-IDCATGRP(TAB-IX)   TO TO-NOT-IDCATGRP                   
279000        MOVE SOEK-IDCATAVS(TAB-IX)   TO TO-NOT-IDCATAVS                   
279100        MOVE SOEK-IDCATRAD-H(TAB-IX) TO TO-NOT-IDCATRAD                   
279200        MOVE +2                      TO TO-NOT-IDSEGMNR                   
279300        PERFORM IMS-ISRT-AVS2-NOT-IO2                                     
279400     END-IF                                                               
279500     .                                                                    
279600     EJECT                                                                
279700 DEAEE-ISRT-RUB SECTION.                                                  
279800     SKIP2                                                                
279900     MOVE 1 TO INDX                                                       
280000     PERFORM UNTIL INDX NOT < +4                                          
280100        IF SOEK-IDRUBNR(TAB-IX, INDX) = ZERO                              
280200           CONTINUE                                                       
280300        ELSE                                                              
280400           MOVE SOEK-IDRUBNR(TAB-IX, INDX) TO TO-RUB-IDRUBNR              
280500           MOVE INDX                       TO TO-RUB-IDSEGMNR             
280600           MOVE SOEK-FLRUBTYP(TAB-IX)      TO TO-RUB-FLRUBTYP             
280700           PERFORM IMS-ISRT-AVS2-RUB-IO2                                  
280800        END-IF                                                            
280900        ADD +1 TO INDX                                                    
281000     END-PERFORM                                                          
281100     .                                                                    
281200     EJECT                                                                
281300 DEAEF-ISRT-FOT SECTION.                                                  
281400     SKIP2                                                                
281500     MOVE 1 TO INDX                                                       
281600     PERFORM UNTIL INDX NOT < +4                                          
281700        IF SOEK-IDFOTNR(TAB-IX, INDX) = ZERO                              
281800           CONTINUE                                                       
281900        ELSE                                                              
282000           MOVE SOEK-IDFOTNR(TAB-IX, INDX) TO TO-FOT-IDFOTNR              
282100           MOVE INDX                       TO TO-FOT-IDSEGMNR             
282200           PERFORM IMS-ISRT-AVS2-FOT-IO2                                  
282300        END-IF                                                            
282400        ADD +1 TO INDX                                                    
282500     END-PERFORM                                                          
282600     .                                                                    
282700     EJECT                                                                
282800 DF-KOLLA-SISTA-RAD SECTION.                                              
282900     SKIP2                                                                
283000     MOVE +1 TO INDX                                                      
283100     PERFORM UNTIL INDX > TAB-IX-MAX                                      
283200        IF (SOEK-IDARTNR(INDX) = ZERO) AND                                
283300           (SOEK-KDPS(INDX) = SPACE) AND                                  
283400           (SOEK-KVPUNKT(INDX) = ZERO) AND                                
283500           (SOEK-IDTTEXNR(INDX) = ZERO) AND                               
283600           (SOEK-KDHOM(INDX) = ZERO) AND                                  
283700           (SOEK-BEART(INDX) = SPACE) AND                                 
283800           (SOEK-IDRUBNR(INDX, 1) = ZERO) AND                             
283900           (SOEK-IDRUBNR(INDX, 2) = ZERO) AND                             
284000           (SOEK-IDRUBNR(INDX, 3) = ZERO) AND                             
284100           (SOEK-IDFOTNR(INDX, 1) = ZERO) AND                             
284200           (SOEK-IDFOTNR(INDX, 2) = ZERO) AND                             
284300           (SOEK-IDFOTNR(INDX, 3) = ZERO) AND                             
284400           (SOEK-KDFBX(INDX) = SPACE) AND                                 
284500           (SOEK-KVKOL(INDX) = SPACE)                                     
284600               CONTINUE                                                   
284700        ELSE                                                              
284800           IF TO-BAS-FINNS = JA                                           
284900              IF NAESTA-TO-POS = SPAR-IDCATPOS                            
285000                 PERFORM DE-UPPDATERA-TILL                                
285100              ELSE                                                        
285200                 MOVE SPAR-IDCATPOS TO POS-IDCATPOS                       
285300                 MOVE WS-POS-TEXT   TO POS-TEXT                           
285400                 MOVE WS-POS-MEDDELANDE TO  MOD-TEMFSINF                  
285500              END-IF                                                      
285600           ELSE                                                           
285700              MOVE SPAR-IDCATPOS TO POS-IDCATPOS                          
285800              MOVE WS-POS-TEXT   TO POS-TEXT                              
285900              MOVE WS-POS-MEDDELANDE TO  MOD-TEMFSINF                     
286000           END-IF                                                         
286100           ADD TAB-IX-MAX TO INDX                                         
286200        END-IF                                                            
286300        ADD +1 TO INDX                                                    
286400     END-PERFORM                                                          
286500     .                                                                    
286600     EJECT                                                                
286700 E-LAANA-KOLUMN-RADER SECTION.                                            
286800******************************************************************        
286900* VID FRÅN-PUBKOD EJ IFYLLD LÅNAS ALLA ARTIKEL-RADER                      
287000* VID FRÅN-PUBKOD IFYLLD    LÅNAS GÄLLANDE ARTIKEL-RADER                  
287100******************************************************************        
287200                                                                          
287300     PERFORM EA-ISRT-TEKOL                                                
287400                                                                          
287500     PERFORM IMS-GU-AVS1-FROM                                             
287600     PERFORM IMS-GU-AVS2-TO                                               
287700     MOVE +100 TO WS-MAX                                                  
287800     MOVE +1 TO WS-RAKNARE                                                
287900                                                                          
288000     MOVE WS-RAD-FROM TO W-IDCATRAD-FROM-MIN-F                            
288100     MOVE WS-RAD-TO   TO W-IDCATRAD-FROM-MAX-F                            
288200     MOVE LOW-VALUE   TO W-KDCATPUB-FROM-MIN-F                            
288300     MOVE HIGH-VALUE  TO W-KDCATPUB-FROM-MAX-F                            
288400     IF MID-IDCATRAD-SPAR > ZERO                                          
288500        MOVE MID-KDCATPUB-SPAR TO W-KDCATPUB-FROM-MIN-F                   
288600     END-IF                                                               
288700     IF SPAR-IFYLLT = JA                                                  
288800        MOVE MID-KDCATPUB-SPAR TO W-KDCATPUB-FROM-MIN-F                   
288900     END-IF                                                               
289000     IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                                 
289100        PERFORM IMS-GET-AVS1-RAD                                          
289200     ELSE                                                                 
289300        PERFORM S99-SOEK-AKTUELL-RAD                                      
289400     END-IF                                                               
289500                                                                          
289600     PERFORM UNTIL (STATUS-WS NOT = SPACE)                                
289700        OR  (WS-RAKNARE NOT < WS-MAX)                                     
289800        OR  (INDATA-FEL = JA)                                             
289900        IF WS-RAD-START < WS-MAX-RAD-TO                                   
290000           PERFORM S98-KOLLA-TIDSINTERVALL                                
290100           IF NY-PUB-OK = JA                                              
290200              PERFORM S07-SPARA-RAD                                       
290300              MOVE RAD-IDCATRAD     TO W-IDCATRAD-FROM2                   
290400              MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                   
290500              PERFORM S01-FLYTTA-STATUS-DATUM                             
290600              PERFORM EB-KOLLA-UPD-RAD                                    
290700           END-IF                                                         
290800           PERFORM S06-ADDERA-RAD-START                                   
290900        ELSE                                                              
291000           MOVE JA TO INDATA-FEL                                          
291100           PERFORM S25-PLATS-FINNS-EJ                                     
291200        END-IF                                                            
291300     END-PERFORM                                                          
291400                                                                          
291500     IF INDATA-FEL = JA                                                   
291600        PERFORM G-VISA-BILD-IGEN                                          
291700        PERFORM X-GRUND-FORMAT                                            
291800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
291900        PERFORM IMS-INSERT-MSG                                            
292000     ELSE                                                                 
292100        IF WS-RAKNARE = WS-MAX                                            
292200           IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                           
292300              IF NY-RAD-FINNS = JA                                        
292400                 PERFORM S30-FLYTTA-TO-PROG                               
292500                 PERFORM IMS-INSERT-ALT-MSG                               
292600              ELSE                                                        
292700                 MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                    
292800                 PERFORM G-VISA-BILD-IGEN                                 
292900                 PERFORM X-GRUND-FORMAT                                   
293000                 MOVE MAX-MOD-LAENGD TO MSG-KVLL                          
293100                 PERFORM IMS-INSERT-MSG                                   
293200              END-IF                                                      
293300           ELSE                                                           
293400              IF AKTUELL-RAD = JA                                         
293500                 PERFORM S30-FLYTTA-TO-PROG                               
293600                 PERFORM IMS-INSERT-ALT-MSG                               
293700              ELSE                                                        
293800                 MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                    
293900                 PERFORM G-VISA-BILD-IGEN                                 
294000                 PERFORM X-GRUND-FORMAT                                   
294100                 MOVE MAX-MOD-LAENGD TO MSG-KVLL                          
294200                 PERFORM IMS-INSERT-MSG                                   
294300              END-IF                                                      
294400           END-IF                                                         
294500        ELSE                                                              
294600           IF WS-RAKNARE < WS-MAX                                         
294700              IF WS-RAKNARE = 1                                           
294800                 MOVE MED-2(SPRAAK-IX) TO MOD-TEMFSINF                    
294900              ELSE                                                        
295000                 MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                    
295100              END-IF                                                      
295200              PERFORM G-VISA-BILD-IGEN                                    
295300              PERFORM X-GRUND-FORMAT                                      
295400              MOVE MAX-MOD-LAENGD TO MSG-KVLL                             
295500              PERFORM IMS-INSERT-MSG                                      
295600           END-IF                                                         
295700        END-IF                                                            
295800     END-IF                                                               
295900     .                                                                    
296000     EJECT                                                                
296100 EA-ISRT-TEKOL SECTION.                                                   
296200******************************************************************        
296300* OM TILL-AVSNITTET REDAN HAR RUBRIKRADER INSERTAS INGA NYA               
296400* RUBRIKER. DETTA FÖR ATT UNDVIKA KONTROLL OCH UPPDATERING AV             
296500* START OCH STOPPDATUM AV BEF. RUBRIKRADER. DETTA GÖRS I 1514.            
296600*                                                                         
296700* VID FRÅN-PUBKOD EJ IFYLLD LÅNAS ALLA PUBKODER                           
296800* VID FRÅN-PUBKOD IFYLLD    LÅNAS GÄLLANDE PUBKOD                         
296900******************************************************************        
297000                                                                          
297100     PERFORM IMS-GU-AVS1-FROM                                             
297200     PERFORM IMS-GU-AVS2-TO                                               
297300                                                                          
297400     PERFORM S04-RAETT-FROM-KOLUMN                                        
297500     PERFORM S05-RAETT-TO-KOLUMN                                          
297600                                                                          
297700     MOVE +1  TO W-IDCATRAD-FROM2-MIN                                     
297800     MOVE +19 TO W-IDCATRAD-FROM2-MAX                                     
297900     PERFORM IMS-GET-AVS2-RAD                                             
298000     IF SEGMENT-FINNS                                                     
298100        CONTINUE                                                          
298200     ELSE                                                                 
298300        IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                              
298400           PERFORM IMS-GNP-AVS1-RAD-FROM                                  
298500           PERFORM UNTIL SEGMENT-SAKNAS                                   
298600              MOVE W-IDCATRAD-TO2   TO TO-RAD-IDCATRAD                    
298700              MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                   
298800                                       W-KDCATPUB-TO2                     
298900                                       TO-RAD-KDCATPUB-FOM                
299000              MOVE RAD-KDCATPUB-TOM TO TO-RAD-KDCATPUB-TOM                
299100              MOVE SPACE        TO TO-RAD-IDUSER                          
299200              MOVE DAGENS-DATUM TO TO-RAD-TIUPPDAT                        
299300              MOVE 'L'          TO TO-RAD-KDRADST                         
299400                                                                          
299500              PERFORM IMS-GET-AVS1-TEXT                                   
299600              IF SEGMENT-FINNS AND TEXT-TEKOL NOT = SPACE                 
299700                 PERFORM IMS-ISRT-AVS2-RAD                                
299800                 PERFORM IMS-ISRT-AVS2-TEXT-IO1                           
299900              END-IF                                                      
300000              PERFORM IMS-GNP-AVS1-RAD-FROM                               
300100           END-PERFORM                                                    
300200        ELSE                                                              
300300           PERFORM BDA-SOEK-GALLANDE                                      
300400           IF SEGMENT-FINNS                                               
300500              MOVE SPAR-KDCATPUB TO W-KDCATPUB-FROM2                      
300600              PERFORM IMS-GET-AVS1-RAD-FROM                               
300700              MOVE W-IDCATRAD-TO2   TO TO-RAD-IDCATRAD                    
300800              MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                   
300900              MOVE WS-KDCATPUB-TO-FOM  TO W-KDCATPUB-TO2                  
301000                                       TO-RAD-KDCATPUB-FOM                
301100              MOVE WS-KDCATPUB-TO-TOM  TO TO-RAD-KDCATPUB-TOM             
301200              MOVE SPACE          TO TO-RAD-IDUSER                        
301300              MOVE DAGENS-DATUM   TO TO-RAD-TIUPPDAT                      
301400              MOVE 'L'            TO TO-RAD-KDRADST                       
301500              PERFORM IMS-GET-AVS1-TEXT                                   
301600              IF SEGMENT-FINNS AND TEXT-TEKOL NOT = SPACE                 
301700                 PERFORM IMS-ISRT-AVS2-RAD                                
301800                 PERFORM IMS-ISRT-AVS2-TEXT-IO1                           
301900              END-IF                                                      
302000           END-IF                                                         
302100        END-IF                                                            
302200     END-IF                                                               
302300     .                                                                    
302400     EJECT                                                                
302500 EB-KOLLA-UPD-RAD SECTION.                                                
302600                                                                          
302700     PERFORM IMS-GET-AVS1-ART-FIRST                                       
302800     IF SEGMENT-FINNS                                                     
302900        IF ART-KVKOL(KOL-IX) = SPACE                                      
303000           IF ART-KDFBX = 'F'                                             
303100              IF ART-KVKOL(1) = SPACE AND                                 
303200                 ART-KVKOL(2) = SPACE AND                                 
303300                 ART-KVKOL(3) = SPACE AND                                 
303400                 ART-KVKOL(4) = SPACE AND                                 
303500                 ART-KVKOL(5) = SPACE                                     
303600                 PERFORM EBA-UPPDATERA-TO-RAD                             
303700              END-IF                                                      
303800           END-IF                                                         
303900        ELSE                                                              
304000           PERFORM EBA-UPPDATERA-TO-RAD                                   
304100        END-IF                                                            
304200     END-IF                                                               
304300     .                                                                    
304400     EJECT                                                                
304500 EBA-UPPDATERA-TO-RAD SECTION.                                            
304600     SKIP2                                                                
304700     MOVE W-IDCATRAD-TO TO TO-RAD-IDCATRAD                                
304800     PERFORM IMS-ISRT-AVS2-RAD                                            
304900     PERFORM EBAA-AVS-ART                                                 
305000     PERFORM S20-AVS-UPPDATERING                                          
305100     PERFORM S21-AVS-UPD-HAEN                                             
305200     .                                                                    
305300     EJECT                                                                
305400 EBAA-AVS-ART SECTION.                                                    
305500     SKIP2                                                                
305600     MOVE ART-KVKOL(KOL-IX) TO SPAR-KVKOL                                 
305700     MOVE 1 TO INDX                                                       
305800     PERFORM UNTIL INDX NOT < +6                                          
305900        MOVE SPACE TO ART-KVKOL(INDX)                                     
306000        ADD +1 TO INDX                                                    
306100     END-PERFORM                                                          
306200     MOVE SPAR-KVKOL TO ART-KVKOL(KOL-TO-IX)                              
306300     PERFORM IMS-ISRT-AVS2-ART                                            
306400     .                                                                    
306500     EJECT                                                                
306600 G-VISA-BILD-IGEN SECTION.                                                
306700     SKIP2                                                                
306800     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDCATNR-FROM                         
306900                                 MOD-IDCATGRP-FROM                        
307000                                 MOD-IDCATAVS-FROM                        
307100                                 MOD-IDKOL-FROM                           
307200                                 MOD-IDCATRAD-FROM                        
307300                                 MOD-IDCATRAD-TO                          
307400                                 MOD-IDCATNR-TO                           
307500                                 MOD-IDCATGRP-TO                          
307600                                 MOD-IDCATAVS-TO                          
307700                                 MOD-IDKOL-TO                             
307800                                 MOD-IDCATRAD-START                       
307900                                 MOD-IDCATPOS-FROM                        
308000                                 MOD-IDCATPOS-TO                          
308100                                 MOD-KDCATPUB-R-FROM-FOM                  
308200                                 MOD-KDCATPUB-R-TO-FOM                    
308300                                 MOD-KDCATPUB-R-TO-TOM                    
308400                                                                          
308500     .                                                                    
308600     EJECT                                                                
308700 H-RENSA-BILD SECTION.                                                    
308800     SKIP2                                                                
308900     MOVE MFS-RENSA-FAELT   TO MOD-IDCATNR-FROM                           
309000                               MOD-IDCATGRP-FROM                          
309100                               MOD-IDCATAVS-FROM                          
309200                               MOD-IDKOL-FROM                             
309300                               MOD-IDCATRAD-FROM                          
309400                               MOD-IDCATRAD-TO                            
309500                               MOD-IDCATNR-TO                             
309600                               MOD-IDCATGRP-TO                            
309700                               MOD-IDCATAVS-TO                            
309800                               MOD-IDKOL-TO                               
309900                               MOD-IDCATRAD-START                         
310000                               MOD-IDCATPOS-FROM                          
310100                               MOD-IDCATPOS-TO                            
310200                               MOD-KDCATPUB-R-FROM-FOM                    
310300                               MOD-KDCATPUB-R-TO-FOM                      
310400                               MOD-KDCATPUB-R-TO-TOM                      
310500     .                                                                    
310600     EJECT                                                                
310700 Q-UPPDATERA-TABELL SECTION.                                              
310800     SKIP2                                                                
310900     MOVE +1 TO TAB-IX                                                    
311000     MOVE NEJ TO NY-POSITION                                              
311100     MOVE NEJ TO LINE-BEFORE                                              
311200                                                                          
311300     MOVE AVS1-IDCATRAD     TO W-IDCATRAD-FROM2                           
311400     MOVE AVS1-KDCATPUB-FOM TO W-KDCATPUB-FROM2                           
311500     PERFORM QA-FLYTTA-DATA                                               
311600     IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                                 
311700        PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM                                
311800     ELSE                                                                 
311900        PERFORM S96-SOEK-AKTUELL-RAD                                      
312000     END-IF                                                               
312100     PERFORM S03-FLYTTA-FROM-RAD                                          
312200     MOVE JA TO ROT-FROM-LAEST                                            
312300                                                                          
312400     PERFORM UNTIL (STATUS-WS NOT = SPACE)                                
312500        OR (NY-POSITION = JA)                                             
312600        OR (UPPDAT-FEL = JA)                                              
312700        IF TAB-IX > TAB-IX-MAX                                            
312800           PERFORM IMS-ROLLBACK                                           
312900*          **************************************************             
313000*          * DET FINNS FLER ÄN 25 ARTIKLAR UNDER ETT POSNR. *             
313100*          * GÖR EN IMS-777-ABEND, STARTAR OM BILDEN        *             
313200*          * IMS BACKAR UR ALLA UPPDATERINGAR               *             
313300*          **************************************************             
313400           MOVE FEL-6(SPRAAK-IX) TO MOD-TEMFSFEL                          
313500           MOVE JA TO UPPDAT-FEL                                          
313600        END-IF                                                            
313700        IF UPPDAT-FEL = NEJ                                               
313800           MOVE RAD-IDCATRAD     TO W-IDCATRAD-FROM2                      
313900           MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-FROM2                      
314000           PERFORM IMS-GET-AVS1-ART                                       
314100           MOVE NEJ TO ROT-FROM-LAEST                                     
314200           IF SEGMENT-FINNS                                               
314300              IF ART-IDCATPOS = SPAR-IDCATPOS                             
314400                 OR ART-KDFBX = 'F'                                       
314500                 IF ART-KVKOL(KOL-IX) = SPACE                             
314600                    PERFORM QB-KOLLA-OK                                   
314700                 ELSE                                                     
314800                    PERFORM QA-FLYTTA-DATA                                
314900                 END-IF                                                   
315000                 IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                     
315100                    PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM                    
315200                 ELSE                                                     
315300                    PERFORM S96-SOEK-AKTUELL-RAD                          
315400                 END-IF                                                   
315500                 PERFORM S03-FLYTTA-FROM-RAD                              
315600                 MOVE NEJ TO ROT-FROM-LAEST                               
315700              ELSE                                                        
315800                 MOVE JA TO NY-POSITION                                   
315900                 MOVE NEJ TO LINE-BEFORE                                  
316000              END-IF                                                      
316100           ELSE                                                           
316200              IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                        
316300                 PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM                       
316400              ELSE                                                        
316500                 PERFORM S96-SOEK-AKTUELL-RAD                             
316600              END-IF                                                      
316700              PERFORM S03-FLYTTA-FROM-RAD                                 
316800              MOVE NEJ TO LINE-BEFORE                                     
316900              MOVE NEJ TO ROT-FROM-LAEST                                  
317000           END-IF                                                         
317100        END-IF                                                            
317200     END-PERFORM                                                          
317300                                                                          
317400     IF UPPDAT-FEL = NEJ                                                  
317500        IF ROT-FROM-LAEST = JA                                            
317600           IF SEGMENT-SAKNAS                                              
317700              MOVE NEJ TO RAD-FINNS                                       
317800           END-IF                                                         
317900        END-IF                                                            
318000     END-IF                                                               
318100     .                                                                    
318200     EJECT                                                                
318300 QA-FLYTTA-DATA SECTION.                                                  
318400                                                                          
318500                                                                          
318600**** MOVE RAD-IDCATRAD     TO W-IDCATRAD-FROM2                            
318700**** MOVE RAD-KDCATPUB-FOM TO W-IDCATRAD-FROM2                            
318800                                                                          
318900     PERFORM QAA-FLYTTA-AVS1-ART                                          
319000     PERFORM IMS-GNP-AVS1-RAD-FIRST                                       
319100     MOVE RAD-KDCATPUB-FOM TO SOEK-KDCATPUB-FOM(TAB-IX)                   
319200     MOVE RAD-KDCATPUB-TOM TO SOEK-KDCATPUB-TOM(TAB-IX)                   
319300     MOVE RAD-IDCATRAD TO SPARAD-FRAN-RAD(TAB-IX)                         
319400     PERFORM QAB-FLYTTA-AVS1-TEXT                                         
319500     PERFORM QAC-FLYTTA-AVS1-BEN                                          
319600     PERFORM QAD-FLYTTA-AVS1-NOT                                          
319700     PERFORM QAE-FLYTTA-AVS1-RUB                                          
319800     PERFORM QAF-FLYTTA-AVS1-FOT                                          
319900     PERFORM QAG-FLYTTA-AVS1-HAEN                                         
320000     ADD +1 TO TAB-IX                                                     
320100     MOVE JA TO LINE-BEFORE                                               
320200     .                                                                    
320300     EJECT                                                                
320400 QAA-FLYTTA-AVS1-ART SECTION.                                             
320500     SKIP2                                                                
320600     MOVE ZERO          TO SOEK-IDCATRAD(TAB-IX)                          
320700     MOVE ART-IDARTNR   TO SOEK-IDARTNR(TAB-IX)                           
320800     MOVE ART-IDCATPOS  TO SOEK-IDCATPOS(TAB-IX)                          
320900     MOVE ART-KDPS      TO SOEK-KDPS(TAB-IX)                              
321000     MOVE ART-KVPUNKT   TO SOEK-KVPUNKT(TAB-IX)                           
321100     MOVE ART-IDTTEXNR  TO SOEK-IDTTEXNR(TAB-IX)                          
321200     MOVE ART-KDFBX     TO SOEK-KDFBX(TAB-IX)                             
321300     MOVE ART-KVKOL(KOL-IX) TO SOEK-KVKOL(TAB-IX)                         
321400                                                                          
321500     IF ART-KDFBX = 'F'                                                   
321600        MOVE SPAR-IDCATPOS TO ART-IDCATPOS                                
321700     END-IF                                                               
321800     .                                                                    
321900     EJECT                                                                
322000 QAB-FLYTTA-AVS1-TEXT SECTION.                                            
322100     SKIP2                                                                
322200     PERFORM IMS-GET-AVS1-TEXT                                            
322300     IF SEGMENT-FINNS                                                     
322400        MOVE TEXT-TEKATANM TO SOEK-TEKATANM(TAB-IX)                       
322500     END-IF                                                               
322600     .                                                                    
322700     EJECT                                                                
322800 QAC-FLYTTA-AVS1-BEN SECTION.                                             
322900     SKIP2                                                                
323000     PERFORM IMS-GET-AVS1-BEN                                             
323100     IF SEGMENT-FINNS                                                     
323200        MOVE BEN-BEART     TO SOEK-BEART(TAB-IX)                          
323300        MOVE BEN-KDHOM     TO SOEK-KDHOM(TAB-IX)                          
323400     END-IF                                                               
323500     .                                                                    
323600     EJECT                                                                
323700 QAD-FLYTTA-AVS1-NOT SECTION.                                             
323800     SKIP2                                                                
323900     PERFORM IMS-GET-AVS1-NOT                                             
324000     IF SEGMENT-FINNS                                                     
324100        IF NOT-IDSEGMNR = 1                                               
324200           MOVE NOT-TENOTE    TO SOEK-TENOTE(TAB-IX)                      
324300           PERFORM IMS-GET-AVS1-NOT                                       
324400           IF SEGMENT-FINNS AND NOT-IDSEGMNR = 2                          
324500              MOVE NOT-IDCATGRP  TO SOEK-IDCATGRP(TAB-IX)                 
324600              MOVE NOT-IDCATAVS  TO SOEK-IDCATAVS(TAB-IX)                 
324700              MOVE NOT-IDCATRAD  TO SOEK-IDCATRAD-H(TAB-IX)               
324800           END-IF                                                         
324900        ELSE                                                              
325000           MOVE NOT-IDCATGRP  TO SOEK-IDCATGRP(TAB-IX)                    
325100           MOVE NOT-IDCATAVS  TO SOEK-IDCATAVS(TAB-IX)                    
325200           MOVE NOT-IDCATRAD  TO SOEK-IDCATRAD-H(TAB-IX)                  
325300        END-IF                                                            
325400     END-IF                                                               
325500     .                                                                    
325600     EJECT                                                                
325700 QAE-FLYTTA-AVS1-RUB SECTION.                                             
325800     SKIP2                                                                
325900     PERFORM IMS-GET-AVS1-RUB                                             
326000     PERFORM UNTIL STATUS-WS NOT = SPACE                                  
326100        MOVE RUB-IDRUBNR   TO                                             
326200             SOEK-IDRUBNR(TAB-IX, RUB-IDSEGMNR)                           
326300        MOVE RUB-FLRUBTYP  TO SOEK-FLRUBTYP(TAB-IX)                       
326400        PERFORM IMS-GET-AVS1-RUB                                          
326500     END-PERFORM                                                          
326600     .                                                                    
326700     EJECT                                                                
326800 QAF-FLYTTA-AVS1-FOT SECTION.                                             
326900     SKIP2                                                                
327000     PERFORM IMS-GET-AVS1-FOT                                             
327100     PERFORM UNTIL STATUS-WS NOT = SPACE                                  
327200        MOVE FOT-IDFOTNR   TO                                             
327300             SOEK-IDFOTNR(TAB-IX, FOT-IDSEGMNR)                           
327400        PERFORM IMS-GET-AVS1-FOT                                          
327500     END-PERFORM                                                          
327600     .                                                                    
327700     EJECT                                                                
327800 QAG-FLYTTA-AVS1-HAEN SECTION.                                            
327900     SKIP2                                                                
328000     PERFORM IMS-GET-AVS1-HAEN                                            
328100     IF SEGMENT-FINNS                                                     
328200        IF SOEK-IDCATGRP(TAB-IX) = ZERO                                   
328300           MOVE HAEN-IDCATGRP TO SOEK-IDCATGRP(TAB-IX)                    
328400           MOVE HAEN-IDCATAVS TO SOEK-IDCATAVS(TAB-IX)                    
328500           MOVE HAEN-IDCATRAD TO SOEK-IDCATRAD-H(TAB-IX)                  
328600        END-IF                                                            
328700     END-IF                                                               
328800     .                                                                    
328900     EJECT                                                                
329000 QB-KOLLA-OK SECTION.                                                     
329100     SKIP2                                                                
329200     IF ART-KDFBX = 'F'                                                   
329300        IF LINE-BEFORE = JA                                               
329400           PERFORM QA-FLYTTA-DATA                                         
329500        ELSE                                                              
329600           MOVE NEJ TO LINE-BEFORE                                        
329700        END-IF                                                            
329800     ELSE                                                                 
329900        IF ART-KVKOL(1) = SPACE AND                                       
330000           ART-KVKOL(2) = SPACE AND                                       
330100           ART-KVKOL(3) = SPACE AND                                       
330200           ART-KVKOL(4) = SPACE AND                                       
330300           ART-KVKOL(5) = SPACE                                           
330400           PERFORM QA-FLYTTA-DATA                                         
330500        ELSE                                                              
330600           MOVE NEJ TO LINE-BEFORE                                        
330700        END-IF                                                            
330800     END-IF                                                               
330900     .                                                                    
331000     EJECT                                                                
331100 S01-FLYTTA-STATUS-DATUM SECTION.                                         
331200                                                                          
331300     IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                                 
331400        MOVE RAD-KDCATPUB-FOM   TO TO-RAD-KDCATPUB-FOM                    
331500        MOVE RAD-KDCATPUB-TOM   TO TO-RAD-KDCATPUB-TOM                    
331600     ELSE                                                                 
331700        IF MID-KDCATPUB-R-TO-FOM = ALL '+'                                
331800           MOVE RAD-KDCATPUB-FOM   TO TO-RAD-KDCATPUB-FOM                 
331900        ELSE                                                              
332000           MOVE WS-KDCATPUB-TO-FOM TO TO-RAD-KDCATPUB-FOM                 
332100        END-IF                                                            
332200        IF MID-KDCATPUB-R-TO-TOM = ALL '+'                                
332300           MOVE RAD-KDCATPUB-TOM   TO TO-RAD-KDCATPUB-TOM                 
332400        ELSE                                                              
332500           MOVE WS-KDCATPUB-TO-TOM TO TO-RAD-KDCATPUB-TOM                 
332600        END-IF                                                            
332700     END-IF                                                               
332800     MOVE TO-RAD-KDCATPUB-FOM TO W-KDCATPUB-TO2                           
332900     MOVE SPACE        TO TO-RAD-IDUSER                                   
333000     MOVE DAGENS-DATUM TO TO-RAD-TIUPPDAT                                 
333100     MOVE 'L'          TO TO-RAD-KDRADST                                  
333200     .                                                                    
333300     EJECT                                                                
333400 S02-BLANKA-SOEK-RAD SECTION.                                             
333500     SKIP2                                                                
333600     MOVE ZERO   TO   SOEK-IDCATRAD(TAB-IX)                               
333700                      SOEK-IDARTNR(TAB-IX)                                
333800                      SOEK-KVPUNKT(TAB-IX)                                
333900                      SOEK-IDTTEXNR(TAB-IX)                               
334000                      SOEK-KDHOM(TAB-IX)                                  
334100                      SOEK-IDRUBNR(TAB-IX, 1)                             
334200                      SOEK-IDRUBNR(TAB-IX, 2)                             
334300                      SOEK-IDRUBNR(TAB-IX, 3)                             
334400                      SOEK-IDFOTNR(TAB-IX, 1)                             
334500                      SOEK-IDFOTNR(TAB-IX, 2)                             
334600                      SOEK-IDFOTNR(TAB-IX, 3)                             
334700                      SOEK-IDCATGRP(TAB-IX)                               
334800                      SOEK-IDCATAVS(TAB-IX)                               
334900                      SOEK-IDCATRAD-H(TAB-IX)                             
335000     MOVE SPACE  TO   SOEK-KDPS(TAB-IX)                                   
335100                      SOEK-TEKATANM(TAB-IX)                               
335200                      SOEK-BEART(TAB-IX)                                  
335300                      SOEK-KDFBX(TAB-IX)                                  
335400                      SOEK-KVKOL(TAB-IX)                                  
335500                      SOEK-FLRUBTYP(TAB-IX)                               
335600                      SOEK-TENOTE(TAB-IX)                                 
335700     .                                                                    
335800     EJECT                                                                
335900 S03-FLYTTA-FROM-RAD SECTION.                                             
336000     SKIP2                                                                
336100*    IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                                 
336200*       PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM                                
336300*    ELSE                                                                 
336400*       PERFORM S96-SOEK-AKTUELL-RAD                                      
336500*    END-IF                                                               
336600     IF SEGMENT-FINNS                                                     
336700        MOVE RAD-IDCATRAD TO W-IDCATRAD-FROM                              
336800     ELSE                                                                 
336900        MOVE JA TO BAS-FROM-SLUT                                          
337000     END-IF                                                               
337100     .                                                                    
337200     EJECT                                                                
337300 S04-RAETT-FROM-KOLUMN SECTION.                                           
337400     SKIP2                                                                
337500     EVALUATE WS-IDKOL-FROM                                               
337600        WHEN 'A'                                                          
337700             MOVE +10 TO W-IDCATRAD-FROM                                  
337800                         W-IDCATRAD-FROM2                                 
337900             MOVE +1  TO KOL-IX                                           
338000        WHEN 'B'                                                          
338100             MOVE +11 TO W-IDCATRAD-FROM                                  
338200                         W-IDCATRAD-FROM2                                 
338300             MOVE +2  TO KOL-IX                                           
338400        WHEN 'C'                                                          
338500             MOVE +12 TO W-IDCATRAD-FROM                                  
338600                         W-IDCATRAD-FROM2                                 
338700             MOVE +3  TO KOL-IX                                           
338800        WHEN 'D'                                                          
338900             MOVE +13 TO W-IDCATRAD-FROM                                  
339000                         W-IDCATRAD-FROM2                                 
339100             MOVE +4  TO KOL-IX                                           
339200        WHEN 'E'                                                          
339300             MOVE +14 TO W-IDCATRAD-FROM                                  
339400                         W-IDCATRAD-FROM2                                 
339500             MOVE +5  TO KOL-IX                                           
339600     END-EVALUATE                                                         
339700     .                                                                    
339800     EJECT                                                                
339900 S05-RAETT-TO-KOLUMN SECTION.                                             
340000     SKIP2                                                                
340100     EVALUATE WS-IDKOL-TO                                                 
340200        WHEN 'A'                                                          
340300             MOVE +10 TO W-IDCATRAD-TO                                    
340400                         W-IDCATRAD-TO2                                   
340500             MOVE +1  TO KOL-TO-IX                                        
340600        WHEN 'B'                                                          
340700             MOVE +11 TO W-IDCATRAD-TO                                    
340800                         W-IDCATRAD-TO2                                   
340900             MOVE +2  TO KOL-TO-IX                                        
341000        WHEN 'C'                                                          
341100             MOVE +12 TO W-IDCATRAD-TO                                    
341200                         W-IDCATRAD-TO2                                   
341300             MOVE +3  TO KOL-TO-IX                                        
341400        WHEN 'D'                                                          
341500             MOVE +13 TO W-IDCATRAD-TO                                    
341600                         W-IDCATRAD-TO2                                   
341700             MOVE +4  TO KOL-TO-IX                                        
341800        WHEN 'E'                                                          
341900             MOVE +14 TO W-IDCATRAD-TO                                    
342000                         W-IDCATRAD-TO2                                   
342100             MOVE +5  TO KOL-TO-IX                                        
342200     END-EVALUATE                                                         
342300     .                                                                    
342400     EJECT                                                                
342500 S06-ADDERA-RAD-START SECTION.                                            
342600                                                                          
342700     IF WS-RAKNARE NOT > WS-MAX                                           
342800        ADD +1 TO WS-RAKNARE                                              
342900*       IF WS-RAD-TO = +9999                                              
343000*          CONTINUE                                                       
343100*       ELSE                                                              
343200*          COMPUTE WS-RAD-START =                                         
343300*          WS-RAD-START + WS-INTERVALL                                    
343400*       END-IF                                                            
343500                                                                          
343600        IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                              
343700           PERFORM IMS-GET-AVS1-RAD                                       
343800        ELSE                                                              
343900           PERFORM S99-SOEK-AKTUELL-RAD                                   
344000        END-IF                                                            
344100                                                                          
344200        IF WS-RAD-TO = +9999                                              
344300           MOVE NEJ TO NY-RAD-FINNS                                       
344400           IF SEGMENT-FINNS                                               
344500              MOVE JA TO NY-RAD-FINNS                                     
344600              MOVE RAD-IDCATRAD TO WS-RAD-START                           
344700           END-IF                                                         
344800        ELSE                                                              
344900           IF SEGMENT-FINNS                                               
345000              MOVE JA TO NY-RAD-FINNS                                     
345100              IF RAD-IDCATRAD = W-IDCATRAD-FROM2                          
345200                 CONTINUE                                                 
345300              ELSE                                                        
345400                 COMPUTE WS-RAD-START =                                   
345500                 WS-RAD-START + WS-INTERVALL                              
345600              END-IF                                                      
345700           END-IF                                                         
345800        END-IF                                                            
345900     END-IF                                                               
346000     .                                                                    
346100     EJECT                                                                
346200 S07-SPARA-RAD SECTION.                                                   
346300     SKIP2                                                                
346400     IF WS-RAD-TO = +9999                                                 
346500        MOVE RAD-IDCATRAD TO TO-RAD-IDCATRAD                              
346600                             W-IDCATRAD-TO                                
346700                             W-IDCATRAD-TO2                               
346800                             WS-RAD-START                                 
346900     ELSE                                                                 
347000        MOVE WS-RAD-START TO TO-RAD-IDCATRAD                              
347100                             W-IDCATRAD-TO                                
347200                             W-IDCATRAD-TO2                               
347300     END-IF                                                               
347400     .                                                                    
347500     EJECT                                                                
347600 S08-BLANKA-IN-SOEK SECTION.                                              
347700     SKIP2                                                                
347800     MOVE ZERO  TO IN-SOEK-IDCATRAD                                       
347900                   IN-SOEK-IDARTNR                                        
348000                   IN-SOEK-KVPUNKT                                        
348100                   IN-SOEK-IDTTEXNR                                       
348200                   IN-SOEK-KDHOM                                          
348300                   IN-SOEK-IDRUBNR(1)                                     
348400                   IN-SOEK-IDRUBNR(2)                                     
348500                   IN-SOEK-IDRUBNR(3)                                     
348600                   IN-SOEK-IDFOTNR(1)                                     
348700                   IN-SOEK-IDFOTNR(2)                                     
348800                   IN-SOEK-IDFOTNR(3)                                     
348900     MOVE SPACE TO IN-SOEK-KDPS                                           
349000                   IN-SOEK-TEKATANM                                       
349100                   IN-SOEK-BEART                                          
349200     .                                                                    
349300     EJECT                                                                
349400 S09-FLYTTA-IN-ART2 SECTION.                                              
349500     SKIP2                                                                
349600     MOVE TO-ART-KDSEGKEY TO SPAR-ART-KDSEGKEY                            
349700     MOVE TO-ART-KDFBX    TO SPAR-ART-KDFBX                               
349800     MOVE TO-ART-IDCATPOS TO SPAR-ART-IDCATPOS                            
349900     MOVE TO-ART-IDARTNR  TO SPAR-ART-IDARTNR                             
350000     MOVE TO-ART-KVKOL(1) TO SPAR-ART-KVKOL(1)                            
350100     MOVE TO-ART-KVKOL(2) TO SPAR-ART-KVKOL(2)                            
350200     MOVE TO-ART-KVKOL(3) TO SPAR-ART-KVKOL(3)                            
350300     MOVE TO-ART-KVKOL(4) TO SPAR-ART-KVKOL(4)                            
350400     MOVE TO-ART-KVKOL(5) TO SPAR-ART-KVKOL(5)                            
350500     MOVE TO-ART-KDPS     TO SPAR-ART-KDPS                                
350600     MOVE TO-ART-KVPUNKT  TO SPAR-ART-KVPUNKT                             
350700     MOVE TO-ART-IDTTEXNR TO SPAR-ART-IDTTEXNR                            
350800     .                                                                    
350900     EJECT                                                                
351000 S10-FLYTTA-ART2 SECTION.                                                 
351100     SKIP2                                                                
351200     MOVE SPAR-ART-KDSEGKEY TO TO-ART-KDSEGKEY                            
351300     MOVE SPAR-ART-KDFBX    TO TO-ART-KDFBX                               
351400     MOVE SPAR-ART-IDCATPOS TO TO-ART-IDCATPOS                            
351500     MOVE SPAR-ART-IDARTNR  TO TO-ART-IDARTNR                             
351600     MOVE SPAR-ART-KVKOL(1) TO TO-ART-KVKOL(1)                            
351700     MOVE SPAR-ART-KVKOL(2) TO TO-ART-KVKOL(2)                            
351800     MOVE SPAR-ART-KVKOL(3) TO TO-ART-KVKOL(3)                            
351900     MOVE SPAR-ART-KVKOL(4) TO TO-ART-KVKOL(4)                            
352000     MOVE SPAR-ART-KVKOL(5) TO TO-ART-KVKOL(5)                            
352100     MOVE SPAR-ART-KDPS     TO TO-ART-KDPS                                
352200     MOVE SPAR-ART-KVPUNKT  TO TO-ART-KVPUNKT                             
352300     MOVE SPAR-ART-IDTTEXNR TO TO-ART-IDTTEXNR                            
352400     .                                                                    
352500     EJECT                                                                
352600 S20-AVS-UPPDATERING SECTION.                                             
352700     SKIP2                                                                
352800     PERFORM IMS-GET-AVS1-TEXT                                            
352900     IF SEGMENT-FINNS                                                     
353000        PERFORM IMS-ISRT-AVS2-TEXT-IO1                                    
353100     END-IF                                                               
353200                                                                          
353300     PERFORM IMS-GET-AVS1-BEN                                             
353400     IF SEGMENT-FINNS                                                     
353500        PERFORM IMS-ISRT-AVS2-BEN-IO1                                     
353600     END-IF                                                               
353700                                                                          
353800     PERFORM IMS-GET-AVS1-NOT                                             
353900     PERFORM UNTIL STATUS-WS NOT = SPACE                                  
354000        PERFORM IMS-ISRT-AVS2-NOT-IO1                                     
354100        PERFORM IMS-GET-AVS1-NOT                                          
354200     END-PERFORM                                                          
354300                                                                          
354400     PERFORM IMS-GET-AVS1-RUB                                             
354500     PERFORM UNTIL STATUS-WS NOT = SPACE                                  
354600        PERFORM IMS-ISRT-AVS2-RUB-IO1                                     
354700        PERFORM IMS-GET-AVS1-RUB                                          
354800     END-PERFORM                                                          
354900                                                                          
355000     PERFORM IMS-GET-AVS1-FOT                                             
355100     PERFORM UNTIL STATUS-WS NOT = SPACE                                  
355200        PERFORM IMS-ISRT-AVS2-FOT-IO1                                     
355300        PERFORM IMS-GET-AVS1-FOT                                          
355400     END-PERFORM                                                          
355500     .                                                                    
355600     EJECT                                                                
355700 S21-AVS-UPD-HAEN SECTION.                                                
355800     SKIP2                                                                
355900     PERFORM IMS-GET-AVS1-HAEN                                            
356000     IF SEGMENT-FINNS                                                     
356100        MOVE SPACE TO TO-NOT-TENOTE                                       
356200        MOVE HAEN-IDCATGRP TO TO-NOT-IDCATGRP                             
356300        MOVE HAEN-IDCATAVS TO TO-NOT-IDCATAVS                             
356400        MOVE HAEN-IDCATRAD TO TO-NOT-IDCATRAD                             
356500        MOVE +2            TO TO-NOT-IDSEGMNR                             
356600        PERFORM IMS-ISRT-AVS2-NOT-HAEN                                    
356700     END-IF                                                               
356800     .                                                                    
356900     EJECT                                                                
357000 S25-PLATS-FINNS-EJ SECTION.                                              
357100     SKIP2                                                                
357200     MOVE RAD-IDCATRAD TO FULL-IDCATRAD                                   
357300     MOVE WS-FULL-TEXT TO FULL-TEXT                                       
357400                                                                          
357500     MOVE WS-FULLT-MEDDELANDE TO  MOD-TEMFSINF                            
357600     .                                                                    
357700     EJECT                                                                
357800 S30-FLYTTA-TO-PROG SECTION.                                              
357900     SKIP2                                                                
358000     MOVE MID-W1I55201 TO MOD-MID-W1I55201                                
358100     IF RAD-KDCATPUB-FOM = LOW-VALUE                                      
358200        MOVE SPACE              TO MOD-MID-KDCATPUB-SPAR                  
358300     ELSE                                                                 
358400        MOVE RAD-KDCATPUB-FOM   TO MOD-MID-KDCATPUB-SPAR                  
358500     END-IF                                                               
358600     PERFORM X-GRUND-FORMAT                                               
358700     IF MID-IDCATRAD-TO = 9999                                            
358800        MOVE WS-RAD-START       TO MOD-MID-IDCATRAD-SPAR                  
358900     ELSE                                                                 
359000        MOVE RAD-IDCATRAD       TO MOD-MID-IDCATRAD-FROM                  
359100        MOVE WS-RAD-TO          TO MOD-MID-IDCATRAD-TO                    
359200        MOVE WS-RAD-START       TO MOD-MID-IDCATRAD-START                 
359300        MOVE MFS-RENSA-FAELT    TO MOD-MID-IDCATRAD-SPAR                  
359400     END-IF                                                               
359500     .                                                                    
359600     EJECT                                                                
359700 S95-KOLLA-POS SECTION.                                                   
359800                                                                          
359900     MOVE NEJ TO SW-NUM1 SW-NUM2 SW-NUM3                                  
360000                 SW-ALFA                                                  
360100     MOVE SPACE TO KOLL-POS KOLL-POS-ALFA                                 
360200     MOVE ZERO TO KOLL-POS-NUM1 KOLL-POS-NUM2 KOLL-POS-NUM3               
360300                                                                          
360400     MOVE KOLL-POSX TO KOLL-POS                                           
360500     IF KOLL-POS1 NUMERIC                                                 
360600        IF KOLL-POS2 NUMERIC                                              
360700           IF KOLL-POS3 NUMERIC                                           
360800              MOVE KOLL-POS(1:3) TO KOLL-POS-NUM3                         
360900              MOVE JA TO SW-NUM3                                          
361000           ELSE                                                           
361100              MOVE KOLL-POS(1:2) TO KOLL-POS-NUM2                         
361200              MOVE JA TO SW-NUM2                                          
361300              MOVE KOLL-POS3 TO KOLL-POS-ALFA                             
361400           END-IF                                                         
361500        ELSE                                                              
361600           MOVE KOLL-POS1 TO KOLL-POS-NUM1                                
361700           MOVE JA TO SW-NUM1                                             
361800           MOVE KOLL-POS(2:2) TO KOLL-POS-ALFA                            
361900        END-IF                                                            
362000     END-IF                                                               
362100                                                                          
362200     IF SW-NUM3 = JA                                                      
362300        MOVE KOLL-POS-NUM3 TO TEST-POS-NUM                                
362400     ELSE                                                                 
362500        IF SW-NUM2 = JA                                                   
362600           MOVE KOLL-POS-NUM2 TO TEST-POS-NUM                             
362700        ELSE                                                              
362800           IF SW-NUM1 = JA                                                
362900              MOVE KOLL-POS-NUM1 TO TEST-POS-NUM                          
363000           END-IF                                                         
363100        END-IF                                                            
363200     END-IF                                                               
363300     MOVE KOLL-POS-ALFA TO TEST-POS-ALFA                                  
363400     .                                                                    
363500     EJECT                                                                
363600 S96-SOEK-AKTUELL-RAD SECTION.                                            
363700                                                                          
363800     MOVE NEJ TO AKTUELL-RAD                                              
363900     MOVE LOW-VALUE TO SPAR-KDCATPUB                                      
364000                                                                          
364100     PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM                                   
364200     PERFORM UNTIL SEGMENT-SAKNAS OR AKTUELL-RAD = JA                     
364300        MOVE MID-KDCATPUB-R-FROM-FOM                                      
364400                             TO WS-KDCATPUB-R-AVV                         
364500        PERFORM S50-Y2K-KDCATPUB-R                                        
364600        IF RAD-KDCATPUB-FOM <= WS-KDCATPUB-AAAAVV                         
364700           IF RAD-KDCATPUB-TOM >= WS-KDCATPUB-AAAAVV                      
364800              IF RAD-KDCATPUB-FOM >= SPAR-KDCATPUB                        
364900                 MOVE JA TO AKTUELL-RAD                                   
365000              END-IF                                                      
365100           END-IF                                                         
365200        END-IF                                                            
365300        IF AKTUELL-RAD = NEJ                                              
365400           PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM                             
365500        END-IF                                                            
365600     END-PERFORM                                                          
365700     IF AKTUELL-RAD = JA                                                  
365800        MOVE SPACE TO STATUS-WS                                           
365900     END-IF                                                               
366000     .                                                                    
366100     EJECT                                                                
366200 S97-SOEK-AKTUELL-RAD-POS SECTION.                                        
366300                                                                          
366400     MOVE NEJ TO AKTUELL-RAD                                              
366500     MOVE LOW-VALUE TO SPAR-KDCATPUB                                      
366600     PERFORM IMS-GU-AVS3-FROM                                             
366700                                                                          
366800     PERFORM IMS-GNP-AVS1-RAD-ART-FROM                                    
366900     PERFORM UNTIL SEGMENT-SAKNAS OR (AKTUELL-RAD = JA)                   
367000        MOVE AVS1-IDCATRAD      TO W-IDCATRAD-FROM3                       
367100        MOVE AVS1-KDCATPUB-FOM  TO W-KDCATPUB-FROM3                       
367200        PERFORM IMS-GNP-AVS1-RAD-FROM-FIRST                               
367300        MOVE MID-KDCATPUB-R-FROM-FOM                                      
367400                             TO WS-KDCATPUB-R-AVV                         
367500        PERFORM S50-Y2K-KDCATPUB-R                                        
367600        IF KOLL-RAD-KDCATPUB-FOM <= WS-KDCATPUB-AAAAVV                    
367700           IF KOLL-RAD-KDCATPUB-TOM >= WS-KDCATPUB-AAAAVV                 
367800              IF KOLL-RAD-KDCATPUB-FOM >= SPAR-KDCATPUB                   
367900                 MOVE JA TO AKTUELL-RAD                                   
368000              END-IF                                                      
368100           END-IF                                                         
368200        END-IF                                                            
368300        IF AKTUELL-RAD = NEJ                                              
368400           PERFORM IMS-GNP-AVS1-RAD-ART-FROM                              
368500        END-IF                                                            
368600     END-PERFORM                                                          
368700     IF AKTUELL-RAD = JA                                                  
368800        MOVE SPACE TO STATUS-WS                                           
368900     END-IF                                                               
369000     .                                                                    
369100     EJECT                                                                
369200 S98-KOLLA-TIDSINTERVALL SECTION.                                         
369300                                                                          
369400     MOVE JA TO NY-PUB-OK                                                 
369500     MOVE +1 TO KOLL-IX                                                   
369600     PERFORM UNTIL KOLL-IX > KOLL-IX-MAX                                  
369700        MOVE ZERO TO KOLL-IDCATRAD(KOLL-IX)                               
369800        MOVE HIGH-VALUE TO KOLL-KDCATPUB-FOM(KOLL-IX)                     
369900                           KOLL-KDCATPUB-TOM(KOLL-IX)                     
370000        ADD +1 TO KOLL-IX                                                 
370100     END-PERFORM                                                          
370200                                                                          
370300     IF POS-UPD = JA                                                      
370400        CONTINUE                                                          
370500     ELSE                                                                 
370600        IF WS-RAD-TO = +9999                                              
370700           MOVE RAD-IDCATRAD TO TO-RAD-IDCATRAD                           
370800        ELSE                                                              
370900           MOVE WS-RAD-START TO TO-RAD-IDCATRAD                           
371000        END-IF                                                            
371100     END-IF                                                               
371200     MOVE TO-RAD-IDCATRAD TO W-IDCATRAD-TO                                
371300                                                                          
371400     PERFORM IMS-GU-AVS2-TO                                               
371500     MOVE +1 TO KOLL-IX                                                   
371600     PERFORM IMS-GHNP-AVS2-RAD                                            
371700     IF SEGMENT-SAKNAS                                                    
371800        MOVE JA TO NY-PUB-OK                                              
371900     ELSE                                                                 
372000        PERFORM UNTIL SEGMENT-SAKNAS OR KOLL-IX > KOLL-IX-MAX             
372100           MOVE TO-RAD-IDCATRAD TO KOLL-IDCATRAD(KOLL-IX)                 
372200           MOVE TO-RAD-KDCATPUB-FOM TO KOLL-KDCATPUB-FOM(KOLL-IX)         
372300           MOVE TO-RAD-KDCATPUB-TOM TO KOLL-KDCATPUB-TOM(KOLL-IX)         
372400           ADD +1 TO KOLL-IX                                              
372500           PERFORM IMS-GHNP-AVS2-RAD                                      
372600        END-PERFORM                                                       
372700                                                                          
372800        IF MID-KDCATPUB-R-FROM-FOM = ALL '+'                              
372900           IF POS-UPD = JA                                                
373000              MOVE WS-KDCATPUB-TO-FOM TO NY-KDCATPUB-FOM                  
373100              MOVE WS-KDCATPUB-TO-TOM TO NY-KDCATPUB-TOM                  
373200           ELSE                                                           
373300              MOVE RAD-KDCATPUB-FOM TO NY-KDCATPUB-FOM                    
373400              MOVE RAD-KDCATPUB-TOM TO NY-KDCATPUB-TOM                    
373500           END-IF                                                         
373600        ELSE                                                              
373700           MOVE WS-KDCATPUB-TO-FOM TO NY-KDCATPUB-FOM                     
373800           MOVE WS-KDCATPUB-TO-TOM TO NY-KDCATPUB-TOM                     
373900        END-IF                                                            
374000                                                                          
374100        MOVE +1 TO KOLL-IX                                                
374200        PERFORM UNTIL KOLL-IX > KOLL-IX-MAX                               
374300           IF NY-KDCATPUB-FOM = KOLL-KDCATPUB-FOM(KOLL-IX)                
374400              MOVE KOLL-IX-MAX TO KOLL-IX                                 
374500              ADD +1 TO KOLL-IX                                           
374600              MOVE NEJ TO NY-PUB-OK                                       
374700           ELSE                                                           
374800              IF NY-KDCATPUB-FOM > KOLL-KDCATPUB-FOM(KOLL-IX)             
374900                ADD +1 TO KOLL-IX                                         
375000              ELSE                                                        
375100                 IF NY-KDCATPUB-FOM < KOLL-KDCATPUB-FOM(KOLL-IX)          
375200                    IF KOLL-IX = +1                                       
375300                       MOVE KOLL-IX TO KOLL-IX-MAX                        
375400                       ADD +1 TO KOLL-IX                                  
375500                    ELSE                                                  
375600                      ADD -1 TO KOLL-IX                                   
375700                      IF NY-KDCATPUB-FOM > KOLL-KDCATPUB-TOM              
375800                                 (KOLL-IX)                                
375900                         ADD +2 TO KOLL-IX                                
376000                         IF NY-KDCATPUB-TOM < KOLL-KDCATPUB-FOM           
376100                                         (KOLL-IX)                        
376200                            MOVE KOLL-IX-MAX TO KOLL-IX                   
376300                            ADD +1 TO KOLL-IX                             
376400                         ELSE                                             
376500                           IF NY-KDCATPUB-TOM = HIGH-VALUE                
376600                              AND KOLL-KDCATPUB-FOM(KOLL-IX)              
376700                              = HIGH-VALUE                                
376800                               MOVE KOLL-IX-MAX TO KOLL-IX                
376900                               ADD +1 TO KOLL-IX                          
377000                            END-IF                                        
377100                         END-IF                                           
377200                      ELSE                                                
377300                         MOVE KOLL-IX-MAX TO KOLL-IX                      
377400                         ADD +1 TO KOLL-IX                                
377500                         MOVE NEJ TO NY-PUB-OK                            
377600                      END-IF                                              
377700                    END-IF                                                
377800                 END-IF                                                   
377900              END-IF                                                      
378000           END-IF                                                         
378100        END-PERFORM                                                       
378200     END-IF                                                               
378300     .                                                                    
378400     EJECT                                                                
378500 S99-SOEK-AKTUELL-RAD SECTION.                                            
378600                                                                          
378700     MOVE NEJ TO AKTUELL-RAD                                              
378800     MOVE LOW-VALUE TO SPAR-KDCATPUB                                      
378900                                                                          
379000     PERFORM IMS-GET-AVS1-RAD                                             
379100     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
379200       (RAD-IDCATRAD NOT = W-IDCATRAD-FROM2)                              
379300         PERFORM IMS-GET-AVS1-RAD                                         
379400     END-PERFORM                                                          
379500                                                                          
379600     IF SEGMENT-FINNS                                                     
379700        MOVE RAD-IDCATRAD TO W-IDCATRAD-FROM                              
379800        PERFORM IMS-GET-AVS1-RAD-FROM-FIRST                               
379900        PERFORM UNTIL SEGMENT-SAKNAS OR AKTUELL-RAD = JA                  
380000           MOVE MID-KDCATPUB-R-FROM-FOM                                   
380100                             TO WS-KDCATPUB-R-AVV                         
380200           PERFORM S50-Y2K-KDCATPUB-R                                     
380300           IF RAD-KDCATPUB-FOM <= WS-KDCATPUB-AAAAVV                      
380400              IF RAD-KDCATPUB-TOM >= WS-KDCATPUB-AAAAVV                   
380500                 IF RAD-KDCATPUB-FOM >= SPAR-KDCATPUB                     
380600                    MOVE JA TO AKTUELL-RAD                                
380700                 END-IF                                                   
380800              END-IF                                                      
380900           END-IF                                                         
381000           IF AKTUELL-RAD = NEJ                                           
381100********** RÄTTAT FEL - KANSKE                                            
381200*             PERFORM IMS-GNP-AVS1-RAD-FROM                               
381300********** RÄTTAT FEL - KANSKE                                            
381400              PERFORM IMS-GNP-AVS1-NEXT-RAD-FROM                          
381500           END-IF                                                         
381600        END-PERFORM                                                       
381700     END-IF                                                               
381800     IF AKTUELL-RAD = JA                                                  
381900        MOVE SPACE TO STATUS-WS                                           
382000     END-IF                                                               
382100     .                                                                    
382200     EJECT                                                                
382300 X-GRUND-FORMAT SECTION.                                                  
382400                                                                          
382500     MOVE MFS-NUM-FAELT-RAETT TO  MOD-IDCATNR-FROM-ATTR                   
382600                                  MOD-IDCATGRP-FROM-ATTR                  
382700                                  MOD-IDCATAVS-FROM-ATTR                  
382800                                  MOD-IDCATNR-TO-ATTR                     
382900                                  MOD-IDCATGRP-TO-ATTR                    
383000                                  MOD-IDCATAVS-TO-ATTR                    
383100                                  MOD-IDCATRAD-FROM-ATTR                  
383200                                  MOD-IDCATRAD-TO-ATTR                    
383300                                  MOD-IDCATRAD-START-ATTR                 
383400     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDCATPOS-FROM-ATTR                  
383500                                  MOD-IDCATPOS-TO-ATTR                    
383600                                  MOD-IDKOL-FROM-ATTR                     
383700                                  MOD-IDKOL-TO-ATTR                       
383800                                  MOD-KDCATPUB-R-FROM-FOM-ATTR            
383900                                  MOD-KDCATPUB-R-TO-FOM-ATTR              
384000                                  MOD-KDCATPUB-R-TO-TOM-ATTR              
384100     .                                                                    
384200     EJECT                                                                
384300*                                                                         
384400* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
384500* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
384600*                                                                         
384700*    -COPY W150Y2K1                                                       
384800     EJECT                                                                
384900* IMS SEKTIONER                                                           
385000                                                                          
385100 IMS-GET-MSG SECTION.                                                     
385200     MOVE '  QC' TO GODK-STATUSKODER                                      
385300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
385400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
385500     PERFORM IMS-STATUSKONTROLL                                           
385600     SKIP3                                                                
385700     .                                                                    
385800 IMS-INSERT-MSG SECTION.                                                  
385900     IF ENGLISH-TEXT                                                      
386000         MOVE 'N' TO MFS-KDHUVOMR                                         
386100     END-IF                                                               
386200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
386300     MOVE SPACE TO GODK-STATUSKODER                                       
386400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
386500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
386600     PERFORM IMS-STATUSKONTROLL                                           
386700     .                                                                    
386800     EJECT                                                                
386900 IMS-INSERT-ALT-MSG SECTION.                                              
387000     MOVE SPACE TO GODK-STATUSKODER                                       
387100     IF ENGLISH-TEXT                                                      
387200         MOVE '2' TO M-SW-KDMFSFOR                                        
387300     END-IF                                                               
387400     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
387500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
387600     PERFORM IMS-STATUSKONTROLL                                           
387700     .                                                                    
387800     SKIP2                                                                
387900 IMS-ROLLBACK     SECTION.                                                
388000     MOVE '  ' TO GODK-STATUSKODER                                        
388100     CALL CBLTDLI USING ROLB MSG-PCB                                      
388200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
388300     PERFORM IMS-STATUSKONTROLL                                           
388400     .                                                                    
388500     EJECT                                                                
388600 IMS-GU-AVS1-FROM SECTION.                                                
388700     STRING 'WLKATH01(WDN501KY =' W-WDN501-FROM-X ')'                     
388800            DELIMITED BY SIZE INTO SSA1                                   
388900     MOVE '  GE' TO GODK-STATUSKODER                                      
389000     CALL CBLTDLI USING GU AVS1-PCB IO-AREA-1 SSA1                        
389100     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
389200     PERFORM IMS-STATUSKONTROLL                                           
389300     .                                                                    
389400     SKIP2                                                                
389500 IMS-GU-AVS3-FROM SECTION.                                                
389600     STRING 'WLKATH01(WDN501KY =' W-WDN501-FROM-X ')'                     
389700            DELIMITED BY SIZE INTO SSA1                                   
389800     MOVE '  GE' TO GODK-STATUSKODER                                      
389900     CALL CBLTDLI USING GU AVS3-PCB IO-AREA-3 SSA1                        
390000     MOVE AVS3-STATUS-CODE TO STATUS-WS                                   
390100     PERFORM IMS-STATUSKONTROLL                                           
390200     .                                                                    
390300     SKIP2                                                                
390400 IMS-GU-AVS2-TO SECTION.                                                  
390500     STRING 'WLKATH01(WDN501KY =' W-WDN501-TO-X ')'                       
390600            DELIMITED BY SIZE INTO SSA1                                   
390700     MOVE '  GE' TO GODK-STATUSKODER                                      
390800     CALL CBLTDLI USING GU AVS2-PCB IO-AREA-2 SSA1                        
390900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
391000     PERFORM IMS-STATUSKONTROLL                                           
391100     .                                                                    
391200     EJECT                                                                
391300 IMS-GNP-AVS1-ILLU SECTION.                                               
391400     STRING 'WLKATH01(WDN501KY =' W-WDN501-FROM-X ')'                     
391500            DELIMITED BY SIZE INTO SSA1                                   
391600     MOVE 'WLKATH11 ' TO SSA2                                             
391700     MOVE '  GE' TO GODK-STATUSKODER                                      
391800     CALL CBLTDLI USING GNP AVS1-PCB IO-AREA-1 SSA1 SSA2                  
391900     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
392000     PERFORM IMS-STATUSKONTROLL                                           
392100     .                                                                    
392200     SKIP2                                                                
392300 IMS-GNP-AVS2-ILLU SECTION.                                               
392400     STRING 'WLKATH01(WDN501KY =' W-WDN501-TO-X ')'                       
392500            DELIMITED BY SIZE INTO SSA1                                   
392600     MOVE 'WLKATH11 ' TO SSA2                                             
392700     MOVE '  GE' TO GODK-STATUSKODER                                      
392800     CALL CBLTDLI USING GNP AVS2-PCB IO-AREA-2 SSA1 SSA2                  
392900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
393000     PERFORM IMS-STATUSKONTROLL                                           
393100     .                                                                    
393200     EJECT                                                                
393300 IMS-GNP-AVS1-RAD-FROM SECTION.                                           
393400     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-FROM-X ')'                   
393500            DELIMITED BY SIZE INTO SSA1                                   
393600     MOVE '  GE' TO GODK-STATUSKODER                                      
393700     CALL CBLTDLI USING GNP AVS1-PCB IO-AREA-1 SSA1                       
393800     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
393900     PERFORM IMS-STATUSKONTROLL                                           
394000     .                                                                    
394100     SKIP2                                                                
394200 IMS-GET-AVS1-RAD-FROM SECTION.                                           
394300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-FROM-X ')'                   
394400            DELIMITED BY SIZE INTO SSA1                                   
394500     MOVE '  GE' TO GODK-STATUSKODER                                      
394600     CALL CBLTDLI USING GNP AVS1-PCB IO-AREA-1 SSA1                       
394700     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
394800     PERFORM IMS-STATUSKONTROLL                                           
394900     .                                                                    
395000     SKIP2                                                                
395100 IMS-GNP-AVS1-RAD-FIRST SECTION.                                          
395200     STRING 'WLKATH12*F(WDN512KY =' W-WDN512KY-FROM-X ')'                 
395300            DELIMITED BY SIZE INTO SSA1                                   
395400     MOVE '  GE' TO GODK-STATUSKODER                                      
395500     CALL CBLTDLI USING GNP AVS1-PCB IO-AREA-1 SSA1                       
395600     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
395700     PERFORM IMS-STATUSKONTROLL                                           
395800     .                                                                    
395900     EJECT                                                                
396000 IMS-GNP-AVS1-RAD-FROM-FIRST SECTION.                                     
396100     STRING 'WLKATH12*F(WDN512KY =' W-WDN512KY-FROM3-X ')'                
396200            DELIMITED BY SIZE INTO SSA1                                   
396300     MOVE '  GE' TO GODK-STATUSKODER                                      
396400     CALL CBLTDLI USING GNP AVS3-PCB IO-AREA-3 SSA1                       
396500     MOVE AVS3-STATUS-CODE TO STATUS-WS                                   
396600     PERFORM IMS-STATUSKONTROLL                                           
396700     .                                                                    
396800     SKIP2                                                                
396900 IMS-GET-AVS1-RAD-FROM-FIRST SECTION.                                     
397000     STRING 'WLKATH12*F(IDCATRAD =' W-IDCATRAD-FROM-X ')'                 
397100            DELIMITED BY SIZE INTO SSA1                                   
397200     MOVE '  GE' TO GODK-STATUSKODER                                      
397300     CALL CBLTDLI USING GNP AVS1-PCB IO-AREA-1 SSA1                       
397400     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
397500     PERFORM IMS-STATUSKONTROLL                                           
397600     .                                                                    
397700     SKIP2                                                                
397800 IMS-GNP-AVS1-NEXT-RAD-FROM SECTION.                                      
397900     STRING 'WLKATH12(IDCATRAD>=' W-IDCATRAD-FROM-X ')'                   
398000            DELIMITED BY SIZE INTO SSA1                                   
398100     MOVE '  GE' TO GODK-STATUSKODER                                      
398200     CALL CBLTDLI USING GNP AVS1-PCB IO-AREA-1 SSA1                       
398300     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
398400     PERFORM IMS-STATUSKONTROLL                                           
398500     .                                                                    
398600     EJECT                                                                
398700 IMS-GHNP-AVS1-RAD SECTION.                                               
398800     STRING 'WLKATH12(IDCATRAD=>' W-IDCATRAD-FROM-MIN-X                   
398900                    '&IDCATRAD=<' W-IDCATRAD-FROM-MAX-X ')'               
399000            DELIMITED BY SIZE INTO SSA1                                   
399100     MOVE '  GE' TO GODK-STATUSKODER                                      
399200     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1                      
399300     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
399400     PERFORM IMS-STATUSKONTROLL                                           
399500     .                                                                    
399600     SKIP2                                                                
399700 IMS-GET-AVS1-RAD SECTION.                                                
399800     STRING 'WLKATH12(WDN512KY=>' W-WDN512KY-FROM-MIN-F-X                 
399900                    '&WDN512KY=<' W-WDN512KY-FROM-MAX-F-X ')'             
400000            DELIMITED BY SIZE INTO SSA1                                   
400100     MOVE '  GE' TO GODK-STATUSKODER                                      
400200     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1                      
400300     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
400400     PERFORM IMS-STATUSKONTROLL                                           
400500     .                                                                    
400600     SKIP2                                                                
400700 IMS-GET-AVS2-RAD SECTION.                                                
400800     STRING 'WLKATH12(IDCATRAD=>' W-IDCATRAD-FROM2-MIN-X                  
400900                    '&IDCATRAD=<' W-IDCATRAD-FROM2-MAX-X ')'              
401000            DELIMITED BY SIZE INTO SSA1                                   
401100     MOVE '  GE' TO GODK-STATUSKODER                                      
401200     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-1 SSA1                      
401300     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
401400     PERFORM IMS-STATUSKONTROLL                                           
401500     .                                                                    
401600     EJECT                                                                
401700 IMS-GNP-AVS1-RAD-ART-FROM SECTION.                                       
401800     STRING 'WLKATH12(IDCATRAD>=' W-IDCATRAD-FROM-X ')'                   
401900            DELIMITED BY SIZE INTO SSA1                                   
402000     STRING 'WLKATH21(IDCATPOS =' W-IDCATPOS-FROM-X ')'                   
402100            DELIMITED BY SIZE INTO SSA2                                   
402200     MOVE '  GE' TO GODK-STATUSKODER                                      
402300     CALL CBLTDLI USING GNP AVS1-PCB IO-AREA-1 SSA1 SSA2                  
402400     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
402500     PERFORM IMS-STATUSKONTROLL                                           
402600     .                                                                    
402700     SKIP2                                                                
402800 IMS-GNP-AVS2-RAD-ART-TO SECTION.                                         
402900     STRING 'WLKATH12(IDCATRAD>=' W-IDCATRAD-TO-X ')'                     
403000            DELIMITED BY SIZE INTO SSA1                                   
403100     STRING 'WLKATH21(IDCATPOS =' W-IDCATPOS-TO-X ')'                     
403200            DELIMITED BY SIZE INTO SSA2                                   
403300     MOVE '  GE' TO GODK-STATUSKODER                                      
403400     CALL CBLTDLI USING GNP AVS2-PCB IO-AREA-2 SSA1 SSA2                  
403500     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
403600     PERFORM IMS-STATUSKONTROLL                                           
403700     .                                                                    
403800     EJECT                                                                
403900 IMS-GNP-AVS2-RAD-TO SECTION.                                             
404000     STRING 'WLKATH12(IDCATRAD>=' W-IDCATRAD-TO-X ')'                     
404100            DELIMITED BY SIZE INTO SSA1                                   
404200     MOVE '  GE' TO GODK-STATUSKODER                                      
404300     CALL CBLTDLI USING GNP AVS2-PCB IO-AREA-2 SSA1                       
404400     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
404500     PERFORM IMS-STATUSKONTROLL                                           
404600     .                                                                    
404700     SKIP2                                                                
404800 IMS-GET-AVS2-RAD-TO SECTION.                                             
404900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
405000            DELIMITED BY SIZE INTO SSA1                                   
405100     MOVE '  GE' TO GODK-STATUSKODER                                      
405200     CALL CBLTDLI USING GNP AVS2-PCB IO-AREA-2 SSA1                       
405300     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
405400     PERFORM IMS-STATUSKONTROLL                                           
405500     .                                                                    
405600     SKIP2                                                                
405700 IMS-GET-AVS2-RAD-TO-NASTA SECTION.                                       
405800     STRING 'WLKATH12(WDN512KY>=' W-WDN512KY-TO-X ')'                     
405900            DELIMITED BY SIZE INTO SSA1                                   
406000     MOVE '  GE' TO GODK-STATUSKODER                                      
406100     CALL CBLTDLI USING GNP AVS2-PCB IO-AREA-2 SSA1                       
406200     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
406300     PERFORM IMS-STATUSKONTROLL                                           
406400     .                                                                    
406500     EJECT                                                                
406600 IMS-GHNP-AVS2-RAD SECTION.                                               
406700     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-TO-X ')'                     
406800            DELIMITED BY SIZE INTO SSA1                                   
406900     MOVE '  GE' TO GODK-STATUSKODER                                      
407000     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1                      
407100     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
407200     PERFORM IMS-STATUSKONTROLL                                           
407300     .                                                                    
407400     SKIP2                                                                
407500 IMS-GHNP-AVS2-RAD-FIRST SECTION.                                         
407600     STRING 'WLKATH12*F(IDCATRAD =' W-IDCATRAD-TO-X ')'                   
407700            DELIMITED BY SIZE INTO SSA1                                   
407800     MOVE '  GE' TO GODK-STATUSKODER                                      
407900     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1                      
408000     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
408100     PERFORM IMS-STATUSKONTROLL                                           
408200     .                                                                    
408300     SKIP2                                                                
408400 IMS-GET-AVS2-RAD-FIRST SECTION.                                          
408500     STRING 'WLKATH12*F(WDN512KY =' W-WDN512KY-TO-X ')'                   
408600            DELIMITED BY SIZE INTO SSA1                                   
408700     MOVE '  GE' TO GODK-STATUSKODER                                      
408800     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1                      
408900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
409000     PERFORM IMS-STATUSKONTROLL                                           
409100     .                                                                    
409200     SKIP2                                                                
409300 IMS-ISRT-AVS2-RUBRIKER SECTION.                                          
409400     MOVE 'WLKATH12 ' TO SSA1                                             
409500     MOVE '  II' TO GODK-STATUSKODER                                      
409600     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1                      
409700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
409800     PERFORM IMS-STATUSKONTROLL                                           
409900     .                                                                    
410000     EJECT                                                                
410100 IMS-ISRT-AVS2-RAD SECTION.                                               
410200     MOVE 'WLKATH12 ' TO SSA1                                             
410300     MOVE '  ' TO GODK-STATUSKODER                                        
410400     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-2 SSA1                      
410500     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
410600     PERFORM IMS-STATUSKONTROLL                                           
410700     .                                                                    
410800     SKIP2                                                                
410900 IMS-GHNP-AVS1-ART SECTION.                                               
411000     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-FROM-X ')'                   
411100            DELIMITED BY SIZE INTO SSA1                                   
411200     MOVE 'WLKATH21 ' TO SSA2                                             
411300     MOVE '  GE' TO GODK-STATUSKODER                                      
411400     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
411500     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
411600     PERFORM IMS-STATUSKONTROLL                                           
411700     .                                                                    
411800     SKIP2                                                                
411900 IMS-GET-AVS1-ART SECTION.                                                
412000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-FROM-X ')'                   
412100            DELIMITED BY SIZE INTO SSA1                                   
412200     MOVE 'WLKATH21 ' TO SSA2                                             
412300     MOVE '  GE' TO GODK-STATUSKODER                                      
412400     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
412500     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
412600     PERFORM IMS-STATUSKONTROLL                                           
412700     .                                                                    
412800     EJECT                                                                
412900 IMS-GET-AVS1-ART-FIRST SECTION.                                          
413000     STRING 'WLKATH12*F(WDN512KY =' W-WDN512KY-FROM-X ')'                 
413100            DELIMITED BY SIZE INTO SSA1                                   
413200     MOVE 'WLKATH21 ' TO SSA2                                             
413300     MOVE '  GE' TO GODK-STATUSKODER                                      
413400     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
413500     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
413600     PERFORM IMS-STATUSKONTROLL                                           
413700     .                                                                    
413800     SKIP3                                                                
413900 IMS-GHNP-AVS2-ART SECTION.                                               
414000     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-TO-X ')'                     
414100            DELIMITED BY SIZE INTO SSA1                                   
414200     MOVE 'WLKATH21 ' TO SSA2                                             
414300     MOVE '  GE' TO GODK-STATUSKODER                                      
414400     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
414500     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
414600     PERFORM IMS-STATUSKONTROLL                                           
414700     .                                                                    
414800     SKIP2                                                                
414900 IMS-GET-AVS2-ART SECTION.                                                
415000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
415100            DELIMITED BY SIZE INTO SSA1                                   
415200     MOVE 'WLKATH21 ' TO SSA2                                             
415300     MOVE '  GE' TO GODK-STATUSKODER                                      
415400     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
415500     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
415600     PERFORM IMS-STATUSKONTROLL                                           
415700     .                                                                    
415800     EJECT                                                                
415900 IMS-ISRT-AVS2-ART SECTION.                                               
416000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
416100            DELIMITED BY SIZE INTO SSA1                                   
416200     MOVE 'WLKATH21 ' TO SSA2                                             
416300     MOVE '  ' TO GODK-STATUSKODER                                        
416400     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
416500     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
416600     PERFORM IMS-STATUSKONTROLL                                           
416700     .                                                                    
416800     SKIP3                                                                
416900 IMS-ISRT-AVS2-ART-KOL SECTION.                                           
417000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
417100            DELIMITED BY SIZE INTO SSA1                                   
417200     MOVE 'WLKATH21 ' TO SSA2                                             
417300     MOVE '  ' TO GODK-STATUSKODER                                        
417400     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-2 SSA1 SSA2                 
417500     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
417600     PERFORM IMS-STATUSKONTROLL                                           
417700     .                                                                    
417800     EJECT                                                                
417900 IMS-GHNP-AVS1-TEXT SECTION.                                              
418000     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-FROM-X ')'                   
418100            DELIMITED BY SIZE INTO SSA1                                   
418200     MOVE 'WLKATH22 ' TO SSA2                                             
418300     MOVE '  GE' TO GODK-STATUSKODER                                      
418400     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
418500     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
418600     PERFORM IMS-STATUSKONTROLL                                           
418700     .                                                                    
418800     SKIP2                                                                
418900 IMS-GET-AVS1-TEXT SECTION.                                               
419000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-FROM-X ')'                   
419100            DELIMITED BY SIZE INTO SSA1                                   
419200     MOVE 'WLKATH22 ' TO SSA2                                             
419300     MOVE '  GE' TO GODK-STATUSKODER                                      
419400     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
419500     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
419600     PERFORM IMS-STATUSKONTROLL                                           
419700     .                                                                    
419800     SKIP2                                                                
419900 IMS-GHNP-AVS2-TEXT SECTION.                                              
420000     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-TO-X ')'                     
420100            DELIMITED BY SIZE INTO SSA1                                   
420200     MOVE 'WLKATH22 ' TO SSA2                                             
420300     MOVE '  GE' TO GODK-STATUSKODER                                      
420400     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
420500     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
420600     PERFORM IMS-STATUSKONTROLL                                           
420700     .                                                                    
420800     EJECT                                                                
420900 IMS-GET-AVS2-TEXT SECTION.                                               
421000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
421100            DELIMITED BY SIZE INTO SSA1                                   
421200     MOVE 'WLKATH22 ' TO SSA2                                             
421300     MOVE '  GE' TO GODK-STATUSKODER                                      
421400     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
421500     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
421600     PERFORM IMS-STATUSKONTROLL                                           
421700     .                                                                    
421800     SKIP2                                                                
421900 IMS-ISRT-AVS2-RUB-TEXT-IO1 SECTION.                                      
422000*    STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-TO-X ')'                     
422100*           DELIMITED BY SIZE INTO SSA1                                   
422200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
422300            DELIMITED BY SIZE INTO SSA1                                   
422400     MOVE 'WLKATH22 ' TO SSA2                                             
422500     MOVE '  II' TO GODK-STATUSKODER                                      
422600     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
422700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
422800     PERFORM IMS-STATUSKONTROLL                                           
422900     .                                                                    
423000     EJECT                                                                
423100 IMS-ISRT-AVS2-TEXT-IO1 SECTION.                                          
423200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
423300            DELIMITED BY SIZE INTO SSA1                                   
423400     MOVE 'WLKATH22 ' TO SSA2                                             
423500     MOVE '  ' TO GODK-STATUSKODER                                        
423600     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
423700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
423800     PERFORM IMS-STATUSKONTROLL                                           
423900     .                                                                    
424000     SKIP2                                                                
424100 IMS-ISRT-AVS2-TEXT-IO2 SECTION.                                          
424200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
424300            DELIMITED BY SIZE INTO SSA1                                   
424400     MOVE 'WLKATH22 ' TO SSA2                                             
424500     MOVE '  ' TO GODK-STATUSKODER                                        
424600     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-2 SSA1 SSA2                 
424700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
424800     PERFORM IMS-STATUSKONTROLL                                           
424900     .                                                                    
425000     EJECT                                                                
425100 IMS-GHNP-AVS1-BEN SECTION.                                               
425200     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-FROM-X ')'                   
425300            DELIMITED BY SIZE INTO SSA1                                   
425400     MOVE 'WLKATH23 ' TO SSA2                                             
425500     MOVE '  GE' TO GODK-STATUSKODER                                      
425600     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
425700     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
425800     PERFORM IMS-STATUSKONTROLL                                           
425900     .                                                                    
426000     SKIP2                                                                
426100 IMS-GET-AVS1-BEN SECTION.                                                
426200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-FROM-X ')'                   
426300            DELIMITED BY SIZE INTO SSA1                                   
426400     MOVE 'WLKATH23 ' TO SSA2                                             
426500     MOVE '  GE' TO GODK-STATUSKODER                                      
426600     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
426700     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
426800     PERFORM IMS-STATUSKONTROLL                                           
426900     .                                                                    
427000     SKIP2                                                                
427100 IMS-GHNP-AVS2-BEN SECTION.                                               
427200     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-TO-X ')'                     
427300            DELIMITED BY SIZE INTO SSA1                                   
427400     MOVE 'WLKATH23 ' TO SSA2                                             
427500     MOVE '  GE' TO GODK-STATUSKODER                                      
427600     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
427700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
427800     PERFORM IMS-STATUSKONTROLL                                           
427900     .                                                                    
428000     EJECT                                                                
428100 IMS-GET-AVS2-BEN SECTION.                                                
428200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
428300            DELIMITED BY SIZE INTO SSA1                                   
428400     MOVE 'WLKATH23 ' TO SSA2                                             
428500     MOVE '  GE' TO GODK-STATUSKODER                                      
428600     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
428700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
428800     PERFORM IMS-STATUSKONTROLL                                           
428900     .                                                                    
429000     EJECT                                                                
429100 IMS-ISRT-AVS2-BEN-IO1 SECTION.                                           
429200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
429300            DELIMITED BY SIZE INTO SSA1                                   
429400     MOVE 'WLKATH23 ' TO SSA2                                             
429500     MOVE '  ' TO GODK-STATUSKODER                                        
429600     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
429700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
429800     PERFORM IMS-STATUSKONTROLL                                           
429900     .                                                                    
430000     SKIP2                                                                
430100 IMS-ISRT-AVS2-BEN-IO2 SECTION.                                           
430200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
430300            DELIMITED BY SIZE INTO SSA1                                   
430400     MOVE 'WLKATH23 ' TO SSA2                                             
430500     MOVE '  ' TO GODK-STATUSKODER                                        
430600     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-2 SSA1 SSA2                 
430700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
430800     PERFORM IMS-STATUSKONTROLL                                           
430900     .                                                                    
431000     SKIP2                                                                
431100 IMS-GHNP-AVS1-NOT SECTION.                                               
431200     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-FROM-X ')'                   
431300            DELIMITED BY SIZE INTO SSA1                                   
431400     MOVE 'WLKATH24 ' TO SSA2                                             
431500     MOVE '  GE' TO GODK-STATUSKODER                                      
431600     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
431700     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
431800     PERFORM IMS-STATUSKONTROLL                                           
431900     .                                                                    
432000     EJECT                                                                
432100 IMS-GET-AVS1-NOT SECTION.                                                
432200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-FROM-X ')'                   
432300            DELIMITED BY SIZE INTO SSA1                                   
432400     MOVE 'WLKATH24 ' TO SSA2                                             
432500     MOVE '  GE' TO GODK-STATUSKODER                                      
432600     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
432700     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
432800     PERFORM IMS-STATUSKONTROLL                                           
432900     .                                                                    
433000     SKIP2                                                                
433100 IMS-ISRT-AVS2-NOT-IO1 SECTION.                                           
433200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
433300            DELIMITED BY SIZE INTO SSA1                                   
433400     MOVE 'WLKATH24 ' TO SSA2                                             
433500     MOVE '  ' TO GODK-STATUSKODER                                        
433600     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
433700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
433800     PERFORM IMS-STATUSKONTROLL                                           
433900     .                                                                    
434000     EJECT                                                                
434100 IMS-ISRT-AVS2-NOT-IO2 SECTION.                                           
434200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
434300            DELIMITED BY SIZE INTO SSA1                                   
434400     MOVE 'WLKATH24 ' TO SSA2                                             
434500     MOVE '  ' TO GODK-STATUSKODER                                        
434600     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-2 SSA1 SSA2                 
434700     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
434800     PERFORM IMS-STATUSKONTROLL                                           
434900     .                                                                    
435000     SKIP2                                                                
435100 IMS-ISRT-AVS2-NOT-HAEN SECTION.                                          
435200     STRING 'WLKATH01(WDN501KY =' W-WDN501-TO-X ')'                       
435300            DELIMITED BY SIZE INTO SSA1                                   
435400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
435500            DELIMITED BY SIZE INTO SSA2                                   
435600     MOVE 'WLKATH24 ' TO SSA3                                             
435700     MOVE '  ' TO GODK-STATUSKODER                                        
435800     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-2 SSA1 SSA2 SSA3            
435900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
436000     PERFORM IMS-STATUSKONTROLL                                           
436100     .                                                                    
436200     EJECT                                                                
436300 IMS-GHNP-AVS1-RUB SECTION.                                               
436400     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-FROM-X ')'                   
436500            DELIMITED BY SIZE INTO SSA1                                   
436600     MOVE 'WLKATH25 ' TO SSA2                                             
436700     MOVE '  GE' TO GODK-STATUSKODER                                      
436800     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
436900     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
437000     PERFORM IMS-STATUSKONTROLL                                           
437100     .                                                                    
437200     SKIP2                                                                
437300 IMS-GET-AVS1-RUB SECTION.                                                
437400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-FROM-X ')'                   
437500            DELIMITED BY SIZE INTO SSA1                                   
437600     MOVE 'WLKATH25 ' TO SSA2                                             
437700     MOVE '  GE' TO GODK-STATUSKODER                                      
437800     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
437900     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
438000     PERFORM IMS-STATUSKONTROLL                                           
438100     .                                                                    
438200     SKIP2                                                                
438300 IMS-GHNP-AVS2-RUB SECTION.                                               
438400     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-TO-X ')'                     
438500            DELIMITED BY SIZE INTO SSA1                                   
438600     MOVE 'WLKATH25 ' TO SSA2                                             
438700     MOVE '  GE' TO GODK-STATUSKODER                                      
438800     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
438900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
439000     PERFORM IMS-STATUSKONTROLL                                           
439100     .                                                                    
439200     EJECT                                                                
439300 IMS-GET-AVS2-RUB SECTION.                                                
439400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
439500            DELIMITED BY SIZE INTO SSA1                                   
439600     MOVE 'WLKATH25 ' TO SSA2                                             
439700     MOVE '  GE' TO GODK-STATUSKODER                                      
439800     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
439900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
440000     PERFORM IMS-STATUSKONTROLL                                           
440100     .                                                                    
440200     SKIP2                                                                
440300 IMS-ISRT-AVS2-RUB-IO1 SECTION.                                           
440400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
440500            DELIMITED BY SIZE INTO SSA1                                   
440600     MOVE 'WLKATH25 ' TO SSA2                                             
440700     MOVE '  ' TO GODK-STATUSKODER                                        
440800     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
440900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
441000     PERFORM IMS-STATUSKONTROLL                                           
441100     .                                                                    
441200     SKIP2                                                                
441300 IMS-ISRT-AVS2-RUB-IO2 SECTION.                                           
441400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
441500            DELIMITED BY SIZE INTO SSA1                                   
441600     MOVE 'WLKATH25 ' TO SSA2                                             
441700     MOVE '  ' TO GODK-STATUSKODER                                        
441800     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-2 SSA1 SSA2                 
441900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
442000     PERFORM IMS-STATUSKONTROLL                                           
442100     .                                                                    
442200     EJECT                                                                
442300 IMS-GHNP-AVS1-FOT SECTION.                                               
442400     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-FROM-X ')'                   
442500            DELIMITED BY SIZE INTO SSA1                                   
442600     MOVE 'WLKATH26 ' TO SSA2                                             
442700     MOVE '  GE' TO GODK-STATUSKODER                                      
442800     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
442900     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
443000     PERFORM IMS-STATUSKONTROLL                                           
443100     .                                                                    
443200     EJECT                                                                
443300 IMS-GET-AVS1-FOT SECTION.                                                
443400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-FROM-X ')'                   
443500            DELIMITED BY SIZE INTO SSA1                                   
443600     MOVE 'WLKATH26 ' TO SSA2                                             
443700     MOVE '  GE' TO GODK-STATUSKODER                                      
443800     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
443900     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
444000     PERFORM IMS-STATUSKONTROLL                                           
444100     .                                                                    
444200     SKIP2                                                                
444300 IMS-GHNP-AVS2-FOT SECTION.                                               
444400     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-TO-X ')'                     
444500            DELIMITED BY SIZE INTO SSA1                                   
444600     MOVE 'WLKATH26 ' TO SSA2                                             
444700     MOVE '  GE' TO GODK-STATUSKODER                                      
444800     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
444900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
445000     PERFORM IMS-STATUSKONTROLL                                           
445100     .                                                                    
445200     EJECT                                                                
445300 IMS-GET-AVS2-FOT SECTION.                                                
445400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
445500            DELIMITED BY SIZE INTO SSA1                                   
445600     MOVE 'WLKATH26 ' TO SSA2                                             
445700     MOVE '  GE' TO GODK-STATUSKODER                                      
445800     CALL CBLTDLI USING GHNP AVS2-PCB IO-AREA-2 SSA1 SSA2                 
445900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
446000     PERFORM IMS-STATUSKONTROLL                                           
446100     .                                                                    
446200     SKIP2                                                                
446300 IMS-ISRT-AVS2-FOT-IO1 SECTION.                                           
446400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
446500            DELIMITED BY SIZE INTO SSA1                                   
446600     MOVE 'WLKATH26 ' TO SSA2                                             
446700     MOVE '  ' TO GODK-STATUSKODER                                        
446800     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
446900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
447000     PERFORM IMS-STATUSKONTROLL                                           
447100     .                                                                    
447200     SKIP2                                                                
447300 IMS-ISRT-AVS2-FOT-IO2 SECTION.                                           
447400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-TO-X ')'                     
447500            DELIMITED BY SIZE INTO SSA1                                   
447600     MOVE 'WLKATH26 ' TO SSA2                                             
447700     MOVE '  ' TO GODK-STATUSKODER                                        
447800     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-2 SSA1 SSA2                 
447900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
448000     PERFORM IMS-STATUSKONTROLL                                           
448100     .                                                                    
448200     EJECT                                                                
448300 IMS-GHNP-AVS1-HAEN SECTION.                                              
448400     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-FROM-X ')'                   
448500            DELIMITED BY SIZE INTO SSA1                                   
448600     MOVE 'WLKATH27 ' TO SSA2                                             
448700     MOVE '  GE' TO GODK-STATUSKODER                                      
448800     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
448900     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
449000     PERFORM IMS-STATUSKONTROLL                                           
449100     .                                                                    
449200     SKIP2                                                                
449300 IMS-GET-AVS1-HAEN SECTION.                                               
449400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-FROM-X ')'                   
449500            DELIMITED BY SIZE INTO SSA1                                   
449600     MOVE 'WLKATH27 ' TO SSA2                                             
449700     MOVE '  GE' TO GODK-STATUSKODER                                      
449800     CALL CBLTDLI USING GHNP AVS1-PCB IO-AREA-1 SSA1 SSA2                 
449900     MOVE AVS1-STATUS-CODE TO STATUS-WS                                   
450000     PERFORM IMS-STATUSKONTROLL                                           
450100     .                                                                    
450200     SKIP2                                                                
450300 IMS-REPL-AVS2 SECTION.                                                   
450400     MOVE '  ' TO GODK-STATUSKODER                                        
450500     CALL CBLTDLI USING REPL AVS2-PCB IO-AREA-2                           
450600     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
450700     PERFORM IMS-STATUSKONTROLL                                           
450800     .                                                                    
450900     EJECT                                                                
451000 IMS-GU-KAT SECTION.                                                      
451100     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
451200            DELIMITED BY SIZE INTO SSA1                                   
451300     MOVE '  GE' TO GODK-STATUSKODER                                      
451400     CALL CBLTDLI USING GU KAT-PCB IO-AREA-2 SSA1                         
451500     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
451600     PERFORM IMS-STATUSKONTROLL                                           
451700     .                                                                    
451800     SKIP2                                                                
451900 IMS-GNP-KATM11 SECTION.                                                  
452000     MOVE 'WLKATM11 ' TO SSA1                                             
452100     MOVE '  GE' TO GODK-STATUSKODER                                      
452200     CALL CBLTDLI USING GNP KAT-PCB IO-AREA-2 SSA1                        
452300     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
452400     PERFORM IMS-STATUSKONTROLL                                           
452500     .                                                                    
452600     SKIP2                                                                
452700 IMS-GNP-KATM11-FIRST SECTION.                                            
452800     MOVE 'WLKATM11*F' TO SSA1                                            
452900     MOVE '  GE' TO GODK-STATUSKODER                                      
453000     CALL CBLTDLI USING GNP KAT-PCB IO-AREA-2 SSA1                        
453100     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
453200     PERFORM IMS-STATUSKONTROLL                                           
453300     .                                                                    
453400     EJECT                                                                
453500 IMS-STATUSKONTROLL SECTION.                                              
453600     SET STATUS-IX TO 1                                                   
453700     SEARCH GODK-STATUS AT END CALL FELLOG                                
453800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
453900          CONTINUE                                                        
454000     END-SEARCH                                                           
454100     .                                                                    
