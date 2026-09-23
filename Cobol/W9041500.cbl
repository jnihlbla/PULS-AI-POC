000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9041500.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   APRIL 90.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*      - VISA STRUKTURRADER.                                              
001100*                                                                         
001200*      - VISAR INFORMATION OM STRUKTUREN SAMT ALLA ARTIKELRADER           
001300*        SOM INGÅR I STRUKTUREN OCH INTE ÄR HISTORIKRADER                 
001400*        - ALLTSÅ NU OCH FRAMÅT                                           
001500*                                                                         
001600****   - RAD SOM HAR STRUKTURTYP KAN MARKERAS FÖR ATT VISA RADENS         
001700****     STRUKTUR . RETURFUNKTION MED PF-TANGENT TILL URSPRUNGLIG         
001800****     STRUKTUR.                                                        
001900****                                                                      
002000****   - RAD KAN MARKERAS FÖR ATT VISA RADEN BILD 1212. RETUR MED         
002100****     MED PF-TANGENT .                                                 
002200*                                                                         
002300*      - LÄSER WDD1 BENREG (WDD3) OCH RASA                                
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W9T415                                              
002700*        MID:         W90415I1                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W90415O1                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003601*    -COPY WY2000W1                                                       
003610     SKIP3                                                                
003700 77  IDPGM                   PIC X(8)    VALUE 'W9041500'.                
003800 77  JA                      PIC X       VALUE 'J'.                       
003900 77  NEJ                     PIC X       VALUE 'N'.                       
004000 77  S-IX                    PIC S9(9)   VALUE +1   COMP SYNC.            
004100 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004200 77  SPAR-INDX               PIC S9(9)   VALUE +0   COMP SYNC.            
004300 77  SPAR-IDRADNR            PIC S9(9)   VALUE ZERO COMP-3.               
004400 77  INDXNOT                 PIC S9(9)   VALUE +0   COMP SYNC.            
004500 77  MAX-IDRAD               PIC S9(9)   VALUE +12.                       
004600 77  MAX-SELECT              PIC S9(9)   VALUE +12.                       
004700 77  MAX-TESTRNOT            PIC S9(9)   VALUE +2.                        
004900 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
005000 77  IDARTNR-KOLL-WS         PIC S9(9)   VALUE ZERO  COMP-3.              
005100 77  IDARTNR-KONV-WS         PIC S9(9)   VALUE ZERO  COMP-3.              
005200 77  IDRADNR-WS              PIC X(4)    VALUE SPACE.                     
005300 77  IDSKYLT-WS              PIC X(3)    VALUE SPACE.                     
005400 77  SPAR-UTRAD              PIC X(72)   VALUE SPACE.                     
005500 77  TEST-IDARTNR            PIC 9(9)    VALUE ZERO.                      
005600 77  SPAR-RADNR              PIC S9(5)   VALUE ZERO  COMP-3.              
005700 77  SPAR-KDBENHOM           PIC S9      VALUE ZERO  COMP-3.              
005800 77  SPAR-RAD-KDBENHOM       PIC S9      VALUE ZERO  COMP-3.              
006000 77  SATS-STR-RA             PIC X(3)    VALUE '222'.                     
006100 77  SATS-STR-RB             PIC X(3)    VALUE '223'.                     
006400 77  SATS-STR-BERPV          PIC X(3)    VALUE '226'.                     
006510 77  SATS-STR-CARP           PIC X(3)    VALUE '221'.                     
006600 77  KDPRTVAL-WS             PIC X       VALUE SPACE.                     
006700   88  GODK-PRINTER VALUE 'A' 'B' 'C' 'D'.                                
006800                                                                          
006900                                                                          
007000 01  SPAR-AAVV               PIC 9(4).                                    
007100*                                                                         
007200 01  FILLER REDEFINES SPAR-AAVV.                                          
007300   03 SPAR-AA                PIC 9(2).                                    
007400   03 SPAR-VV                PIC 9(2).                                    
007500                                                                          
007600 01  DAGENS-DATUM            PIC 9(6).                                    
007700*                                                                         
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900   03  DAGENS-DATUM-AR       PIC 9(2).                                    
008000   03  DAGENS-DATUM-MANAD    PIC 9(2).                                    
008100   03  DAGENS-DATUM-DAG      PIC 9(2).                                    
008200                                                                          
008300 77  INDATA-SW               PIC X       VALUE 'J'.                       
008400   88  INDATA-OK                         VALUE 'J'.                       
008500   88  INDATA-FEL                        VALUE 'N'.                       
008600                                                                          
008700 77  LAES-SW                 PIC X       VALUE 'J'.                       
008800   88  LAES-UNIK                         VALUE 'J'.                       
008900   88  LAES-ALLA                         VALUE 'N'.                       
009000                                                                          
009100 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
009200   88  NYCKLAR-OK                        VALUE 'J'.                       
009300   88  NYCKLAR-FEL                       VALUE 'N'.                       
009400                                                                          
009500 77  INPUT-SW                PIC X       VALUE 'J'.                       
009600   88  INPUT-FINNS                       VALUE 'J'.                       
009700   88  INPUT-FINNS-EJ                    VALUE 'N'.                       
009800                                                                          
009900 77  BEN-SW                  PIC X       VALUE 'J'.                       
010000   88  BENAMNING-FINNS                   VALUE 'J'.                       
010100   88  BENAMNING-FINNS-EJ                VALUE 'N'.                       
010200                                                                          
010300 77  FLYTT-SW                PIC X       VALUE 'J'.                       
010400   88  FLYTT-OK                          VALUE 'J'.                       
010500   88  FLYTT-EJ-OK                       VALUE 'N'.                       
010600                                                                          
010700 77  ALLT-SW                 PIC X       VALUE 'J'.                       
010800   88  ALLT-OK                           VALUE 'J'.                       
010900                                                                          
011000 77  STATUS-SW               PIC X       VALUE 'J'.                       
011100   88  STATUS-OK                         VALUE 'J'.                       
011200   88  STATUS-EJ-OK                      VALUE 'N'.                       
011300                                                                          
011400 77  FORTSATTNING-SW         PIC X       VALUE 'J'.                       
011500   88  FORTSATTNING-OK                   VALUE 'J'.                       
011600   88  FORTSATTNING-EJ-OK                VALUE 'N'.                       
011700                                                                          
011800 77  PF-TANGENT-SW           PIC X       VALUE 'J'.                       
011900   88  PF-TANGENT-TRYCKT                 VALUE 'J'.                       
012000   88  PF-TANGENT-EJ-TRYCKT              VALUE 'N'.                       
012100                                                                          
012200 77  PRINT-SW                PIC X       VALUE 'J'.                       
012300   88  PRINT-OK                          VALUE 'J'.                       
012400   88  PRINT-EJ-OK                       VALUE 'N'.                       
012500                                                                          
012600 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
012700   88  EGEN-MID                          VALUE '9415'.                    
012800   88  GODK-MID                          VALUE '9413' '9414'              
012900                                               '9415'.                    
013100   88  GODK-MID-MED-IDSKYLT              VALUE '9415'.                    
013300   88  GODK-MID-MED-IDRADNR              VALUE '9413' '9415'.             
013400     EJECT                                                                
013500                                                                          
013600 01  UTRAD                   PIC X(72)   VALUE SPACE.                     
013700 01  FILLER REDEFINES UTRAD.                                              
013800   03 IDREANTPSA          PIC Z9(1).9(3).                                 
013900   03 FILLER              PIC X(1).                                       
014000   03 IDLEVNR             PIC X(5).                                       
014100   03 FILLER              PIC X(1).                                       
014200   03 BELEVART            PIC X(30).                                      
014300   03 FILLER              PIC X(1).                                       
014400   03 IBEART              PIC X(15).                                      
014500   03 FILLER              PIC X(1).                                       
014600   03 STRTYP              PIC X(1).                                       
014700   03 FILLER              PIC X(2).                                       
014800   03 STATUSKOD           PIC X(1).                                       
014900   03 FILLER              PIC X(1).                                       
015000   03 AARVECKA            PIC Z(4).                                       
015100   03 FILLER              PIC X(3).                                       
015200 01  FILLER REDEFINES UTRAD.                                              
015300   03 AIDREANTPSA         PIC Z9(1).9(3).                                 
015400   03 FILLER              PIC X(7).                                       
015500   03 IDARTNR             PIC Z(9).                                       
015600   03 FILLER              PIC X(22).                                      
015700   03 BEART               PIC X(15).                                      
015800   03 FILLER              PIC X(1).                                       
015900   03 ASTRTYP             PIC X(1).                                       
016000   03 FILLER              PIC X(2).                                       
016100   03 STATKOD             PIC X(1).                                       
016200   03 FILLER              PIC X(1).                                       
016300   03 AAVV                PIC Z(4).                                       
016400   03 FILLER              PIC X(2).                                       
016500   03 FARLIG              PIC X(1).                                       
016600 01  FILLER REDEFINES UTRAD.                                              
016700   03 INFORAD             PIC X(70).                                      
016800   03 FORTS               PIC X(1).                                       
016900   03 FILLER              PIC X(1).                                       
017000                                                                          
017100 01  MEDDELANDE.                                                          
017200   03  MED-1.                                                             
017300      05 FILLER              PIC X(30) VALUE                              
017400           'MER INFO FINNS TRYCK PF8      '.                              
017500      05 FILLER              PIC X(30) VALUE                              
017600           'PRESS PF8 FOR MORE INFORMATION'.                              
017700   03  FILLER REDEFINES MED-1.                                            
017800      05 MED1 OCCURS 2       PIC X(30).                                   
017900*                                                                         
018000   03  MED-2.                                                             
018100      05 FILLER              PIC X(20) VALUE                              
018200           'DETTA ÄR SISTA SIDAN'.                                        
018300      05 FILLER              PIC X(20) VALUE                              
018400           'LAST PAGE IS SHOWN  '.                                        
018500   03  FILLER REDEFINES MED-2.                                            
018600      05 MED2 OCCURS 2       PIC X(20).                                   
018700*                                                                         
018800   03  MED-3.                                                             
018900      05 FILLER             PIC X(21) VALUE                               
019000           'DETTA ÄR FÖRSTA SIDAN'.                                       
019100      05 FILLER             PIC X(21) VALUE                               
019200           'FIRST PAGE IS SHOWN  '.                                       
019300   03  FILLER REDEFINES MED-3.                                            
019400      05 MED3 OCCURS 2       PIC X(21).                                   
019500*                                                                         
019600   03  FELMED-4.                                                          
019700       05  FILLER            PIC X(61)   VALUE                            
019800       'FEL I W006PRT  DEFINITION, KONTAKTA SYSTEMAVD'.                   
019900       05  FILLER            PIC X(61)   VALUE                            
020000       'MAJOR ERROR IN W006PRT , CONTACT YOUR SYSTEM SUPPORT'.            
020100   03  FILLER REDEFINES FELMED-4.                                         
020200       05  FELMED4 OCCURS 2  PIC X(61).                                   
020300*                                                                         
020400   03  MED-4-AREA.                                                        
020500       05  FILLER            PIC X(27)   VALUE                            
020600             'LISTA KÖAD FÖR UTSKRIFT PÅ '.                               
020700       05  FILLER            PIC X(27)   VALUE                            
020800             'LIST IS QUEUED TO PRINTER  '.                               
020900   03  FILLER REDEFINES MED-4-AREA.                                       
021000       05  MED4TXT OCCURS 2  PIC X(27).                                   
021100   03  MED-4.                                                             
021200       05  MED4-TXT          PIC X(27).                                   
021300       05  MED4-IDLTERM      PIC X(8)    VALUE  SPACE.                    
021400       05  FILLER            PIC X       VALUE  ','.                      
021500       05  MED4-BEPRT        PIC X(25)   VALUE  SPACE.                    
021600*                                                                         
021700   03  FEL-1.                                                             
021800      05 FILLER              PIC X(11) VALUE                              
021900           'NYCKLAR FEL'.                                                 
022000      05 FILLER              PIC X(11) VALUE                              
022100           'WRONG KEYS '.                                                 
022200   03  FILLER REDEFINES FEL-1.                                            
022300      05 FEL1 OCCURS 2       PIC X(11).                                   
022400*                                                                         
022500   03  FEL-2.                                                             
022600      05 FILLER              PIC X(20) VALUE                              
022700           'ARTIKEL SAKNAS      '.                                        
022800      05 FILLER              PIC X(20) VALUE                              
022900           'THIS PART IS MISSING'.                                        
023000   03  FILLER REDEFINES FEL-2.                                            
023100      05 FEL2 OCCURS 2       PIC X(20).                                   
023200*                                                                         
023300   03  FEL-3.                                                             
023400      05 FILLER              PIC X(20) VALUE                              
023500           'DENNA RAD SAKNAS    '.                                        
023600      05 FILLER              PIC X(20) VALUE                              
023700           'THIS LINE IS MISSING'.                                        
023800   03  FILLER REDEFINES FEL-3.                                            
023900      05 FEL3 OCCURS 2       PIC X(20).                                   
024000*                                                                         
024100   03  FEL-4.                                                             
024200      05 FILLER              PIC X(26) VALUE                              
024300           'KORRIGERA UPPLYSTA FÄLT '.                                    
024400      05 FILLER              PIC X(26) VALUE                              
024500           'CORRECT HIGHLIGHTED FIELDS'.                                  
024600   03  FILLER REDEFINES FEL-4.                                            
024700      05 FEL4 OCCURS 2       PIC X(26).                                   
024800*                                                                         
024900   03  FEL-5.                                                             
025000      05 FILLER              PIC X(28) VALUE                              
025100           'STRUKTUR SAKNAS             '.                                
025200      05 FILLER              PIC X(28) VALUE                              
025300           'STRUCTURE IS MISSING        '.                                
025400   03  FILLER REDEFINES FEL-5.                                            
025500      05 FEL5 OCCURS 2       PIC X(28).                                   
025600*                                                                         
025700   03  FEL-6.                                                             
025800      05 FILLER              PIC X(19) VALUE                              
025900           'MATA IN NYA NYCKLAR'.                                         
026000      05 FILLER              PIC X(19) VALUE                              
026100           'ENTER NEW KEYS     '.                                         
026200   03  FILLER REDEFINES FEL-6.                                            
026300      05 FEL6 OCCURS 2       PIC X(19).                                   
026400*                                                                         
026500   03  FEL-7.                                                             
026600      05 FILLER              PIC X(26) VALUE                              
026700           'TRYCK PF15 FÖR NEDBRYTNING'.                                  
026800      05 FILLER              PIC X(26) VALUE                              
026900           'PRESS PF15 FOR SPLIT-UP   '.                                  
027000   03  FILLER REDEFINES FEL-7.                                            
027100      05 FEL7 OCCURS 2       PIC X(26).                                   
027200*                                                                         
027300   03  FEL-8.                                                             
027400      05 FILLER              PIC X(26) VALUE                              
027500           'STRUKTUR EJ KLAR          '.                                  
027600      05 FILLER              PIC X(26) VALUE                              
027700           'STRUCTURE NOT COMPLETE    '.                                  
027800   03  FILLER REDEFINES FEL-8.                                            
027900      05 FEL8 OCCURS 2       PIC X(26).                                   
028000     EJECT                                                                
028100 01  GENERELLA-SUBPROGRAM.                                                
028200   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
028300   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
028400   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
028500   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
028600   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
028700   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
028710   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
028800     EJECT                                                                
028900*01  -COPY WWLAND03                                                       
029000     EJECT                                                                
029100*01  -COPY WDECAREA                                                       
029200     EJECT                                                                
029300*01  -COPY WMEDAREA                                                       
029400     EJECT                                                                
029500*01  -COPY W006PRT                                                        
029600     EJECT                                                                
029700*01  -COPY WDATAREA                                                       
029710     EJECT                                                                
029711*                    ****  PARAMETRAR TILL W005INIT                       
029720*01  -COPY WMSGINIT                                                       
029800     EJECT                                                                
029900******************************************************************        
030000*                                                                         
030100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
030200*                                                                         
030300 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
030400     SKIP3                                                                
030500*01  MID -COPY W90415I1                                                   
030600     EJECT                                                                
030700*01  -COPY WMSGAREA                                                       
030800     EJECT                                                                
030900*  03  MOD -COPY W90415O1 -RED MSG-AREA.                                  
031000     EJECT                                                                
031100****************************************************************          
031200*    PROG-TO-PROG-SW   AREA                                    *          
031300****************************************************************          
031400 01  FILLER                  PIC X(16) VALUE 'PROG-TO-PROG-SW'.           
031500 01  PROG-TO-PROG-SW.                                                     
031600     03  P-WS-LL     PIC S9(4)  VALUE +127 COMP SYNC.                     
031700     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
031800     03  P-TRANSKOD  PIC X(8)   VALUE 'W1T293X '.                         
031900     03  P-IDTRANS   PIC X(4)   VALUE '1213'.                             
032000     03  P-MFSFOR    PIC X      VALUE '1'.                                
032100*    03  MID   -COPY W1I21301 -PRE PROGSW-.                               
032200     EJECT                                                                
032300*01  -COPY WMFSAREA                                                       
032400     EJECT                                                                
032500******************************************************************        
032600*                                                                         
032700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032800*                                                                         
032900 01  IMS-WS.                                                              
033000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
033100     SKIP3                                                                
033200 01  NYCKLAR-TILL-DLI.                                                    
033300   03  W-IDARTNR-X.                                                       
033400     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
033500   03  W-IDSKYLT-X.                                                       
033600     05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
033700   03  W-KDSTRRAD-X.                                                      
033800     05  W-KDSTRRAD          PIC X       VALUE SPACE.                     
033900   03  W-IDRADNR-X.                                                       
034000     05  W-IDRADNR           PIC  S9(5)  VALUE ZERO  COMP-3.              
034100   03  W-BEART-X.                                                         
034200     05  W-BEART             PIC  X(25)  VALUE SPACE.                     
034300     SKIP3                                                                
034400*                        **** STATUS-KOD FRÅN IMS                         
034500   03  STATUS-WS             PIC XX.                                      
034600     88  SEGMENT-FINNS                   VALUE '  '.                      
034700     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
034800     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
034900     SKIP3                                                                
035000   03  GODK-STATUSKODER.                                                  
035100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035200     SKIP3                                                                
035300 01    SSA1                  PIC X(64).                                   
035400 01    SSA2                  PIC X(64).                                   
035500 01    SSA3                  PIC X(64).                                   
035600     EJECT                                                                
035700*                            IMS FUNKTIONSKODER                           
035800*01    -COPY W0003                                                        
035900     EJECT                                                                
036000*                            DLI INPUT-OUTPUT AREA                        
036100 01  DLI-IO-AREA-1.                                                       
036200   03  IO-AREA-1             PIC X(900)  VALUE SPACE.                     
036210*  03  WLARTC01  -COPY WDK601                -RED IO-AREA-1.              
036220     EJECT                                                                
036230*  03  WLARTC11  -COPY WDK611                -RED IO-AREA-1.              
036240     EJECT                                                                
036300     SKIP3                                                                
036600 01  DLI-IO-AREA.                                                         
036700   03  IO-AREA               PIC X(250)  VALUE SPACE.                     
036800     SKIP3                                                                
036900*  03  WLSATB01  -COPY WDJ101  -PRE SATB-  -RED IO-AREA.                  
037000     EJECT                                                                
037100*  03  WLSATB11  -COPY WDJ111  -PRE SATB-  -RED IO-AREA.                  
037200     EJECT                                                                
037300*  03  WLSATB22  -COPY WDJ122  -PRE SATB-  -RED IO-AREA.                  
037400     EJECT                                                                
037500 01  DLI-IO-AREA-2.                                                       
037600   03  IO-AREA-2             PIC X(250)  VALUE SPACE.                     
037700     SKIP3                                                                
037800*  03  WLBENA01  -COPY WDD301  -PRE BEN-   -RED IO-AREA-2.                
037900     EJECT                                                                
038000*  03  WLBENA11  -COPY WDD311  -PRE BEN-   -RED IO-AREA-2.                
038100     EJECT                                                                
038200                                                                          
038300 LINKAGE SECTION.                                                         
038400*01  -COPY W0009     -PRE MSG-                                            
038500     EJECT                                                                
038600*01  -COPY W0009     -PRE ALT-                                            
038700     EJECT                                                                
039100*01  -COPY W0008     -PRE USEA-                                           
039200     05  FILLER              PIC X.                                       
039210     EJECT                                                                
039220*01  -COPY W0008     -PRE SATB-                                           
039230     05  FILLER              PIC X.                                       
039300     EJECT                                                                
039400*01  -COPY W0008     -PRE BENA-                                           
039500     05  FILLER              PIC X.                                       
039600     EJECT                                                                
039700*01  -COPY W0008     -PRE BENA-A-                                         
039800     05  FILLER              PIC X.                                       
039900     EJECT                                                                
039910*01  -COPY W0008     -PRE ARTC-                                           
039920     05  FILLER              PIC X.                                       
039930     EJECT                                                                
040000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB SATB-PCB              
040100                           BENA-PCB BENA-A-PCB ARTC-PCB.                  
040200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB SATB-PCB              
040300                           BENA-PCB BENA-A-PCB ARTC-PCB.                  
040400     PERFORM IMS-GET-MSG                                                  
040500     IF SEGMENT-FINNS                                                     
040600        MOVE NEJ TO PF-TANGENT-SW                                         
040700        PERFORM A-INIT                                                    
040800***     PERFORM B-KOLLA-INPUT                                             
040900        PERFORM C-KOLLA-NYCKLAR                                           
041000***     IF MFS-PRINT                                                      
041100***        IF PRINT-OK                                                    
041200***           MOVE MID        TO PROGSW-MID                               
041300***           MOVE MED-4      TO MOD-TEMFSINF                             
041400***           PERFORM MFS-ROER-EJ-FAELT-UT                                
041500***           PERFORM IMS-INSERT-ALT-MSG                                  
041600***        ELSE                                                           
041700***           IF INDATA-FEL                                               
041800***              PERFORM MFS-ROER-EJ-FAELT-UT                             
041900***           END-IF                                                      
042000***        END-IF                                                         
042100***     ELSE                                                              
042200           IF NYCKLAR-OK                                                  
042300             COMPUTE IDARTNR-KONV-WS = +999999999                         
042400                                       - IDARTNR-KOLL-WS                  
042500             MOVE IDARTNR-KONV-WS TO W-IDARTNR                            
042600             PERFORM IMS-GET-SATB-ART                                     
042700             IF SEGMENT-FINNS                                             
042800               IF SATB-STR-IDUSER = MSG-SIGNON-USERID                     
042900                 MOVE JA TO FORTSATTNING-SW                               
043000                 MOVE FEL8(S-IX) TO MOD-TEMFSFEL                          
043100               ELSE                                                       
043200                 MOVE IDARTNR-WS TO W-IDARTNR                             
043300                 PERFORM IMS-GET-SATB-ART                                 
043400                 IF SEGMENT-FINNS                                         
043500                   MOVE JA TO FORTSATTNING-SW                             
043600                 ELSE                                                     
043700                   MOVE NEJ TO FORTSATTNING-SW                            
043800                 END-IF                                                   
043900               END-IF                                                     
044000             ELSE                                                         
044100               MOVE IDARTNR-WS TO W-IDARTNR                               
044200               PERFORM IMS-GET-SATB-ART                                   
044300               IF SEGMENT-FINNS                                           
044400                 MOVE JA TO FORTSATTNING-SW                               
044500               ELSE                                                       
044600                 MOVE NEJ TO FORTSATTNING-SW                              
044700               END-IF                                                     
044800             END-IF                                                       
044900                                                                          
045000             IF FORTSATTNING-OK                                           
045100               IF MFS-FIRST                                               
045200                  PERFORM D-BEHANDLA-ARTIKEL                              
045300               ELSE                                                       
045400                  IF MFS-NEXT                                             
045500                     PERFORM E-NAESTA-SIDA                                
045600                  ELSE                                                    
045700                     PERFORM F-SAMMA-SIDA                                 
045800                  END-IF                                                  
045900               END-IF                                                     
046000               IF NYCKLAR-OK AND ALLT-OK                                  
046100                  PERFORM G-LAES-VISA-INFO                                
046200****              PERFORM MFS-RENSA-FAELT-INMAT                           
046300               END-IF                                                     
046400             ELSE                                                         
046500****            MOVE SPACE TO MOD-IDSATSNR-DOLT                           
046600                MOVE FEL2(S-IX) TO MOD-TEMFSFEL                           
046700                PERFORM MFS-RENSA-FAELT-IN                                
046800                PERFORM MFS-RENSA-FAELT-UT                                
046900****            PERFORM MFS-FORM-ATTR                                     
047000                MOVE NEJ TO NYCKLAR-SW                                    
047100             END-IF                                                       
047200           ELSE                                                           
047300****          PERFORM MFS-FORM-ATTR                                       
047310              CONTINUE                                                    
047400           END-IF                                                         
047500****    END-IF                                                            
047600        MOVE LENGTH OF MOD-W90415O1 TO MSG-KVLL                           
047610        ADD +4                      TO MSG-KVLL                           
047700        PERFORM IMS-INSERT-MSG                                            
047800     END-IF                                                               
047900                                                                          
048000     MOVE ZERO TO RETURN-CODE                                             
048100     GOBACK                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 A-INIT SECTION.                                                          
048500                                                                          
048600     IF MSG-DUBBLA-TRANSKODER                                             
048700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90415I1-CTX             
048800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
048900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
049000       MOVE JA TO PF-TANGENT-SW                                           
049100     ELSE                                                                 
049200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90415I1-CTX              
049300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
049400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
049500     END-IF                                                               
049600                                                                          
049700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
049800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
049900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
050000                                                                          
050100     MOVE LOW-VALUE TO MSG-AREA                                           
050200     MOVE 'W90415O1' TO MFS-IDMOD                                         
050300     MOVE '9415' TO MOD-IDTRANS                                           
050400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
050500                                                                          
050600     IF NOT EGEN-MID                                                      
050700       MOVE SPACE TO MFS-KDTRTYP                                          
050800       MOVE '7'   TO MFS-IDPFK                                            
050900     END-IF                                                               
051000                                                                          
051100     IF ENGLISH-TEXT                                                      
051200        MOVE 'GB ' TO MED-IDSKYLT                                         
051400        MOVE +2    TO S-IX                                                
051500***     MOVE '2'   TO P-MFSFOR                                            
051700     ELSE                                                                 
051800        MOVE 'S  ' TO MED-IDSKYLT                                         
051810***     MOVE '0'   TO MFS-KDHUVOMR                                        
051900     END-IF                                                               
052000     ACCEPT DAGENS-DATUM FROM DATE                                        
052100     .                                                                    
052200     EJECT                                                                
052300 B-KOLLA-INPUT SECTION.                                                   
052400                                                                          
052500***  MOVE +1 TO INDX                                                      
052600***  MOVE NEJ TO INPUT-SW                                                 
052700***  PERFORM UNTIL INDX > MAX-IDRAD                                       
052800***    IF (MID-SELECT(INDX) = ALL '+') OR                                 
052900***       (MID-SELECT(INDX) = SPACE )                                     
053000***       MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                        
053100***    ELSE                                                               
053200***      IF MID-SELECT(INDX) = 'S'                                        
053300***         MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR(INDX)            
053400***         MOVE JA                  TO INPUT-SW                          
053500***         MOVE MID-IDARTNR-UT      TO W-IDARTNR                         
053600***         MOVE ZERO                TO W-KDSTRRAD                        
053700***         MOVE MID-IDRADNR(INDX)   TO W-IDRADNR                         
053800***         MOVE INDX                TO SPAR-INDX                         
053900***         ADD +20                  TO INDX                              
054000***      ELSE                                                             
054100***        IF MFS-FIRST OR MFS-NEXT                                       
054200***          PERFORM MFS-RENSA-FAELT-INMAT                                
054300***          MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                         
054400***        ELSE                                                           
054500***          MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR(INDX)             
054600***          MOVE MFS-ROER-EJ-FAELT  TO MOD-SELECT(INDX)                  
054700***          MOVE FEL4(S-IX)         TO MOD-TEMFSFEL                      
054800***        END-IF                                                         
054900***      END-IF                                                           
055000***    END-IF                                                             
055100***    ADD +1 TO INDX                                                     
055200***  END-PERFORM                                                          
055300                                                                          
055400***  IF INPUT-FINNS                                                       
055500***     PERFORM IMS-GET-SATB-RAD-UNIK                                     
055600***     IF SEGMENT-FINNS                                                  
055700***        PERFORM BA-KOLLA-STRUKTURTYP                                   
055800***     ELSE                                                              
055900***       MOVE '9' TO W-KDSTRRAD                                          
056000***       PERFORM IMS-GET-SATB-RAD-UNIK                                   
056100***       IF SEGMENT-FINNS                                                
056200***         PERFORM BA-KOLLA-STRUKTURTYP                                  
056300***       END-IF                                                          
056400***     END-IF                                                            
056500***  ELSE                                                                 
056600***    IF PF-TANGENT-TRYCKT                                               
056700***       IF MFS-FIRST OR  MFS-NEXT  OR  MFS-PRINT                        
056800***          CONTINUE                                                     
056900***       ELSE                                                            
057000***          IF EGEN-MID                                                  
057100***             IF MID-IDSATSNR-DOLT = ZERO                               
057200***                CONTINUE                                               
057300***             ELSE                                                      
057400***                MOVE MID-IDSATSNR-DOLT TO MID-IDARTNR-UT               
057500***                MOVE MID-IDSATSNR-DOLT TO MOD-IDSATSNR-DOLT            
057600***                MOVE ZERO              TO MID-IDRADNR-UT               
057700***                MOVE '7'               TO MFS-IDPFK                    
057800***                MOVE SPACE             TO MFS-KDTRTYP                  
057900***             END-IF                                                    
058000***          END-IF                                                       
058100***       END-IF                                                          
058200***    END-IF                                                             
058300***  END-IF                                                               
058400***  MOVE MID-KDPRTVAL TO MOD-KDPRTVAL                                    
058500***  IF MFS-PRINT                                                         
058600***     MOVE JA           TO PRINT-SW                                     
058700***     MOVE MID-KDPRTVAL TO KDPRTVAL-WS                                  
058800***     IF GODK-PRINTER                                                   
059100***       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ATTR                  
059200***       EVALUATE MID-KDPRTVAL                                           
059400***         WHEN 'A' MOVE SATS-STR-RA     TO PRT-IDPRTLST                 
059500***         WHEN 'B' MOVE SATS-STR-RB     TO PRT-IDPRTLST                 
059910***         WHEN 'C' MOVE SATS-STR-BERPV  TO PRT-IDPRTLST                 
059920***         WHEN 'D' MOVE SATS-STR-CARP   TO PRT-IDPRTLST                 
060000***       END-EVALUATE                                                    
060100*         **************************************************              
060200*         * HÄMTAR PRINTERNS LOGISKA NAMN TILL TEMFSINF *                 
060300*         **************************************************              
060400                                                                          
060500***       MOVE 1               TO PRT-KDCALL                              
060600***       CALL W006PRT  USING PRT-W006PRT                                 
060700***       IF PRT-IDLTERM = 'SAKNAS  '                                     
060800***         MOVE NEJ TO INDATA-SW                                         
060900***         MOVE FELMED4(S-IX) TO MED-4                                   
061000***         MOVE MED-4         TO MOD-TEMFSINF                            
061100***       ELSE                                                            
061200***         MOVE MED4TXT(S-IX) TO MED4-TXT                                
061300***         MOVE PRT-IDLTERM   TO MED4-IDLTERM                            
061400***         IF S-IX = +1                                                  
061500***           MOVE PRT-BEPRTLST TO MED4-BEPRT                             
061600***         ELSE                                                          
061700***           MOVE SPACE       TO MED4-BEPRT                              
061800***         END-IF                                                        
061900***       END-IF                                                          
062300***     ELSE                                                              
062400***        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-ATTR                   
062500***        MOVE NEJ                TO PRINT-SW   INDATA-SW                
062600***        MOVE FEL4(S-IX)         TO MOD-TEMFSFEL                        
062700***     END-IF                                                            
062800**   END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 BA-KOLLA-STRUKTURTYP SECTION.                                            
063200                                                                          
063300***  IF PF-TANGENT-TRYCKT                                                 
063400***     IF MFS-FIRST  OR  MFS-NEXT  OR  MFS-PRINT                         
063500***        CONTINUE                                                       
063600***     ELSE                                                              
063700***        IF EGEN-MID                                                    
063800***           IF SATB-RAD-IDSTRTYP = 'S' OR 'R' OR 'K'                    
063900***              MOVE SATB-RAD-IDARTNR TO MID-IDARTNR-UT                  
064000***              MOVE ZERO        TO MID-IDRADNR-UT                       
064100***              MOVE '7'         TO MFS-IDPFK                            
064200***              MOVE SPACE       TO MFS-KDTRTYP                          
064300***           ELSE                                                        
064400***              MOVE FEL5(S-IX) TO MOD-TEMFSFEL                          
064500***              MOVE MFS-ALFA-FAELT-FEL                                  
064600***              TO MOD-SELECT-ATTR(SPAR-INDX)                            
064700***              MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(SPAR-INDX)          
064800***           END-IF                                                      
064900***        END-IF                                                         
065000***     END-IF                                                            
065100***  END-IF                                                               
065200                                                                          
065300     .                                                                    
065400     EJECT                                                                
065500 C-KOLLA-NYCKLAR SECTION.                                                 
065600                                                                          
065700     MOVE JA TO NYCKLAR-SW                                                
065800***  MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
065900***                          MOD-IDRADNR-IN                               
066000***                          MOD-IDSKYLT-IN                               
066002                                                                          
066010     MOVE ALL '+' TO MSGI-WMSGINIT                                        
066020     MOVE '001'             TO MSGI-KDCALL                                
066030     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
066031                               MSGI-IDLTERM-USER                          
066032     MOVE '9415'            TO MSGI-IDTRANS                               
066040     IF MFS-IDTRANS = '9415'                                              
066050     OR (MID-IDARTNR-IN NUMERIC                                           
066051     AND MID-IDARTNR-IN > ZERO)                                           
066060         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
066070     END-IF                                                               
066080     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
066090     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
066091     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
066092     MOVE IDARTNR-WS TO IDARTNR-KOLL-WS                                   
066093                                                                          
066100     IF MID-IDARTNR-IN = ALL '+'                                          
066500***    IF EGEN-MID                                                        
066600***      MOVE MID-IDSATSNR-DOLT TO MOD-IDSATSNR-DOLT                      
066700***    ELSE                                                               
066800***      MOVE MSGI-IDARTNR      TO MOD-IDSATSNR-DOLT                      
066900***    END-IF                                                             
066910       CONTINUE                                                           
067000     ELSE                                                                 
067300       MOVE '7'             TO MFS-IDPFK                                  
067400       MOVE SPACE           TO MFS-KDTRTYP                                
067500***    MOVE MSGI-IDARTNR    TO MOD-IDSATSNR-DOLT                          
067600***    MOVE NEJ TO PRINT-SW                                               
067710     END-IF                                                               
067800                                                                          
067900     IF (IDARTNR-WS NUMERIC) AND (IDARTNR-WS > ZERO)                      
068000       AND (IDARTNR-WS < 99999999)                                        
068100       MOVE IDARTNR-WS TO W-IDARTNR                                       
068200     ELSE                                                                 
068300       MOVE NEJ TO NYCKLAR-SW                                             
068400       IF IDARTNR-WS NOT NUMERIC                                          
068500         MOVE FEL1(S-IX) TO MOD-TEMFSFEL                                  
068600       ELSE                                                               
068700         MOVE FEL2(S-IX) TO MOD-TEMFSFEL                                  
068800       END-IF                                                             
068900     END-IF                                                               
069000                                                                          
069100     IF GODK-MID-MED-IDSKYLT                                              
069200***    IF MID-IDSKYLT-IN = ALL '+'                                        
069300***      IF MID-IDSKYLT-UT = SPACE                                        
069400           IF ENGLISH-TEXT                                                
069500             MOVE 'GB' TO IDSKYLT-WS                                      
069600           ELSE                                                           
069700             MOVE 'S ' TO IDSKYLT-WS                                      
069800           END-IF                                                         
069900***      ELSE                                                             
070000***        MOVE MID-IDSKYLT-UT TO IDSKYLT-WS                              
070100***      END-IF                                                           
070200***    ELSE                                                               
070300***      MOVE MID-IDSKYLT-IN TO IDSKYLT-WS                                
070400***      MOVE '7'          TO MFS-IDPFK                                   
070500***      MOVE SPACE        TO MFS-KDTRTYP                                 
070600         MOVE NEJ TO PRINT-SW                                             
070700***    END-IF                                                             
070800     ELSE                                                                 
070900       IF ENGLISH-TEXT                                                    
071000         MOVE 'GB' TO IDSKYLT-WS                                          
071100       ELSE                                                               
071200         MOVE 'S ' TO IDSKYLT-WS                                          
071300       END-IF                                                             
071400     END-IF                                                               
071500                                                                          
071600     SET WWLAND03-IX TO +1                                                
071700     SEARCH WWLAND03-IDSKYLT-RAD                                          
071800       AT END                                                             
071900         MOVE NEJ TO NYCKLAR-SW                                           
072000         MOVE FEL1(S-IX) TO MOD-TEMFSFEL                                  
072100       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = IDSKYLT-WS                    
072200         CONTINUE                                                         
072300     END-SEARCH                                                           
072400     MOVE IDSKYLT-WS TO W-IDSKYLT                                         
072500                                                                          
072600     IF GODK-MID-MED-IDRADNR                                              
072700       IF MID-IDRADNR-IN = ALL '+'                                        
072800         MOVE MID-IDRADNR-UT TO IDRADNR-WS                                
072900         INSPECT IDRADNR-WS REPLACING LEADING SPACE BY ZERO               
073000       ELSE                                                               
073100         MOVE MID-IDRADNR-IN TO IDRADNR-WS                                
073110         INSPECT IDRADNR-WS REPLACING LEADING SPACE BY ZERO               
073130         MOVE '7'             TO MFS-IDPFK                                
073140         MOVE SPACE           TO MFS-KDTRTYP                              
073400***      MOVE NEJ TO PRINT-SW                                             
073500       END-IF                                                             
073600     ELSE                                                                 
073700       MOVE ZERO TO IDRADNR-WS                                            
073800     END-IF                                                               
073900                                                                          
074000     IF IDRADNR-WS NUMERIC                                                
074100       MOVE IDRADNR-WS TO W-IDRADNR                                       
074200       IF IDRADNR-WS = ZERO                                               
074300         MOVE '00010' TO SPAR-RADNR                                       
074400       ELSE                                                               
074500         MOVE IDRADNR-WS TO SPAR-RADNR                                    
074600       END-IF                                                             
074700     ELSE                                                                 
074800       MOVE NEJ TO NYCKLAR-SW                                             
074900       MOVE FEL1(S-IX) TO MOD-TEMFSFEL                                    
075000     END-IF                                                               
075100                                                                          
075200     IF GODK-MID OR NYCKLAR-OK                                            
075300       MOVE IDARTNR-WS       TO MOD-IDARTNR-UT                            
075400       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
075500***    MOVE IDSKYLT-WS       TO MOD-IDSKYLT-UT                            
075600       MOVE IDRADNR-WS       TO MOD-IDRADNR-UT                            
075700       INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE             
075710       CONTINUE                                                           
075800     ELSE                                                                 
075900       MOVE FEL6(S-IX)      TO MOD-TEMFSFEL                               
076000***    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
076100***                            MOD-IDSKYLT-UT                             
076200***                            MOD-IDRADNR-UT                             
076300       MOVE NEJ TO NYCKLAR-SW                                             
076400       MOVE NEJ TO PRINT-SW                                               
076500     END-IF                                                               
076600                                                                          
076700     IF NYCKLAR-FEL                                                       
076800       PERFORM MFS-RENSA-FAELT-IN                                         
076900       PERFORM MFS-RENSA-FAELT-UT                                         
077000     END-IF                                                               
077100     .                                                                    
077200     EJECT                                                                
077300                                                                          
077400 D-BEHANDLA-ARTIKEL SECTION.                                              
077500                                                                          
077600     IF SEGMENT-FINNS                                                     
077700***    MOVE SATB-STR-KDBENHOM TO SPAR-KDBENHOM                            
077800***    IF (W-IDSKYLT = 'S  ') AND                                         
077900***      (SATB-STR-BEART-SVE = SPACE)                                     
078000***      PERFORM IMS-GET-BEN-SEQ                                          
078100***      IF SEGMENT-FINNS                                                 
078200***        MOVE BEN-TEXT-BEART        TO MOD-BEART                        
078300***      ELSE                                                             
078400***        MOVE SPACE                 TO MOD-BEART                        
078500***      END-IF                                                           
078600***    ELSE                                                               
078700***      MOVE SATB-STR-BEART-SVE      TO MOD-BEART                        
078800***    END-IF                                                             
078900***                                                                       
079000***    IF W-IDSKYLT NOT = 'S  '                                           
079100***      IF SATB-STR-BEART-SVE NOT = SPACE                                
079200***        MOVE 'S'                   TO W-IDSKYLT                        
079300***        MOVE SATB-STR-BEART-SVE    TO W-BEART                          
079400***        PERFORM IMS-GET-BENA01-ASEQ                                    
079500***        PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
079600***                      (BEN-BEN-KDHOMONYM = SPAR-KDBENHOM)              
079700***          IF SEGMENT-FINNS                                             
079800***            IF BEN-BEN-KDHOMONYM = SPAR-KDBENHOM                       
079900***              MOVE JA TO BEN-SW                                        
080000***            ELSE                                                       
080100***              PERFORM IMS-GET-BENA01-ASEQ-NEXT                         
080200***            END-IF                                                     
080300***          ELSE                                                         
080400***            MOVE NEJ TO BEN-SW                                         
080500***            MOVE SPACE             TO MOD-BEART                        
080600***          END-IF                                                       
080700***        END-PERFORM                                                    
080800***                                                                       
080900***        IF SEGMENT-FINNS                                               
081000***          MOVE IDSKYLT-WS          TO W-IDSKYLT                        
081100***          PERFORM IMS-GET-BENA11-ASEQ                                  
081200***          IF SEGMENT-FINNS                                             
081300***            MOVE BEN-TEXT-BEART    TO MOD-BEART                        
081400***          ELSE                                                         
081500***            MOVE SPACE             TO MOD-BEART                        
081600***          END-IF                                                       
081700***        END-IF                                                         
081800***      ELSE                                                             
081900***        PERFORM IMS-GET-BEN-SEQ                                        
082000***        IF SEGMENT-FINNS                                               
082100***          MOVE BEN-TEXT-BEART      TO MOD-BEART                        
082200***        ELSE                                                           
082300***          MOVE SPACE               TO MOD-BEART                        
082400***        END-IF                                                         
082500***      END-IF                                                           
082600***    END-IF                                                             
082700***                                                                       
082800***    IF SATB-STR-KDPRODSL NOT = ZERO AND                                
082900***       SATB-STR-IDFKNGRP NOT = ZERO                                    
083000***      MOVE SATB-STR-KDPRODSL       TO MOD-KDPRODSL                     
083100***      MOVE SATB-STR-IDFKNGRP       TO MOD-IDFKNGRP                     
083200***      MOVE SATB-STR-IDSTRTYP       TO MOD-IDSTRTYP                     
083300***    ELSE                                                               
083400***      MOVE SATB-STR-IDSTRTYP       TO MOD-IDSTRTYP                     
083500***      PERFORM IMS-GET-ARTC01                                           
083600***        IF SEGMENT-FINNS                                               
083700***          MOVE ART-KDPRODSL       TO MOD-KDPRODSL                      
083800***          MOVE ART-IDFKNGRP       TO MOD-IDFKNGRP                      
083810***          PERFORM IMS-GET-ARTC11                                       
083820***          IF SEGMENT-FINNS                                             
083830***             MOVE CLAG-KDPSLLOC   TO MOD-KDPSLLOC                      
083840***          END-IF                                                       
083900***        END-IF                                                         
084000***    END-IF                                                             
084010       CONTINUE                                                           
084100     ELSE                                                                 
084200       MOVE FEL2(S-IX) TO MOD-TEMFSFEL                                    
084300       PERFORM MFS-RENSA-FAELT-IN                                         
084400       PERFORM MFS-RENSA-FAELT-UT                                         
084500       MOVE NEJ TO NYCKLAR-SW                                             
084600     END-IF                                                               
084700     .                                                                    
084800     EJECT                                                                
084900                                                                          
085000 E-NAESTA-SIDA SECTION.                                                   
085100                                                                          
085200     PERFORM MFS-ROER-EJ-FAELT-UT                                         
085300     MOVE MID-IDRADNR-DOLT2 TO W-IDRADNR                                  
085400     .                                                                    
085500     EJECT                                                                
085600                                                                          
085700 F-SAMMA-SIDA SECTION.                                                    
085800                                                                          
085900     MOVE MID-IDRADNR-DOLT TO W-IDRADNR                                   
086000***  IF INPUT-FINNS                                                       
086100***    IF PF-TANGENT-TRYCKT                                               
086200***      IF EGEN-MID                                                      
086300***        MOVE NEJ TO ALLT-SW                                            
086400***        MOVE FEL5(S-IX) TO MOD-TEMFSFEL                                
086500***        PERFORM MFS-ROER-EJ-FAELT-IN                                   
086600***        PERFORM MFS-ROER-EJ-FAELT-UT                                   
086700***      END-IF                                                           
086800***    ELSE                                                               
086900***      IF MFS-ENTER                                                     
087000***        MOVE NEJ TO ALLT-SW                                            
087100***        MOVE FEL7(S-IX) TO MOD-TEMFSFEL                                
087200***        PERFORM MFS-ROER-EJ-FAELT-IN                                   
087300***        PERFORM MFS-ROER-EJ-FAELT-UT                                   
087400***      END-IF                                                           
087500***    END-IF                                                             
087600***  ELSE                                                                 
087700***    PERFORM MFS-ROER-EJ-FAELT-UT                                       
087800       MOVE JA TO ALLT-SW                                                 
087900***  END-IF                                                               
088000     .                                                                    
088100     EJECT                                                                
088200                                                                          
088300 G-LAES-VISA-INFO SECTION.                                                
088400                                                                          
088500     MOVE NEJ TO FLYTT-SW                                                 
088600     MOVE +1 TO INDX                                                      
088700     PERFORM IMS-GET-SATB-RAD-NEXT                                        
088800     IF SEGMENT-FINNS                                                     
088900       MOVE SATB-RAD-IDRADNR TO MOD-IDRADNR-DOLT                          
089000     ELSE                                                                 
089100       MOVE ZERO TO MOD-IDRADNR-DOLT                                      
089200     END-IF                                                               
089300                                                                          
089400     PERFORM UNTIL (INDX > MAX-IDRAD) OR (SEGMENT-SAKNAS)                 
089500       IF SATB-RAD-IDRADNR >= SPAR-RADNR                                  
089600                                                                          
089700         PERFORM UNTIL INDX > MAX-IDRAD                                   
089800           IF SEGMENT-FINNS                                               
089900             PERFORM GA-KOLLA-RADSTATUS                                   
090000             MOVE SATB-RAD-KDSTRRAD TO W-KDSTRRAD                         
090100             MOVE SATB-RAD-IDRADNR TO MOD-IDRADNR(INDX)                   
090200                                      W-IDRADNR                           
090300                                      SPAR-IDRADNR                        
090400             IF STATUS-OK                                                 
090500               PERFORM GB-BEHANDLA-RAD                                    
090600               MOVE UTRAD TO MOD-UTRAD(INDX)                              
090700               MOVE SPACE TO UTRAD                                        
090800               MOVE ZERO TO SPAR-AAVV                                     
090900               ADD +1 TO INDX                                             
091000                                                                          
091100               PERFORM IMS-GET-SATB-NOTER                                 
091200               IF SEGMENT-FINNS                                           
091300                 PERFORM GC-HAMTA-NOTERINGSRAD                            
091400                 IF INDX > MAX-IDRAD                                      
091500                   MOVE JA    TO FLYTT-SW                                 
091600                   ADD -1 TO INDX                                         
091700                   MOVE MFS-RENSA-FAELT TO MOD-UTRAD(INDX)                
091800                                           MOD-IDRADNR(INDX)              
091900                   ADD +1 TO INDX                                         
092000                 ELSE                                                     
092100                   MOVE NEJ TO FLYTT-SW                                   
092200                   MOVE UTRAD TO MOD-UTRAD(INDX)                          
092300                   MOVE SPACE TO UTRAD                                    
092400                                 SPAR-UTRAD                               
092500                   MOVE ZERO TO SPAR-AAVV                                 
092600                   MOVE MFS-RENSA-FAELT TO MOD-IDRADNR(INDX)              
092700                   ADD +1 TO INDX                                         
092800                 END-IF                                                   
092900               END-IF                                                     
093000             END-IF                                                       
093100             IF FLYTT-EJ-OK                                               
093200               PERFORM IMS-GET-SATB-RAD-NEXT                              
093300             END-IF                                                       
093400           ELSE                                                           
093500             MOVE MFS-RENSA-FAELT TO MOD-IDRADNR(INDX)                    
093600                                     MOD-UTRAD(INDX)                      
093700             ADD +1 TO INDX                                               
093800           END-IF                                                         
093900         END-PERFORM                                                      
094000       ELSE                                                               
094100         PERFORM IMS-GET-SATB-RAD-NEXT                                    
094200         IF SEGMENT-SAKNAS                                                
094300           MOVE FEL3(S-IX) TO MOD-TEMFSFEL                                
094400         END-IF                                                           
094500       END-IF                                                             
094600     END-PERFORM                                                          
094700                                                                          
094800     IF FLYTT-OK                                                          
094900       MOVE SPAR-IDRADNR TO MOD-IDRADNR-DOLT2                             
095000       MOVE MED1(S-IX)             TO MOD-TEMFSINF                        
095100     ELSE                                                                 
095200       IF SEGMENT-FINNS                                                   
095300         PERFORM GA-KOLLA-RADSTATUS                                       
095400         IF STATUS-OK                                                     
095500           MOVE SATB-RAD-IDRADNR TO MOD-IDRADNR-DOLT2                     
095600           MOVE MED1(S-IX)         TO MOD-TEMFSINF                        
095700         ELSE                                                             
095800           PERFORM IMS-GET-SATB-RAD-NEXT                                  
095900           IF SEGMENT-FINNS                                               
096000             PERFORM UNTIL (SEGMENT-SAKNAS) OR (STATUS-OK)                
096100               IF SEGMENT-FINNS                                           
096200                 PERFORM GA-KOLLA-RADSTATUS                               
096300                 IF STATUS-OK                                             
096400                   MOVE SATB-RAD-IDRADNR TO MOD-IDRADNR-DOLT2             
096500                   MOVE MED1(S-IX) TO MOD-TEMFSINF                        
096600                 ELSE                                                     
096700                   PERFORM IMS-GET-SATB-RAD-NEXT                          
096800                 END-IF                                                   
096900               ELSE                                                       
097000                 MOVE ZERO TO MOD-IDRADNR-DOLT2                           
097100               END-IF                                                     
097200             END-PERFORM                                                  
097300           ELSE                                                           
097400             MOVE ZERO TO MOD-IDRADNR-DOLT2                               
097500           END-IF                                                         
097600         END-IF                                                           
097700       ELSE                                                               
097800         MOVE ZERO TO MOD-IDRADNR-DOLT2                                   
097900       END-IF                                                             
098000     END-IF                                                               
098100     .                                                                    
098200     EJECT                                                                
098300                                                                          
098400 GA-KOLLA-RADSTATUS SECTION.                                              
098500                                                                          
098600     IF SATB-RAD-KDISATS = 'N' OR 'T'                                     
098700         MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                          
098800         MOVE JA TO STATUS-SW                                             
098900         MOVE 'AAMMDD' TO DAT-KDDATFORM                                   
099000         CALL WDATKONV USING DAT-KDDATFORM                                
099100                             DAT-I-TIDATUM                                
099200                             DAT-O-TIDATUM                                
099300                             DAT-KDSVAR                                   
099400           MOVE DAT-TIAA-VECKA TO SPAR-AA                                 
099500           MOVE DAT-TIVV     TO SPAR-VV                                   
099600     ELSE                                                                 
099700       IF SATB-RAD-KDISATS = 'E'                                          
099800           MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                        
099900           MOVE JA TO STATUS-SW                                           
100000           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
100100           CALL WDATKONV USING DAT-KDDATFORM                              
100200                               DAT-I-TIDATUM                              
100300                               DAT-O-TIDATUM                              
100400                               DAT-KDSVAR                                 
100500             MOVE DAT-TIAA-VECKA TO SPAR-AA                               
100600             MOVE DAT-TIVV       TO SPAR-VV                               
100700       ELSE                                                               
100701         MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                          
100702         MOVE DAGENS-DATUM        TO TMP2-YYMMDD                          
100710         PERFORM WY2000P1                                                 
100800         IF  SATB-RAD-KDISATS = 'U'                                       
100900         AND TMP1-YYMMDD > TMP2-YYMMDD                                    
101000            MOVE SATB-RAD-TISTODAT TO DAT-I-TIDATUM                       
101100            MOVE JA TO STATUS-SW                                          
101200            MOVE 'AAMMDD' TO DAT-KDDATFORM                                
101300            CALL WDATKONV USING DAT-KDDATFORM                             
101400                                DAT-I-TIDATUM                             
101500                                DAT-O-TIDATUM                             
101600                                DAT-KDSVAR                                
101700            MOVE DAT-TIAA-VECKA TO SPAR-AA                                
101800            MOVE DAT-TIVV       TO SPAR-VV                                
101900         ELSE                                                             
102000           MOVE NEJ TO STATUS-SW                                          
102100                                                                          
102101           MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                        
102102           MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
102110           PERFORM WY2000P1                                               
102200           IF  SATB-RAD-KDISATS = ' '                                     
102300           AND TMP1-YYMMDD > TMP2-YYMMDD                                  
102400             MOVE JA TO STATUS-SW                                         
102401             MOVE SATB-RAD-TISTADAT   TO TMP1-YYMMDD                      
102402             MOVE DAGENS-DATUM        TO TMP2-YYMMDD                      
102410             PERFORM WY2000P1                                             
102500             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
102600               MOVE SATB-RAD-TISTADAT TO DAT-I-TIDATUM                    
102700               MOVE 'AAMMDD' TO DAT-KDDATFORM                             
102800               CALL WDATKONV USING DAT-KDDATFORM                          
102900                                   DAT-I-TIDATUM                          
103000                                   DAT-O-TIDATUM                          
103100                                   DAT-KDSVAR                             
103200               MOVE DAT-TIAA-VECKA TO SPAR-AA                             
103300               MOVE DAT-TIVV       TO SPAR-VV                             
103400             END-IF                                                       
103500           ELSE                                                           
103600             MOVE NEJ TO STATUS-SW                                        
103700           END-IF                                                         
103800         END-IF                                                           
103900       END-IF                                                             
104000     END-IF                                                               
104100     .                                                                    
104200     EJECT                                                                
104300                                                                          
104400 GB-BEHANDLA-RAD SECTION.                                                 
104500                                                                          
104600     MOVE NEJ TO BEN-SW                                                   
104700     IF SATB-RAD-IDARTNR NOT = ZERO                                       
104800       IF SATB-RAD-BEART-SVE = SPACE                                      
104900         MOVE SATB-RAD-IDARTNR        TO W-IDARTNR                        
105000         MOVE SATB-RAD-IDARTNR        TO IDARTNR                          
105100         MOVE SATB-RAD-REANTPSA       TO AIDREANTPSA                      
105200         MOVE SATB-RAD-KDISATS        TO STATKOD                          
105300         MOVE SPAR-AAVV               TO AAVV                             
105400         MOVE SATB-RAD-IDSTRTYP       TO ASTRTYP                          
105500         PERFORM IMS-GET-ARTC11                                           
105600         IF SEGMENT-FINNS                                                 
105700           MOVE CLAG-KDFARLIG         TO FARLIG                           
105800           MOVE IDSKYLT-WS            TO W-IDSKYLT                        
105900           PERFORM IMS-GET-BEN-SEQ                                        
106000           IF SEGMENT-FINNS                                               
106100             MOVE BEN-TEXT-BEART      TO BEART                            
106200           END-IF                                                         
106300         END-IF                                                           
106400       ELSE                                                               
106500         MOVE SATB-RAD-REANTPSA       TO AIDREANTPSA                      
106600         MOVE SATB-RAD-IDARTNR        TO IDARTNR                          
106700         IF IDSKYLT-WS NOT = 'S  '                                        
106800           MOVE 'S'                   TO W-IDSKYLT                        
106900           MOVE SATB-RAD-BEART-SVE    TO W-BEART                          
107000           MOVE SATB-RAD-KDBENHOM     TO SPAR-RAD-KDBENHOM                
107100           PERFORM IMS-GET-BENA01-ASEQ                                    
107200           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
107300                      (BEN-BEN-KDHOMONYM = SPAR-RAD-KDBENHOM)             
107400             IF SEGMENT-FINNS                                             
107500               IF BEN-BEN-KDHOMONYM = SPAR-RAD-KDBENHOM                   
107600                 MOVE JA TO BEN-SW                                        
107700               ELSE                                                       
107800                 PERFORM IMS-GET-BENA01-ASEQ-NEXT                         
107900               END-IF                                                     
108000             ELSE                                                         
108100               MOVE SPACE             TO BEART                            
108200               MOVE NEJ TO BEN-SW                                         
108300             END-IF                                                       
108400           END-PERFORM                                                    
108500                                                                          
108600           IF SEGMENT-FINNS                                               
108700             MOVE IDSKYLT-WS          TO W-IDSKYLT                        
108800             PERFORM IMS-GET-BENA11-ASEQ                                  
108900             IF SEGMENT-FINNS                                             
109000               MOVE BEN-TEXT-BEART    TO BEART                            
109100             ELSE                                                         
109200               MOVE SPACE             TO BEART                            
109300             END-IF                                                       
109400           END-IF                                                         
109500         ELSE                                                             
109600           MOVE SATB-RAD-BEART-SVE    TO BEART                            
109700         END-IF                                                           
109800         MOVE SATB-RAD-IDSTRTYP       TO ASTRTYP                          
109900         MOVE SATB-RAD-KDISATS        TO STATKOD                          
110000         MOVE SPAR-AAVV               TO AAVV                             
110100       END-IF                                                             
110200     ELSE                                                                 
110300       MOVE SATB-RAD-REANTPSA         TO IDREANTPSA                       
110400       MOVE SATB-RAD-IDLEVNR          TO IDLEVNR                          
110500       MOVE SATB-RAD-BELEVART         TO BELEVART                         
110600       IF IDSKYLT-WS NOT = 'S  '                                          
110700         MOVE 'S'                     TO W-IDSKYLT                        
110800         MOVE SATB-RAD-BEART-SVE      TO W-BEART                          
110900         MOVE SATB-RAD-KDBENHOM       TO SPAR-RAD-KDBENHOM                
111000         PERFORM IMS-GET-BENA01-ASEQ                                      
111100         PERFORM UNTIL (SEGMENT-SAKNAS) OR                                
111200                       (BEN-BEN-KDHOMONYM = SPAR-RAD-KDBENHOM)            
111300           IF SEGMENT-FINNS                                               
111400             IF BEN-BEN-KDHOMONYM = SPAR-RAD-KDBENHOM                     
111500               MOVE JA TO BEN-SW                                          
111600             ELSE                                                         
111700               PERFORM IMS-GET-BENA01-ASEQ-NEXT                           
111800             END-IF                                                       
111900           ELSE                                                           
112000             MOVE SPACE               TO IBEART                           
112100             MOVE NEJ TO BEN-SW                                           
112200           END-IF                                                         
112300         END-PERFORM                                                      
112400                                                                          
112500         IF SEGMENT-FINNS                                                 
112600           MOVE IDSKYLT-WS            TO W-IDSKYLT                        
112700           PERFORM IMS-GET-BENA11-ASEQ                                    
112800           IF SEGMENT-FINNS                                               
112900             MOVE BEN-TEXT-BEART      TO IBEART                           
113000           ELSE                                                           
113100             MOVE SPACE               TO IBEART                           
113200           END-IF                                                         
113300         END-IF                                                           
113400       ELSE                                                               
113500         MOVE SATB-RAD-BEART-SVE      TO IBEART                           
113600       END-IF                                                             
113700                                                                          
113800       MOVE SATB-RAD-IDSTRTYP         TO STRTYP                           
113900       MOVE SATB-RAD-KDISATS          TO STATUSKOD                        
114000       MOVE SPAR-AAVV                 TO AARVECKA                         
114100     END-IF                                                               
114200                                                                          
114300     IF SATB-RAD-IDARTNR = ZERO AND                                       
114400        SATB-RAD-IDLEVNR = SPACE AND                                      
114500        SATB-RAD-BELEVART = SPACE                                         
114600        MOVE SATB-RAD-BEART-SVE TO BEART                                  
114700     END-IF                                                               
114800     .                                                                    
114900     EJECT                                                                
115000                                                                          
115100 GC-HAMTA-NOTERINGSRAD SECTION.                                           
115200                                                                          
115300     MOVE +1 TO INDXNOT                                                   
115400     MOVE SATB-NOT-TESTRNOT(1) TO INFORAD                                 
115500     IF SATB-NOT-TESTRNOT(2) = SPACE                                      
115600       MOVE SPACE TO FORTS                                                
115700     ELSE                                                                 
115800       MOVE '*' TO FORTS                                                  
115900     END-IF                                                               
116000     .                                                                    
116100     EJECT                                                                
116200 MFS-RENSA-FAELT-IN SECTION.                                              
116300                                                                          
116400***  MOVE MFS-RENSA-FAELT TO MOD-KDPRTVAL                                 
116500     MOVE +1 TO INDX                                                      
116600     PERFORM UNTIL INDX > MAX-SELECT                                      
116700***    MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                           
116800       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR(INDX)                          
116900       ADD +1 TO INDX                                                     
117000     END-PERFORM                                                          
117100     .                                                                    
117200                                                                          
117300 MFS-RENSA-FAELT-INMAT SECTION.                                           
117400                                                                          
117500***  MOVE MFS-RENSA-FAELT TO MOD-KDPRTVAL                                 
117600***  MOVE +1 TO INDX                                                      
117700***  PERFORM UNTIL INDX > MAX-SELECT                                      
117800***    MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                           
117900***    ADD +1 TO INDX                                                     
118000***  END-PERFORM                                                          
118100     .                                                                    
118200                                                                          
118300 MFS-FORM-ATTR SECTION.                                                   
118400                                                                          
118500***  MOVE +1 TO INDX                                                      
118600***  PERFORM UNTIL INDX > MAX-SELECT                                      
118700***    MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-ATTR(INDX)                   
118800***    ADD +1 TO INDX                                                     
118900***  END-PERFORM                                                          
119000     .                                                                    
119100                                                                          
119200 MFS-RENSA-FAELT-UT SECTION.                                              
119300                                                                          
119400***  MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
119500***                          MOD-KDPRODSL                                 
119600***                          MOD-IDFKNGRP                                 
119700***                          MOD-IDSTRTYP                                 
119800     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-DOLT                             
119900                             MOD-IDRADNR-DOLT2                            
120000***                          MOD-IDSATSNR-DOLT                            
120100***                          MOD-KDPRTVAL                                 
120110***                          MOD-KDPSLLOC                                 
120200     MOVE +1 TO INDX                                                      
120300     PERFORM UNTIL INDX > MAX-IDRAD                                       
120400***    MOVE MFS-RENSA-FAELT TO MOD-SELECT(INDX)                           
120500       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR(INDX)                          
120600                               MOD-UTRAD(INDX)                            
120700       ADD +1 TO INDX                                                     
120800     END-PERFORM                                                          
120900     .                                                                    
121000     SKIP2                                                                
121100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
121200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-DOLT                           
121300                               MOD-IDRADNR-DOLT2                          
121400***                            MOD-IDSATSNR-DOLT                          
121500***                            MOD-BEART                                  
121600***                            MOD-KDPRODSL                               
121700***                            MOD-IDFKNGRP                               
121800***                            MOD-IDSTRTYP                               
121900***                            MOD-KDPRTVAL                               
121910***                            MOD-KDPSLLOC                               
122000     MOVE +1 TO INDX                                                      
122100     PERFORM UNTIL INDX > MAX-IDRAD                                       
122200       MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR(INDX)                        
122300***                              MOD-SELECT(INDX)                         
122400                                 MOD-UTRAD(INDX)                          
122500       ADD +1 TO INDX                                                     
122600     END-PERFORM                                                          
122700     .                                                                    
122800     SKIP2                                                                
122900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
123000                                                                          
123100***  MOVE MFS-ROER-EJ-FAELT TO  MOD-KDPRTVAL                              
123200     MOVE +1 TO INDX                                                      
123300     PERFORM UNTIL INDX > MAX-SELECT                                      
123400***    MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT(INDX)                         
123500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR(INDX)                        
123600       ADD +1 TO INDX                                                     
123700     END-PERFORM                                                          
123800     .                                                                    
123900     EJECT                                                                
124000******************************************************                    
124100*                  IMS SEKTIONER                     *                    
124200******************************************************                    
124300 IMS-GET-MSG SECTION.                                                     
124400     MOVE '  QC' TO GODK-STATUSKODER                                      
124500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
124600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124700     PERFORM IMS-STATUSKONTROLL                                           
124800     .                                                                    
124900                                                                          
125000 IMS-INSERT-MSG SECTION.                                                  
125100     SKIP2                                                                
125200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
125300     MOVE SPACE TO GODK-STATUSKODER                                       
125400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
125500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
125600     PERFORM IMS-STATUSKONTROLL                                           
125700     .                                                                    
125800                                                                          
125900 IMS-INSERT-ALT-MSG SECTION.                                              
126000     SKIP2                                                                
126100     MOVE SPACE TO GODK-STATUSKODER                                       
126200     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
126300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
126400     PERFORM IMS-STATUSKONTROLL                                           
126500     .                                                                    
126600     EJECT                                                                
126700 IMS-GET-ARTC01 SECTION.                                                  
126800     SKIP2                                                                
126900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
127000          DELIMITED BY SIZE INTO SSA1                                     
127100     MOVE '  GE' TO GODK-STATUSKODER                                      
127200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1                    
127300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
127400     PERFORM IMS-STATUSKONTROLL                                           
127500     .                                                                    
127600                                                                          
127610 IMS-GET-ARTC11 SECTION.                                                  
127620     SKIP2                                                                
127630     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
127640          DELIMITED BY SIZE INTO SSA1                                     
127641     MOVE 'WLARTC11 ' TO SSA2                                             
127650     MOVE '  GE' TO GODK-STATUSKODER                                      
127660     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1 SSA2               
127670     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
127680     PERFORM IMS-STATUSKONTROLL                                           
127690     .                                                                    
127691                                                                          
127700 IMS-GET-SATB-ART SECTION.                                                
127800     SKIP2                                                                
127900     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
128000          DELIMITED BY SIZE INTO SSA1                                     
128100     MOVE '  GE' TO GODK-STATUSKODER                                      
128200     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
128300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
128400     PERFORM IMS-STATUSKONTROLL                                           
128500     .                                                                    
128600                                                                          
128700                                                                          
128800 IMS-GET-SATB-RAD-UNIK SECTION.                                           
128900     SKIP2                                                                
129000     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
129100          DELIMITED BY SIZE INTO SSA1                                     
129200     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
129300                                  W-IDRADNR-X ')'                         
129400          DELIMITED BY SIZE INTO SSA2                                     
129500     MOVE '  GE' TO GODK-STATUSKODER                                      
129600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1 SSA2                 
129700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
129800     PERFORM IMS-STATUSKONTROLL                                           
129900     .                                                                    
130000                                                                          
130100 IMS-GET-SATB-RAD-NEXT SECTION.                                           
130200     SKIP2                                                                
130300     STRING 'WLSATB11(IDRADNR =>' W-IDRADNR-X ')'                         
130400          DELIMITED BY SIZE INTO SSA1                                     
130500     MOVE '  GE' TO GODK-STATUSKODER                                      
130600     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
130700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
130800     PERFORM IMS-STATUSKONTROLL                                           
130900     .                                                                    
131000                                                                          
131100 IMS-GET-SATB-NOTER SECTION.                                              
131200     SKIP2                                                                
131300     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
131400                                  W-IDRADNR-X ')'                         
131500          DELIMITED BY SIZE INTO SSA1                                     
131600     MOVE 'WLSATB22 ' TO SSA2                                             
131700     MOVE '  GE' TO GODK-STATUSKODER                                      
131800     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1 SSA2                
131900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
132000     PERFORM IMS-STATUSKONTROLL                                           
132100     .                                                                    
132200     EJECT                                                                
132300                                                                          
132400 IMS-GET-BEN-SEQ SECTION.                                                 
132500     SKIP2                                                                
132600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
132700          DELIMITED BY SIZE INTO SSA1                                     
132800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
132900          DELIMITED BY SIZE INTO SSA2                                     
133000     MOVE '  GE' TO GODK-STATUSKODER                                      
133100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-2 SSA1 SSA2               
133200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500                                                                          
133600 IMS-GET-BENA01-ASEQ SECTION.                                             
133700     SKIP2                                                                
133800     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
133900                                  W-BEART-X ')'                           
134000          DELIMITED BY SIZE INTO SSA1                                     
134100     MOVE '  GE' TO GODK-STATUSKODER                                      
134200     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA-2 SSA1                  
134300     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
134400     PERFORM IMS-STATUSKONTROLL                                           
134500     .                                                                    
134600     EJECT                                                                
134700                                                                          
134800 IMS-GET-BENA01-ASEQ-NEXT SECTION.                                        
134900     SKIP2                                                                
135000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
135100                                  W-BEART-X ')'                           
135200          DELIMITED BY SIZE INTO SSA1                                     
135300     MOVE '  GE' TO GODK-STATUSKODER                                      
135400     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA-2 SSA1                  
135500     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
135600     PERFORM IMS-STATUSKONTROLL                                           
135700     .                                                                    
135800     EJECT                                                                
135900                                                                          
136000 IMS-GET-BENA11-ASEQ SECTION.                                             
136100     SKIP2                                                                
136200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
136300          DELIMITED BY SIZE INTO SSA1                                     
136400     MOVE '  GE' TO GODK-STATUSKODER                                      
136500     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA-2 SSA1                 
136600     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
136700     PERFORM IMS-STATUSKONTROLL                                           
136800     .                                                                    
136900                                                                          
137000 IMS-STATUSKONTROLL SECTION.                                              
137100     SKIP2                                                                
137200     SET STATUS-IX TO 1                                                   
137300     SEARCH GODK-STATUS                                                   
137400       AT END CALL FELLOG                                                 
137500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
137600     END-SEARCH                                                           
137700     .                                                                    
137800     EJECT                                                                
137810     EJECT                                                                
137900*    -COPY WY2000P1                                                       
