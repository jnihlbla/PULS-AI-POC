000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4071700.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   05/03/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        UPPDATERINGSBILD FÖR ATTESTANSVARIGA FÖR OLIKA VÄRDE-            
001000*        GRÄNSER PÅ KREDITNOTOR.BILDEN VISAR DE LEV.ANM. SOM HAR          
001100*        KVAR INGÅENDE KREDITERINGAR UTAN ATTEST AV ANSVARIG              
001200*        PERSON MED RÄTT BEHÖRIGHET.HELA LA VÄNTAR PÅ ATT ALLA IN-        
001300*        GÅENDE KN-RADER SKALL BLI GODKÄNDA INNAN HELA LA GÅR             
001400*        VIDARE TILL STATUS 4 ELLER 8 VIA RUTIN W418D2.                   
001500*        MAN KAN ÄVEN GE KANTKOD 'T' OCH HOPPAR DÅ TILL BILD 4718,        
001600*        DÄR MAN KAN LÄGGA TILL TEXTRADER SOM TAS MED NÄR MAN I           
001700*        ETT SENARE LÄGE SKAPAR MAIL.                                     
001800*                                                                         
001900*        PROGRAMMET UPPDATERAR WDR5 (WDGX4103)                            
002000*        PROGRAMMET UPPDATERAR WDA2                                       
002100*        PROGRAMMET LÄSER      WDA2A                                      
002200*        PROGRAMMET LÄSER      WDR5 (WDGX6328)                            
002300*        PROGRAMMET LÄSER      WDR5 (WDGX4103)                            
002400*        PROGRAMMET LÄSER      WDG2 (WDGX9305 VIA W510CURR)               
002500*                                                                         
002600*        PROGRAMMET SKAPAR MAIL TILL LEVERANSANMÄRKNINGSANSVARIG          
002700*        FÖR UNDERKÄNDA KREDINOTOR.                                       
002800*                                                                         
002900*        PROGRAMMET SKAPAR MAIL TILL NÄSTA NIVÅ AV ATTESTANSVARIG         
003000*        NÄR KN-SUMMAN ÄR HÖGRE ÄN VÄRDEGRÄNSEN FÖR AKTUELL NIVÅ.         
003100*                                                                         
003200*    E-TRACKER: 1572353                                                   
003300*    E-TRACKER: 2072166 5173755                                           
003400*                                                                         
003500*    INDATA.                                                              
003600*        TRANSAKTION: W4T717                                              
003700*                     W4T717U                                             
003800*        MID:         W4I71701                                            
003900*                                                                         
004000*    UTDATA.                                                              
004100*        MOD:         W4O71701                                            
004200                                                                          
004300     SKIP3                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500                                                                          
004600 DATA DIVISION.                                                           
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900 77  IDPGM                       PIC X(08)   VALUE 'W4071700'.            
005000                                                                          
005100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  WS-IDUSER-IN                PIC X(8)    VALUE SPACE.                 
005700 77  WS-IDDISTR-IN               PIC X(4)    VALUE ZERO.                  
005800 77  WS-IDKUNDNR-IN              PIC X(6)    VALUE ZERO.                  
005900 77  WS-IDRAPPNR-IN              PIC X(7)    VALUE ZERO.                  
006000 77  WS-FLTOT-IN                 PIC X       VALUE SPACE.                 
006100 77  WS-IDTRANS-HOPP             PIC X(4)    VALUE '4712'.                
006200 77  WS-IDTRANS-HOPP-1           PIC X(4)    VALUE '4718'.                
006300 77  WS-TIDATETIME               PIC 9(14)   VALUE ZERO.                  
006400 77  SPAR-SUBEL                  PIC 9(7)    VALUE ZERO.                  
006500 77  SPAR-IDUSER-GODK            PIC X(8)    VALUE SPACE.                 
006600 77  SPAR-BEANST-GODK            PIC X(25)   VALUE SPACE.                 
006700 77  W-DATE-AAMM                 PIC 9(4)   VALUE ZERO.                   
006800 77  WS-KDVALISO-HUV             PIC X(3)   VALUE 'SEK'.                  
006900                                                                          
007000 01  FILLER.                                                              
007100   03 COUNTER                    PIC 99  VALUE ZERO.                      
007200                                                                          
007300 01 WS-4718-TABELL.                                                       
007400    03 WS-TEXT-GRP OCCURS 5.                                              
007500       05 WS-TEMAIL              PIC X(66).                               
007600                                                                          
007700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
007800 77  MAIL-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
007900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
008000 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
008100 77  4105-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008200                                                                          
008300 77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                   
008400     88 STARTA-ANNAN-BILD                    VALUE 'J'.                   
008500                                                                          
008600 77  SW-KNOTA-ATTEST-KLAR        PIC X       VALUE 'N'.                   
008700     88 KNOTA-ATTEST-KLAR                    VALUE 'J'.                   
008800                                                                          
008900 77  SW-ALLA-LEVANM-KN-KLARA     PIC X       VALUE 'N'.                   
009000     88 ALLA-LEVANM-KN-KLARA                 VALUE 'J'.                   
009100                                                                          
009200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009300                                                                          
009400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009500     88  INDATA-OK                           VALUE 'J'.                   
009600     88  INDATA-FEL                          VALUE 'N'.                   
009700                                                                          
009800 77  FLGODK-SW                   PIC X       VALUE 'N'.                   
009900     88  FLGODK-OK                           VALUE 'J'.                   
010000     88  FLGODK-FEL                          VALUE 'N'.                   
010100                                                                          
010200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010300     88  NYCKLAR-OK                          VALUE 'J'.                   
010400     88  NYCKLAR-FEL                         VALUE 'N'.                   
010500                                                                          
010600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010700     88  EGEN-MID                            VALUE '4717'.                
010800     88  GODK-MID                            VALUE '4712' '4713'          
010900                                                   '4714' '4715'          
011000                                                   '4716' '4717'          
011100                                                   '4722'.                
011200     88  HELP-MID                            VALUE '0551'.                
011300     EJECT                                                                
011400                                                                          
011500 01  W-SUKRENOT                   PIC 9(7)V9(2).                          
011600 01  WS-SUKRENOT-NUM              PIC 9(7)V9(2).                          
011700 01  FILLER REDEFINES WS-SUKRENOT-NUM.                                    
011800    03  WS-SUKRENOT-HEL           PIC 9(7).                               
011900    03  WS-SUKRENOT-DEC           PIC 9(2).                               
012000                                                                          
012100     EJECT                                                                
012200 01  BILD-HOPP-AREOR.                                                     
012300                                                                          
012400   03  FILLER            PIC X(16)   VALUE 'P-TO-P-IO-AREA'.              
012500   03  P-TO-P-IO-AREA.                                                    
012600     05  FILLER                  PIC S9(4)   VALUE +117 COMP SYNC.        
012700     05  FILLER                  PIC X(1)    VALUE LOW-VALUE.             
012800     05  FILLER                  PIC X(1)    VALUE LOW-VALUE.             
012900     05  P-TO-P-KDTRANS          PIC X(8).                                
013000     05  FILLER                  PIC X(4)    VALUE '4717'.                
013100     05  FILLER                  PIC X(1)    VALUE '2'.                   
013200     05  FILLER                  PIC X(100)  VALUE ALL '+'.               
013300                                                                          
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'WSIDFTG'.             
013600*01  -COPY WWIDFTG                                                        
013700     EJECT                                                                
013800*                                                                         
013900     EJECT                                                                
014000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014100 01  GENERELLA-SUBPROGRAM.                                                
014200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
014700     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
014800     EJECT                                                                
014900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015000*01 -COPY WMEDAREA                                                        
015100     EJECT                                                                
015200*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
015300*01 -COPY W510CURR                                                        
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
015600*                                                                         
015700 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
015800*                                                                         
015900 01  DECAREA.                                                             
016000*    03  WDECAREA   -COPY WDECAREA                                        
016100     EJECT                                                                
016200     SKIP3                                                                
016300 01  MESSAGE-CODES.                                                       
016400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
016900     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
017000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
017100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017200     03  ERR-WRONG-COMMAND-CODE  PIC X(3)    VALUE '304'.                 
017300     03  ERR-USER-NOT-AUTHORIZED PIC X(3)    VALUE '405'.                 
017400     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
017500     03  ERR-NOT-AUTHORIZED-ATT  PIC X(3)    VALUE '602'.                 
017600     03  ERR-RAD-FINNS-REDAN     PIC X(3)    VALUE '245'.                 
017700     03  INF-NEED-APPR-NEXT-LEV  PIC X(3)    VALUE '603'.                 
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018000*                                                                         
018100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
018200     SKIP3                                                                
018300*01 -COPY WMSGINIT                                                        
018400     EJECT                                                                
018500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
018600*                                                                         
018700 01  SPAR-AREA.                                                           
018800     03  SPAR-IDTRANS           PIC X(4)    VALUE '4717'.                 
018900     03  SPAR-IDDISTR-ENTER     PIC S9(5) VALUE ZERO COMP-3.              
019000     03  SPAR-IDDISTR-NEXT      PIC S9(5) VALUE ZERO COMP-3.              
019100     03  SPAR-IDKUNDNR-ENTER    PIC S9(7) VALUE ZERO COMP-3.              
019200     03  SPAR-IDKUNDNR-NEXT     PIC S9(7) VALUE ZERO COMP-3.              
019300     03  SPAR-IDRAPPNR-ENTER    PIC S9(7) VALUE ZERO.                     
019400     03  SPAR-IDRAPPNR-NEXT     PIC S9(7) VALUE ZERO.                     
019500     03  SPAR-IDUSER-ENTER      PIC X(8)  VALUE SPACE.                    
019600     03  SPAR-IDUSER-NEXT       PIC X(8)  VALUE SPACE.                    
019700     03  SPAR-IDDC-ENTER        PIC X(2)  VALUE SPACE.                    
019800     03  SPAR-IDDC-NEXT         PIC X(2)  VALUE SPACE.                    
019900     03  SPAR-KDKRENOT-ENTER    PIC X(2)  VALUE SPACE.                    
020000     03  SPAR-KDKRENOT-NEXT     PIC X(2)  VALUE SPACE.                    
020100     03  SPAR-MID-IDUSER        PIC X(8)  VALUE SPACE.                    
020200     03  SPAR-MID-FLTOT         PIC X     VALUE SPACE.                    
020300     03  SPAR-MID-IDDISTR       PIC X(4)  VALUE SPACE.                    
020400     03  SPAR-MID-IDKUNDNR      PIC X(6)  VALUE SPACE.                    
020500     03  SPAR-MID-IDRAPPNR      PIC X(7)  VALUE SPACE.                    
020600     EJECT                                                                
020700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020800*                                                                         
020900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021000     SKIP3                                                                
021100*01  MID -COPY W4I71701                                                   
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021400     SKIP3                                                                
021500*01  -COPY WMSGAREA                                                       
021600     EJECT                                                                
021700     03  MOD REDEFINES MSG-AREA.                                          
021800*      05  -COPY W4O71701                                                 
021900     EJECT                                                                
022000                                                                          
022100*   --- PARAMETRAR TILL PROGRAM W0054100                                  
022200                                                                          
022300 01  FILLER                     PIC X(16)   VALUE 'WMSGMAIL-AREA'.        
022400*01  -COPY WMSGMAIL                                                       
022500                                                                          
022600     EJECT                                                                
022700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022800     SKIP3                                                                
022900*01  -COPY WMFSAREA                                                       
023000     EJECT                                                                
023100******************************************************************        
023200*  MAILRADER                                                              
023300                                                                          
023400 01  LIST-HRAD1.                                                          
023500     03   FILLER                  PIC X(12) VALUE                         
023600                                        'W4071700-001'.                   
023700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
023800     03   FILLER                  PIC X(38)  VALUE                        
023900                        'REJECTED DISCREPANCY LINES BY APPROVER'.         
024000                                                                          
024100 01  LIST-HRAD2.                                                          
024200     03   FILLER                  PIC X(12) VALUE                         
024300                                        'W4071700-001'.                   
024400     03   FILLER                  PIC X(4)  VALUE SPACE.                  
024500     03   FILLER                  PIC X(47)  VALUE                        
024600                'GO TO SCREEN 4717 TO APPROVE DISCREPANCY CREDIT'.        
024700                                                                          
024800 01  LIST-HRAD3.                                                          
024900     03   FILLER                  PIC X(9)  VALUE 'DISTRICT '.            
025000     03   FILLER                  PIC X(3)  VALUE SPACE.                  
025100     03   FILLER                  PIC X(5)  VALUE 'CUST '.                
025200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
025300     03   FILLER                  PIC X(9)  VALUE 'REPORT.NO'.            
025400     03   FILLER                  PIC X(3)  VALUE SPACE.                  
025500     03   FILLER                  PIC X(11) VALUE 'CREDIT TYPE'.          
025600     03   FILLER                  PIC X(2)  VALUE SPACE.                  
025700     03   FILLER                  PIC X(4)  VALUE 'DC  '.                 
025800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
025900     03   FILLER                  PIC X(10) VALUE 'SCREEN NO.'.           
026000                                                                          
026100 01  LIST-LRAD.                                                           
026200     03   FILLER                  PIC X(4)  VALUE SPACE.                  
026300     03   LRAD-IDDISTR            PIC Z(4)  VALUE ZERO.                   
026400     03   FILLER                  PIC X(2)  VALUE SPACE.                  
026500     03   LRAD-IDKUNDNR           PIC Z(6)  VALUE ZERO.                   
026600     03   FILLER                  PIC X(6)  VALUE SPACE.                  
026700     03   LRAD-IDRAPPNR           PIC Z(7)  VALUE ZERO.                   
026800     03   FILLER                  PIC X(3)  VALUE SPACE.                  
026900     03   LRAD-KDKRENOT           PIC X(2)  VALUE SPACE.                  
027000     03   FILLER                  PIC X(11) VALUE SPACE.                  
027100     03   LRAD-IDDC               PIC X(2)  VALUE SPACE.                  
027200     03   FILLER                  PIC X(10) VALUE SPACE.                  
027300     03   FILLER                  PIC X(4)  VALUE '4717'.                 
027400                                                                          
027500 01  LIST-LRAD-1.                                                         
027600     03   FILLER                  PIC X(21) VALUE                         
027700                           'REJECTED BY APPROVER:'.                       
027800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
027900     03   LRAD-IDUSER             PIC X(8)  VALUE SPACE.                  
028000     03   FILLER                  PIC X(2)  VALUE SPACE.                  
028100     03   LRAD-BEANST             PIC X(25) VALUE SPACE.                  
028200                                                                          
028300 01  LIST-LRAD-2.                                                         
028400     03   FILLER                  PIC X(18) VALUE                         
028500                           'REASON FOR CREDIT:'.                          
028600                                                                          
028700 01  LIST-BLANKRAD.                                                       
028800     03   FILLER                  PIC X(66) VALUE SPACE.                  
028900                                                                          
029000******************************************************************        
029100                                                                          
029200     EJECT                                                                
029300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029400*                                                                         
029500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029600     SKIP3                                                                
029700 01  NYCKLAR-TILL-DLI.                                                    
029800*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
029900                                                                          
030000     03  W-IDUSER-X.                                                      
030100         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
030200                                                                          
030300     03  W-IDUSER-6328-X.                                                 
030400         05  W-IDUSER-GODK-6328  PIC X(8)    VALUE SPACE.                 
030500                                                                          
030600     03  W-SUBEL-6328-X.                                                  
030700         05  W-SUBEL-WDGX6328    PIC 9(7)    VALUE ZERO.                  
030800                                                                          
030900     03  W-IDUSER-4106-X.                                                 
031000         05  W-IDUSER-GODK-4106  PIC X(8)    VALUE SPACE.                 
031100                                                                          
031200     03  W-KDLEVATT-X.                                                    
031300         05  W-KDLEVATT          PIC 9(1)    VALUE 1.                     
031400                                                                          
031500     03  W-WDA2A1KY-MIN-X.                                                
031600         05  W-KDLEVANM-MIN      PIC  X(1)   VALUE '3'.                   
031700         05  W-IDFTG-MIN         PIC  9(2)   VALUE ZERO.                  
031800         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
031900         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
032000         05  W-IDRAPPNR-MIN      PIC 9(7)    VALUE ZERO.                  
032100                                                                          
032200     03  W-WDA2A1KY-MAX-X.                                                
032300         05  W-KDLEVANM-MAX      PIC  X(1)   VALUE '3'.                   
032400         05  W-IDFTG-MAX         PIC  9(2)   VALUE ZERO.                  
032500         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
032600         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
032700         05  W-IDRAPPNR-MAX      PIC 9(7)    VALUE ZERO.                  
032800                                                                          
032900     03  W-WDA2A1KY-X.                                                    
033000         05  W-KDLEVANM-WDA2A    PIC  X(1)   VALUE '3'.                   
033100         05  W-IDFTG-WDA2A       PIC  9(2)   VALUE ZERO.                  
033200         05  W-IDDISTR-WDA2A     PIC S9(5)   VALUE ZERO COMP-3.           
033300         05  W-IDKUNDNR-WDA2A    PIC S9(7)   VALUE ZERO COMP-3.           
033400         05  W-IDRAPPNR-WDA2A    PIC 9(7)    VALUE ZERO.                  
033500                                                                          
033600     03  W-IDLEVANM-X.                                                    
033700         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
033800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
033900         05  W-IDRAPPNR          PIC  9(7)   VALUE ZERO.                  
034000                                                                          
034100     03  W-WDGXKEY-4103-X.                                                
034200         05  W-IDHTYP-4103       PIC X(4)    VALUE '4103'.                
034300         05  W-IDDISTR-4103      PIC S9(5)   VALUE ZERO COMP-3.           
034400         05  W-IDKUNDNR-4103     PIC S9(7)   VALUE ZERO COMP-3.           
034500         05  W-IDRAPPNR-4103     PIC  9(7)   VALUE ZERO.                  
034600         05  FILLER              PIC X(12)   VALUE LOW-VALUE.             
034700                                                                          
034800     03  W-KEY4104-X.                                                     
034900         05  W-IDDC-4104         PIC X(2)    VALUE SPACE.                 
035000         05  W-KDKRENOT-4104     PIC X(2)    VALUE SPACE.                 
035100                                                                          
035200     03  W-TIDATETIME-X.                                                  
035300         05  W-TIDATETIME        PIC X(14)   VALUE SPACE.                 
035400                                                                          
035500     03  W-TIDATETIME-MIN-X.                                              
035600         05  W-TIDATETIME-MIN    PIC X(14)   VALUE LOW-VALUE.             
035700                                                                          
035800     03  W-TIDATETIME-MAX-X.                                              
035900         05  W-TIDATETIME-MAX    PIC X(14)   VALUE HIGH-VALUE.            
036000                                                                          
036100     03  W-WDGXKEY-6327-X.                                                
036200         05  W-IDHTYP-6327       PIC X(4)    VALUE '6327'.                
036300         05  W-KDARBTYP          PIC X(8)    VALUE 'DISC    '.            
036400         05  W-IDDC-6327         PIC X(2)    VALUE SPACE.                 
036500         05  FILLER              PIC X(16)   VALUE LOW-VALUE.             
036600                                                                          
036700     03  W-KY6328-X.                                                      
036800         05  W-SUBEL-6328        PIC 9(7)    VALUE ZERO.                  
036900         05  W-IDUSER-6328       PIC X(8)    VALUE SPACE.                 
037000                                                                          
037100     03  W-KY6328-MIN-X.                                                  
037200         05  W-SUBEL-6328-MIN    PIC 9(7)    VALUE ZERO.                  
037300         05  W-IDUSER-6328-MIN   PIC X(8)    VALUE LOW-VALUE.             
037400                                                                          
037500     03  W-KY6328-MAX-X.                                                  
037600         05  W-SUBEL-6328-MAX    PIC 9(7)    VALUE 9999999.               
037700         05  W-IDUSER-6328-MAX   PIC X(8)    VALUE HIGH-VALUE.            
037800                                                                          
037900     03  W-KDSEGKEY-X.                                                    
038000         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
038100     SKIP2                                                                
038200*    --- STATUS-KOD FRÅN IMS                                              
038300 01  STATUS-WS                   PIC XX.                                  
038400     88  STATUS-OK                           VALUE '  '.                  
038500     88  SEGMENT-FINNS                       VALUE '  '.                  
038600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
038700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
038800     88  BASEN-SLUT                          VALUE 'GB'.                  
038900     88  TRANSKOD-FEL                        VALUE 'A1'.                  
039000     88  SECURITY-FEL                        VALUE 'A4'.                  
039100     SKIP2                                                                
039200 01  GODK-STATUSKODER.                                                    
039300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039400     SKIP3                                                                
039500 01  SSA1                        PIC X(96).                               
039600 01  SSA2                        PIC X(64).                               
039700 01  SSA3                        PIC X(96).                               
039800     EJECT                                                                
039900*    --- IMS FUNKTIONSKODER                                               
040000*01  -COPY W0003                                                          
040100     EJECT                                                                
040200*    ---  DLI INPUT-OUTPUT AREA                                           
040300                                                                          
040400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA2A1'.                      
040500 01  DLI-IO-WDA2A1.                                                       
040600*    03  -COPY WDA2A1                                                     
040700     EJECT                                                                
040800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
040900 01  DLI-IO-WDA201.                                                       
041000*    03  -COPY WDA201                                                     
041100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6327'.                    
041200 01  DLI-IO-WDGX6327.                                                     
041300*    03  -COPY WDGX6327                                                   
041400     EJECT                                                                
041500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
041600 01  DLI-IO-WDGX6328.                                                     
041700*    03  -COPY WDGX6328                                                   
041800     EJECT                                                                
041900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4103'.                    
042000 01  DLI-IO-WDGX4103.                                                     
042100*    03  -COPY WDGX4103                                                   
042200     EJECT                                                                
042300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4104'.                    
042400 01  DLI-IO-WDGX4104.                                                     
042500*    03  -COPY WDGX4104                                                   
042600     EJECT                                                                
042700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4105'.                    
042800 01  DLI-IO-WDGX4105.                                                     
042900*    03  -COPY WDGX4105                                                   
043000     EJECT                                                                
043100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4106'.                    
043200 01  DLI-IO-WDGX4106.                                                     
043300*    03  -COPY WDGX4106                                                   
043400     EJECT                                                                
043500 LINKAGE SECTION.                                                         
043600*01  -COPY W0009   -PRE MSG-                                              
043700*01  -COPY W0009   -PRE ALT-                                              
043800     EJECT                                                                
043900*01  -COPY W0009   -PRE MAIL-                                             
044000     EJECT                                                                
044100*01  -COPY W0008   -PRE WDP7-                                             
044200     05  FILLER                  PIC X.                                   
044300                                                                          
044400*01  -COPY W0008  -PRE WDA2A-                                             
044500     05  FILLER                  PIC X.                                   
044600                                                                          
044700*01  -COPY W0008  -PRE WDA2-                                              
044800     05  FILLER                  PIC X.                                   
044900                                                                          
045000*01  -COPY W0008  -PRE 6327-                                              
045100     05  FILLER                  PIC X.                                   
045200                                                                          
045300*01  -COPY W0008  -PRE 4103-                                              
045400     05  FILLER                  PIC X.                                   
045500     EJECT                                                                
045600*01  -COPY W0008  -PRE WDG2-                                              
045700     05  FILLER                  PIC X.                                   
045800     EJECT                                                                
045900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB MAIL-PCB WDP7-PCB              
046000     WDA2A-PCB WDA2-PCB 6327-PCB 4103-PCB WDG2-PCB.                       
046100 MAIN SECTION.                                                            
046200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB MAIL-PCB WDP7-PCB              
046300     WDA2A-PCB WDA2-PCB 6327-PCB 4103-PCB WDG2-PCB.                       
046400                                                                          
046500     PERFORM IMS-GET-MSG                                                  
046600     IF SEGMENT-FINNS                                                     
046700       PERFORM A-INIT                                                     
046800       PERFORM B-KOLLA-NYCKLAR                                            
046900       IF NYCKLAR-OK                                                      
047000         IF MFS-UPDATE                                                    
047100           IF MID-FLGODK = 'Y' OR 'J'                                     
047200             MOVE  'JA'  TO  FLGODK-SW                                    
047300             PERFORM I-CHECK-FLGODK                                       
047400           ELSE                                                           
047500             PERFORM G-KOLLA-INPUT                                        
047600             IF INDATA-OK                                                 
047700               PERFORM H-UPPDATERA                                        
047800             END-IF                                                       
047900           END-IF                                                         
048000         ELSE                                                             
048100           IF MFS-FIRST                                                   
048200             PERFORM C-FOERSTA-SIDA                                       
048300           ELSE                                                           
048400             IF MFS-NEXT                                                  
048500               PERFORM D-NAESTA-SIDA                                      
048600             ELSE                                                         
048700               PERFORM E-SAMMA-SIDA                                       
048800             END-IF                                                       
048900           END-IF                                                         
049000         END-IF                                                           
049100         IF STARTA-ANNAN-BILD                                             
049200           CONTINUE                                                       
049300         ELSE                                                             
049400           IF INDATA-OK                                                   
049500            IF FLGODK-FEL                                                 
049600               PERFORM F-LAES-VISA-INFO                                   
049700            END-IF                                                        
049800           END-IF                                                         
049900         END-IF                                                           
050000       END-IF                                                             
050100*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
050200*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
050300       IF STARTA-ANNAN-BILD                                               
050400         CONTINUE                                                         
050500       ELSE                                                               
050600         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71701 + 4                    
050700         PERFORM IMS-INSERT-MSG                                           
050800       END-IF                                                             
050900     END-IF                                                               
051000                                                                          
051100     MOVE ZERO TO RETURN-CODE                                             
051200     GOBACK                                                               
051300     .                                                                    
051400     EJECT                                                                
051500 A-INIT SECTION.                                                          
051600                                                                          
051700     IF MSG-DUBBLA-TRANSKODER                                             
051800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I71701                 
051900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
052000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
052100     ELSE                                                                 
052200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I71701                  
052300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
052400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
052500     END-IF                                                               
052600                                                                          
052700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
052800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
052900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
053000                                                                          
053100     MOVE LOW-VALUE TO MSG-AREA                                           
053200     MOVE 'W4O717N1' TO MFS-IDMOD                                         
053300     MOVE '4717' TO MOD-IDTRANS                                           
053400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
053500                                                                          
053600     MOVE SPACE                       TO MED-IDMFSINF                     
053700     MOVE SPACE                       TO MED-IDMFSFEL                     
053800                                                                          
053900     IF EGEN-MID OR HELP-MID                                              
054000       CONTINUE                                                           
054100     ELSE                                                                 
054200       MOVE SPACE TO MFS-KDTRTYP                                          
054300       MOVE '7' TO MFS-IDPFK                                              
054400     END-IF                                                               
054500                                                                          
054600     MOVE +0          TO W-IDDISTR-MIN                                    
054700                         W-IDKUNDNR-MIN                                   
054800     MOVE ZERO        TO W-IDRAPPNR-MIN                                   
054900     MOVE LOW-VALUE   TO W-TIDATETIME-MIN-X                               
055000     MOVE HIGH-VALUE  TO W-TIDATETIME-MAX-X                               
055100                                                                          
055200     MOVE +99999      TO W-IDDISTR-MAX                                    
055300     MOVE +9999999    TO W-IDKUNDNR-MAX                                   
055400     MOVE 9999999     TO W-IDRAPPNR-MAX                                   
055500                                                                          
055600     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
055700     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
055800     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
055900     MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                          
056000     MOVE 'M'               TO CURR-KDVALTYP                              
056100     .                                                                    
056200     EJECT                                                                
056300 B-KOLLA-NYCKLAR SECTION.                                                 
056400                                                                          
056500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
056600     MOVE '001'             TO MSGI-KDCALL                                
056700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
056800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
056900     MOVE '4717'            TO MSGI-IDTRANS                               
057000     IF EGEN-MID                                                          
057100        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
057200                                   WS-IDDISTR-IN                          
057300        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
057400                                   WS-IDKUNDNR-IN                         
057500        MOVE MID-IDRAPPNR-IN    TO MSGI-IDRAPPNR                          
057600                                   WS-IDRAPPNR-IN                         
057700                                                                          
057800        MOVE MID-IDUSER-IN      TO WS-IDUSER-IN                           
057900        MOVE MID-FLTOT-IN       TO WS-FLTOT-IN                            
058000     END-IF                                                               
058100     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
058200     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
058300                                                                          
058400*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
058500     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
058600     MOVE '2'             TO MFS-KDMFSFOR                                 
058700                                                                          
058800     MOVE JA TO NYCKLAR-SW                                                
058900                                                                          
059000*    -- KONTROLL AV IDDISTR                                               
059100     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
059200                                                                          
059300     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
059400       MOVE '7'         TO MFS-IDPFK                                      
059500       MOVE SPACE       TO MFS-KDTRTYP                                    
059600       INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO               
059700       IF MSGI-IDDISTR NUMERIC                                            
059800         MOVE MSGI-IDDISTR TO W-IDDISTR-MIN                               
059900                              W-IDDISTR-MAX                               
060000       ELSE                                                               
060100         MOVE NEJ TO NYCKLAR-SW                                           
060200       END-IF                                                             
060300     END-IF                                                               
060400                                                                          
060500*    -- KONTROLL AV IDKUNDNR                                              
060600     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
060700                                                                          
060800     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
060900       MOVE '7'         TO MFS-IDPFK                                      
061000       MOVE SPACE       TO MFS-KDTRTYP                                    
061100       INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
061200       IF MSGI-IDKUNDNR NUMERIC                                           
061300         MOVE MSGI-IDKUNDNR TO W-IDKUNDNR-MIN                             
061400                               W-IDKUNDNR-MAX                             
061500       ELSE                                                               
061600         MOVE NEJ TO NYCKLAR-SW                                           
061700       END-IF                                                             
061800     END-IF                                                               
061900                                                                          
062000*    -- KONTROLL AV IDRAPPNR                                              
062100     MOVE MFS-RENSA-FAELT TO MOD-IDRAPPNR-IN                              
062200                                                                          
062300     IF MID-IDRAPPNR-IN NOT = ALL '+'                                     
062400       MOVE '7'         TO MFS-IDPFK                                      
062500       MOVE SPACE       TO MFS-KDTRTYP                                    
062600       INSPECT MSGI-IDRAPPNR REPLACING LEADING SPACE BY ZERO              
062700       IF MSGI-IDRAPPNR NUMERIC                                           
062800         MOVE MSGI-IDRAPPNR TO W-IDRAPPNR-MIN                             
062900                               W-IDRAPPNR-MAX                             
063000       ELSE                                                               
063100         MOVE NEJ TO NYCKLAR-SW                                           
063200       END-IF                                                             
063300     END-IF                                                               
063400                                                                          
063500*    -- KONTROLL AV IDUSER                                                
063600     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-IN                                
063700                                                                          
063800     IF MID-IDUSER-IN  NOT = ALL '+'                                      
063900       MOVE '7'         TO MFS-IDPFK                                      
064000       MOVE SPACE       TO MFS-KDTRTYP                                    
064100                                                                          
064200       MOVE MID-IDUSER-IN   TO W-IDUSER                                   
064300     END-IF                                                               
064400                                                                          
064500*    -- KONTROLL AV FLTOT (VISA ALLA)                                     
064600     MOVE MFS-RENSA-FAELT       TO MOD-FLTOT-IN                           
064700                                                                          
064800     IF MID-FLTOT-IN  NOT = ALL '+'                                       
064900       MOVE '7'                 TO MFS-IDPFK                              
065000       MOVE SPACE               TO MFS-KDTRTYP                            
065100       IF MID-FLTOT-IN = 'J' OR 'Y'                                       
065200         CONTINUE                                                         
065300       ELSE                                                               
065400         MOVE NEJ TO NYCKLAR-SW                                           
065500       END-IF                                                             
065600     END-IF                                                               
065700                                                                          
065800                                                                          
065900     MOVE MSGI-IDFTG        TO W-IDFTG-MIN                                
066000                               W-IDFTG-MAX                                
066100                               W-IDFTG-WDA2A                              
066200                                                                          
066300     IF GODK-MID OR NYCKLAR-OK                                            
066400       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
066500       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
066600       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
066700       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
066800       MOVE MSGI-IDRAPPNR        TO MOD-IDRAPPNR-UT                       
066900       INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE            
067000       IF EGEN-MID                                                        
067100         IF MFS-FIRST                                                     
067200           IF MID-IDUSER-IN = ALL '+'                                     
067300             MOVE SPACE            TO MOD-IDUSER-UT                       
067400           ELSE                                                           
067500             MOVE MID-IDUSER-IN    TO MOD-IDUSER-UT                       
067600           END-IF                                                         
067700           IF MID-FLTOT-IN = ALL '+'                                      
067800             MOVE SPACE            TO MOD-FLTOT-UT                        
067900           ELSE                                                           
068000             MOVE WS-FLTOT-IN      TO MOD-FLTOT-UT                        
068100           END-IF                                                         
068200           IF MID-IDDISTR-IN = ALL '+'                                    
068300             MOVE SPACE            TO MOD-IDDISTR-UT                      
068400           END-IF                                                         
068500           IF MID-IDKUNDNR-IN = ALL '+'                                   
068600             MOVE SPACE            TO MOD-IDKUNDNR-UT                     
068700           END-IF                                                         
068800           IF MID-IDRAPPNR-IN = ALL '+'                                   
068900             MOVE SPACE            TO MOD-IDRAPPNR-UT                     
069000           END-IF                                                         
069100         ELSE                                                             
069200           IF SPAR-IDTRANS = '4717'                                       
069300             IF SPAR-MID-IDUSER = ALL '+'                                 
069400               MOVE SPACE            TO MOD-IDUSER-UT                     
069500             ELSE                                                         
069600               MOVE SPAR-MID-IDUSER  TO MOD-IDUSER-UT                     
069700             END-IF                                                       
069800             IF SPAR-MID-FLTOT = ALL '+'                                  
069900               MOVE SPACE            TO MOD-FLTOT-UT                      
070000             ELSE                                                         
070100               MOVE SPAR-MID-FLTOT   TO MOD-FLTOT-UT                      
070200             END-IF                                                       
070300             IF SPAR-MID-IDDISTR  = ALL '+'                               
070400               MOVE SPACE            TO MOD-IDDISTR-UT                    
070500             END-IF                                                       
070600             IF SPAR-MID-IDKUNDNR = ALL '+'                               
070700               MOVE SPACE            TO MOD-IDKUNDNR-UT                   
070800             END-IF                                                       
070900             IF SPAR-MID-IDRAPPNR = ALL '+'                               
071000               MOVE SPACE            TO MOD-IDRAPPNR-UT                   
071100             END-IF                                                       
071200           END-IF                                                         
071300         END-IF                                                           
071400       ELSE                                                               
071500*-OM MAN HOPPAR FRÅN EN ANNAN BILD SÅ VISAS UNIK LEVANM.                  
071600         MOVE SPACE              TO MOD-IDUSER-UT                         
071700         MOVE SPACE              TO MOD-FLTOT-UT                          
071800         IF MSGI-IDDISTR NUMERIC                                          
071900           MOVE MSGI-IDDISTR     TO W-IDDISTR-WDA2A                       
072000         END-IF                                                           
072100         IF MSGI-IDKUNDNR NUMERIC                                         
072200           MOVE MSGI-IDKUNDNR    TO W-IDKUNDNR-WDA2A                      
072300         END-IF                                                           
072400         IF MSGI-IDRAPPNR NUMERIC                                         
072500           MOVE MSGI-IDRAPPNR    TO W-IDRAPPNR-WDA2A                      
072600         END-IF                                                           
072700       END-IF                                                             
072800     ELSE                                                                 
072900       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
073000                               MOD-IDKUNDNR-UT                            
073100                               MOD-IDRAPPNR-UT                            
073200                               MOD-IDUSER-UT                              
073300                               MOD-FLTOT-UT                               
073400     END-IF                                                               
073500                                                                          
073600     IF EGEN-MID                                                          
073700       IF MID-FLTOT-IN = ALL '+'                                          
073800         IF ( MID-IDDISTR-IN NOT = ALL '+' ) AND                          
073900            ( MID-IDUSER-IN NOT = ALL '+')                                
074000           MOVE ERR-FLERA-FUNKTIONER    TO MED-IDMFSFEL                   
074100           MOVE NEJ TO NYCKLAR-SW                                         
074200         END-IF                                                           
074300                                                                          
074400         IF (MID-IDDISTR-IN = ALL '+') AND                                
074500            (MID-IDUSER-IN = ALL '+')                                     
074600           IF MID-IDKUNDNR-IN NOT = ALL '+'                               
074700             MOVE NEJ TO NYCKLAR-SW                                       
074800           END-IF                                                         
074900           IF MID-IDRAPPNR-IN NOT = ALL '+'                               
075000             MOVE NEJ TO NYCKLAR-SW                                       
075100           END-IF                                                         
075200         END-IF                                                           
075300       ELSE                                                               
075400         IF ( MID-IDDISTR-IN NOT = ALL '+' ) OR                           
075500            ( MID-IDKUNDNR-IN NOT = ALL '+') OR                           
075600            ( MID-IDRAPPNR-IN NOT = ALL '+') OR                           
075700            ( MID-IDUSER-IN NOT = ALL '+')                                
075800           MOVE ERR-FLERA-FUNKTIONER    TO MED-IDMFSFEL                   
075900           MOVE NEJ TO NYCKLAR-SW                                         
076000         END-IF                                                           
076100       END-IF                                                             
076200     END-IF                                                               
076300                                                                          
076400     IF NYCKLAR-FEL                                                       
076500       IF MED-IDMFSFEL  = SPACE                                           
076600         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
076700       END-IF                                                             
076800       CALL WMEDKONV USING MED-WMEDAREA                                   
076900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
077000       PERFORM MFS-RENSA-FAELT-IN                                         
077100       PERFORM MFS-RENSA-FAELT-UT                                         
077200     END-IF                                                               
077300     .                                                                    
077400     EJECT                                                                
077500 C-FOERSTA-SIDA SECTION.                                                  
077600                                                                          
077700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
077800     CALL WMEDKONV USING MED-WMEDAREA                                     
077900     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
078000                                                                          
078100     PERFORM MFS-RENSA-FAELT-IN                                           
078200                                                                          
078300*FIX FÖR ATT KUNNA TRYCKA PF7                                             
078400     IF  ( MID-IDDISTR-IN = ALL '+' )  AND                                
078500         ( MID-IDKUNDNR-IN = ALL '+' ) AND                                
078600         ( MID-IDRAPPNR-IN = ALL '+' ) AND                                
078700         ( MID-IDUSER-IN = ALL '+' )   AND                                
078800         ( MID-FLTOT-IN = ALL '+' )                                       
078900                                                                          
079000       IF SPAR-IDTRANS = '4717'                                           
079100         MOVE SPAR-MID-IDDISTR  TO WS-IDDISTR-IN                          
079200         IF SPAR-MID-IDDISTR NUMERIC                                      
079300            MOVE SPAR-MID-IDDISTR TO  W-IDDISTR-MIN                       
079400                                      W-IDDISTR-MAX                       
079500                                      W-IDDISTR-4103                      
079600         END-IF                                                           
079700         MOVE SPAR-MID-IDKUNDNR TO WS-IDKUNDNR-IN                         
079800         IF SPAR-MID-IDKUNDNR NUMERIC                                     
079900            MOVE SPAR-MID-IDKUNDNR TO W-IDKUNDNR-MIN                      
080000                                      W-IDKUNDNR-MAX                      
080100                                      W-IDKUNDNR-4103                     
080200         END-IF                                                           
080300         MOVE SPAR-MID-IDRAPPNR TO WS-IDRAPPNR-IN                         
080400         IF SPAR-MID-IDRAPPNR NUMERIC                                     
080500            MOVE SPAR-MID-IDRAPPNR TO W-IDRAPPNR-MIN                      
080600                                      W-IDRAPPNR-MAX                      
080700                                      W-IDRAPPNR-4103                     
080800         END-IF                                                           
080900         MOVE SPAR-MID-IDUSER      TO WS-IDUSER-IN                        
081000                                      W-IDUSER                            
081100                                                                          
081200         MOVE SPAR-MID-FLTOT       TO WS-FLTOT-IN                         
081300       ELSE                                                               
081400         IF MSGI-IDDISTR NUMERIC                                          
081500           MOVE MSGI-IDDISTR    TO W-IDDISTR-MIN                          
081600                                   W-IDDISTR-MAX                          
081700                                   W-IDDISTR-4103                         
081800         END-IF                                                           
081900                                                                          
082000         IF MSGI-IDKUNDNR NUMERIC                                         
082100           MOVE MSGI-IDKUNDNR   TO W-IDKUNDNR-MIN                         
082200                                   W-IDKUNDNR-MAX                         
082300                                   W-IDKUNDNR-4103                        
082400         END-IF                                                           
082500                                                                          
082600         IF MSGI-IDRAPPNR NUMERIC                                         
082700           MOVE MSGI-IDRAPPNR   TO W-IDRAPPNR-MIN                         
082800                                   W-IDRAPPNR-MAX                         
082900                                   W-IDRAPPNR-4103                        
083000         END-IF                                                           
083100       END-IF                                                             
083200     END-IF                                                               
083300     .                                                                    
083400     EJECT                                                                
083500 D-NAESTA-SIDA SECTION.                                                   
083600                                                                          
083700     IF SPAR-IDTRANS = '4717'                                             
083800       MOVE SPAR-IDDISTR-NEXT  TO W-IDDISTR-MIN                           
083900                                  W-IDDISTR-4103                          
084000       MOVE SPAR-IDKUNDNR-NEXT TO W-IDKUNDNR-MIN                          
084100                                  W-IDKUNDNR-4103                         
084200       MOVE SPAR-IDRAPPNR-NEXT TO W-IDRAPPNR-MIN                          
084300                                  W-IDRAPPNR-4103                         
084400       MOVE SPAR-IDDC-NEXT     TO W-IDDC-4104                             
084500       MOVE SPAR-KDKRENOT-NEXT TO W-KDKRENOT-4104                         
084600       MOVE SPAR-IDUSER-NEXT   TO W-IDUSER                                
084700                                                                          
084800       MOVE SPAR-MID-IDUSER    TO WS-IDUSER-IN                            
084900       MOVE SPAR-MID-FLTOT     TO WS-FLTOT-IN                             
085000       MOVE SPAR-MID-IDDISTR   TO WS-IDDISTR-IN                           
085100       MOVE SPAR-MID-IDKUNDNR  TO WS-IDKUNDNR-IN                          
085200       MOVE SPAR-MID-IDRAPPNR  TO WS-IDRAPPNR-IN                          
085300                                                                          
085400       IF WS-IDUSER-IN NOT = ALL '+'                                      
085500         CONTINUE                                                         
085600       ELSE                                                               
085700         IF WS-IDDISTR-IN NOT = ALL '+'                                   
085800           MOVE SPAR-IDDISTR-NEXT  TO W-IDDISTR-MAX                       
085900         END-IF                                                           
086000         IF WS-IDKUNDNR-IN NOT = ALL '+'                                  
086100           MOVE SPAR-IDKUNDNR-NEXT TO W-IDKUNDNR-MAX                      
086200         END-IF                                                           
086300         IF WS-IDRAPPNR-IN NOT = ALL '+'                                  
086400           MOVE SPAR-IDRAPPNR-NEXT TO W-IDRAPPNR-MAX                      
086500         END-IF                                                           
086600       END-IF                                                             
086700     ELSE                                                                 
086800       PERFORM MFS-RENSA-FAELT-IN                                         
086900     END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 E-SAMMA-SIDA SECTION.                                                    
087300                                                                          
087400     IF EGEN-MID OR HELP-MID                                              
087500       IF SPAR-IDTRANS = '4717'                                           
087600         MOVE SPAR-IDDISTR-ENTER  TO W-IDDISTR-MIN                        
087700         MOVE SPAR-IDKUNDNR-ENTER TO W-IDKUNDNR-MIN                       
087800         MOVE SPAR-IDRAPPNR-ENTER TO W-IDRAPPNR-MIN                       
087900         MOVE SPAR-IDDC-ENTER     TO W-IDDC-4104                          
088000         MOVE SPAR-KDKRENOT-ENTER TO W-KDKRENOT-4104                      
088100         MOVE SPAR-IDUSER-ENTER   TO W-IDUSER                             
088200                                                                          
088300         MOVE SPAR-MID-IDUSER     TO WS-IDUSER-IN                         
088400         MOVE SPAR-MID-FLTOT      TO WS-FLTOT-IN                          
088500         MOVE SPAR-MID-IDDISTR    TO WS-IDDISTR-IN                        
088600         MOVE SPAR-MID-IDKUNDNR   TO WS-IDKUNDNR-IN                       
088700         MOVE SPAR-MID-IDRAPPNR   TO WS-IDRAPPNR-IN                       
088800                                                                          
088900         IF WS-IDUSER-IN NOT = ALL '+'                                    
089000           CONTINUE                                                       
089100         ELSE                                                             
089200           IF WS-IDDISTR-IN NOT = ALL '+'                                 
089300             MOVE SPAR-IDDISTR-ENTER  TO W-IDDISTR-MAX                    
089400           END-IF                                                         
089500           IF WS-IDKUNDNR-IN NOT = ALL '+'                                
089600             MOVE SPAR-IDKUNDNR-ENTER TO W-IDKUNDNR-MAX                   
089700           END-IF                                                         
089800           IF WS-IDRAPPNR-IN NOT = ALL '+'                                
089900             MOVE SPAR-IDRAPPNR-ENTER TO W-IDRAPPNR-MAX                   
090000           END-IF                                                         
090100         END-IF                                                           
090200         IF MID-INPUT = ALL '+'                                           
090300           PERFORM MFS-RENSA-FAELT-IN                                     
090400         ELSE                                                             
090500          IF MID-FLGODK = 'J' OR 'Y'                                      
090600             MOVE  'JA'  TO  FLGODK-SW                                    
090700             MOVE INF-PRESS-PF11 TO MED-IDMFSINF                          
090800             CALL WMEDKONV USING MED-WMEDAREA                             
090900             MOVE MED-MFSINF TO MOD-TEMFSFEL                              
091000             PERFORM EAA-MID-INDATA-TILL-MOD                              
091100          ELSE                                                            
091200             MOVE +1 TO INDX                                              
091300             PERFORM UNTIL INDX > MAX-INDX                                
091400             IF MID-KDCMD (INDX) NOT  = '+'                               
091500               IF MID-KDCMD (INDX) = 'L' OR 'T'                           
091600                 PERFORM EB-STARTA-ANNAN-BILD                             
091700                 MOVE JA           TO SW-STARTA-ANNAN-BILD                
091800                 MOVE MAX-INDX TO INDX                                    
091900               ELSE                                                       
092000                 IF MID-KDCMD (INDX) = 'A' OR 'R'                         
092100                   MOVE INF-PRESS-PF11 TO MED-IDMFSINF                    
092200                   CALL WMEDKONV USING MED-WMEDAREA                       
092300                   MOVE MED-MFSINF TO MOD-TEMFSFEL                        
092400                   PERFORM EA-MID-INDATA-TILL-MOD                         
092500                 ELSE                                                     
092600                   MOVE MFS-ALFA-FAELT-FEL TO                             
092700                                     MOD-KDCMD-ATTR (INDX)                
092800                   MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (INDX)             
092900                   MOVE ERR-WRONG-COMMAND-CODE TO MED-IDMFSFEL            
093000                   CALL WMEDKONV USING MED-WMEDAREA                       
093100                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
093200                   MOVE MAX-INDX TO INDX                                  
093300                   MOVE NEJ        TO SW-STARTA-ANNAN-BILD                
093400                 END-IF                                                   
093500               END-IF                                                     
093600             END-IF                                                       
093700             ADD +1 TO INDX                                               
093800           END-PERFORM                                                    
093900          END-IF                                                          
094000         END-IF                                                           
094100       ELSE                                                               
094200         PERFORM MFS-RENSA-FAELT-IN                                       
094300       END-IF                                                             
094400     ELSE                                                                 
094500       PERFORM MFS-RENSA-FAELT-IN                                         
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900 EA-MID-INDATA-TILL-MOD SECTION.                                          
095000                                                                          
095100     MOVE +1                          TO INDX                             
095200     PERFORM UNTIL INDX               >  MAX-INDX                         
095300       IF MID-KDCMD(INDX)            NOT = ALL '+'                        
095400          MOVE MFS-ROER-EJ-FAELT     TO MOD-KDCMD(INDX)                   
095500          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR(INDX)              
095600       ELSE                                                               
095700          MOVE MFS-RENSA-FAELT       TO MOD-KDCMD(INDX)                   
095800       END-IF                                                             
095900       ADD +1                        TO INDX                              
096000     END-PERFORM                                                          
096100     .                                                                    
096200     EJECT                                                                
096300 EAA-MID-INDATA-TILL-MOD SECTION.                                         
096400                                                                          
096500       IF MID-FLGODK = ALL '+'                                            
096600          MOVE MFS-RENSA-FAELT       TO MOD-FLGODK                        
096700       ELSE                                                               
096800          MOVE MFS-ROER-EJ-FAELT     TO MOD-FLGODK                        
096900          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLGODK-ATTR                   
097000       END-IF                                                             
097100     .                                                                    
097200     EJECT                                                                
097300 EB-STARTA-ANNAN-BILD SECTION.                                            
097400                                                                          
097500     MOVE MID-IDDISTR (INDX)             TO MSGI-IDDISTR                  
097600     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
097700     MOVE MID-IDKUNDNR(INDX)             TO MSGI-IDKUNDNR                 
097800     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
097900     MOVE MID-IDRAPPNR(INDX)             TO MSGI-IDRAPPNR                 
098000     INSPECT MSGI-IDRAPPNR REPLACING LEADING SPACE BY ZERO                
098100                                                                          
098200     IF MID-KDCMD (INDX) = 'T'                                            
098300       MOVE MID-IDDC(INDX)               TO MSGI-IDDC-KEY                 
098400       MOVE MID-KDKRENOT(INDX)           TO MSGI-KDKRENOT                 
098500                                                                          
098600       MOVE '001'                        TO MSGI-KDCALL                   
098700       MOVE MSG-SIGNON-USERID            TO MSGI-IDUSER                   
098800       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
098900                                                                          
099000       STRING 'W' WS-IDTRANS-HOPP-1 (1:1)                                 
099100              'T' WS-IDTRANS-HOPP-1 (2:3) '  '                            
099200                  DELIMITED BY SIZE INTO P-TO-P-KDTRANS                   
099300     ELSE                                                                 
099400                                                                          
099500       MOVE '001'                        TO MSGI-KDCALL                   
099600       MOVE MSG-SIGNON-USERID            TO MSGI-IDUSER                   
099700       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
099800                                                                          
099900       STRING 'W' WS-IDTRANS-HOPP (1:1)                                   
100000              'T' WS-IDTRANS-HOPP (2:3) '  '                              
100100                  DELIMITED BY SIZE INTO P-TO-P-KDTRANS                   
100200                                                                          
100300     END-IF                                                               
100400                                                                          
100500     PERFORM S01-INSERT-ALTMSG                                            
100600     .                                                                    
100700     EJECT                                                                
100800 F-LAES-VISA-INFO SECTION.                                                
100900*- MAN KAN FRÅGA PÅ BARA IDUSER ELLER BARA DISTRIKT ELLER                 
101000*- DISTRIKT + KUND ELLER DISTR + KUND + RAPPORT                           
101100                                                                          
101200     IF MFS-UPDATE                                                        
101300       IF SPAR-IDTRANS = '4717'                                           
101400         IF SPAR-MID-IDUSER = ALL '+'                                     
101500           IF SPAR-MID-IDDISTR NOT = ALL '+'                              
101600             MOVE SPAR-MID-IDDISTR  TO W-IDDISTR-MIN                      
101700                                       W-IDDISTR-MAX                      
101800           END-IF                                                         
101900           IF SPAR-MID-IDKUNDNR NOT = ALL '+'                             
102000             MOVE SPAR-MID-IDKUNDNR TO W-IDKUNDNR-MIN                     
102100                                       W-IDKUNDNR-MAX                     
102200           END-IF                                                         
102300           IF SPAR-MID-IDRAPPNR NOT = ALL '+'                             
102400             MOVE SPAR-MID-IDRAPPNR TO W-IDRAPPNR-MIN                     
102500                                       W-IDRAPPNR-MAX                     
102600           END-IF                                                         
102700           PERFORM IMS-GU-WDA2A1                                          
102800         ELSE                                                             
102900           MOVE SPAR-MID-IDUSER     TO W-IDUSER                           
103000           PERFORM IMS-GU-WDA2A1-IDUSER                                   
103100         END-IF                                                           
103200         MOVE SPAR-MID-IDUSER        TO WS-IDUSER-IN                      
103300         MOVE SPAR-MID-FLTOT         TO WS-FLTOT-IN                       
103400         MOVE SPAR-MID-IDDISTR       TO WS-IDDISTR-IN                     
103500         MOVE SPAR-MID-IDKUNDNR      TO WS-IDKUNDNR-IN                    
103600         MOVE SPAR-MID-IDRAPPNR      TO WS-IDRAPPNR-IN                    
103700       ELSE                                                               
103800         IF MSGI-IDDISTR NUMERIC                                          
103900           MOVE MSGI-IDDISTR     TO W-IDDISTR-MIN                         
104000                                    W-IDDISTR-MAX                         
104100         END-IF                                                           
104200         IF MSGI-IDKUNDNR NUMERIC                                         
104300           MOVE MSGI-IDKUNDNR    TO W-IDKUNDNR-MIN                        
104400                                    W-IDKUNDNR-MAX                        
104500         END-IF                                                           
104600         IF MSGI-IDRAPPNR NUMERIC                                         
104700           MOVE MSGI-IDRAPPNR    TO W-IDRAPPNR-MIN                        
104800                                    W-IDRAPPNR-MAX                        
104900         END-IF                                                           
105000         PERFORM IMS-GU-WDA2A1                                            
105100       END-IF                                                             
105200     ELSE                                                                 
105300       IF EGEN-MID                                                        
105400         IF WS-IDUSER-IN = ALL '+'                                        
105500*- LÄS ALLA RADER FÖR GIVNA NYCKLAR/IDLEVANM                              
105600           PERFORM IMS-GU-WDA2A1                                          
105700         ELSE                                                             
105800*- LÄS ALLA RADER FÖR DENNA ISSUER/IDUSER-ADM                             
105900           PERFORM IMS-GU-WDA2A1-IDUSER                                   
106000         END-IF                                                           
106100       ELSE                                                               
106200*- LÄS UNIK LEV.ANM.                                                      
106300         PERFORM IMS-GU-WDA2A1-UNIK                                       
106400       END-IF                                                             
106500     END-IF                                                               
106600                                                                          
106700     IF SEGMENT-SAKNAS                                                    
106800       IF MED-IDMFSINF = '101'                                            
106900         CONTINUE                                                         
107000       ELSE                                                               
107100         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
107200         CALL WMEDKONV USING MED-WMEDAREA                                 
107300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
107400       END-IF                                                             
107500       PERFORM MFS-RENSA-FAELT-UT                                         
107600       PERFORM MFS-STAENG-FAELT-IN                                        
107700                                                                          
107800       MOVE WS-IDUSER-IN    TO SPAR-MID-IDUSER                            
107900       MOVE WS-FLTOT-IN     TO SPAR-MID-FLTOT                             
108000       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
108100       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
108200       MOVE WS-IDRAPPNR-IN  TO SPAR-MID-IDRAPPNR                          
108300     ELSE                                                                 
108400       MOVE SEQA-IDDISTR     TO SPAR-IDDISTR-ENTER                        
108500       MOVE SEQA-IDKUNDNR    TO SPAR-IDKUNDNR-ENTER                       
108600       MOVE SEQA-IDRAPPNR    TO SPAR-IDRAPPNR-ENTER                       
108700       MOVE W-IDUSER         TO SPAR-IDUSER-ENTER                         
108800       MOVE WS-IDUSER-IN     TO SPAR-MID-IDUSER                           
108900       MOVE WS-FLTOT-IN      TO SPAR-MID-FLTOT                            
109000       MOVE WS-IDDISTR-IN    TO SPAR-MID-IDDISTR                          
109100       MOVE WS-IDKUNDNR-IN   TO SPAR-MID-IDKUNDNR                         
109200       MOVE WS-IDRAPPNR-IN   TO SPAR-MID-IDRAPPNR                         
109300                                                                          
109400       IF EGEN-MID                                                        
109500         IF WS-IDRAPPNR-IN = ALL '+'                                      
109600           PERFORM FA-LAES-GRUNDDATA                                      
109700                                                                          
109800*--FLYTTA NEXTNYCKLAR FÖR BLÄDDRING.                                      
109900                                                                          
110000           IF SEGMENT-FINNS                                               
110100             MOVE SEQA-IDDISTR    TO SPAR-IDDISTR-NEXT                    
110200             MOVE SEQA-IDKUNDNR   TO SPAR-IDKUNDNR-NEXT                   
110300             MOVE SEQA-IDRAPPNR   TO SPAR-IDRAPPNR-NEXT                   
110400             MOVE W-IDUSER        TO SPAR-IDUSER-NEXT                     
110500             MOVE W-IDDC-4104     TO SPAR-IDDC-NEXT                       
110600             MOVE W-KDKRENOT-4104 TO SPAR-KDKRENOT-NEXT                   
110700             MOVE WS-IDUSER-IN    TO SPAR-MID-IDUSER                      
110800             MOVE WS-FLTOT-IN     TO SPAR-MID-FLTOT                       
110900             MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                     
111000             MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                    
111100             MOVE WS-IDRAPPNR-IN  TO SPAR-MID-IDRAPPNR                    
111200                                                                          
111300             IF MED-IDMFSINF = SPACE OR '006'                             
111400               MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                  
111500               CALL WMEDKONV USING MED-WMEDAREA                           
111600               MOVE MED-MFSINF TO MOD-TEMFSINF                            
111700             END-IF                                                       
111800           ELSE                                                           
111900             MOVE SEQA-IDDISTR    TO SPAR-IDDISTR-NEXT                    
112000             MOVE SEQA-IDKUNDNR   TO SPAR-IDKUNDNR-NEXT                   
112100             MOVE SEQA-IDRAPPNR   TO SPAR-IDRAPPNR-NEXT                   
112200             MOVE W-IDUSER        TO SPAR-IDUSER-NEXT                     
112300             MOVE W-IDDC-4104     TO SPAR-IDDC-NEXT                       
112400             MOVE W-KDKRENOT-4104 TO SPAR-KDKRENOT-NEXT                   
112500             MOVE WS-IDUSER-IN    TO SPAR-MID-IDUSER                      
112600             MOVE WS-FLTOT-IN     TO SPAR-MID-FLTOT                       
112700             MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                     
112800             MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                    
112900             MOVE WS-IDRAPPNR-IN  TO SPAR-MID-IDRAPPNR                    
113000             IF MED-IDMFSINF = '006'                                      
113100               CONTINUE                                                   
113200             ELSE                                                         
113300               IF MED-IDMFSINF = SPACE  AND                               
113400                  MED-IDMFSFEL = SPACE                                    
113500                 MOVE INF-LAST-PAGE  TO MED-IDMFSINF                      
113600                 CALL WMEDKONV USING MED-WMEDAREA                         
113700                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
113800               END-IF                                                     
113900             END-IF                                                       
114000           END-IF                                                         
114100         ELSE                                                             
114200           PERFORM FB-VISA-UNIK-RAPPORT                                   
114300         END-IF                                                           
114400       ELSE                                                               
114500         PERFORM FB-VISA-UNIK-RAPPORT                                     
114600       END-IF                                                             
114700                                                                          
114800       MOVE '002'      TO MSGI-KDCALL                                     
114900       MOVE '4717'   TO SPAR-IDTRANS                                      
115000       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
115100       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
115200     END-IF                                                               
115300     .                                                                    
115400     EJECT                                                                
115500 FA-LAES-GRUNDDATA SECTION.                                               
115600                                                                          
115700     MOVE +1 TO INDX                                                      
115800                                                                          
115900     PERFORM UNTIL INDX > MAX-INDX                                        
116000       IF SEGMENT-FINNS                                                   
116100         MOVE SEQA-IDDISTR  TO MOD-IDDISTR (INDX)                         
116200         MOVE SEQA-IDKUNDNR TO MOD-IDKUNDNR (INDX)                        
116300         MOVE SEQA-IDRAPPNR TO MOD-IDRAPPNR (INDX)                        
116400         MOVE SEQA-IDUSER-ADM  TO MOD-IDUSER (INDX)                       
116500         MOVE SEQA-BEANST(1:9) TO MOD-BEANST (INDX)                       
116600                                                                          
116700         MOVE SEQA-IDDISTR  TO W-IDDISTR-4103                             
116800         MOVE SEQA-IDKUNDNR TO W-IDKUNDNR-4103                            
116900         MOVE SEQA-IDRAPPNR TO W-IDRAPPNR-4103                            
117000         PERFORM IMS-GU-WDGX4103                                          
117100         IF SEGMENT-FINNS                                                 
117200           PERFORM FAA-VISA-RADDATA                                       
117300         ELSE                                                             
117400           PERFORM MFS-RENSA-4103-RAD-UT                                  
117500           MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)          
117600           MOVE SPACE         TO W-IDDC-4104                              
117700           MOVE SPACE         TO W-KDKRENOT-4104                          
117800           ADD +1 TO INDX                                                 
117900         END-IF                                                           
118000                                                                          
118100         IF WS-IDUSER-IN = ALL '+'                                        
118200           IF SEGMENT-FINNS AND INDX > MAX-INDX                           
118300             CONTINUE                                                     
118400           ELSE                                                           
118500             PERFORM IMS-GN-WDA2A1                                        
118600             IF SEGMENT-FINNS AND INDX > MAX-INDX                         
118700               MOVE SPACE         TO W-IDDC-4104                          
118800               MOVE SPACE         TO W-KDKRENOT-4104                      
118900             END-IF                                                       
119000           END-IF                                                         
119100         ELSE                                                             
119200           IF SEGMENT-FINNS AND INDX > MAX-INDX                           
119300             CONTINUE                                                     
119400           ELSE                                                           
119500             PERFORM IMS-GN-WDA2A1-IDUSER                                 
119600             IF SEGMENT-FINNS AND INDX > MAX-INDX                         
119700               MOVE SPACE         TO W-IDDC-4104                          
119800               MOVE SPACE         TO W-KDKRENOT-4104                      
119900             END-IF                                                       
120000           END-IF                                                         
120100         END-IF                                                           
120200       ELSE                                                               
120300         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
120400         MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)            
120500         ADD 1 TO INDX                                                    
120600       END-IF                                                             
120700     END-PERFORM                                                          
120800     .                                                                    
120900     EJECT                                                                
121000 FAA-VISA-RADDATA SECTION.                                                
121100                                                                          
121200     IF MFS-FIRST OR MFS-UPDATE OR INDX > +1                              
121300       PERFORM IMS-GNP-WDGX4104                                           
121400     ELSE                                                                 
121500       IF W-IDDC-4104 = SPACE                                             
121600         PERFORM IMS-GNP-WDGX4104                                         
121700       ELSE                                                               
121800         PERFORM IMS-GNP-WDGX4104-KVAL                                    
121900       END-IF                                                             
122000     END-IF                                                               
122100                                                                          
122200                                                                          
122300     IF SEGMENT-SAKNAS                                                    
122400       PERFORM MFS-RENSA-4103-RAD-UT                                      
122500       MOVE MFS-STAENG-FAELT-NOMOD TO                                     
122600                          MOD-KDCMD-ATTR (INDX)                           
122700       MOVE SPACE         TO W-IDDC-4104                                  
122800       MOVE SPACE         TO W-KDKRENOT-4104                              
122900                                                                          
123000       ADD 1 TO INDX                                                      
123100     ELSE                                                                 
123200       IF INDX = +1                                                       
123300         MOVE 4104-IDDC      TO SPAR-IDDC-ENTER                           
123400         MOVE 4104-KDKRENOT  TO SPAR-KDKRENOT-ENTER                       
123500       END-IF                                                             
123600                                                                          
123700       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
123800         IF SEGMENT-FINNS                                                 
123900           MOVE SEQA-IDDISTR  TO MOD-IDDISTR (INDX)                       
124000           MOVE SEQA-IDKUNDNR TO MOD-IDKUNDNR (INDX)                      
124100           MOVE SEQA-IDRAPPNR TO MOD-IDRAPPNR (INDX)                      
124200           MOVE SEQA-IDUSER-ADM  TO MOD-IDUSER (INDX)                     
124300           MOVE SEQA-BEANST(1:9) TO MOD-BEANST (INDX)                     
124400                                                                          
124500           MOVE 4104-IDDC     TO MOD-IDDC (INDX)                          
124600                                 W-IDDC-4104                              
124700           MOVE 4104-KDKRENOT TO MOD-KDKRENOT (INDX)                      
124800                                 W-KDKRENOT-4104                          
124900                                                                          
125000*- PRISET SKALL VARA I SEK.MAN HÄMTAR KURSEN OCH RÄKNAR OM.               
125100           IF 4104-KDVALISO = 'SEK'                                       
125200             MOVE 4104-SUKRENOT  TO MOD-SUKRENOT (INDX)                   
125300           ELSE                                                           
125400*CHINA-PRICE1                                                             
125500*INDIA-PRICE1                                                             
125600*KOREA-PRICE1                                                             
125700             MOVE W-IDFTG-MIN    TO WS-IDFTG                              
125800             IF (4104-KDVALISO = 'CNY' AND IDFTG-CN) OR                   
125900                (4104-KDVALISO = 'INR' AND IDFTG-IN) OR                   
126000                (4104-KDVALISO = 'KRW' AND IDFTG-KR) OR                   
126010                (4104-KDVALISO = 'TRY' AND IDFTG-TR) OR                   
126011                (4104-KDVALISO = 'MXN' AND IDFTG-MX) OR                   
126012                (4104-KDVALISO = 'BRL' AND IDFTG-BR) OR                   
126020                (4104-KDVALISO = 'MYR' AND IDFTG-MY) OR                   
126030                (4104-KDVALISO = 'THB' AND IDFTG-TH) OR                   
126040                (4104-KDVALISO = 'TWD' AND IDFTG-TW) OR                   
126050                (4104-KDVALISO = 'ZAR' AND IDFTG-ZA)                      
126100               MOVE 4104-SUKRENOT TO MOD-SUKRENOT (INDX)                  
126200             ELSE                                                         
126300               MOVE 4104-KDVALISO TO CURR-KDVALISO-ROW                    
126400***   LÄS PRKURS HTYP 9305             *****                              
126500               CALL W510CURR USING CURR-W510CURR WDG2-PCB                 
126600               IF CURR-KDSVAR = ' '                                       
126700                 CONTINUE                                                 
126800               ELSE                                                       
126900                 MOVE 1           TO CURR-PRKURS-NEW                      
127000               END-IF                                                     
127100               COMPUTE MOD-SUKRENOT(INDX) ROUNDED =                       
127200                         4104-SUKRENOT * CURR-PRKURS-NEW                  
127300               END-COMPUTE                                                
127400             END-IF                                                       
127500           END-IF                                                         
127600                                                                          
127700           PERFORM IMS-GNP-WDGX4106                                       
127800           IF SEGMENT-SAKNAS                                              
127900             MOVE MFS-RENSA-FAELT TO MOD-BEANST-GODK (INDX)               
128000                                     MOD-IDANSTNR-GODK (INDX)             
128100                                     MOD-TIMMDD (INDX)                    
128200                                     MOD-FLKLAR  (INDX)                   
128300           ELSE                                                           
128400             MOVE 4106-BEANST-GODK(1:8) TO                                
128500                                 MOD-BEANST-GODK (INDX)                   
128600             MOVE 4106-IDUSER-GODK(3:5) TO                                
128700                                 MOD-IDANSTNR-GODK (INDX)                 
128800             MOVE 4106-TIUPPDAT TO MOD-TIMMDD (INDX)                      
128900             IF 4106-FLKLAR = 'J'                                         
129000               MOVE 'Y'           TO MOD-FLKLAR (INDX)                    
129100             ELSE                                                         
129200               MOVE 4106-FLKLAR   TO MOD-FLKLAR (INDX)                    
129300             END-IF                                                       
129400                                                                          
129500             PERFORM IMS-GNP-WDGX4106                                     
129600             PERFORM UNTIL SEGMENT-SAKNAS OR INDX = MAX-INDX              
129700               ADD 1 TO INDX                                              
129800               MOVE MFS-RENSA-FAELT TO MOD-IDDISTR(INDX)                  
129900                                       MOD-IDKUNDNR(INDX)                 
130000                                       MOD-IDRAPPNR(INDX)                 
130100                                       MOD-IDDC(INDX)                     
130200                                       MOD-KDKRENOT(INDX)                 
130300                                       MOD-SUKRENOT(INDX)                 
130400                                       MOD-BEANST(INDX)                   
130500                                       MOD-IDUSER(INDX)                   
130600               MOVE 4106-BEANST-GODK(1:8) TO                              
130700                                   MOD-BEANST-GODK (INDX)                 
130800               MOVE 4106-IDUSER-GODK(3:5) TO                              
130900                                   MOD-IDANSTNR-GODK (INDX)               
131000               MOVE 4106-TIUPPDAT TO MOD-TIMMDD (INDX)                    
131100               IF 4106-FLKLAR = 'J'                                       
131200                 MOVE 'Y'         TO MOD-FLKLAR (INDX)                    
131300               ELSE                                                       
131400                 MOVE 4106-FLKLAR TO MOD-FLKLAR (INDX)                    
131500               END-IF                                                     
131600                                                                          
131700               MOVE MFS-STAENG-FAELT-NOMOD TO                             
131800                                     MOD-KDCMD-ATTR(INDX)                 
131900                                                                          
132000               PERFORM IMS-GNP-WDGX4106                                   
132100             END-PERFORM                                                  
132200           END-IF                                                         
132300           ADD 1 TO INDX                                                  
132400         END-IF                                                           
132500         IF SEGMENT-FINNS AND INDX > MAX-INDX                             
132600           CONTINUE                                                       
132700         ELSE                                                             
132800           PERFORM IMS-GNP-WDGX4104                                       
132900           IF SEGMENT-FINNS AND INDX > MAX-INDX                           
133000             MOVE 4104-IDDC     TO W-IDDC-4104                            
133100             MOVE 4104-KDKRENOT TO W-KDKRENOT-4104                        
133200           END-IF                                                         
133300         END-IF                                                           
133400       END-PERFORM                                                        
133500     END-IF                                                               
133600     .                                                                    
133700     EJECT                                                                
133800 FB-VISA-UNIK-RAPPORT SECTION.                                            
133900                                                                          
134000     MOVE +1 TO INDX                                                      
134100     MOVE SEQA-IDDISTR  TO MOD-IDDISTR (INDX)                             
134200     MOVE SEQA-IDKUNDNR TO MOD-IDKUNDNR (INDX)                            
134300     MOVE SEQA-IDRAPPNR TO MOD-IDRAPPNR (INDX)                            
134400     MOVE SEQA-IDUSER-ADM  TO MOD-IDUSER (INDX)                           
134500     MOVE SEQA-BEANST(1:9) TO MOD-BEANST (INDX)                           
134600                                                                          
134700     MOVE SEQA-IDDISTR  TO W-IDDISTR-4103                                 
134800                           SPAR-IDDISTR-NEXT                              
134900     MOVE SEQA-IDKUNDNR TO W-IDKUNDNR-4103                                
135000                           SPAR-IDKUNDNR-NEXT                             
135100     MOVE SEQA-IDRAPPNR TO W-IDRAPPNR-4103                                
135200                           SPAR-IDRAPPNR-NEXT                             
135300     PERFORM IMS-GU-WDGX4103                                              
135400     IF SEGMENT-FINNS                                                     
135500       PERFORM IMS-GNP-WDGX4104                                           
135600       IF SEGMENT-SAKNAS                                                  
135700         PERFORM MFS-RENSA-4103-RAD-UT                                    
135800         MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)            
135900         ADD +1 TO INDX                                                   
136000       ELSE                                                               
136100         PERFORM FBA-VISA-RADDATA-UNIK                                    
136200       END-IF                                                             
136300     ELSE                                                                 
136400       PERFORM MFS-RENSA-4103-RAD-UT                                      
136500       MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)              
136600       ADD +1 TO INDX                                                     
136700     END-IF                                                               
136800                                                                          
136900     PERFORM UNTIL INDX > MAX-INDX                                        
137000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
137100       MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)              
137200       ADD +1 TO INDX                                                     
137300     END-PERFORM                                                          
137400     .                                                                    
137500     EJECT                                                                
137600 FBA-VISA-RADDATA-UNIK SECTION.                                           
137700                                                                          
137800     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                      
137900       IF SEGMENT-FINNS                                                   
138000         MOVE SEQA-IDDISTR  TO MOD-IDDISTR (INDX)                         
138100         MOVE SEQA-IDKUNDNR TO MOD-IDKUNDNR (INDX)                        
138200         MOVE SEQA-IDRAPPNR TO MOD-IDRAPPNR (INDX)                        
138300         MOVE SEQA-IDUSER-ADM  TO MOD-IDUSER (INDX)                       
138400         MOVE SEQA-BEANST(1:9) TO MOD-BEANST (INDX)                       
138500                                                                          
138600         MOVE 4104-IDDC     TO MOD-IDDC (INDX)                            
138700                               SPAR-IDDC-ENTER                            
138800                               W-IDDC-4104                                
138900         MOVE 4104-KDKRENOT TO MOD-KDKRENOT (INDX)                        
139000                               SPAR-KDKRENOT-ENTER                        
139100                               W-KDKRENOT-4104                            
139200                                                                          
139300*- PRISET SKALL VARA I SEK.MAN HÄMTAR KURSEN OCH RÄKNAR OM.               
139400         IF 4104-KDVALISO = 'SEK'                                         
139500           MOVE 4104-SUKRENOT  TO MOD-SUKRENOT (INDX)                     
139600         ELSE                                                             
139700*CHINA-PRICE2                                                             
139800*INDIA-PRICE2                                                             
139900*KOREA-PRICE2                                                             
140000           MOVE W-IDFTG-MIN      TO WS-IDFTG                              
140100           IF (4104-KDVALISO = 'CNY' AND IDFTG-CN) OR                     
140200              (4104-KDVALISO = 'INR' AND IDFTG-IN) OR                     
140300              (4104-KDVALISO = 'KRW' AND IDFTG-KR) OR                     
140310              (4104-KDVALISO = 'TRY' AND IDFTG-TR) OR                     
140311              (4104-KDVALISO = 'MXN' AND IDFTG-MX) OR                     
140312              (4104-KDVALISO = 'BRL' AND IDFTG-BR) OR                     
140320              (4104-KDVALISO = 'MYR' AND IDFTG-MY) OR                     
140330              (4104-KDVALISO = 'THB' AND IDFTG-TH) OR                     
140340              (4104-KDVALISO = 'TWD' AND IDFTG-TW) OR                     
140350              (4104-KDVALISO = 'ZAR' AND IDFTG-ZA)                        
140400             MOVE 4104-SUKRENOT TO MOD-SUKRENOT (INDX)                    
140500           ELSE                                                           
140600             MOVE 4104-KDVALISO TO CURR-KDVALISO-ROW                      
140700***   LÄS PRKURS HTYP 9305             *****                              
140800             CALL W510CURR USING CURR-W510CURR WDG2-PCB                   
140900             IF CURR-KDSVAR = ' '                                         
141000               CONTINUE                                                   
141100             ELSE                                                         
141200               MOVE 1         TO CURR-PRKURS-NEW                          
141300             END-IF                                                       
141400             COMPUTE MOD-SUKRENOT(INDX) ROUNDED =                         
141500                       4104-SUKRENOT * CURR-PRKURS-NEW                    
141600             END-COMPUTE                                                  
141700           END-IF                                                         
141800         END-IF                                                           
141900                                                                          
142000         PERFORM IMS-GNP-WDGX4106                                         
142100         IF SEGMENT-SAKNAS                                                
142200           MOVE MFS-RENSA-FAELT TO MOD-BEANST-GODK (INDX)                 
142300                                   MOD-IDANSTNR-GODK (INDX)               
142400                                   MOD-TIMMDD (INDX)                      
142500                                   MOD-FLKLAR  (INDX)                     
142600         ELSE                                                             
142700           MOVE 4106-BEANST-GODK(1:8) TO                                  
142800                               MOD-BEANST-GODK (INDX)                     
142900           MOVE 4106-IDUSER-GODK(3:5) TO                                  
143000                               MOD-IDANSTNR-GODK (INDX)                   
143100           MOVE 4106-TIUPPDAT TO MOD-TIMMDD (INDX)                        
143200           IF 4106-FLKLAR = 'J'                                           
143300             MOVE 'Y'         TO MOD-FLKLAR (INDX)                        
143400           ELSE                                                           
143500             MOVE 4106-FLKLAR TO MOD-FLKLAR (INDX)                        
143600           END-IF                                                         
143700                                                                          
143800           PERFORM IMS-GNP-WDGX4106                                       
143900           PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                
144000             ADD 1 TO INDX                                                
144100             MOVE MFS-RENSA-FAELT TO MOD-IDDISTR(INDX)                    
144200                                     MOD-IDKUNDNR(INDX)                   
144300                                     MOD-IDRAPPNR(INDX)                   
144400                                     MOD-IDDC(INDX)                       
144500                                     MOD-KDKRENOT(INDX)                   
144600                                     MOD-SUKRENOT(INDX)                   
144700                                     MOD-BEANST(INDX)                     
144800                                     MOD-IDUSER(INDX)                     
144900             MOVE 4106-BEANST-GODK(1:8) TO                                
145000                                 MOD-BEANST-GODK (INDX)                   
145100             MOVE 4106-IDUSER-GODK(3:5) TO                                
145200                                 MOD-IDANSTNR-GODK (INDX)                 
145300             MOVE 4106-TIUPPDAT TO MOD-TIMMDD (INDX)                      
145400             IF 4106-FLKLAR = 'J'                                         
145500               MOVE 'Y'         TO MOD-FLKLAR (INDX)                      
145600             ELSE                                                         
145700               MOVE 4106-FLKLAR TO MOD-FLKLAR (INDX)                      
145800             END-IF                                                       
145900                                                                          
146000             MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDCMD-ATTR(INDX)          
146100                                                                          
146200             PERFORM IMS-GNP-WDGX4106                                     
146300           END-PERFORM                                                    
146400         END-IF                                                           
146500         ADD +1 TO INDX                                                   
146600       END-IF                                                             
146700       PERFORM IMS-GNP-WDGX4104                                           
146800     END-PERFORM                                                          
146900     .                                                                    
147000     EJECT                                                                
147100 G-KOLLA-INPUT SECTION.                                                   
147200     MOVE JA  TO INDATA-SW                                                
147300     IF MID-INPUT = ALL '+'                                               
147400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
147500       CALL WMEDKONV USING MED-WMEDAREA                                   
147600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
147700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
147800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
147900       MOVE NEJ TO INDATA-SW                                              
148000     ELSE                                                                 
148100                                                                          
148200       MOVE +1  TO INDX                                                   
148300       PERFORM UNTIL INDX  >  MAX-INDX                                    
148400         IF MID-KDCMD(INDX)              NOT = ALL '+'                    
148500           IF MID-KDCMD(INDX)           = 'A' OR 'R'                      
148600             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR(INDX)           
148700           ELSE                                                           
148800             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)            
148900             MOVE NEJ                  TO INDATA-SW                       
149000           END-IF                                                         
149100         ELSE                                                             
149200            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)             
149300         END-IF                                                           
149400         ADD +1                          TO INDX                          
149500       END-PERFORM                                                        
149600                                                                          
149700       IF INDATA-FEL                                                      
149800         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
149900         CALL WMEDKONV USING MED-WMEDAREA                                 
150000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
150100         PERFORM MFS-ROER-EJ-FAELT-UT                                     
150200         PERFORM MFS-ROER-EJ-FAELT-IN                                     
150300       ELSE                                                               
150400         PERFORM GA-KOLLA-OM-OK-UPPDATERA                                 
150500         IF INDATA-FEL                                                    
150600           IF MED-IDMFSFEL = SPACE                                        
150700             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
150800           END-IF                                                         
150900           CALL WMEDKONV USING MED-WMEDAREA                               
151000           MOVE MED-MFSFEL TO MOD-TEMFSINF                                
151100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
151200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
151300         END-IF                                                           
151400       END-IF                                                             
151500     END-IF                                                               
151600     .                                                                    
151700     EJECT                                                                
151800 GA-KOLLA-OM-OK-UPPDATERA SECTION.                                        
151900                                                                          
152000     MOVE +1                         TO INDX                              
152100     PERFORM UNTIL INDX  >  MAX-INDX                                      
152200       IF MID-KDCMD(INDX) NOT = ALL '+'                                   
152300         IF MID-IDUSER(INDX) = MSGI-IDUSER                                
152400           MOVE ERR-NOT-AUTHORIZED-ATT TO MED-IDMFSFEL                    
152500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)               
152600           MOVE NEJ                  TO INDATA-SW                         
152700         END-IF                                                           
152800                                                                          
152900         MOVE 'DISC'          TO W-KDARBTYP                               
153000         MOVE MID-IDDC (INDX) TO W-IDDC-6327                              
153100         MOVE LOW-VALUE       TO W-IDUSER-6328-MIN                        
153200         MOVE ZERO            TO W-SUBEL-6328-MIN                         
153300         MOVE HIGH-VALUE      TO W-IDUSER-6328-MAX                        
153400         MOVE 9999999         TO W-SUBEL-6328-MAX                         
153500         MOVE MSGI-IDUSER     TO W-IDUSER-GODK-6328                       
153600                                                                          
153700         PERFORM IMS-GU-WDGX6327                                          
153800         IF SEGMENT-FINNS                                                 
153900           PERFORM IMS-GNP-WDGX6328                                       
154000           IF SEGMENT-FINNS                                               
154100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)            
154200             MOVE 6328-SUBEL           TO SPAR-SUBEL                      
154300             MOVE 6328-IDUSER-GODK     TO SPAR-IDUSER-GODK                
154400                                                                          
154500             PERFORM GAA-KOLLA-OM-RAETT-ATTESTNIVA                        
154600           ELSE                                                           
154700               MOVE ERR-NOT-AUTHORIZED-ATT TO MED-IDMFSFEL                
154800               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)           
154900               MOVE NEJ                    TO INDATA-SW                   
155000           END-IF                                                         
155100         ELSE                                                             
155200           MOVE ERR-NOT-AUTHORIZED-ATT TO MED-IDMFSFEL                    
155300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)               
155400           MOVE NEJ                    TO INDATA-SW                       
155500         END-IF                                                           
155600                                                                          
155700         INSPECT MID-IDDISTR(INDX) REPLACING                              
155800                 LEADING SPACE BY ZERO                                    
155900         MOVE MID-IDDISTR (INDX)  TO W-IDDISTR-4103                       
156000         INSPECT MID-IDKUNDNR(INDX) REPLACING                             
156100                 LEADING SPACE BY ZERO                                    
156200         MOVE MID-IDKUNDNR (INDX) TO W-IDKUNDNR-4103                      
156300         INSPECT MID-IDRAPPNR(INDX) REPLACING                             
156400                 LEADING SPACE BY ZERO                                    
156500         MOVE MID-IDRAPPNR (INDX) TO W-IDRAPPNR-4103                      
156600         MOVE MID-IDDC (INDX)     TO W-IDDC-4104                          
156700         MOVE MID-KDKRENOT (INDX) TO W-KDKRENOT-4104                      
156800         MOVE MSGI-IDUSER         TO W-IDUSER-GODK-4106                   
156900                                                                          
157000         PERFORM IMS-GU-WDGX4106                                          
157100         IF SEGMENT-FINNS                                                 
157200           IF 4106-FLKLAR = NEJ                                           
157300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)            
157400           ELSE                                                           
157500             MOVE ERR-RAD-FINNS-REDAN  TO MED-IDMFSFEL                    
157600             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)             
157700             MOVE NEJ                  TO INDATA-SW                       
157800           END-IF                                                         
157900         END-IF                                                           
158000       END-IF                                                             
158100       ADD +1                        TO INDX                              
158200     END-PERFORM                                                          
158300     .                                                                    
158400     EJECT                                                                
158500 GAA-KOLLA-OM-RAETT-ATTESTNIVA SECTION.                                   
158600                                                                          
158700     INSPECT MID-IDDISTR(INDX) REPLACING                                  
158800             LEADING SPACE BY ZERO                                        
158900     MOVE MID-IDDISTR (INDX)  TO W-IDDISTR-4103                           
159000     INSPECT MID-IDKUNDNR(INDX) REPLACING                                 
159100             LEADING SPACE BY ZERO                                        
159200     MOVE MID-IDKUNDNR (INDX) TO W-IDKUNDNR-4103                          
159300     INSPECT MID-IDRAPPNR(INDX) REPLACING                                 
159400             LEADING SPACE BY ZERO                                        
159500     MOVE MID-IDRAPPNR (INDX) TO W-IDRAPPNR-4103                          
159600     MOVE MID-IDDC (INDX)     TO W-IDDC-4104                              
159700     MOVE MID-KDKRENOT (INDX) TO W-KDKRENOT-4104                          
159800                                                                          
159900     PERFORM IMS-GU-WDGX4104-KVAL                                         
160000     IF SEGMENT-FINNS                                                     
160100       PERFORM IMS-GNP-WDGX4106                                           
160200       PERFORM UNTIL SEGMENT-SAKNAS                                       
160300         MOVE 'DISC'            TO W-KDARBTYP                             
160400         MOVE MID-IDDC (INDX)   TO W-IDDC-6327                            
160500         MOVE LOW-VALUE         TO W-IDUSER-6328-MIN                      
160600         MOVE ZERO              TO W-SUBEL-6328-MIN                       
160700         MOVE HIGH-VALUE        TO W-IDUSER-6328-MAX                      
160800         MOVE 9999999           TO W-SUBEL-6328-MAX                       
160900         MOVE 4106-IDUSER-GODK  TO W-IDUSER-GODK-6328                     
161000                                                                          
161100         PERFORM IMS-GU-WDGX6327                                          
161200         IF SEGMENT-FINNS                                                 
161300           PERFORM IMS-GNP-WDGX6328                                       
161400           IF SEGMENT-FINNS                                               
161500             IF (6328-IDUSER-GODK = SPAR-IDUSER-GODK) AND                 
161600                (6328-SUBEL = SPAR-SUBEL)             AND                 
161700                (4106-FLKLAR = 'N')                                       
161800               CONTINUE                                                   
161900             ELSE                                                         
162000               IF 6328-SUBEL = SPAR-SUBEL                                 
162100                 MOVE INF-NEED-APPR-NEXT-LEV TO MED-IDMFSFEL              
162200                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)         
162300                 MOVE NEJ                  TO INDATA-SW                   
162400               END-IF                                                     
162500               IF 6328-SUBEL > SPAR-SUBEL                                 
162600                 MOVE INF-NEED-APPR-NEXT-LEV TO MED-IDMFSFEL              
162700                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)         
162800                 MOVE NEJ                  TO INDATA-SW                   
162900               END-IF                                                     
163000             END-IF                                                       
163100           END-IF                                                         
163200         END-IF                                                           
163300                                                                          
163400         PERFORM IMS-GNP-WDGX4106                                         
163500       END-PERFORM                                                        
163600     END-IF                                                               
163700     .                                                                    
163800     EJECT                                                                
163900 H-UPPDATERA SECTION.                                                     
164000                                                                          
164100     MOVE +1 TO INDX                                                      
164200     PERFORM UNTIL INDX > MAX-INDX                                        
164300                                                                          
164400       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
164500                                                                          
164600                                                                          
164700         INSPECT MID-IDDISTR(INDX) REPLACING                              
164800                 LEADING SPACE BY ZERO                                    
164900         MOVE MID-IDDISTR (INDX)  TO W-IDDISTR-4103                       
165000         INSPECT MID-IDKUNDNR(INDX) REPLACING                             
165100                 LEADING SPACE BY ZERO                                    
165200         MOVE MID-IDKUNDNR (INDX) TO W-IDKUNDNR-4103                      
165300         INSPECT MID-IDRAPPNR(INDX) REPLACING                             
165400                 LEADING SPACE BY ZERO                                    
165500         MOVE MID-IDRAPPNR (INDX) TO W-IDRAPPNR-4103                      
165600         MOVE MID-IDDC (INDX)     TO W-IDDC-4104                          
165700         MOVE MID-KDKRENOT (INDX) TO W-KDKRENOT-4104                      
165800         MOVE MSGI-IDUSER         TO W-IDUSER-GODK-4106                   
165900                                                                          
166000         MOVE SPACE               TO WS-4718-TABELL                       
166100         PERFORM IMS-GU-WDGX4105                                          
166200         IF SEGMENT-FINNS                                                 
166300           MOVE +1                TO 4105-IX                              
166400           PERFORM UNTIL 4105-IX  >  5                                    
166500             IF 4105-TEMEMO (4105-IX) NOT = ALL SPACE                     
166600               MOVE 4105-TEMEMO (4105-IX) TO WS-TEMAIL (4105-IX)          
166700             END-IF                                                       
166800             ADD +1               TO 4105-IX                              
166900           END-PERFORM                                                    
167000         END-IF                                                           
167100                                                                          
167200         PERFORM IMS-GHU-WDGX4106                                         
167300         IF SEGMENT-FINNS                                                 
167400           PERFORM HA-REPL-IDUSER-GODK                                    
167500         ELSE                                                             
167600           PERFORM HB-ISRT-IDUSER-GODK                                    
167700         END-IF                                                           
167800                                                                          
167900         IF KNOTA-ATTEST-KLAR                                             
168000           PERFORM HC-EV-UPPDAT-WDA201                                    
168100         END-IF                                                           
168200                                                                          
168300       END-IF                                                             
168400       ADD +1                          TO INDX                            
168500     END-PERFORM                                                          
168600                                                                          
168700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
168800     CALL WMEDKONV USING MED-WMEDAREA                                     
168900     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
169000     PERFORM MFS-FORM-ATTR                                                
169100     PERFORM MFS-RENSA-FAELT-IN                                           
169200* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
169300     .                                                                    
169400     EJECT                                                                
169500 HA-REPL-IDUSER-GODK SECTION.                                             
169600                                                                          
169700     MOVE NEJ             TO SW-KNOTA-ATTEST-KLAR                         
169800                                                                          
169900     IF MID-KDCMD (INDX) = 'A'                                            
170000       MOVE JA            TO 4106-FLKLAR                                  
170100                                                                          
170200       MOVE 'DISC'          TO W-KDARBTYP                                 
170300       MOVE MID-IDDC (INDX) TO W-IDDC-6327                                
170400       MOVE LOW-VALUE       TO W-IDUSER-6328-MIN                          
170500       MOVE ZERO            TO W-SUBEL-6328-MIN                           
170600       MOVE HIGH-VALUE      TO W-IDUSER-6328-MAX                          
170700       MOVE 9999999         TO W-SUBEL-6328-MAX                           
170800       MOVE MSGI-IDUSER     TO W-IDUSER-GODK-6328                         
170900                                                                          
171000       PERFORM IMS-GU-WDGX6327                                            
171100       IF SEGMENT-FINNS                                                   
171200         PERFORM IMS-GNP-WDGX6328                                         
171300         IF SEGMENT-FINNS                                                 
171400           MOVE MID-SUKRENOT (INDX)       TO DEC-IDFRIDATA                
171500           MOVE 7                         TO DEC-KVHELTAL                 
171600           MOVE 2                         TO DEC-KVDECIMAL                
171700           CALL WDECEDIT USING WDECAREA                                   
171800           IF DEC-KDSVAR-OK                                               
171900             MOVE DEC-IDEDITDATA          TO WS-SUKRENOT-NUM              
172000             IF (WS-SUKRENOT-HEL < 6328-SUBEL) OR                         
172100                (WS-SUKRENOT-HEL = 6328-SUBEL)                            
172200               MOVE JA  TO SW-KNOTA-ATTEST-KLAR                           
172300             ELSE                                                         
172400               MOVE INF-NEED-APPR-NEXT-LEV  TO MED-IDMFSFEL               
172500               CALL WMEDKONV USING MED-WMEDAREA                           
172600               MOVE MED-MFSFEL TO MOD-TEMFSINF                            
172700               IF 6328-IDUSER-PRI = SPACE                                 
172800                 PERFORM S05-MAIL-NEXT-LEVEL-FIRST                        
172900               ELSE                                                       
173000                 PERFORM S02-SKAPA-MAIL-NEXT-LEVEL                        
173100               END-IF                                                     
173200             END-IF                                                       
173300           END-IF                                                         
173400         END-IF                                                           
173500       END-IF                                                             
173600     ELSE                                                                 
173700       MOVE NEJ           TO 4106-FLKLAR                                  
173800       PERFORM S03-SKAPA-MAIL-ISSUER                                      
173900     END-IF                                                               
174000     ACCEPT 4106-TIUPPDAT FROM DATE                                       
174100                                                                          
174200     PERFORM IMS-REPL-WDGX4106                                            
174300     .                                                                    
174400     EJECT                                                                
174500 HB-ISRT-IDUSER-GODK SECTION.                                             
174600                                                                          
174700     MOVE NEJ             TO SW-KNOTA-ATTEST-KLAR                         
174800                                                                          
174900     MOVE FUNCTION CURRENT-DATE (1:14) TO 4106-TIDATETIME                 
175000                                          WS-TIDATETIME                   
175100     IF MID-KDCMD (INDX) = 'A'                                            
175200       MOVE JA            TO 4106-FLKLAR                                  
175300     ELSE                                                                 
175400       MOVE NEJ           TO 4106-FLKLAR                                  
175500       PERFORM S03-SKAPA-MAIL-ISSUER                                      
175600     END-IF                                                               
175700     ACCEPT 4106-TIUPPDAT FROM DATE                                       
175800                                                                          
175900     MOVE 'DISC'          TO W-KDARBTYP                                   
176000     MOVE MID-IDDC (INDX) TO W-IDDC-6327                                  
176100     MOVE LOW-VALUE       TO W-IDUSER-6328-MIN                            
176200     MOVE ZERO            TO W-SUBEL-6328-MIN                             
176300     MOVE HIGH-VALUE      TO W-IDUSER-6328-MAX                            
176400     MOVE 9999999         TO W-SUBEL-6328-MAX                             
176500     MOVE MSGI-IDUSER     TO W-IDUSER-GODK-6328                           
176600                             4106-IDUSER-GODK                             
176700                                                                          
176800     PERFORM IMS-GU-WDGX6327                                              
176900     IF SEGMENT-FINNS                                                     
177000       PERFORM IMS-GNP-WDGX6328                                           
177100       IF SEGMENT-FINNS                                                   
177200         MOVE 6328-BEANST-GODK      TO 4106-BEANST-GODK                   
177300                                                                          
177400         IF MID-KDCMD (INDX) = 'A'                                        
177500           MOVE MID-SUKRENOT (INDX)       TO DEC-IDFRIDATA                
177600           MOVE 7                         TO DEC-KVHELTAL                 
177700           MOVE 2                         TO DEC-KVDECIMAL                
177800           CALL WDECEDIT USING WDECAREA                                   
177900           IF DEC-KDSVAR-OK                                               
178000             MOVE DEC-IDEDITDATA          TO WS-SUKRENOT-NUM              
178100             IF (WS-SUKRENOT-HEL < 6328-SUBEL) OR                         
178200                (WS-SUKRENOT-HEL = 6328-SUBEL)                            
178300               MOVE JA  TO SW-KNOTA-ATTEST-KLAR                           
178400             ELSE                                                         
178500               MOVE INF-NEED-APPR-NEXT-LEV  TO MED-IDMFSFEL               
178600               CALL WMEDKONV USING MED-WMEDAREA                           
178700               MOVE MED-MFSFEL TO MOD-TEMFSINF                            
178800               IF 6328-IDUSER-PRI = SPACE                                 
178900                 PERFORM S05-MAIL-NEXT-LEVEL-FIRST                        
179000               ELSE                                                       
179100                 PERFORM S02-SKAPA-MAIL-NEXT-LEVEL                        
179200               END-IF                                                     
179300             END-IF                                                       
179400           END-IF                                                         
179500         END-IF                                                           
179600       END-IF                                                             
179700     END-IF                                                               
179800                                                                          
179900     PERFORM IMS-ISRT-WDGX4106                                            
180000     IF SEGMENT-FINNS-REDAN                                               
180100       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
180200         ADD +1 TO WS-TIDATETIME                                          
180300         MOVE WS-TIDATETIME    TO 4106-TIDATETIME                         
180400         PERFORM IMS-ISRT-WDGX4106                                        
180500       END-PERFORM                                                        
180600     END-IF                                                               
180700     .                                                                    
180800     EJECT                                                                
180900 HC-EV-UPPDAT-WDA201 SECTION.                                             
181000                                                                          
181100     MOVE JA TO SW-ALLA-LEVANM-KN-KLARA                                   
181200                                                                          
181300                                                                          
181400     INSPECT MID-IDDISTR(INDX) REPLACING                                  
181500             LEADING SPACE BY ZERO                                        
181600     MOVE MID-IDDISTR (INDX)  TO W-IDDISTR-4103                           
181700     INSPECT MID-IDKUNDNR(INDX) REPLACING                                 
181800             LEADING SPACE BY ZERO                                        
181900     MOVE MID-IDKUNDNR (INDX) TO W-IDKUNDNR-4103                          
182000     INSPECT MID-IDRAPPNR(INDX) REPLACING                                 
182100             LEADING SPACE BY ZERO                                        
182200     MOVE MID-IDRAPPNR (INDX) TO W-IDRAPPNR-4103                          
182300     MOVE MID-IDDC (INDX)     TO W-IDDC-4104                              
182400     MOVE MID-KDKRENOT (INDX) TO W-KDKRENOT-4104                          
182500                                                                          
182600     PERFORM IMS-GHU-WDGX4104                                             
182700     IF SEGMENT-FINNS                                                     
182800       MOVE JA                TO 4104-FLKREATT                            
182900       PERFORM IMS-REPL-WDGX4104                                          
183000     END-IF                                                               
183100                                                                          
183200     PERFORM IMS-GU-WDGX4103                                              
183300     IF SEGMENT-FINNS                                                     
183400       PERFORM IMS-GNP-WDGX4104                                           
183500       PERFORM UNTIL SEGMENT-SAKNAS                                       
183600         IF 4104-FLKREATT = NEJ                                           
183700           MOVE NEJ TO SW-ALLA-LEVANM-KN-KLARA                            
183800         END-IF                                                           
183900         PERFORM IMS-GNP-WDGX4104                                         
184000       END-PERFORM                                                        
184100     END-IF                                                               
184200                                                                          
184300     IF ALLA-LEVANM-KN-KLARA                                              
184400       INSPECT MID-IDDISTR(INDX) REPLACING                                
184500               LEADING SPACE BY ZERO                                      
184600       MOVE MID-IDDISTR (INDX)  TO W-IDDISTR                              
184700       INSPECT MID-IDKUNDNR(INDX) REPLACING                               
184800               LEADING SPACE BY ZERO                                      
184900       MOVE MID-IDKUNDNR (INDX) TO W-IDKUNDNR                             
185000       INSPECT MID-IDRAPPNR(INDX) REPLACING                               
185100               LEADING SPACE BY ZERO                                      
185200       MOVE MID-IDRAPPNR (INDX) TO W-IDRAPPNR                             
185300                                                                          
185400       PERFORM IMS-GHU-WDA201                                             
185500       IF SEGMENT-FINNS                                                   
185600         MOVE 2                 TO  ANM-KDLEVATT                          
185700         PERFORM IMS-REPL-WDA201                                          
185800       END-IF                                                             
185900     END-IF                                                               
186000     .                                                                    
186100     EJECT                                                                
186200                                                                          
186300 I-CHECK-FLGODK    SECTION.                                               
186500*    CHECKS THE VALUE OF FLGODK IS LESS THEN 200,000 IF YES THEN          
186600*     AUTOAPPROVE                                                         
186700                                                                          
186800     MOVE +0          TO W-IDDISTR-MIN                                    
186900                         W-IDKUNDNR-MIN                                   
187000                         W-IDRAPPNR-MIN                                   
187100                                                                          
187200     MOVE +99999      TO W-IDDISTR-MAX                                    
187300     MOVE +9999999    TO W-IDKUNDNR-MAX                                   
187400     MOVE 9999999     TO W-IDRAPPNR-MAX                                   
187500                                                                          
187600     MOVE   '57'      TO W-IDFTG-MIN                                      
187700                         W-IDFTG-MAX                                      
187800     MOVE  'NEJ'      TO FLGODK-SW                                        
187900                                                                          
188000     PERFORM IMS-GU-WDA2A1                                                
188100     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
188200       MOVE SEQA-IDDISTR      TO W-IDDISTR-4103                           
188300       MOVE SEQA-IDKUNDNR     TO W-IDKUNDNR-4103                          
188400       MOVE SEQA-IDRAPPNR     TO W-IDRAPPNR-4103                          
188500       PERFORM IMS-GU-WDGX4103                                            
188600       IF SEGMENT-FINNS                                                   
188700         MOVE JA              TO SW-ALLA-LEVANM-KN-KLARA                  
188800         PERFORM IMS-GHNP-WDGX4104                                        
188900         PERFORM UNTIL SEGMENT-SAKNAS                                     
189000           IF 4104-FLKREATT         = JA                                  
189010             CONTINUE                                                     
189020           ELSE                                                           
189030             MOVE 4104-IDDC         TO W-IDDC-4104                        
189100             MOVE 4104-KDKRENOT     TO W-KDKRENOT-4104                    
189200                                                                          
189300                                                                          
189400*   IF THE PRICE IS NOT SEK THEN CONVERT IT TO SEK **********             
189500             IF 4104-KDVALISO    = 'SEK'                                  
189600               MOVE 4104-SUKRENOT  TO W-SUKRENOT                          
189700             ELSE                                                         
189800               MOVE 4104-KDVALISO   TO CURR-KDVALISO-ROW                  
189900               CALL W510CURR USING CURR-W510CURR WDG2-PCB                 
190000               IF CURR-KDSVAR = ' '                                       
190100                 CONTINUE                                                 
190200               ELSE                                                       
190300                 MOVE 1             TO CURR-PRKURS-NEW                    
190400               END-IF                                                     
190500               COMPUTE W-SUKRENOT ROUNDED =                               
190600                       4104-SUKRENOT * CURR-PRKURS-NEW                    
190700               END-COMPUTE                                                
190800             END-IF                                                       
190900             IF W-SUKRENOT < 200000                                       
191000                                                                          
191100               MOVE JA             TO 4104-FLKREATT                       
191200               PERFORM IMS-REPL-WDGX4104                                  
191300                                                                          
191400               MOVE 'DISC'         TO W-KDARBTYP                          
191500               MOVE 4104-IDDC      TO W-IDDC-6327                         
191600               MOVE LOW-VALUE      TO W-IDUSER-6328-MIN                   
191700               MOVE ZERO           TO W-SUBEL-6328-MIN                    
191800               MOVE HIGH-VALUE     TO W-IDUSER-6328-MAX                   
191900               MOVE 9999999        TO W-SUBEL-6328-MAX                    
192000               MOVE MSGI-IDUSER    TO W-IDUSER-GODK-6328                  
192100                                                                          
192200               PERFORM IMS-GU-WDGX6327                                    
192300               IF SEGMENT-FINNS                                           
192400                 PERFORM IMS-GNP-WDGX6328                                 
192500                 IF SEGMENT-FINNS                                         
192600                   MOVE 6328-BEANST-GODK                                  
192700                                     TO SPAR-BEANST-GODK                  
192800                 ELSE                                                     
192900                   MOVE SPACE      TO SPAR-BEANST-GODK                    
193000                 END-IF                                                   
193100               END-IF                                                     
193200                                                                          
193300               PERFORM IMS-GHNP-WDGX4106                                  
193400               IF SEGMENT-FINNS                                           
193500                 MOVE JA          TO 4106-FLKLAR                          
193600                 MOVE SPAR-BEANST-GODK                                    
193700                                  TO 4106-BEANST-GODK                     
193800                 ACCEPT 4106-TIUPPDAT FROM DATE                           
193900                 PERFORM IMS-REPL-WDGX4106                                
194000               ELSE                                                       
194100                 MOVE FUNCTION CURRENT-DATE (1:14)                        
194200                                  TO 4106-TIDATETIME                      
194300                 MOVE JA          TO 4106-FLKLAR                          
194310                 MOVE SPAR-BEANST-GODK                                    
194320                                  TO 4106-BEANST-GODK                     
194400                 ACCEPT 4106-TIUPPDAT FROM DATE                           
194500                 MOVE MSGI-IDUSER TO 4106-IDUSER-GODK                     
194600                 PERFORM IMS-ISRT-WDGX4106                                
194700                 IF SEGMENT-FINNS-REDAN                                   
194800                   PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                  
194900                     ADD +1       TO WS-TIDATETIME                        
195000                     MOVE WS-TIDATETIME                                   
195100                                  TO 4106-TIDATETIME                      
195200                     PERFORM IMS-ISRT-WDGX4106                            
195300                   END-PERFORM                                            
195400                 END-IF                                                   
195500               END-IF                                                     
195600             ELSE                                                         
195700               MOVE NEJ             TO SW-ALLA-LEVANM-KN-KLARA            
195800             END-IF                                                       
195810           END-IF                                                         
195900           PERFORM IMS-GHNP-WDGX4104                                      
196000         END-PERFORM                                                      
196100         IF ALLA-LEVANM-KN-KLARA                                          
196200           MOVE 4103-IDDISTR TO W-IDDISTR                                 
196300           MOVE 4103-IDKUNDNR TO W-IDKUNDNR                               
196400           MOVE 4103-IDRAPPNR TO W-IDRAPPNR                               
196500           PERFORM IMS-GHU-WDA201                                         
196600           IF SEGMENT-FINNS                                               
196700             MOVE 2         TO  ANM-KDLEVATT                              
196800             PERFORM IMS-REPL-WDA201                                      
196900           END-IF                                                         
197000         END-IF                                                           
197100       END-IF                                                             
197200       PERFORM IMS-GN-WDA2A1                                              
197300     END-PERFORM                                                          
197400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
197500     CALL WMEDKONV USING MED-WMEDAREA                                     
197600     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
197700     PERFORM MFS-FORM-ATTR                                                
197800     PERFORM MFS-RENSA-FAELT-IN                                           
197900     .                                                                    
198000     EJECT                                                                
198100 S01-INSERT-ALTMSG SECTION.                                               
198200                                                                          
198300     PERFORM IMS-CHANGE-ALTMSG                                            
198400     IF STATUS-OK                                                         
198500       PERFORM IMS-INSERT-ALTMSG                                          
198600     ELSE                                                                 
198700       IF MID-KDCMD (INDX) = 'L'                                          
198800         IF SECURITY-FEL                                                  
198900           STRING 'NOT AUTHORIZED TO USE '                                
199000                  WS-IDTRANS-HOPP                                         
199100                  DELIMITED BY SIZE INTO MOD-TEMFSINF                     
199200         ELSE                                                             
199300           STRING 'WRONG PICTURE '                                        
199400                  WS-IDTRANS-HOPP                                         
199500                  DELIMITED BY SIZE INTO MOD-TEMFSINF                     
199600         END-IF                                                           
199700       ELSE                                                               
199800         IF SECURITY-FEL                                                  
199900           STRING 'NOT AUTHORIZED TO USE '                                
200000                  WS-IDTRANS-HOPP-1                                       
200100                  DELIMITED BY SIZE INTO MOD-TEMFSINF                     
200200         ELSE                                                             
200300           STRING 'WRONG PICTURE '                                        
200400                  WS-IDTRANS-HOPP-1                                       
200500                  DELIMITED BY SIZE INTO MOD-TEMFSINF                     
200600         END-IF                                                           
200700       END-IF                                                             
200800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71701 + 4                      
200900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
201000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
201100       PERFORM IMS-INSERT-MSG                                             
201200     END-IF                                                               
201300     .                                                                    
201400     EJECT                                                                
201500 S02-SKAPA-MAIL-NEXT-LEVEL SECTION.                                       
201600                                                                          
201700                                                                          
201800     MOVE 'DISC'          TO W-KDARBTYP                                   
201900     MOVE MID-IDDC (INDX) TO W-IDDC-6327                                  
202000     MOVE LOW-VALUE       TO W-IDUSER-6328-MIN                            
202100     MOVE ZERO            TO W-SUBEL-6328-MIN                             
202200     MOVE HIGH-VALUE      TO W-IDUSER-6328-MAX                            
202300     MOVE 9999999         TO W-SUBEL-6328-MAX                             
202400     MOVE 6328-IDUSER-PRI TO W-IDUSER-GODK-6328                           
202500                                                                          
202600     PERFORM IMS-GU-WDGX6327                                              
202700     IF SEGMENT-FINNS                                                     
202800       PERFORM IMS-GNP-WDGX6328                                           
202900       IF SEGMENT-FINNS                                                   
203000                                                                          
203100         MOVE '4717'            TO MAIL-IDTRANS                           
203200         MOVE '1'               TO MAIL-KDMFSFOR                          
203300         MOVE 'ATTEST CREDIT'   TO MAIL-IDMAILTTL                         
203400         MOVE SPACE             TO MAIL-IDMAIL                            
203500         MOVE SPACE             TO LRAD-IDUSER                            
203600         MOVE SPACE             TO LRAD-BEANST                            
203700                                                                          
203800         MOVE ZERO TO COUNTER                                             
203900         INSPECT 6328-IDMAIL TALLYING COUNTER FOR ALL '@'                 
204000         IF COUNTER = 0                                                   
204100           CONTINUE                                                       
204200         ELSE                                                             
204300           MOVE 6328-IDMAIL       TO MAIL-IDMAIL                          
204400           MOVE +1                TO MAIL-IX                              
204500           MOVE LIST-HRAD2        TO MAIL-TEMAIL (MAIL-IX)                
204600           ADD +1                 TO MAIL-IX                              
204700           MOVE LIST-BLANKRAD     TO MAIL-TEMAIL (MAIL-IX)                
204800                                                                          
204900           PERFORM S04-SKAPA-MAIL                                         
205000         END-IF                                                           
205100       END-IF                                                             
205200     END-IF                                                               
205300     .                                                                    
205400     EJECT                                                                
205500 S03-SKAPA-MAIL-ISSUER  SECTION.                                          
205600                                                                          
205700     MOVE 'DISC'          TO W-KDARBTYP                                   
205800     MOVE MID-IDDC (INDX) TO W-IDDC-6327                                  
205900     MOVE LOW-VALUE       TO W-IDUSER-6328-MIN                            
206000     MOVE ZERO            TO W-SUBEL-6328-MIN                             
206100     MOVE HIGH-VALUE      TO W-IDUSER-6328-MAX                            
206200     MOVE 9999999         TO W-SUBEL-6328-MAX                             
206300     MOVE MID-IDUSER (INDX) TO W-IDUSER-GODK-6328                         
206400                                                                          
206500     PERFORM IMS-GU-WDGX6327                                              
206600     IF SEGMENT-FINNS                                                     
206700       PERFORM IMS-GNP-WDGX6328                                           
206800       IF SEGMENT-FINNS                                                   
206900                                                                          
207000         MOVE '4717'            TO MAIL-IDTRANS                           
207100         MOVE '1'               TO MAIL-KDMFSFOR                          
207200         MOVE 'REJECT CREDIT'   TO MAIL-IDMAILTTL                         
207300         MOVE SPACE             TO MAIL-IDMAIL                            
207400                                                                          
207500         MOVE ZERO TO COUNTER                                             
207600         INSPECT 6328-IDMAIL TALLYING COUNTER FOR ALL '@'                 
207700         IF COUNTER = 0                                                   
207800           CONTINUE                                                       
207900         ELSE                                                             
208000           MOVE MSGI-IDUSER       TO LRAD-IDUSER                          
208100           MOVE MSGI-BEANST       TO LRAD-BEANST                          
208200           MOVE 6328-IDMAIL       TO MAIL-IDMAIL                          
208300           MOVE +1                TO MAIL-IX                              
208400           MOVE LIST-HRAD1        TO MAIL-TEMAIL (MAIL-IX)                
208500           ADD +1                 TO MAIL-IX                              
208600           MOVE LIST-BLANKRAD     TO MAIL-TEMAIL (MAIL-IX)                
208700                                                                          
208800           PERFORM S04-SKAPA-MAIL                                         
208900         END-IF                                                           
209000       END-IF                                                             
209100     END-IF                                                               
209200     .                                                                    
209300     EJECT                                                                
209400 S04-SKAPA-MAIL  SECTION.                                                 
209500                                                                          
209600*-- REDIGERA-HUVUD                                                        
209700     ADD +1                     TO MAIL-IX                                
209800     MOVE LIST-HRAD3            TO MAIL-TEMAIL (MAIL-IX)                  
209900                                                                          
210000     ADD +1                     TO MAIL-IX                                
210100     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (MAIL-IX)                  
210200                                                                          
210300*-- REDIGERA-RADER                                                        
210400     ADD +1                     TO MAIL-IX                                
210500     INSPECT MID-IDDISTR(INDX) REPLACING                                  
210600             LEADING SPACE BY ZERO                                        
210700     MOVE MID-IDDISTR (INDX)    TO LRAD-IDDISTR                           
210800     INSPECT MID-IDKUNDNR(INDX) REPLACING                                 
210900             LEADING SPACE BY ZERO                                        
211000     MOVE MID-IDKUNDNR(INDX)    TO LRAD-IDKUNDNR                          
211100     INSPECT MID-IDRAPPNR(INDX) REPLACING                                 
211200             LEADING SPACE BY ZERO                                        
211300     MOVE MID-IDRAPPNR(INDX)    TO LRAD-IDRAPPNR                          
211400     MOVE MID-KDKRENOT(INDX)    TO LRAD-KDKRENOT                          
211500     MOVE MID-IDDC (INDX)       TO LRAD-IDDC                              
211600     MOVE LIST-LRAD             TO MAIL-TEMAIL (MAIL-IX)                  
211700                                                                          
211800     IF LRAD-IDUSER = SPACE                                               
211900       CONTINUE                                                           
212000     ELSE                                                                 
212100       ADD +1                   TO MAIL-IX                                
212200       MOVE LIST-BLANKRAD       TO MAIL-TEMAIL (MAIL-IX)                  
212300       ADD +1                   TO MAIL-IX                                
212400       MOVE LIST-LRAD-1         TO MAIL-TEMAIL (MAIL-IX)                  
212500     END-IF                                                               
212600                                                                          
212700     IF WS-4718-TABELL = SPACE                                            
212800       CONTINUE                                                           
212900     ELSE                                                                 
213000       ADD +1                   TO MAIL-IX                                
213100       MOVE LIST-BLANKRAD       TO MAIL-TEMAIL (MAIL-IX)                  
213200       ADD +1                   TO MAIL-IX                                
213300       MOVE LIST-LRAD-2         TO MAIL-TEMAIL (MAIL-IX)                  
213400       ADD +1                   TO MAIL-IX                                
213500       MOVE LIST-BLANKRAD       TO MAIL-TEMAIL (MAIL-IX)                  
213600       ADD +1                   TO MAIL-IX                                
213700       MOVE WS-TEMAIL (1)       TO MAIL-TEMAIL (MAIL-IX)                  
213800       ADD +1                   TO MAIL-IX                                
213900       MOVE WS-TEMAIL (2)       TO MAIL-TEMAIL (MAIL-IX)                  
214000       ADD +1                   TO MAIL-IX                                
214100       MOVE WS-TEMAIL (3)       TO MAIL-TEMAIL (MAIL-IX)                  
214200       ADD +1                   TO MAIL-IX                                
214300       MOVE WS-TEMAIL (4)       TO MAIL-TEMAIL (MAIL-IX)                  
214400       ADD +1                   TO MAIL-IX                                
214500       MOVE WS-TEMAIL (5)       TO MAIL-TEMAIL (MAIL-IX)                  
214600     END-IF                                                               
214700                                                                          
214800     ADD +1                     TO MAIL-IX                                
214900                                                                          
215000     MOVE MAIL-IX               TO MAIL-KVMAILLN                          
215100                                                                          
215200     PERFORM IMS-INSERT-MAIL                                              
215300     .                                                                    
215400     EJECT                                                                
215500 S05-MAIL-NEXT-LEVEL-FIRST SECTION.                                       
215600                                                                          
215700     MOVE MID-SUKRENOT (INDX)          TO DEC-IDFRIDATA                   
215800     MOVE 7                            TO DEC-KVHELTAL                    
215900     MOVE 2                            TO DEC-KVDECIMAL                   
216000     CALL WDECEDIT USING WDECAREA                                         
216100     IF DEC-KDSVAR-OK                                                     
216200       IF DEC-IDEDITDATA       > ZERO                                     
216300         MOVE DEC-IDEDITDATA     TO WS-SUKRENOT-NUM                       
216400         MOVE WS-SUKRENOT-HEL    TO W-SUBEL-WDGX6328                      
216500         MOVE ZERO               TO W-SUBEL-6328-MIN                      
216600         MOVE 9999999            TO W-SUBEL-6328-MAX                      
216700         MOVE 'DISC'             TO W-KDARBTYP                            
216800         MOVE MID-IDDC (INDX)    TO W-IDDC-6327                           
216900         MOVE LOW-VALUE          TO W-IDUSER-6328-MIN                     
217000         MOVE HIGH-VALUE         TO W-IDUSER-6328-MAX                     
217100                                                                          
217200         PERFORM IMS-GU-WDGX6327                                          
217300         IF SEGMENT-FINNS                                                 
217400           PERFORM IMS-GNP-WDGX6328-MIN-MAX                               
217500           IF SEGMENT-FINNS                                               
217600                                                                          
217700             MOVE '4717'            TO MAIL-IDTRANS                       
217800             MOVE '1'               TO MAIL-KDMFSFOR                      
217900             MOVE 'ATTEST CREDIT'   TO MAIL-IDMAILTTL                     
218000             MOVE SPACE             TO MAIL-IDMAIL                        
218100             MOVE SPACE             TO LRAD-IDUSER                        
218200             MOVE SPACE             TO LRAD-BEANST                        
218300                                                                          
218400             MOVE ZERO TO COUNTER                                         
218500             INSPECT 6328-IDMAIL TALLYING COUNTER FOR ALL '@'             
218600             IF COUNTER = 0                                               
218700               CONTINUE                                                   
218800             ELSE                                                         
218900               MOVE 6328-IDMAIL       TO MAIL-IDMAIL                      
219000               MOVE +1                TO MAIL-IX                          
219100               MOVE LIST-HRAD2        TO MAIL-TEMAIL (MAIL-IX)            
219200               ADD +1                 TO MAIL-IX                          
219300               MOVE LIST-BLANKRAD     TO MAIL-TEMAIL (MAIL-IX)            
219400                                                                          
219500               PERFORM S04-SKAPA-MAIL                                     
219600             END-IF                                                       
219700           END-IF                                                         
219800         END-IF                                                           
219900       END-IF                                                             
220000     END-IF                                                               
220100     .                                                                    
220200     EJECT                                                                
220300 MFS-RENSA-FAELT-UT SECTION.                                              
220400                                                                          
220500*    --- ALLA UTDATA-FÄLT                                                 
220600*    --- INKL. BLÄDDRINGSNYCKLAR                                          
220700                                                                          
220800     MOVE +1 TO INDX                                                      
220900     PERFORM UNTIL INDX > MAX-INDX                                        
221000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
221100       ADD +1 TO INDX                                                     
221200     END-PERFORM                                                          
221300     .                                                                    
221400     SKIP3                                                                
221500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
221600                                                                          
221700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
221800     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR (INDX)                           
221900                             MOD-IDKUNDNR (INDX)                          
222000                             MOD-IDRAPPNR (INDX)                          
222100                             MOD-IDDC (INDX)                              
222200                             MOD-KDKRENOT (INDX)                          
222300                             MOD-SUKRENOT (INDX)                          
222400                             MOD-BEANST (INDX)                            
222500                             MOD-IDUSER (INDX)                            
222600                             MOD-BEANST-GODK (INDX)                       
222700                             MOD-IDANSTNR-GODK (INDX)                     
222800                             MOD-TIMMDD (INDX)                            
222900                             MOD-FLKLAR (INDX)                            
223000     .                                                                    
223100     SKIP3                                                                
223200 MFS-RENSA-4103-RAD-UT SECTION.                                           
223300                                                                          
223400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
223500     MOVE MFS-RENSA-FAELT TO MOD-IDDC (INDX)                              
223600                             MOD-KDKRENOT (INDX)                          
223700                             MOD-SUKRENOT (INDX)                          
223800                             MOD-BEANST-GODK (INDX)                       
223900                             MOD-IDANSTNR-GODK (INDX)                     
224000                             MOD-TIMMDD (INDX)                            
224100                             MOD-FLKLAR (INDX)                            
224200     .                                                                    
224300     SKIP3                                                                
224400 MFS-RENSA-FAELT-IN SECTION.                                              
224500                                                                          
224600*    --- ALLA INDATA-FÄLT                                                 
224700                                                                          
224800     MOVE +1 TO INDX                                                      
224900     PERFORM UNTIL INDX > MAX-INDX                                        
225000       MOVE MFS-RENSA-FAELT TO MOD-KDCMD (INDX)                           
225100       ADD +1 TO INDX                                                     
225200     END-PERFORM                                                          
225300     .                                                                    
225400     EJECT                                                                
225500 MFS-STAENG-FAELT-IN SECTION.                                             
225600                                                                          
225700*    --- ALLA INDATA-FÄLT                                                 
225800                                                                          
225900     MOVE +1 TO INDX                                                      
226000     PERFORM UNTIL INDX > MAX-INDX                                        
226100       MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDCMD-ATTR (INDX)               
226200       ADD +1 TO INDX                                                     
226300     END-PERFORM                                                          
226400     .                                                                    
226500     EJECT                                                                
226600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
226700                                                                          
226800*    --- ALLA UTDATA-FÄLT                                                 
226900*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
227000                                                                          
227100     MOVE +1 TO INDX                                                      
227200     PERFORM UNTIL INDX > MAX-INDX                                        
227300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
227400       ADD +1 TO INDX                                                     
227500     END-PERFORM                                                          
227600     .                                                                    
227700     SKIP2                                                                
227800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
227900                                                                          
228000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
228100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR (INDX)                         
228200                               MOD-IDKUNDNR (INDX)                        
228300                               MOD-IDRAPPNR (INDX)                        
228400                               MOD-IDDC (INDX)                            
228500                               MOD-KDKRENOT (INDX)                        
228600                               MOD-SUKRENOT (INDX)                        
228700                               MOD-BEANST (INDX)                          
228800                               MOD-IDUSER (INDX)                          
228900                               MOD-BEANST-GODK (INDX)                     
229000                               MOD-IDANSTNR-GODK (INDX)                   
229100                               MOD-TIMMDD (INDX)                          
229200                               MOD-FLKLAR (INDX)                          
229300     .                                                                    
229400     SKIP3                                                                
229500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
229600                                                                          
229700*    --- ALLA INDATA-FÄLT                                                 
229800                                                                          
229900     MOVE +1 TO INDX                                                      
230000     PERFORM UNTIL INDX > MAX-INDX                                        
230100       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (INDX)                         
230200       ADD +1 TO INDX                                                     
230300     END-PERFORM                                                          
230400     .                                                                    
230500     EJECT                                                                
230600 MFS-FORM-ATTR SECTION.                                                   
230700                                                                          
230800*    --- ALLA INDATA-FÄLT                                                 
230900                                                                          
231000     MOVE +1 TO INDX                                                      
231100     PERFORM UNTIL INDX > MAX-INDX                                        
231200     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (INDX)                     
231300       ADD +1 TO INDX                                                     
231400     END-PERFORM                                                          
231500     MOVE MFS-FORMATETS-ATTR TO MOD-FLGODK-ATTR                           
231600     .                                                                    
231700     SKIP2                                                                
231800* --- IMS SEKTIONER ---                                                   
231900     SKIP3                                                                
232000 IMS-GET-MSG SECTION.                                                     
232100                                                                          
232200     MOVE '  QC' TO GODK-STATUSKODER                                      
232300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
232400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
232500     PERFORM IMS-STATUSKONTROLL                                           
232600     .                                                                    
232700     SKIP3                                                                
232800 IMS-INSERT-MSG SECTION.                                                  
232900                                                                          
233000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
233100     MOVE SPACE TO GODK-STATUSKODER                                       
233200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
233300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
233400     PERFORM IMS-STATUSKONTROLL                                           
233500     .                                                                    
233600     EJECT                                                                
233700 IMS-CHANGE-ALTMSG SECTION.                                               
233800     MOVE '  A1A4' TO GODK-STATUSKODER                                    
233900     CALL CBLTDLI USING CHNG ALT-PCB P-TO-P-KDTRANS                       
234000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
234100     PERFORM IMS-STATUSKONTROLL                                           
234200     .                                                                    
234300     SKIP3                                                                
234400 IMS-INSERT-ALTMSG SECTION.                                               
234500     MOVE SPACE TO GODK-STATUSKODER                                       
234600     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-IO-AREA                       
234700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
234800     PERFORM IMS-STATUSKONTROLL                                           
234900     .                                                                    
235000     EJECT                                                                
235100 IMS-INSERT-MAIL SECTION.                                                 
235200                                                                          
235300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
235400     MOVE SPACE TO GODK-STATUSKODER                                       
235500     CALL CBLTDLI USING ISRT MAIL-PCB MAIL-WMSGMAIL                       
235600     MOVE MAIL-STATUS-CODE       TO STATUS-WS                             
235700     PERFORM IMS-STATUSKONTROLL                                           
235800     .                                                                    
235900     EJECT                                                                
236000 IMS-GU-WDA2A1-UNIK  SECTION.                                             
236100                                                                          
236200     STRING 'WDA2A1  (WDA2A1KY =' W-WDA2A1KY-X                            
236300                    '&KDLEVATT =' W-KDLEVATT-X ')'                        
236400          DELIMITED BY SIZE INTO SSA1                                     
236500     MOVE '  GE' TO GODK-STATUSKODER                                      
236600     CALL CBLTDLI USING GU WDA2A-PCB DLI-IO-WDA2A1 SSA1                   
236700     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
236800     PERFORM IMS-STATUSKONTROLL                                           
236900     .                                                                    
237000     EJECT                                                                
237100 IMS-GU-WDA2A1   SECTION.                                                 
237200                                                                          
237300     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
237400                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X                        
237500                    '&KDLEVATT =' W-KDLEVATT-X ')'                        
237600          DELIMITED BY SIZE INTO SSA1                                     
237700     MOVE '  GE' TO GODK-STATUSKODER                                      
237800     CALL CBLTDLI USING GU WDA2A-PCB DLI-IO-WDA2A1 SSA1                   
237900     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
238000     PERFORM IMS-STATUSKONTROLL                                           
238100     .                                                                    
238200     EJECT                                                                
238300 IMS-GN-WDA2A1   SECTION.                                                 
238400                                                                          
238500     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
238600                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X                        
238700                    '&KDLEVATT =' W-KDLEVATT-X ')'                        
238800          DELIMITED BY SIZE INTO SSA1                                     
238900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
239000     CALL CBLTDLI USING GN WDA2A-PCB DLI-IO-WDA2A1 SSA1                   
239100     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
239200     PERFORM IMS-STATUSKONTROLL                                           
239300     .                                                                    
239400     EJECT                                                                
239500 IMS-GU-WDA2A1-IDUSER  SECTION.                                           
239600                                                                          
239700     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
239800                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X                        
239900                    '&IDUSERAD =' W-IDUSER-X                              
240000                    '&KDLEVATT =' W-KDLEVATT-X ')'                        
240100          DELIMITED BY SIZE INTO SSA1                                     
240200     MOVE '  GE' TO GODK-STATUSKODER                                      
240300     CALL CBLTDLI USING GU WDA2A-PCB DLI-IO-WDA2A1 SSA1                   
240400     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
240500     PERFORM IMS-STATUSKONTROLL                                           
240600     .                                                                    
240700     EJECT                                                                
240800 IMS-GN-WDA2A1-IDUSER  SECTION.                                           
240900                                                                          
241000     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
241100                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X                        
241200                    '&IDUSERAD =' W-IDUSER-X                              
241300                    '&KDLEVATT =' W-KDLEVATT-X ')'                        
241400          DELIMITED BY SIZE INTO SSA1                                     
241500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
241600     CALL CBLTDLI USING GN WDA2A-PCB DLI-IO-WDA2A1 SSA1                   
241700     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
241800     PERFORM IMS-STATUSKONTROLL                                           
241900     .                                                                    
242000     EJECT                                                                
242100 IMS-GHU-WDA201 SECTION.                                                  
242200                                                                          
242300     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
242400          DELIMITED BY SIZE INTO SSA1                                     
242500     MOVE '  GE' TO GODK-STATUSKODER                                      
242600     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA201 SSA1                   
242700     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
242800     PERFORM IMS-STATUSKONTROLL                                           
242900     .                                                                    
243000     SKIP3                                                                
243100 IMS-REPL-WDA201 SECTION.                                                 
243200                                                                          
243300     MOVE '  ' TO GODK-STATUSKODER                                        
243400     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA201                       
243500     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
243600     PERFORM IMS-STATUSKONTROLL                                           
243700     .                                                                    
243800     EJECT                                                                
243900 IMS-GU-WDGX6327 SECTION.                                                 
244000                                                                          
244100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6327-X ')'                    
244200          DELIMITED BY SIZE INTO SSA1                                     
244300     MOVE '  GE' TO GODK-STATUSKODER                                      
244400     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6327 SSA1                  
244500     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
244600     PERFORM IMS-STATUSKONTROLL                                           
244700     .                                                                    
244800     EJECT                                                                
244900 IMS-GNP-WDGX6328-MIN-MAX SECTION.                                        
245000     STRING 'WDGX6328(KY6328  =>' W-KY6328-MIN-X                          
245100                    '&KY6328  =<' W-KY6328-MAX-X                          
245200                    '&SUBEL    >' W-SUBEL-6328-X ')'                      
245300          DELIMITED BY SIZE INTO SSA1                                     
245400     MOVE '  GE' TO GODK-STATUSKODER                                      
245500     CALL CBLTDLI USING GNP 6327-PCB DLI-IO-WDGX6328 SSA1                 
245600     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
245700     PERFORM IMS-STATUSKONTROLL                                           
245800     .                                                                    
245900     SKIP3                                                                
246000 IMS-GNP-WDGX6328 SECTION.                                                
246100                                                                          
246200     STRING 'WDGX6328(KY6328  >=' W-KY6328-MIN-X                          
246300                    '&KY6328  <=' W-KY6328-MAX-X                          
246400                    '&IDUSERGK =' W-IDUSER-6328-X ')'                     
246500          DELIMITED BY SIZE INTO SSA1                                     
246600     MOVE '  GE' TO GODK-STATUSKODER                                      
246700     CALL CBLTDLI USING GNP 6327-PCB DLI-IO-WDGX6328 SSA1                 
246800     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
246900     PERFORM IMS-STATUSKONTROLL                                           
247000     .                                                                    
247100     EJECT                                                                
247200 IMS-GU-WDGX4103 SECTION.                                                 
247300                                                                          
247400     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
247500          DELIMITED BY SIZE INTO SSA1                                     
247600     MOVE '  GE' TO GODK-STATUSKODER                                      
247700     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
247800     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
247900     PERFORM IMS-STATUSKONTROLL                                           
248000     .                                                                    
248100     EJECT                                                                
248200 IMS-GNP-WDGX4104-KVAL SECTION.                                           
248300                                                                          
248400     STRING 'WDGX4104*F(KEY4104  =' W-KEY4104-X ')'                       
248500          DELIMITED BY SIZE INTO SSA1                                     
248600     MOVE '  GE' TO GODK-STATUSKODER                                      
248700     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4104 SSA1                 
248800     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
248900     PERFORM IMS-STATUSKONTROLL                                           
249000     .                                                                    
249100     EJECT                                                                
249200 IMS-GNP-WDGX4104 SECTION.                                                
249300                                                                          
249400     MOVE 'WDGX4104 ' TO SSA1                                             
249500     MOVE '  GE' TO GODK-STATUSKODER                                      
249600     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4104 SSA1                 
249700     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
249900     PERFORM IMS-STATUSKONTROLL                                           
250000     .                                                                    
250100     EJECT                                                                
250200 IMS-GHNP-WDGX4104 SECTION.                                               
250300                                                                          
250400     MOVE 'WDGX4104 ' TO SSA1                                             
250500     MOVE '  GE' TO GODK-STATUSKODER                                      
250600     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
250700     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
250900     PERFORM IMS-STATUSKONTROLL                                           
251000     .                                                                    
251100     EJECT                                                                
251200 IMS-GHU-WDGX4104 SECTION.                                                
251300                                                                          
251400     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
251500          DELIMITED BY SIZE INTO SSA1                                     
251600     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
251700          DELIMITED BY SIZE INTO SSA2                                     
251800     MOVE '  GE' TO GODK-STATUSKODER                                      
251900     CALL CBLTDLI USING GHU 4103-PCB DLI-IO-WDGX4104 SSA1 SSA2            
252000     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
252100     PERFORM IMS-STATUSKONTROLL                                           
252200     .                                                                    
252300     EJECT                                                                
252400 IMS-REPL-WDGX4104 SECTION.                                               
252500                                                                          
252600     MOVE '  ' TO GODK-STATUSKODER                                        
252700     CALL CBLTDLI USING REPL 4103-PCB DLI-IO-WDGX4104                     
252800     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
252900     PERFORM IMS-STATUSKONTROLL                                           
253000     .                                                                    
253100     EJECT                                                                
253200 IMS-GU-WDGX4104-KVAL SECTION.                                            
253300                                                                          
253400     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
253500          DELIMITED BY SIZE INTO SSA1                                     
253600     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
253700          DELIMITED BY SIZE INTO SSA2                                     
253800     MOVE '  GE' TO GODK-STATUSKODER                                      
253900     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4104 SSA1 SSA2             
254000     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
254100     PERFORM IMS-STATUSKONTROLL                                           
254200     .                                                                    
254300     EJECT                                                                
254400 IMS-GU-WDGX4105 SECTION.                                                 
254500                                                                          
254600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
254700          DELIMITED BY SIZE INTO SSA1                                     
254800     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
254900          DELIMITED BY SIZE INTO SSA2                                     
255000     STRING 'WDGX4105(KDSEGKEY =' W-KDSEGKEY-X ')'                        
255100          DELIMITED BY SIZE INTO SSA3                                     
255200     MOVE '  GE' TO GODK-STATUSKODER                                      
255300     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4105 SSA1 SSA2             
255400                                                     SSA3                 
255500     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
255600     PERFORM IMS-STATUSKONTROLL                                           
255700     .                                                                    
255800     EJECT                                                                
255900 IMS-GNP-WDGX4106 SECTION.                                                
256000                                                                          
256100     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
256200          DELIMITED BY SIZE INTO SSA1                                     
256300     MOVE 'WDGX4106 ' TO SSA2                                             
256400     MOVE '  GE' TO GODK-STATUSKODER                                      
256500     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4106 SSA1 SSA2            
256600     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
256700     PERFORM IMS-STATUSKONTROLL                                           
256800     .                                                                    
256900     EJECT                                                                
257000 IMS-GHNP-WDGX4106 SECTION.                                               
257100                                                                          
257200     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
257300          DELIMITED BY SIZE INTO SSA1                                     
257400     MOVE 'WDGX4106 ' TO SSA2                                             
257500     MOVE '  GE' TO GODK-STATUSKODER                                      
257600     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4106 SSA1 SSA2           
257700     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
257800     PERFORM IMS-STATUSKONTROLL                                           
257900     .                                                                    
258000     EJECT                                                                
258100 IMS-GU-WDGX4106 SECTION.                                                 
258200                                                                          
258300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
258400          DELIMITED BY SIZE INTO SSA1                                     
258500     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
258600          DELIMITED BY SIZE INTO SSA2                                     
258700     STRING 'WDGX4106(TIDATETI>=' W-TIDATETIME-MIN-X                      
258800                    '&TIDATETI<=' W-TIDATETIME-MAX-X                      
258900                    '&IDUSERGK =' W-IDUSER-4106-X ')'                     
259000          DELIMITED BY SIZE INTO SSA3                                     
259100     MOVE '  GE' TO GODK-STATUSKODER                                      
259200     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4106 SSA1 SSA2             
259300                                                     SSA3                 
259400     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
259500     PERFORM IMS-STATUSKONTROLL                                           
259600     .                                                                    
259700     EJECT                                                                
259800 IMS-GHU-WDGX4106 SECTION.                                                
259900                                                                          
260000     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
260100          DELIMITED BY SIZE INTO SSA1                                     
260200     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
260300          DELIMITED BY SIZE INTO SSA2                                     
260400     STRING 'WDGX4106(TIDATETI>=' W-TIDATETIME-MIN-X                      
260500                    '&TIDATETI<=' W-TIDATETIME-MAX-X                      
260600                    '&IDUSERGK =' W-IDUSER-4106-X ')'                     
260700          DELIMITED BY SIZE INTO SSA3                                     
260800     MOVE '  GE' TO GODK-STATUSKODER                                      
260900     CALL CBLTDLI USING GHU 4103-PCB DLI-IO-WDGX4106 SSA1 SSA2            
261000                                                     SSA3                 
261100     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
261200     PERFORM IMS-STATUSKONTROLL                                           
261300     .                                                                    
261400     EJECT                                                                
261500 IMS-REPL-WDGX4106 SECTION.                                               
261600                                                                          
261700     MOVE '  ' TO GODK-STATUSKODER                                        
261800     CALL CBLTDLI USING REPL 4103-PCB DLI-IO-WDGX4106                     
261900     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
262000     PERFORM IMS-STATUSKONTROLL                                           
262100     .                                                                    
262200     EJECT                                                                
262300 IMS-ISRT-WDGX4106 SECTION.                                               
262400                                                                          
262500     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
262600          DELIMITED BY SIZE INTO SSA1                                     
262700     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
262800          DELIMITED BY SIZE INTO SSA2                                     
262900     MOVE 'WDGX4106 ' TO SSA3                                             
263000     MOVE '  II' TO GODK-STATUSKODER                                      
263100     CALL CBLTDLI USING ISRT 4103-PCB DLI-IO-WDGX4106 SSA1 SSA2           
263200                                                     SSA3                 
263300     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
263400     PERFORM IMS-STATUSKONTROLL                                           
263500     .                                                                    
263600     EJECT                                                                
263700 IMS-STATUSKONTROLL SECTION.                                              
263800                                                                          
263900     SET STATUS-IX TO 1                                                   
264000     SEARCH GODK-STATUS                                                   
264100       AT END                                                             
264200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
264300         DELIMITED BY SIZE INTO FELTEXT                                   
264400         CALL FELLOG                                                      
264500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
264600         CONTINUE                                                         
264700     END-SEARCH                                                           
264800     .                                                                    
