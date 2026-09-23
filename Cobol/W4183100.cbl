000100                                                                          
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4183100.                                                
000400 AUTHOR.         SUSANNE OLSSON.                                          
000500 DATE-WRITTEN.   02/03/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET UPPDATERAR WDA2 MED STATUS 9 FÖR DE LEV.ANM.          
001200*        SOM SKALL KREDITERAS VIA BILLIT.KOMPLETTERAR MED UPP-            
001300*        GIFTER OCH SKICKAR EN TRANS TILL BILLIT VIA WZ01-MODULEN         
001400*        (CARPARTS.BILLIT.RECEIVE3).                                      
001500*        FÖR DDI-DISTRIKT KREDITERAR MAN I LOCAL VALUTA OCH FÖR           
001600*        FÖR ANDRA I SEK/CENTRAL PRICING VALUTA.                          
001700*        ÄNDRING 2006-05 VID HELGER, BATCHEN SÖNDAG MORGON,SÅ             
001800*        SKICKAR MAN EN STARTTRANS VIA WZ01 TILL BILL-IT WF0204X          
001900*        FÖR ATT HINNA SKAPA KREDIT INNAN W510D2 GÅR MOT SAP-BOKN.        
002000*                                                                         
002100*        ÄNDRING 2007-04 SKALL SKAPA INTERN DOKUMENT FÖR KREDIT           
002200*        DÄR MAN HAR MINUSTECKEN PÅ SUMMAN.KOD 74 FÖR N-FAKTUROR.         
002300*                                                                         
002400*        BMP                                                              
002500*                                                                         
002600*        PROGRAMMET UPPDATERAR WDA2                                       
002700*        PROGRAMMET LÄSER      WDD3                                       
002800*        PROGRAMMET LÄSER      WDB6                                       
002900*        PROGRAMMET LÄSER      WDR5 (WDGX4103)                            
003000*                                                                         
003100*    E-TRACKER: 1572353                                                   
003200*    E-TRACKER: 2072166                                                   
003300*    E-TRACKER: 3107778                                                   
003400*    E-TRACKER: 3403780                                                   
003500*    E-TRACKER: 850114  2007-04-04                                        
003600*    E-TRACKER: 5144757 2007-06-07                                        
003700*    E-TRACKER: 10143271 2011-12-13 CHINA WAREHOUSE PROJECT-1             
003800*                                                                         
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP2                                                                
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004600     SKIP2                                                                
004700*          --- KREDITNOTAPOSTER FÖR BILLIT                                
004800     SELECT W418AI                     ASSIGN TO W41831D1.                
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100     SKIP3                                                                
005200 FILE SECTION.                                                            
005300     SKIP3                                                                
005400 FD  W418AI                                                               
005500     RECORDING       V                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800*01  -COPY W41831A      -L.                                               
005900                                                                          
006000*01  -COPY W41831B      -L.                                               
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400 77  IDPGM                       PIC X(8)    VALUE 'W4183100'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  W-KVPOST-IN                 PIC S9(5)   VALUE +0   COMP-3.           
006800 77  WS-ANTAL-SEND               PIC S9(7)   VALUE +0   COMP-3.           
006900 77  INDX                        PIC S9(3)   VALUE +0   COMP-3.           
007000 77  MAX-INDX                    PIC S9(3)   VALUE +23  COMP-3.           
007100 77  GODK-IX                     PIC S9(3)   VALUE +0   COMP-3.           
007200 77  MAX-GODK-IX                 PIC S9(3)   VALUE +5   COMP-3.           
007300 77  RKOD-ABEND                  PIC S9(3)   COMP SYNC VALUE +33.         
007400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
007500 77  SPAR-IDDISTR                PIC S9(5)   VALUE ZERO COMP-3.           
007600 77  SPAR-IDKUNDNR               PIC S9(7)   VALUE ZERO COMP-3.           
007700 77  SPAR-IDRAPPNR               PIC  9(7)   VALUE ZERO.                  
007800 77  HELP-IDARTNR                PIC  9(8)   VALUE ZERO.                  
007900 77  HELP-IDRADNR                PIC  9(5)   VALUE ZERO.                  
008000 77  HELP-IDKUNDNR               PIC  9(6)   VALUE ZERO.                  
008100 77  HELP-IDFAKREF               PIC  9(8)   VALUE ZERO.                  
008200 77  HELP-IDKONTO                PIC  9(11)  VALUE ZERO.                  
008300 77  WS-IDBUNDLE                 PIC  9(8)   VALUE ZERO.                  
008400 77  WS-IDBREAK                  PIC  X(8)   VALUE SPACE.                 
008500 77  WS-TIME-WAIT                PIC S9(9)   COMP VALUE +0001.            
008600 77  TEST-IDFKNGRP               PIC S9(5)   COMP-3.                      
008700     88  FKNGRP-VSA-IMP                      VALUE 1788.                  
008800     88  FKNGRP-EXT-WARRANTY                 VALUE 1728.                  
008900                                                                          
009000 01  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
009100 01  FILLER                      REDEFINES WS-IDPARTNR.                   
009200     03  WS-INT                  PIC 9(2).                                
009300     03  FILLER                  PIC 9(7).                                
009400                                                                          
009500 01  WS-IDARTNR-LYNK             PIC X(50).                               
009600 01  FILLER                      REDEFINES WS-IDARTNR-LYNK.               
009700     03  WS-IDARTNR-V            PIC X(10).                               
009800     03  WS-IDARTNR-L            PIC X(12).                               
009900     03  FILLER                  PIC X(28).                               
010000 77  WS-REDUIN                   PIC X(30)   VALUE SPACE.                 
010100 77  WS-REDUUT                   PIC X(30)   VALUE SPACE.                 
010200                                                                          
010300 77  WDR501-SW                   PIC X       VALUE 'N'.                   
010400     88  WDR501-FINNS                        VALUE 'J'.                   
010500                                                                          
010600 77  NY-LEVANM-SW                PIC X       VALUE 'N'.                   
010700     88  NY-LEVANM                           VALUE 'J'.                   
010800                                                                          
010900 77  SEND-TO-BILLIT-SW           PIC X       VALUE 'N'.                   
011000     88  SEND-TO-BILLIT                      VALUE 'J'.                   
011100                                                                          
011200 77  W418AI-EOF-SW               PIC X       VALUE 'N'.                   
011300     88  END-OF-W418AI                       VALUE 'J'.                   
011400                                                                          
011500*01  -COPY WWDC99                                                         
011600                                                                          
011700 01 WS-GODK-TABELL.                                                       
011800    03 WS-GODK-GRP OCCURS 5.                                              
011900       05 WS-SPAR-IDUSER-GODK    PIC X(8).                                
012000       05 WS-SPAR-BEANST-GODK    PIC X(25).                               
012100       05 WS-SPAR-TISEKEL        PIC 9(2).                                
012200       05 WS-SPAR-TIUPPDAT       PIC 9(6).                                
012300                                                                          
012400     SKIP2                                                                
012500                                                                          
012600 01  CHKP-VAR.                                                            
012700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
012800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
012900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
013000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
013100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
013200     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
013300     SKIP2                                                                
013400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
013500 01  FELTEXT.                                                             
013600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013800                                                                          
013900 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
014000*01  FILLER  -COPY WWDIST79 -RED TEST-IDDISTR.                            
014100                                                                          
014200 01  KDRC-DISPLAY                PIC Z(5).                                
014300     EJECT                                                                
014400                                                                          
014500 01  W-PRLANDCO-RAD              PIC 9(7)V9(2).                           
014600 01  FILLER REDEFINES W-PRLANDCO-RAD.                                     
014700    03  W-PRLANDCO-HEL           PIC 9(7).                                
014800    03  W-PRLANDCO-DEC           PIC 9(2).                                
014900                                                                          
015000 01  W-PRLANDCO-RAD-X.                                                    
015100    03  W-PRLANDCO-X-HEL         PIC X(7).                                
015200    03  FILLER                   PIC X(1)    VALUE '.'.                   
015300    03  W-PRLANDCO-X-DEC         PIC X(2).                                
015400                                                                          
015500                                                                          
015600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015700 01  FILLER REDEFINES DAGENS-DATUM.                                       
015800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
016000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
016100     EJECT                                                                
016200 01  DYNAMISKA-SUBPROGRAM.                                                
016300*                                                                         
016400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016800     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
016900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
017000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
017100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017200     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
017300     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
017400     EJECT                                                                
017500*    --- PARAMETRAR TILL POSTSUM                                          
017600*                                                                         
017700*01  -COPY W0005   -PRE  POSTSUM-                                         
017800     EJECT                                                                
017900*                                                                         
018000*    ---  LÄNKAREA TILL W418OKOD                                          
018100     SKIP3                                                                
018200*    03 -COPY W418OKOD           -PRE OKOD-.                              
018300     EJECT                                                                
018400*                                                                         
018500*    --- PARAMETRAR TILL W009CIA                                          
018600*01  -COPY W009CIA                                                        
018700     EJECT                                                                
018800 01  FILLER              PIC X(16)   VALUE ' WDATAREA'.                   
018900*01    -COPY WDATAREA                                                     
019000     EJECT                                                                
019100                                                                          
019200*    --- AREOR FÖR KOMMUNIKATION                                          
019300 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
019400*01  -COPY WZ01SEND                                                       
019500     EJECT                                                                
019600 01  IN-AREA-START               PIC X(24)   VALUE                        
019700                                             'IN-AREA-START'.             
019800     SKIP2                                                                
019900 01  IN-AREA.                                                             
020000     03  IN-AREA-0.                                                       
020100         05  IN-IDPTYP           PIC X(3) VALUE SPACE.                    
020200         05  FILLER              PIC X(400).                              
020300*   03  FILLER -COPY W41831A  -PRE IN31A-  -RED  IN-AREA-0                
020400*   03  FILLER -COPY W41831B  -PRE IN31B-  -RED  IN-AREA-0                
020500*                                                                         
020600     EJECT                                                                
020700 01  UT-AREA-START               PIC X(24)   VALUE                        
020800                                 'UT-AREA-START  '.                       
020900                                                                          
021000 01  UT-AREA.                                                             
021100*    03  FILLER -COPY WZ01REQU  -PRE UT-                                  
021200*    03  FILLER -COPY WF0212I1  -PRE UT-                                  
021300     EJECT                                                                
021400 01  SEND-AREA.                                                           
021500*    03  -COPY WZ01REQU                                                   
021600     EJECT                                                                
021700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021800     SKIP3                                                                
021900 01  NYCKLAR-TILL-DLI.                                                    
022000     03  W-IDLEVANM-X.                                                    
022100         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
022200         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
022300         05  W-IDRAPPNR          PIC  9(7)    VALUE ZERO.                 
022400                                                                          
022500     03  W-WDA211KY-X.                                                    
022600         05  W-IDARTNR-WDA2      PIC S9(9)   COMP-3 VALUE ZERO.           
022700         05  W-IDRADNR-WDA2      PIC S9(5)   COMP-3 VALUE ZERO.           
022800                                                                          
022900*    -NYCKLAR TIL WDF502 --> LYNK-CROSS-DB                                
023000     03  W-IDARTNR-WDF502-X.                                              
023100         05  W-IDARTNR-F5        PIC S9(9)   VALUE ZERO COMP-3.           
023200                                                                          
023300     03  W-IDARTNR-X.                                                     
023400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023500                                                                          
023600     03  W-IDSKYLT-X.                                                     
023700         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
023800                                                                          
023900     03  W-IDDC-B6-X.                                                     
024000         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
024100                                                                          
024200     03  W-IDDC-B6-RET-X.                                                 
024300         05 W-IDDC-B6-RET        PIC X(2)    VALUE SPACE.                 
024400                                                                          
024500     03  W-WDGXKEY-4103-X.                                                
024600         05  W-IDHTYP-4103       PIC X(4)    VALUE '4103'.                
024700         05  W-IDDISTR-4103      PIC S9(5)   VALUE ZERO COMP-3.           
024800         05  W-IDKUNDNR-4103     PIC S9(7)   VALUE ZERO COMP-3.           
024900         05  W-IDRAPPNR-4103     PIC  9(7)   VALUE ZERO.                  
025000         05  FILLER              PIC X(12)   VALUE LOW-VALUE.             
025100                                                                          
025200     03  W-WDGXKEY-4104-X.                                                
025300         05  W-IDDC-4104         PIC X(2)    VALUE SPACE.                 
025400         05  W-KDKRENOT-4104     PIC X(2)    VALUE SPACE.                 
025500                                                                          
025600     03  W-TIDATETIME-X.                                                  
025700         05  W-TIDATETIME        PIC X(14)   VALUE SPACE.                 
025800                                                                          
025900     03  W-TIDATETIME-MIN-X.                                              
026000         05  W-TIDATETIME-MIN    PIC X(14)   VALUE LOW-VALUE.             
026100                                                                          
026200     03  W-TIDATETIME-MAX-X.                                              
026300         05  W-TIDATETIME-MAX    PIC X(14)   VALUE HIGH-VALUE.            
026400                                                                          
026500     SKIP2                                                                
026600*    --- STATUS-KOD FRÅN IMS                                              
026700 01  STATUS-WS                   PIC XX.                                  
026800     88  SEGMENT-FINNS                       VALUE '  '.                  
026900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
027200     88  IMS-EJ-OK                           VALUE 'XD'.                  
027300     SKIP2                                                                
027400 01  GODK-STATUSKODER.                                                    
027500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027600     SKIP3                                                                
027700 01  SSA1                        PIC X(64).                               
027800 01  SSA2                        PIC X(64).                               
027900     EJECT                                                                
028000*    --- IMS FUNKTIONSKODER                                               
028100*01  -COPY W0003                                                          
028200     EJECT                                                                
028300*    ---  DLI INPUT-OUTPUT AREA                                           
028400                                                                          
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
028600 01  DLI-IO-WDA201.                                                       
028700*    03  -COPY WDA201                                                     
028800     EJECT                                                                
028900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
029000 01  DLI-IO-WDA211.                                                       
029100*    03  -COPY WDA211                                                     
029200     EJECT                                                                
029300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
029400 01  DLI-IO-WDD301.                                                       
029500*    03  -COPY WDD301                                                     
029600     EJECT                                                                
029700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
029800 01  DLI-IO-WDD311.                                                       
029900*    03  -COPY WDD311                                                     
030000     EJECT                                                                
030100 01  FILLER         PIC X(16) VALUE 'WDB601 AREA'.                        
030200 01   DLI-IO-AREA-B601.                                                   
030300*     03  -COPY WDB601                                                    
030400     EJECT                                                                
030500 01  FILLER         PIC X(16) VALUE 'WDB601 RET '.                        
030600 01   DLI-IO-AREA-B601-RET.                                               
030700*     03  -COPY WDB601 -PRE RET-                                          
030800     EJECT                                                                
030900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4103'.                    
031000 01  DLI-IO-WDGX4103.                                                     
031100*    03  -COPY WDGX4103                                                   
031200     EJECT                                                                
031300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4104'.                    
031400 01  DLI-IO-WDGX4104.                                                     
031500*    03  -COPY WDGX4104                                                   
031600     EJECT                                                                
031700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4106'.                    
031800 01  DLI-IO-WDGX4106.                                                     
031900*    03  -COPY WDGX4106                                                   
032000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF502'.                      
032100     EJECT                                                                
032200*  VOLVO-LYNK-ARTCROSS                                                    
032300 01  DLI-IO-WDF502.                                                       
032400*    03  -COPY WDF502                                                     
032500     EJECT                                                                
032600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
032700 01  DLI-IO-WDK601.                                                       
032800*    03  -COPY WDK601                                                     
032900     EJECT                                                                
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
033100 01  DLI-IO-WDK611.                                                       
033200*    03  -COPY WDK611                                                     
033300     EJECT                                                                
033400                                                                          
033500 LINKAGE SECTION.                                                         
033600*01  -COPY W0009   -PRE MSG-                                              
033700                                                                          
033800*01  -COPY W0009   -PRE RECEIVE-                                          
033900                                                                          
034000*01  -COPY W0009   -PRE CREDIT-                                           
034100                                                                          
034200*01  -COPY W0008   -PRE WDA2-                                             
034300     05  FILLER                  PIC X.                                   
034400                                                                          
034500*01  -COPY W0008   -PRE WDD3-                                             
034600     05  FILLER                  PIC X.                                   
034700                                                                          
034800*01  -COPY W0008   -PRE WDB6-                                             
034900     05  FILLER                  PIC X.                                   
035000                                                                          
035100*01  -COPY W0008   -PRE 4103-                                             
035200     05  FILLER                  PIC X.                                   
035300                                                                          
035400*01  -COPY W0008   -PRE WDF5-                                             
035500     05  FILLER                  PIC X.                                   
035600*                                                                         
035700*01  -COPY W0008   -PRE WDK6-                                             
035800     05  FILLER                  PIC X.                                   
035900                                                                          
036000     EJECT                                                                
036100 PROCEDURE DIVISION  USING MSG-PCB RECEIVE-PCB CREDIT-PCB WDA2-PCB        
036200                           WDD3-PCB WDB6-PCB 4103-PCB WDF5-PCB            
036300                           WDK6-PCB.                                      
036400 MAIN SECTION.                                                            
036500     ENTRY 'DLITCBL' USING MSG-PCB RECEIVE-PCB CREDIT-PCB WDA2-PCB        
036600                           WDD3-PCB WDB6-PCB 4103-PCB WDF5-PCB            
036700                           WDK6-PCB.                                      
036800                                                                          
036900     SKIP2                                                                
037000     PERFORM A-INIT                                                       
037100                                                                          
037200     PERFORM S01-LAES-W418AI                                              
037300     PERFORM UNTIL END-OF-W418AI                                          
037400       PERFORM B-BEHANDLA-LEVANM                                          
037500     END-PERFORM                                                          
037600                                                                          
037700     IF DAT-TID = 7                                                       
037800       IF IN-IDPTYP = '31A' OR '31B'                                      
037900         PERFORM C-START-PGM-WF020400                                     
038000       END-IF                                                             
038100     END-IF                                                               
038200                                                                          
038300     PERFORM Z-FINIT                                                      
038400                                                                          
038500     MOVE ZERO TO RETURN-CODE                                             
038600     GOBACK                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 A-INIT SECTION.                                                          
039000     SKIP2                                                                
039100                                                                          
039200     PERFORM IMS-RESTART                                                  
039300                                                                          
039400     OPEN INPUT W418AI                                                    
039500                                                                          
039600                                                                          
039700     MOVE +0                           TO CHKP-ANT                        
039800     MOVE +0                           TO WS-ANTAL-SEND                   
039900                                                                          
040000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
040100                                                                          
040200     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
040300     CALL WDATKONV USING DAT-KDDATFORM                                    
040400                         DAT-I-TIDATUM                                    
040500                         DAT-O-TIDATUM                                    
040600                         DAT-KDSVAR                                       
040700     .                                                                    
040800     EJECT                                                                
040900 B-BEHANDLA-LEVANM  SECTION.                                              
041000                                                                          
041100     MOVE IN31A-IDDISTR                TO SPAR-IDDISTR                    
041200                                          W-IDDISTR-4103                  
041300     MOVE IN31A-IDKUNDNR               TO SPAR-IDKUNDNR                   
041400                                          W-IDKUNDNR-4103                 
041500     MOVE IN31A-IDRAPPNR               TO SPAR-IDRAPPNR                   
041600                                          W-IDRAPPNR-4103                 
041700     MOVE NEJ                          TO NY-LEVANM-SW                    
041800     MOVE NEJ                          TO SEND-TO-BILLIT-SW               
041900     MOVE NEJ                          TO WDR501-SW                       
042000     MOVE +0                           TO W-KVPOST-IN                     
042100                                                                          
042200     PERFORM IMS-GU-WDGX4103                                              
042300     IF SEGMENT-FINNS                                                     
042400       MOVE JA                         TO WDR501-SW                       
042500     END-IF                                                               
042600                                                                          
042700     PERFORM S02-SEND-OPEN                                                
042800                                                                          
042900     CALL W009WAIT USING WS-TIME-WAIT                                     
043000     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-IDBUNDLE                     
043100                                                                          
043200     PERFORM UNTIL END-OF-W418AI OR NY-LEVANM                             
043300                                                                          
043400       EVALUATE IN-IDPTYP                                                 
043500         WHEN '31A'                                                       
043600             IF IN31A-IDDISTR  NOT = SPAR-IDDISTR  OR                     
043700                IN31A-IDKUNDNR NOT = SPAR-IDKUNDNR OR                     
043800                IN31A-IDRAPPNR NOT = SPAR-IDRAPPNR                        
043900                MOVE JA                TO NY-LEVANM-SW                    
044000             ELSE                                                         
044100               PERFORM BA-UPPDATERA-STATUS                                
044200               PERFORM S01-LAES-W418AI                                    
044300             END-IF                                                       
044400         WHEN '31B'                                                       
044500             IF SEND-TO-BILLIT                                            
044600               PERFORM BB-UPPDATERA-WDA211                                
044700               MOVE IN31B-IDDC    TO WS-IDDC                              
044800               PERFORM BC-SKAPA-RADER-BILLIT                              
044900               PERFORM BD-CREATE-EXTRA-CREDIT-ITALY                       
045000               PERFORM S03-SEND-PUT                                       
045100                                                                          
045300               IF XDC-NON-VCC-OWNED                                       
045400                 CONTINUE                                                 
045500               ELSE                                                       
045600                 IF IN31B-PRLANDCO-RAD > ZERO                             
045700                   MOVE 'LANDING COST             '   TO UT-BEART         
045800                                                                          
045900*----IDRADNR+IDARTNR TILLHÖR NYCKELN TILL WDA211. BEHÖVS SENARE           
046000*----TILLBAKA FRÅN BILLIT TILL RUTIN W418S2.                              
046100                                                                          
046200                   MOVE 'VO'                TO CIA-IDARTPRE-IN            
046300                   MOVE IN31B-IDRADNR       TO CIA-IDARTBET-IN            
046400                   CALL W009CIA USING          CIA-W009CIA                
046500                   MOVE CIA-IDARTBET-UT     TO UT-IDOPTION (1)            
046600                                                                          
046700                   MOVE 'VO'                TO CIA-IDARTPRE-IN            
046800                   MOVE IN31B-IDARTNR       TO CIA-IDARTBET-IN            
046900                   CALL W009CIA USING          CIA-W009CIA                
047000                   MOVE CIA-IDARTBET-UT     TO UT-IDOPTION (2)            
047100                                                                          
047200                   MOVE IN31B-IDARTNR       TO HELP-IDARTNR               
047300                   MOVE HELP-IDARTNR        TO UT-IDSEQ (1)               
047400                   MOVE '99999999'          TO UT-IDSEQ (2)               
047500                                                                          
047600                   MOVE IN31B-PRLANDCO-RAD  TO UT-PRARTBTO                
047700                                               UT-PRARTNTO                
047800                   PERFORM S05-SKAPA-TKOST-BILLIT                         
047900                   PERFORM S03-SEND-PUT                                   
048000                 END-IF                                                   
048100                 IF IN31B-PRFRAKT > ZERO                                  
048200                   MOVE 'FREIGHT COST             '   TO UT-BEART         
048300                   MOVE SPACE               TO UT-IDOPTION (1)            
048400                                               UT-IDOPTION (2)            
048500                   MOVE IN31B-PRFRAKT       TO UT-PRARTBTO                
048600                                               UT-PRARTNTO                
048700                   MOVE '99999999'          TO UT-IDSEQ (1)               
048800                   MOVE SPACE               TO UT-IDSEQ (2)               
048900                                                                          
049000                   PERFORM S05-SKAPA-TKOST-BILLIT                         
049100                   PERFORM S03-SEND-PUT                                   
049200                 END-IF                                                   
049300                 IF IN31B-PRFOERS > ZERO                                  
049400                   MOVE 'INSURANCE COST           '   TO UT-BEART         
049500                   MOVE SPACE               TO UT-IDOPTION (1)            
049600                                               UT-IDOPTION (2)            
049700                   MOVE IN31B-PRFOERS       TO UT-PRARTBTO                
049800                                               UT-PRARTNTO                
049900                   MOVE '99999999'          TO UT-IDSEQ (1)               
050000                   MOVE SPACE               TO UT-IDSEQ (2)               
050100                                                                          
050200                   PERFORM S05-SKAPA-TKOST-BILLIT                         
050300                   PERFORM S03-SEND-PUT                                   
050400                 END-IF                                                   
050500                 IF IN31B-PRLEGKST > ZERO                                 
050600                   MOVE 'LEGALIZATION FEE         '   TO UT-BEART         
050700                   MOVE SPACE               TO UT-IDOPTION (1)            
050800                                               UT-IDOPTION (2)            
050900                   MOVE IN31B-PRLEGKST      TO UT-PRARTBTO                
051000                                               UT-PRARTNTO                
051100                   MOVE '99999999'          TO UT-IDSEQ (1)               
051200                   MOVE SPACE               TO UT-IDSEQ (2)               
051300                                                                          
051400                   PERFORM S05-SKAPA-TKOST-BILLIT                         
051500                   PERFORM S03-SEND-PUT                                   
051600                 END-IF                                                   
051700               END-IF                                                     
051800                                                                          
051900               PERFORM S01-LAES-W418AI                                    
052000                                                                          
052100             ELSE                                                         
052200               PERFORM S01-LAES-W418AI                                    
052300             END-IF                                                       
052400       END-EVALUATE                                                       
052500                                                                          
052600     END-PERFORM                                                          
052700     PERFORM S04-SEND-CLOSE                                               
052800     IF NY-LEVANM                                                         
052900       PERFORM X-TAG-CHECKPOINT                                           
053000       ADD +1                           TO CHKP-ANT                       
053100     END-IF                                                               
053200                                                                          
053300     .                                                                    
053400     EJECT                                                                
053500 BA-UPPDATERA-STATUS SECTION.                                             
053600                                                                          
053700     MOVE IN31A-IDLEVANM               TO W-IDLEVANM-X                    
053800                                                                          
053900     PERFORM IMS-GHU-WDA201                                               
054000                                                                          
054100     IF ANM-KDLEVANM = '3' OR '7'                                         
054200       MOVE '9'                        TO ANM-KDLEVANM                    
054300       MOVE IN31A-KDLEVANM-UPD         TO ANM-KDLEVANM-UPD                
054400       MOVE IN31A-FLFARLIG             TO ANM-FLFARLIG                    
054500       IF IN31A-KDLEVANM-UPD = '4'                                        
054600          MOVE IN31A-IDPERSON          TO ANM-IDPERSON                    
054700          MOVE IN31A-KDARBTYP          TO ANM-KDARBTYP                    
054800          MOVE IN31A-KVRADER-RT        TO ANM-KVRADER-RT                  
054900                                          ANM-KVRADER-OBEH                
055000       END-IF                                                             
055100                                                                          
055200       PERFORM IMS-REPL-WDA201                                            
055300       MOVE JA                         TO SEND-TO-BILLIT-SW               
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700 BB-UPPDATERA-WDA211 SECTION.                                             
055800                                                                          
055900     MOVE IN31B-IDARTNR                TO W-IDARTNR-WDA2                  
056000     MOVE IN31B-IDRADNR                TO W-IDRADNR-WDA2                  
056100                                                                          
056200     PERFORM IMS-GHU-WDA211                                               
056300                                                                          
056400     MOVE IN31B-FLLSBOK                TO LEV-FLLSBOK                     
056500     MOVE IN31B-FLINVUPD               TO LEV-FLINVUPD                    
056600     MOVE IN31B-KDAVVTYP               TO LEV-KDAVVTYP                    
056700                                                                          
056800     PERFORM IMS-REPL-WDA211                                              
056900     .                                                                    
057000     EJECT                                                                
057100 BC-SKAPA-RADER-BILLIT SECTION.                                           
057200                                                                          
057300     MOVE 1                            TO UT-REQU-IDMSGVER                
057400     MOVE SPACE                        TO UT-REQU-KDPGMACT                
057500     MOVE 'W4183100'                   TO UT-REQU-IDUSER                  
057600                                                                          
057700     MOVE IN31B-IDDISTR TO TEST-IDDISTR                                   
057800                                                                          
058000     IF XDC-NON-VCC-OWNED                                                 
058100       PERFORM BCA-SKAPA-RADER-BILLIT                                     
058200     ELSE                                                                 
058300       PERFORM BCB-SKAPA-RADER-BILLIT                                     
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700 BCA-SKAPA-RADER-BILLIT SECTION.                                          
058800                                                                          
058900*    MOVE 'VCCN'                       TO UT-IDLEGSEL                     
059000                                                                          
059100*    IF NDC-IN                                                            
059200*      MOVE 'VCIN'                     TO UT-IDLEGSEL                     
059300*    END-IF                                                               
059400                                                                          
059500*    IF NDC-KR                                                            
059600*      MOVE 'VCKR'                     TO UT-IDLEGSEL                     
059700*    END-IF                                                               
059800                                                                          
059900     IF IN31B-KDANMORS = '74'                                             
060000       MOVE SPACE                      TO UT-IDBUNDLE                     
060100       MOVE WS-IDBUNDLE                TO UT-IDBUNDLE (1:8)               
060200     ELSE                                                                 
060300       MOVE WS-IDBUNDLE                TO UT-IDBUNDLE (1:8)               
060400       MOVE ' W418AI'                  TO UT-IDBUNDLE (9:7)               
060500     END-IF                                                               
060600     MOVE SPACE                        TO UT-IDREF                        
060700                                                                          
060800     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
060900     MOVE IN31B-IDRAPPNR       TO CIA-IDARTBET-IN                         
061000     CALL W009CIA USING           CIA-W009CIA                             
061100     MOVE CIA-IDARTBET-UT      TO UT-IDREF                                
061200                                                                          
061300     MOVE IN31B-DALEVANM               TO UT-DAREFDAT                     
061400                                                                          
061500     ADD +1                            TO W-KVPOST-IN                     
061600     MOVE W-KVPOST-IN                  TO UT-IDREFRAD                     
061700                                                                          
061800     IF IN31B-KDANMORS = '97'                                             
061900       MOVE 'UNPACKING '               TO UT-BEVOLREF                     
062000     ELSE                                                                 
062100       IF IN31B-KDANMORS = '74'                                           
062200         MOVE SPACE                    TO UT-BEVOLREF                     
062300         MOVE IN31B-IDFAKREF           TO HELP-IDFAKREF                   
062400         MOVE HELP-IDFAKREF            TO WS-REDUIN                       
062500         INSPECT WS-REDUIN REPLACING LEADING ZERO BY SPACE                
062600         CALL W009REDU USING WS-REDUIN WS-REDUUT                          
062700         MOVE WS-REDUUT                TO UT-BEVOLREF (1:8)               
062800       ELSE                                                               
062900         MOVE SPACE                    TO UT-BEVOLREF                     
063000       END-IF                                                             
063100     END-IF                                                               
063200                                                                          
063300     IF IN31B-IDDC NOT = DCS-IDDC                                         
063400        MOVE IN31B-IDDC TO W-IDDC-B6                                      
063500        PERFORM IMS-GU-WDB601                                             
063600     END-IF                                                               
063700                                                                          
063800     MOVE DCS-IDLEGSEL                 TO UT-IDLEGSEL                     
063900     MOVE DCS-IDLANDX2                 TO UT-IDLANDX3-SEND                
064000                                                                          
064100     IF UT-IDLANDX3-SEND = SPACE                                          
064200       STRING 'SÄNDANDE DC SAKNAR ISO-LANDKOD' IN31B-IDDC                 
064300            DELIMITED BY SIZE INTO FELTEXT                                
064400       DISPLAY FELTEXT                                                    
064500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
064600     END-IF                                                               
064700                                                                          
064800*- MOTTAGANDE LAND SKALL BARA FYLLAS I VID RETUR.(FÖR INTRASTATEN)        
064900     MOVE IN31B-KDANMORS        TO OKOD-KDANMORS                          
065000     CALL W418OKOD USING OKOD-W418OKOD                                    
065100                                                                          
065200     IF OKOD-FL-RETILL = JA                                               
065300        IF IN31B-IDDC-RET NOT = RET-DCS-IDDC                              
065400           MOVE IN31B-IDDC-RET TO W-IDDC-B6-RET                           
065500           PERFORM IMS-GU-WDB601-RET                                      
065600        END-IF                                                            
065700        MOVE RET-DCS-IDLANDX2 TO UT-IDLANDX3-REC                          
065800       IF UT-IDLANDX3-REC = SPACE                                         
065900         STRING 'RETUR-DC SAKNAR ISO-LANDKOD' IN31B-IDDC-RET              
066000              DELIMITED BY SIZE INTO FELTEXT                              
066100         DISPLAY FELTEXT                                                  
066200         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
066300       END-IF                                                             
066400                                                                          
066500**- FIX FÖR BILL-IT: 2003-04-24 BEGÄRD AV BOSSE H                         
066600**- RETURER FRÅN ITALIEN DC=25 SKALL HA CDC'S KN-NR-SERIE.                
066700**- ALLA ANDRA KODER FRÅN DC=25 SKALL HA SDC-25 KN-NR-SERIE ,             
066800**- FIX FÖR BILL-IT: 2005-02-14 E'TRACKER 1752460                         
066900**- RETURER FRÅN ITALIEN KOD 72 TILL LDC=25 SKALL HA W418.(DC=25)         
067000**- FIX FÖR BILL-IT: 2006-03-23 NYA LDC FÖR ITALIEN.                      
067100**- RETURER FRÅN ITALIEN LDC-IT SKALL HA SAMMA REGLER SOM DC25.           
067200**- NYTT LDC-3D=LDC-25-REGLER: 2006-03-23 E'TRACKER 3107778               
067300**                                                                        
067400**- ENLIGT SUSSIE 060413 SKALL UNDANTAG GÖRAS FÖR ITALIEN                 
067500**- OBEROENDE VILKA DC SOM ÄR INBLANDADE                                  
067600**                                                                        
067700       MOVE 'W41X'                     TO UT-IDSYSTEM-SEND                
067800                                                                          
067900       IF IN31B-KDANMORS = '72' AND                                       
068000          DCS-IDLANDX2 = 'IT' AND RET-DCS-IDLANDX2 = 'IT'                 
068100          MOVE 'W418'                  TO UT-IDSYSTEM-SEND                
068200       END-IF                                                             
068300                                                                          
068400       IF DCS-IDLANDX2 = 'IT' AND RET-DCS-IDLANDX2 = 'IT'                 
068500          MOVE 'W418'                  TO UT-IDSYSTEM-SEND                
068600       END-IF                                                             
068700                                                                          
068800       IF IN31B-KDANMORS = '74'                                           
068900          MOVE SPACE                   TO UT-IDLANDX3-REC                 
069000          MOVE 'W418'                  TO UT-IDSYSTEM-SEND                
069100       END-IF                                                             
069200     ELSE                                                                 
069300       MOVE SPACE                      TO UT-IDLANDX3-REC                 
069400       MOVE 'W418'                     TO UT-IDSYSTEM-SEND                
069500     END-IF                                                               
069600                                                                          
069700*** SEPARATE ECOM CREDIT FROM NORMAL CREDIT                               
069800     IF DIST79-ECOM-PRICE                                                 
069900       MOVE 'ECOM'                     TO UT-IDSYSTEM-SEND                
070000     END-IF                                                               
070100***                                                                       
070200                                                                          
070300     MOVE IN31B-IDPARTNR               TO UT-IDPARTNR                     
070400     MOVE SPACE                        TO UT-IDEXCUST (1)                 
070500                                                                          
070600     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
070700     MOVE IN31B-IDDISTR        TO CIA-IDARTBET-IN                         
070800     CALL W009CIA USING           CIA-W009CIA                             
070900     MOVE CIA-IDARTBET-UT      TO UT-IDEXCUST (1)                         
071000                                                                          
071100     MOVE SPACE                        TO UT-IDEXCUST (2)                 
071200                                                                          
071300     IF IN31B-IDKUNDNR = ZERO                                             
071400       CONTINUE                                                           
071500     ELSE                                                                 
071600       MOVE 'VO'               TO CIA-IDARTPRE-IN                         
071700       MOVE IN31B-IDKUNDNR     TO CIA-IDARTBET-IN                         
071800       CALL W009CIA USING         CIA-W009CIA                             
071900       MOVE CIA-IDARTBET-UT    TO UT-IDEXCUST (2)                         
072000     END-IF                                                               
072100                                                                          
072200     MOVE SPACE                        TO UT-IDEXCUST (3)                 
072300     MOVE SPACE                        TO UT-IDOPTION (1)                 
072400                                                                          
072500     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
072600     MOVE IN31B-IDRADNR       TO CIA-IDARTBET-IN                          
072700     CALL W009CIA USING          CIA-W009CIA                              
072800     MOVE CIA-IDARTBET-UT     TO UT-IDOPTION (1)                          
072900                                                                          
073000     MOVE SPACE                        TO UT-IDOPTION (2)                 
073100     MOVE SPACE                        TO UT-IDOPTION (3)                 
073200     IF IN31B-PRLANDCO-RAD > ZERO                                         
073300       MOVE IN31B-PRLANDCO-RAD         TO W-PRLANDCO-RAD                  
073400       MOVE W-PRLANDCO-HEL             TO W-PRLANDCO-X-HEL                
073500       MOVE W-PRLANDCO-DEC             TO W-PRLANDCO-X-DEC                
073600       MOVE W-PRLANDCO-RAD-X           TO UT-IDOPTION (3)                 
073700     END-IF                                                               
073800                                                                          
073900     MOVE SPACE                        TO UT-IDOPTION (4)                 
074000     MOVE SPACE                        TO UT-IDOPTION (5)                 
074100     IF IN31B-KDANMORS = '74'                                             
074200       MOVE 'CREDIT         '          TO UT-IDOPTION (5)                 
074300     END-IF                                                               
074400*- ENLIGT BOSSE H 030526, SKALL MAN LÄGGA SPACE I IDAPPEND.               
074500     MOVE SPACE                        TO UT-IDAPPEND                     
074600     MOVE SPACE                        TO UT-IDARTNR-FINANCE              
074700                                                                          
074800     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
074900     MOVE IN31B-IDARTNR        TO CIA-IDARTBET-IN                         
075000     CALL W009CIA USING           CIA-W009CIA                             
075100     MOVE CIA-IDARTBET-UT      TO UT-IDARTNR-FINANCE                      
075200     MOVE IN31B-IDSTATNR       TO UT-IDSTATNR                             
075300                                                                          
075400     IF ANM-IDSYSTEM  = 'LYNK'                                            
075500*      TA FRAM LYNK-ARTNR                                                 
075600       MOVE IN31B-IDARTNR      TO W-IDARTNR-F5                            
075700       PERFORM IMS-GU-WDF502                                              
075800       IF SEGMENT-FINNS                                                   
075900         MOVE UT-IDARTNR-FINANCE TO WS-IDARTNR-LYNK                       
076000         MOVE XLEV-IDLEVART      TO WS-IDARTNR-L                          
076100         MOVE WS-IDARTNR-LYNK    TO UT-IDARTNR-FINANCE                    
076200       END-IF                                                             
076300     END-IF                                                               
076400*- VKART ÄR I GRAM OCH SKALL RÄKNAS OM I KG                               
076500     COMPUTE UT-VKARTNTO = (IN31B-VKART * IN31B-KVKREANT) / 1000          
076600     END-COMPUTE                                                          
076700                                                                          
076800     IF DIST79-DEALER-PRICE OR                                            
077000        DIST79-ECOM-PRICE                                                 
077100       MOVE IN31B-PRARTBTO-LOC         TO UT-PRARTBTO                     
077200                                          UT-PRARTNTO                     
077300     ELSE                                                                 
077400       MOVE IN31B-PRARTBTO             TO UT-PRARTBTO                     
077500                                          UT-PRARTNTO                     
077600     END-IF                                                               
077700     MOVE ZERO                         TO UT-REARTRAB                     
077800     MOVE IN31B-KVLEVANM               TO UT-KVBEART                      
077900     MOVE IN31B-KVKREANT               TO UT-KVLEVART                     
078000                                                                          
078100     IF DIST79-DEALER-PRICE  AND ( IN31B-BEART-VIPS NOT = SPACE )         
078200       MOVE IN31B-BEART-VIPS           TO UT-BEART                        
078300     ELSE                                                                 
078400       MOVE IN31B-IDARTNR              TO W-IDARTNR                       
078500       MOVE IN31B-IDSKYLT              TO W-IDSKYLT                       
078600                                                                          
078700       PERFORM IMS-GU-WDD301                                              
078800       IF SEGMENT-FINNS                                                   
078900          PERFORM IMS-GNP-WDD311                                          
079000       END-IF                                                             
079100                                                                          
079200       IF SEGMENT-FINNS                                                   
079300         IF TEXT-BEART = SPACE                                            
079400           MOVE 'PART DESCRIPTION MISSING '  TO UT-BEART                  
079500         ELSE                                                             
079600           MOVE TEXT-BEART                   TO UT-BEART                  
079700         END-IF                                                           
079800       ELSE                                                               
079900         MOVE 'PART DESCRIPTION MISSING '    TO UT-BEART                  
080000       END-IF                                                             
080100     END-IF                                                               
080200                                                                          
080300*- ENLIGT BILLIT SÅ SKA MAN ALLTID GE NEJ HÄR. DET SKALL INTE VARA        
080400*- SEPARAT KREDITNOTA FÖR SW. LIKT DAGENS LÄGE: 2002-10-18                
080500     MOVE 'N'                          TO UT-FLSOFT                       
080600                                                                          
080700     MOVE NEJ                          TO UT-FLSPECPR                     
080800     MOVE IN31B-FLFREE                 TO UT-FLFREE                       
080900     IF UT-FLFREE = 'Y'                                                   
081000       MOVE 'J'                        TO UT-FLFREE                       
081100     END-IF                                                               
081200     MOVE 'N'                          TO UT-FLPRIV                       
081300                                                                          
081400     MOVE IN31B-KDVAT                  TO UT-KDVAT                        
081500* START FIX INLAGD 031230                                                 
081600     IF UT-IDLANDX3-SEND = 'IT' AND                                       
081700        UT-KDVAT         = '70'                                           
081800       MOVE 'ID'                       TO UT-KDVAT                        
081900     END-IF                                                               
082000* SLUT  FIX INLAGD 031230                                                 
082200     IF XDC-NON-VCC-OWNED                                                 
082300       MOVE DCS-KDVALISO               TO UT-KDVALISO                     
082400     END-IF                                                               
082500     MOVE 'NOW'                        TO UT-KDINVFRQ                     
082600                                                                          
082700*    IF IN31B-KDANMORS = '74'                                             
082800*      MOVE 'CLA'                      TO UT-KDFINDOC                     
082900*    ELSE                                                                 
083000*      MOVE 'CR'                       TO UT-KDFINDOC                     
083100*    END-IF                                                               
083200     MOVE IN31B-IDPARTNR TO WS-IDPARTNR                                   
083300     IF WS-INT NUMERIC                                                    
083400       MOVE 'CR'                       TO UT-KDFINDOC                     
083500     ELSE                                                                 
083600       MOVE 'INT'                      TO UT-KDFINDOC                     
083700     END-IF                                                               
083800     MOVE IN31B-IDRAPPNR               TO UT-IDBREAK (1)                  
083900     MOVE IN31B-IDDC                   TO WS-IDBREAK(1:2)                 
084000     MOVE IN31B-IDKUNDNR               TO HELP-IDKUNDNR                   
084100     MOVE HELP-IDKUNDNR                TO WS-IDBREAK(3:6)                 
084200     MOVE WS-IDBREAK                   TO UT-IDBREAK (2)                  
084300     MOVE IN31B-IDARTNR                TO HELP-IDARTNR                    
084400     MOVE HELP-IDARTNR                 TO UT-IDSEQ (1)                    
084500                                          UT-IDSEQ (2)                    
084600     MOVE SPACE                        TO UT-IDSEQ (3)                    
084700     MOVE IN31B-KDARTURS               TO UT-KDARTURS                     
084800     MOVE IN31B-KDANMORS               TO UT-KDANMORS                     
084900                                                                          
085000     MOVE SPACE                        TO UT-IDLEVNR                      
085100     IF IN31B-KDANMORS = '74'                                             
085200       MOVE ZERO                       TO UT-IDFAKREF                     
085300       MOVE ZERO                       TO UT-DAFAKREF                     
085400**** WILL USE IDLEVNR GET THE RIGHT CURRENCY RATE IN BILL-IT              
085500       MOVE '0000'                     TO UT-IDLEVNR                      
085600     ELSE                                                                 
085700       MOVE IN31B-IDFAKREF             TO UT-IDFAKREF                     
085800       MOVE IN31B-DAFAKREF             TO UT-DAFAKREF                     
085900**** WILL USE IDLEVNR GET THE RIGHT CURRENCY RATE IN BILL-IT              
086000       MOVE IN31B-DAFAKREF(3:4)        TO UT-IDLEVNR                      
086100     END-IF                                                               
086200                                                                          
086300     MOVE IN31B-IDDC                   TO UT-IDDC                         
086400     MOVE LEV-KDFRAKT                  TO UT-KDFRAKT                      
086500                                                                          
086600*-ENLIGT BOSSE H SKA MAN ANGE LEV.VILLKOR FÖR RETURER.(INTRASTAT)         
086700     IF OKOD-FL-RETILL = JA                                               
086800       IF IN31B-IDDC-RET = '11'                                           
086900         MOVE 'CIP             GOTHENBURG         '                       
087000                                       TO UT-BELEVVIL                     
087100       ELSE                                                               
087200         MOVE 'CIP             (INCOTERMS 2010)   '                       
087300                                       TO UT-BELEVVIL                     
087400       END-IF                                                             
087500     ELSE                                                                 
087600       MOVE SPACE                      TO UT-BELEVVIL                     
087700     END-IF                                                               
087800                                                                          
087900     MOVE SPACE                        TO UT-IDACCNT (1)                  
088000     MOVE SPACE                        TO UT-IDACCNT (2)                  
088100     MOVE SPACE                        TO UT-IDACCNT (3)                  
088200     MOVE SPACE                        TO UT-IDACCNT (4)                  
088300     IF IN31B-KDANMORS = '74'                                             
088400       IF LEV-IDKONTO NOT = ZERO                                          
088500         MOVE LEV-IDKONTO              TO HELP-IDKONTO                    
088600         MOVE HELP-IDKONTO             TO WS-REDUIN                       
088700                                                                          
088800         INSPECT WS-REDUIN REPLACING LEADING ZERO BY SPACE                
088900         CALL W009REDU USING WS-REDUIN WS-REDUUT                          
089000         MOVE WS-REDUUT                TO UT-IDACCNT (1)                  
089100       END-IF                                                             
089200       IF LEV-IDANALYS NOT = SPACE                                        
089300         MOVE LEV-IDANALYS             TO UT-IDACCNT (2)                  
089400       END-IF                                                             
089500       IF LEV-IDKST NOT = SPACE                                           
089600         MOVE LEV-IDKST                TO UT-IDACCNT (3)                  
089700       END-IF                                                             
089800     END-IF                                                               
089900                                                                          
090000     MOVE 'W418'                       TO UT-IDSYSTEM-REC                 
090100     MOVE IN31B-BEANST                 TO UT-BEANST                       
090200     MOVE IN31B-IDUSER-ADM             TO UT-IDUSER                       
090300     MOVE SPACE                        TO UT-BETEXT                       
090400                                          UT-BETEXT-CRE                   
090500     MOVE SPACE                        TO WS-GODK-TABELL                  
090600                                                                          
090700     IF IN31B-KDANMORS = '74'                                             
090800       CONTINUE                                                           
090900     ELSE                                                                 
091000       IF WDR501-FINNS                                                    
091100         MOVE IN31B-IDDC TO W-IDDC-4104                                   
091200                                                                          
091300         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
091400              LEV-KDANMORS = '97'                                         
091500           MOVE 'RP'     TO W-KDKRENOT-4104                               
091600         ELSE                                                             
091700           MOVE 'CN'     TO W-KDKRENOT-4104                               
091800         END-IF                                                           
091900                                                                          
092000         PERFORM IMS-GNP-WDGX4104                                         
092100         IF SEGMENT-FINNS                                                 
092200           MOVE +1           TO GODK-IX                                   
092300           PERFORM IMS-GNP-WDGX4106                                       
092400           PERFORM UNTIL SEGMENT-SAKNAS OR GODK-IX > MAX-GODK-IX          
092500            MOVE 4106-IDUSER-GODK  TO WS-SPAR-IDUSER-GODK(GODK-IX)        
092600            MOVE 4106-BEANST-GODK  TO WS-SPAR-BEANST-GODK(GODK-IX)        
092700            MOVE 20                TO WS-SPAR-TISEKEL    (GODK-IX)        
092800            MOVE 4106-TIUPPDAT     TO WS-SPAR-TIUPPDAT   (GODK-IX)        
092900            ADD +1            TO GODK-IX                                  
093000            PERFORM IMS-GNP-WDGX4106                                      
093100           END-PERFORM                                                    
093200         END-IF                                                           
093300       END-IF                                                             
093400       MOVE WS-GODK-GRP (1)              TO UT-BETEXT (1:41)              
093500       MOVE WS-GODK-GRP (2)              TO UT-BETEXT (42:41)             
093600       MOVE WS-GODK-GRP (3)              TO UT-BETEXT (83:41)             
093700       MOVE WS-GODK-GRP (4)              TO UT-BETEXT-CRE (1:41)          
093800       MOVE WS-GODK-GRP (5)              TO UT-BETEXT-CRE (42:41)         
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200 BCB-SKAPA-RADER-BILLIT SECTION.                                          
094300                                                                          
094400     MOVE 'VCCS'                       TO UT-IDLEGSEL                     
094500     IF IN31B-KDANMORS = '74'                                             
094600       MOVE SPACE                      TO UT-IDBUNDLE                     
094700       MOVE WS-IDBUNDLE                TO UT-IDBUNDLE (1:8)               
094800     ELSE                                                                 
094900       MOVE WS-IDBUNDLE                TO UT-IDBUNDLE (1:8)               
095000       MOVE ' W418AI'                  TO UT-IDBUNDLE (9:7)               
095100     END-IF                                                               
095200     MOVE SPACE                        TO UT-IDREF                        
095300                                                                          
095400     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
095500     MOVE IN31B-IDRAPPNR       TO CIA-IDARTBET-IN                         
095600     CALL W009CIA USING           CIA-W009CIA                             
095700     MOVE CIA-IDARTBET-UT      TO UT-IDREF                                
095800                                                                          
095900     MOVE IN31B-DALEVANM               TO UT-DAREFDAT                     
096000                                                                          
096100     ADD +1                            TO W-KVPOST-IN                     
096200     MOVE W-KVPOST-IN                  TO UT-IDREFRAD                     
096300                                                                          
096400     IF IN31B-KDANMORS = '97'                                             
096500       MOVE 'UNPACKING '               TO UT-BEVOLREF                     
096600     ELSE                                                                 
096700       IF IN31B-KDANMORS = '74'                                           
096800         MOVE SPACE                    TO UT-BEVOLREF                     
096900         MOVE IN31B-IDFAKREF           TO HELP-IDFAKREF                   
097000         MOVE HELP-IDFAKREF            TO WS-REDUIN                       
097100         INSPECT WS-REDUIN REPLACING LEADING ZERO BY SPACE                
097200         CALL W009REDU USING WS-REDUIN WS-REDUUT                          
097300         MOVE WS-REDUUT                TO UT-BEVOLREF (1:8)               
097400       ELSE                                                               
097500         MOVE SPACE                    TO UT-BEVOLREF                     
097600       END-IF                                                             
097700     END-IF                                                               
097800                                                                          
097900     IF IN31B-IDDC NOT = DCS-IDDC                                         
098000        MOVE IN31B-IDDC TO W-IDDC-B6                                      
098100        PERFORM IMS-GU-WDB601                                             
098200     END-IF                                                               
098300     MOVE DCS-IDLANDX2 TO UT-IDLANDX3-SEND                                
098400     IF UT-IDLANDX3-SEND = SPACE                                          
098500       STRING 'SÄNDANDE DC SAKNAR ISO-LANDKOD' IN31B-IDDC                 
098600            DELIMITED BY SIZE INTO FELTEXT                                
098700       DISPLAY FELTEXT                                                    
098800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
098900     END-IF                                                               
099000                                                                          
099100*- MOTTAGANDE LAND SKALL BARA FYLLAS I VID RETUR.(FÖR INTRASTATEN)        
099200     MOVE IN31B-KDANMORS        TO OKOD-KDANMORS                          
099300     CALL W418OKOD USING OKOD-W418OKOD                                    
099400                                                                          
099500     IF OKOD-FL-RETILL = JA                                               
099600        IF IN31B-IDDC-RET NOT = RET-DCS-IDDC                              
099700           MOVE IN31B-IDDC-RET TO W-IDDC-B6-RET                           
099800           PERFORM IMS-GU-WDB601-RET                                      
099900        END-IF                                                            
100000        MOVE RET-DCS-IDLANDX2 TO UT-IDLANDX3-REC                          
100100       IF UT-IDLANDX3-REC = SPACE                                         
100200         STRING 'RETUR-DC SAKNAR ISO-LANDKOD' IN31B-IDDC-RET              
100300              DELIMITED BY SIZE INTO FELTEXT                              
100400         DISPLAY FELTEXT                                                  
100500         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
100600       END-IF                                                             
100700                                                                          
100800**- FIX FÖR BILL-IT: 2003-04-24 BEGÄRD AV BOSSE H                         
100900**- RETURER FRÅN ITALIEN DC=25 SKALL HA CDC'S KN-NR-SERIE.                
101000**- ALLA ANDRA KODER FRÅN DC=25 SKALL HA SDC-25 KN-NR-SERIE ,             
101100**- FIX FÖR BILL-IT: 2005-02-14 E'TRACKER 1752460                         
101200**- RETURER FRÅN ITALIEN KOD 72 TILL LDC=25 SKALL HA W418.(DC=25)         
101300**- FIX FÖR BILL-IT: 2006-03-23 NYA LDC FÖR ITALIEN.                      
101400**- RETURER FRÅN ITALIEN LDC-IT SKALL HA SAMMA REGLER SOM DC25.           
101500**- NYTT LDC-3D=LDC-25-REGLER: 2006-03-23 E'TRACKER 3107778               
101600**                                                                        
101700**- ENLIGT SUSSIE 060413 SKALL UNDANTAG GÖRAS FÖR ITALIEN                 
101800**- OBEROENDE VILKA DC SOM ÄR INBLANDADE                                  
101900**                                                                        
102000       MOVE 'W41X'                     TO UT-IDSYSTEM-SEND                
102100                                                                          
102200       IF IN31B-KDANMORS = '72' AND                                       
102300          DCS-IDLANDX2 = 'IT' AND RET-DCS-IDLANDX2 = 'IT'                 
102400          MOVE 'W418'                  TO UT-IDSYSTEM-SEND                
102500       END-IF                                                             
102600                                                                          
102700       IF DCS-IDLANDX2 = 'IT' AND RET-DCS-IDLANDX2 = 'IT'                 
102800          MOVE 'W418'                  TO UT-IDSYSTEM-SEND                
102900       END-IF                                                             
103000                                                                          
103100       IF IN31B-KDANMORS = '74'                                           
103200          MOVE SPACE                   TO UT-IDLANDX3-REC                 
103300          MOVE 'W418'                  TO UT-IDSYSTEM-SEND                
103400       END-IF                                                             
103500     ELSE                                                                 
103600       MOVE SPACE                      TO UT-IDLANDX3-REC                 
103700       MOVE 'W418'                     TO UT-IDSYSTEM-SEND                
103800     END-IF                                                               
103900                                                                          
104000*** SEPARATE ECOM CREDIT FROM NORMAL CREDIT                               
104100     IF DIST79-ECOM-PRICE                                                 
104200       MOVE 'ECOM'                     TO UT-IDSYSTEM-SEND                
104300     END-IF                                                               
104400***                                                                       
104500                                                                          
104600     MOVE IN31B-IDPARTNR               TO UT-IDPARTNR                     
104700     MOVE SPACE                        TO UT-IDEXCUST (1)                 
104800                                                                          
104900     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
105000     MOVE IN31B-IDDISTR        TO CIA-IDARTBET-IN                         
105100     CALL W009CIA USING           CIA-W009CIA                             
105200     MOVE CIA-IDARTBET-UT      TO UT-IDEXCUST (1)                         
105300                                                                          
105400     MOVE SPACE                        TO UT-IDEXCUST (2)                 
105500                                                                          
105600     IF IN31B-IDKUNDNR = ZERO                                             
105700       CONTINUE                                                           
105800     ELSE                                                                 
105900       MOVE 'VO'               TO CIA-IDARTPRE-IN                         
106000       MOVE IN31B-IDKUNDNR     TO CIA-IDARTBET-IN                         
106100       CALL W009CIA USING         CIA-W009CIA                             
106200       MOVE CIA-IDARTBET-UT    TO UT-IDEXCUST (2)                         
106300     END-IF                                                               
106400                                                                          
106500     MOVE SPACE                        TO UT-IDEXCUST (3)                 
106600     MOVE SPACE                        TO UT-IDOPTION (1)                 
106700                                                                          
106800     MOVE 'VO'                TO CIA-IDARTPRE-IN                          
106900     MOVE IN31B-IDRADNR       TO CIA-IDARTBET-IN                          
107000     CALL W009CIA USING          CIA-W009CIA                              
107100     MOVE CIA-IDARTBET-UT     TO UT-IDOPTION (1)                          
107200                                                                          
107300     MOVE SPACE                        TO UT-IDOPTION (2)                 
107400     MOVE SPACE                        TO UT-IDOPTION (3)                 
107500     IF IN31B-PRLANDCO-RAD > ZERO                                         
107600       MOVE IN31B-PRLANDCO-RAD         TO W-PRLANDCO-RAD                  
107700       MOVE W-PRLANDCO-HEL             TO W-PRLANDCO-X-HEL                
107800       MOVE W-PRLANDCO-DEC             TO W-PRLANDCO-X-DEC                
107900       MOVE W-PRLANDCO-RAD-X           TO UT-IDOPTION (3)                 
108000     END-IF                                                               
108100                                                                          
108200     MOVE SPACE                        TO UT-IDOPTION (4)                 
108300     MOVE SPACE                        TO UT-IDOPTION (5)                 
108400     IF IN31B-KDANMORS = '74'                                             
108500       MOVE 'CREDIT         '          TO UT-IDOPTION (5)                 
108600     END-IF                                                               
108700*- ENLIGT BOSSE H 030526, SKALL MAN LÄGGA SPACE I IDAPPEND.               
108800     MOVE SPACE                        TO UT-IDAPPEND                     
108900     MOVE SPACE                        TO UT-IDARTNR-FINANCE              
109000                                                                          
109100     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
109200     MOVE IN31B-IDARTNR        TO CIA-IDARTBET-IN                         
109300     CALL W009CIA USING           CIA-W009CIA                             
109400     MOVE CIA-IDARTBET-UT      TO UT-IDARTNR-FINANCE                      
109500     MOVE IN31B-IDSTATNR       TO UT-IDSTATNR                             
109600                                                                          
109700     IF ANM-IDSYSTEM  = 'LYNK'                                            
109800*      TA FRAM LYNK-ARTNR                                                 
109900       MOVE IN31B-IDARTNR      TO W-IDARTNR-F5                            
110000       PERFORM IMS-GU-WDF502                                              
110100       IF SEGMENT-FINNS                                                   
110200         MOVE UT-IDARTNR-FINANCE TO WS-IDARTNR-LYNK                       
110300         MOVE XLEV-IDLEVART      TO WS-IDARTNR-L                          
110400         MOVE WS-IDARTNR-LYNK    TO UT-IDARTNR-FINANCE                    
110500       END-IF                                                             
110600     END-IF                                                               
110700*- VKART ÄR I GRAM OCH SKALL RÄKNAS OM I KG                               
110800     COMPUTE UT-VKARTNTO = (IN31B-VKART * IN31B-KVKREANT) / 1000          
110900     END-COMPUTE                                                          
111000                                                                          
111100     IF DIST79-DEALER-PRICE OR                                            
111300        DIST79-ECOM-PRICE                                                 
111400       MOVE IN31B-PRARTBTO-LOC         TO UT-PRARTBTO                     
111500                                          UT-PRARTNTO                     
111600     ELSE                                                                 
111700       MOVE IN31B-PRARTBTO             TO UT-PRARTBTO                     
111800                                          UT-PRARTNTO                     
111900     END-IF                                                               
112000     MOVE ZERO                         TO UT-REARTRAB                     
112100     MOVE IN31B-KVLEVANM               TO UT-KVBEART                      
112200     MOVE IN31B-KVKREANT               TO UT-KVLEVART                     
112300                                                                          
112400     IF DIST79-DEALER-PRICE  AND ( IN31B-BEART-VIPS NOT = SPACE )         
112500       MOVE IN31B-BEART-VIPS           TO UT-BEART                        
112600     ELSE                                                                 
112700       MOVE IN31B-IDARTNR              TO W-IDARTNR                       
112800       MOVE IN31B-IDSKYLT              TO W-IDSKYLT                       
112900                                                                          
113000       PERFORM IMS-GU-WDD301                                              
113100       IF SEGMENT-FINNS                                                   
113200          PERFORM IMS-GNP-WDD311                                          
113300       END-IF                                                             
113400                                                                          
113500       IF SEGMENT-FINNS                                                   
113600         IF TEXT-BEART = SPACE                                            
113700           MOVE 'PART DESCRIPTION MISSING '  TO UT-BEART                  
113800         ELSE                                                             
113900           MOVE TEXT-BEART                   TO UT-BEART                  
114000         END-IF                                                           
114100       ELSE                                                               
114200         MOVE 'PART DESCRIPTION MISSING '    TO UT-BEART                  
114300       END-IF                                                             
114400     END-IF                                                               
114500                                                                          
114600*- ENLIGT BILLIT SÅ SKA MAN ALLTID GE NEJ HÄR. DET SKALL INTE VARA        
114700*- SEPARAT KREDITNOTA FÖR SW. LIKT DAGENS LÄGE: 2002-10-18                
114800     MOVE 'N'                          TO UT-FLSOFT                       
114900                                                                          
115000     MOVE NEJ                          TO UT-FLSPECPR                     
115100     MOVE IN31B-FLFREE                 TO UT-FLFREE                       
115200     IF UT-FLFREE = 'Y'                                                   
115300       MOVE 'J'                        TO UT-FLFREE                       
115400     END-IF                                                               
115500     MOVE 'N'                          TO UT-FLPRIV                       
115600                                                                          
115700     MOVE IN31B-KDVAT                  TO UT-KDVAT                        
115800* START FIX INLAGD 031230                                                 
115900     IF UT-IDLANDX3-SEND = 'IT' AND                                       
116000        UT-KDVAT         = '70'                                           
116100       MOVE 'ID'                       TO UT-KDVAT                        
116200     END-IF                                                               
116300* SLUT  FIX INLAGD 031230                                                 
116400     IF DIST79-DEALER-PRICE OR                                            
116600        DIST79-ECOM-PRICE                                                 
116700       MOVE IN31B-KDVALISO             TO UT-KDVALISO                     
116800     ELSE                                                                 
116900       MOVE 'SEK'                      TO UT-KDVALISO                     
117000     END-IF                                                               
117100     MOVE 'NOW'                        TO UT-KDINVFRQ                     
117200                                                                          
117300     IF IN31B-KDANMORS = '74'                                             
117400       MOVE 'CLA'                      TO UT-KDFINDOC                     
117500     ELSE                                                                 
117600       MOVE 'CR'                       TO UT-KDFINDOC                     
117700     END-IF                                                               
117800                                                                          
117900     MOVE IN31B-IDRAPPNR               TO UT-IDBREAK (1)                  
118000     MOVE IN31B-IDDC                   TO WS-IDBREAK(1:2)                 
118100     MOVE IN31B-IDKUNDNR               TO HELP-IDKUNDNR                   
118200     MOVE HELP-IDKUNDNR                TO WS-IDBREAK(3:6)                 
118300     MOVE WS-IDBREAK                   TO UT-IDBREAK (2)                  
118400     MOVE IN31B-IDARTNR                TO HELP-IDARTNR                    
118500     MOVE HELP-IDARTNR                 TO UT-IDSEQ (1)                    
118600                                          UT-IDSEQ (2)                    
118700     MOVE SPACE                        TO UT-IDSEQ (3)                    
118800     MOVE IN31B-KDARTURS               TO UT-KDARTURS                     
118900     MOVE IN31B-KDANMORS               TO UT-KDANMORS                     
119000                                                                          
119100***** EXTENDED WARRANTY SHOULD BE ON OWN INVOICE                          
119200     MOVE IN31B-IDARTNR      TO W-IDARTNR                                 
119300     PERFORM IMS-GU-WDK601                                                
119400     IF SEGMENT-FINNS                                                     
119500       MOVE ART-IDFKNGRP     TO TEST-IDFKNGRP                             
119600       IF FKNGRP-EXT-WARRANTY                                             
119700         MOVE IN31B-IDRAPPNR           TO WS-IDBREAK                      
119800         MOVE 'S'                      TO WS-IDBREAK(8:1)                 
119900         MOVE WS-IDBREAK               TO UT-IDBREAK (1)                  
120000       END-IF                                                             
120100     END-IF                                                               
120200                                                                          
120300                                                                          
120400     MOVE SPACE                        TO UT-IDLEVNR                      
120500     IF IN31B-KDANMORS = '74'                                             
120600       MOVE ZERO                       TO UT-IDFAKREF                     
120700       MOVE ZERO                       TO UT-DAFAKREF                     
120800**** WILL USE IDLEVNR GET THE RIGHT CURRENCY RATE IN BILL-IT              
120900       MOVE '0000'                     TO UT-IDLEVNR                      
121000     ELSE                                                                 
121100       MOVE IN31B-IDFAKREF             TO UT-IDFAKREF                     
121200       MOVE IN31B-DAFAKREF             TO UT-DAFAKREF                     
121300**** WILL USE IDLEVNR GET THE RIGHT CURRENCY RATE IN BILL-IT              
121400       MOVE IN31B-DAFAKREF(3:4)        TO UT-IDLEVNR                      
121500     END-IF                                                               
121600                                                                          
121700     MOVE IN31B-IDDC                   TO UT-IDDC                         
121800     MOVE LEV-KDFRAKT                  TO UT-KDFRAKT                      
121900                                                                          
122000*-ENLIGT BOSSE H SKA MAN ANGE LEV.VILLKOR FÖR RETURER.(INTRASTAT)         
122100     IF OKOD-FL-RETILL = JA                                               
122200       IF IN31B-IDDC-RET = '11'                                           
122300         MOVE 'CIP             GOTHENBURG         '                       
122400                                       TO UT-BELEVVIL                     
122500       ELSE                                                               
122600         MOVE 'CIP             (INCOTERMS 2010)   '                       
122700                                       TO UT-BELEVVIL                     
122800       END-IF                                                             
122900     ELSE                                                                 
123000       MOVE SPACE                      TO UT-BELEVVIL                     
123100     END-IF                                                               
123200                                                                          
123300     MOVE SPACE                        TO UT-IDACCNT (1)                  
123400     MOVE SPACE                        TO UT-IDACCNT (2)                  
123500     MOVE SPACE                        TO UT-IDACCNT (3)                  
123600     MOVE SPACE                        TO UT-IDACCNT (4)                  
123700     IF IN31B-KDANMORS = '74'                                             
123800       IF LEV-IDKONTO NOT = ZERO                                          
123900         MOVE LEV-IDKONTO              TO HELP-IDKONTO                    
124000         MOVE HELP-IDKONTO             TO WS-REDUIN                       
124100                                                                          
124200         INSPECT WS-REDUIN REPLACING LEADING ZERO BY SPACE                
124300         CALL W009REDU USING WS-REDUIN WS-REDUUT                          
124400         MOVE WS-REDUUT                TO UT-IDACCNT (1)                  
124500       END-IF                                                             
124600       IF LEV-IDANALYS NOT = SPACE                                        
124700         MOVE LEV-IDANALYS             TO UT-IDACCNT (2)                  
124800       END-IF                                                             
124900       IF LEV-IDKST NOT = SPACE                                           
125000         MOVE LEV-IDKST                TO UT-IDACCNT (3)                  
125100       END-IF                                                             
125200     END-IF                                                               
125300                                                                          
125400     MOVE 'W418'                       TO UT-IDSYSTEM-REC                 
125500     MOVE IN31B-BEANST                 TO UT-BEANST                       
125600     MOVE IN31B-IDUSER-ADM             TO UT-IDUSER                       
125700     MOVE SPACE                        TO UT-BETEXT                       
125800                                          UT-BETEXT-CRE                   
125900     MOVE SPACE                        TO WS-GODK-TABELL                  
126000                                                                          
126100     IF IN31B-KDANMORS = '74'                                             
126200       CONTINUE                                                           
126300     ELSE                                                                 
126400       IF WDR501-FINNS                                                    
126500         MOVE IN31B-IDDC TO W-IDDC-4104                                   
126600                                                                          
126700         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
126800              LEV-KDANMORS = '97'                                         
126900           MOVE 'RP'     TO W-KDKRENOT-4104                               
127000         ELSE                                                             
127100           MOVE 'CN'     TO W-KDKRENOT-4104                               
127200         END-IF                                                           
127300                                                                          
127400         PERFORM IMS-GNP-WDGX4104                                         
127500         IF SEGMENT-FINNS                                                 
127600           MOVE +1           TO GODK-IX                                   
127700           PERFORM IMS-GNP-WDGX4106                                       
127800           PERFORM UNTIL SEGMENT-SAKNAS OR GODK-IX > MAX-GODK-IX          
127900            MOVE 4106-IDUSER-GODK  TO WS-SPAR-IDUSER-GODK(GODK-IX)        
128000            MOVE 4106-BEANST-GODK  TO WS-SPAR-BEANST-GODK(GODK-IX)        
128100            MOVE 20                TO WS-SPAR-TISEKEL    (GODK-IX)        
128200            MOVE 4106-TIUPPDAT     TO WS-SPAR-TIUPPDAT   (GODK-IX)        
128300            ADD +1            TO GODK-IX                                  
128400            PERFORM IMS-GNP-WDGX4106                                      
128500           END-PERFORM                                                    
128600         END-IF                                                           
128700       END-IF                                                             
128800       MOVE WS-GODK-GRP (1)              TO UT-BETEXT (1:41)              
128900       MOVE WS-GODK-GRP (2)              TO UT-BETEXT (42:41)             
129000       MOVE WS-GODK-GRP (3)              TO UT-BETEXT (83:41)             
129100       MOVE WS-GODK-GRP (4)              TO UT-BETEXT-CRE (1:41)          
129200       MOVE WS-GODK-GRP (5)              TO UT-BETEXT-CRE (42:41)         
129300     END-IF                                                               
129400     .                                                                    
129500     EJECT                                                                
129600                                                                          
129700 BD-CREATE-EXTRA-CREDIT-ITALY SECTION.                                    
129800**** ADDITIONAL CREDIT NOTE SHOULD ONLY BE CREATED WHEN THE GOODS         
129900**** HAVE BEEN SENT TO THE DEALER FROM A IT WAREHOUSE AND                 
130000**** THE RETURN IS SENT TO CDC, THIS IS DONE ONLY FOR SOME                
130100**** KDANMORS CODES                                                       
130200     IF  UT-IDLANDX3-REC  = 'SE'                                          
130300     AND UT-IDLANDX3-SEND = 'IT'                                          
130400     AND UT-IDEXCUST(1)   = '1822'                                        
130500     AND UT-KDFINDOC      = 'CR'                                          
130600       IF UT-KDANMORS     = '20'                                          
130700       OR UT-KDANMORS     = '22'                                          
130800       OR UT-KDANMORS     = '42'                                          
130900       OR UT-KDANMORS     = '52'                                          
131000       OR UT-KDANMORS     = '62'                                          
131100       OR UT-KDANMORS     = '75'                                          
131200       OR UT-KDANMORS     = '82'                                          
131300       OR UT-KDANMORS     = '92'                                          
131400**** HERE WE CHANGE THE DATA THAT NEEDS TO BE REPLACED FOR THE            
131500**** FIRST CREDIT NOTE                                                    
131600         MOVE 'W418'                   TO UT-IDSYSTEM-SEND                
131700         PERFORM S03-SEND-PUT                                             
131800**** THEN WE CREATE THE DATA FOR THE SECOND CREDITNOTE                    
131900         ADD +1                        TO W-KVPOST-IN                     
132000         MOVE W-KVPOST-IN              TO UT-IDREFRAD                     
132100                                                                          
132200****     HARDCODED PARMANUMBER HERE, IF THERE COMMING                     
132300****     MORE THAT WANTS THIS SOLUTION, CHANGE TO READ WDB1               
132400****     WITH DISTR AND FIRST CUSTOMER FOUND                              
132500         MOVE 'IT99999'                TO UT-IDPARTNR                     
132600                                                                          
132700         MOVE SPACE                    TO UT-IDEXCUST (1)                 
132800         MOVE 'VO'               TO CIA-IDARTPRE-IN                       
132900         MOVE DCS-IDDISTR-REFILL TO CIA-IDARTBET-IN                       
133000         CALL W009CIA USING       CIA-W009CIA                             
133100         MOVE CIA-IDARTBET-UT    TO UT-IDEXCUST (1)                       
133200                                                                          
133300         MOVE SPACE              TO UT-IDEXCUST (2)                       
133400         MOVE 'VO'               TO CIA-IDARTPRE-IN                       
133500         MOVE '10'               TO CIA-IDARTBET-IN                       
133600         CALL W009CIA USING       CIA-W009CIA                             
133700         MOVE CIA-IDARTBET-UT    TO UT-IDEXCUST (2)                       
133800                                                                          
133900         MOVE IN31B-IDARTNR      TO W-IDARTNR                             
134000         PERFORM IMS-GU-WDK601                                            
134100         IF SEGMENT-FINNS                                                 
134200           PERFORM IMS-GNP-WDK611                                         
134300           IF SEGMENT-FINNS                                               
134400             MOVE CLAG-PRARTSTD        TO UT-PRARTBTO                     
134500                                          UT-PRARTNTO                     
134600           END-IF                                                         
134700         END-IF                                                           
134800                                                                          
134900         MOVE 'SEK'                    TO UT-KDVALISO                     
135000         MOVE '0000'                   TO UT-IDLEVNR                      
135100                                                                          
135200         MOVE IN31B-IDDC-RET           TO UT-IDDC                         
135300                                                                          
135400         MOVE 'W41Z'                   TO UT-IDSYSTEM-REC                 
135500         MOVE 'INT2'                   TO UT-KDFINDOC                     
135600       END-IF                                                             
135700     END-IF                                                               
135800     .                                                                    
135900     EJECT                                                                
136000                                                                          
136100 C-START-PGM-WF020400 SECTION.                                            
136200                                                                          
136300     PERFORM S06-SEND-TO-WF0204-OPEN                                      
136400     PERFORM S07-SEND-TO-WF0204-PUT                                       
136500     PERFORM S08-SEND-TO-WF0204-CLOSE                                     
136600     .                                                                    
136700     EJECT                                                                
136800 Z-FINIT SECTION.                                                         
136900                                                                          
137000                                                                          
137100     CLOSE W418AI                                                         
137200     SKIP2                                                                
137300     MOVE 'S' TO POSTSUM-OPKOD                                            
137400     CALL POSTSUM USING POSTSUM-PARM                                      
137500     .                                                                    
137600     EJECT                                                                
137700 S01-LAES-W418AI  SECTION.                                                
137800     SKIP2                                                                
137900     READ W418AI INTO IN-AREA                                             
138000     AT END                                                               
138100        SET END-OF-W418AI TO TRUE                                         
138200                                                                          
138300     NOT AT END                                                           
138400        MOVE 'W418AI' TO POSTSUM-FDNAMN                                   
138500        MOVE 'W41831D1' TO POSTSUM-DDNAMN2                                
138600        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
138700        CALL POSTSUM USING POSTSUM-PARM                                   
138800     END-READ                                                             
138900     .                                                                    
139000     EJECT                                                                
139100 S02-SEND-OPEN SECTION.                                                   
139200     MOVE 'OPEN' TO SEND-KDFUNC                                           
139300     MOVE 'CARPARTS.BILLIT.RECEIVE3' TO SEND-ADDISPABS                    
139400                                                                          
139500     CALL WZ01SEND USING           SEND-CONTROL-AREA                      
139600                                   SEND-OPEN-AREA                         
139700********              ...FELHANTERING...                                  
139800     IF SEND-KDRC > 0                                                     
139900      MOVE SEND-KDRC TO KDRC-DISPLAY                                      
140000      STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                      
140100      DELIMITED BY SIZE INTO FELTEXT                                      
140200      DISPLAY FELTEXT                                                     
140300      CALL ABEND USING RKOD-ABEND-MED-DUMP                                
140400     END-IF                                                               
140500     .                                                                    
140600     EJECT                                                                
140700 S03-SEND-PUT SECTION.                                                    
140800     MOVE 'PUT'                           TO SEND-KDFUNC                  
140900     MOVE LENGTH OF UT-AREA               TO SEND-KVDLEN                  
141000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
141100                         SEND-KVDLEN                                      
141200                         UT-AREA                                          
141300**FELHANTERING...                                                         
141400     IF SEND-KDRC > 1                                                     
141500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
141600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
141700       DELIMITED BY SIZE INTO FELTEXT                                     
141800       DISPLAY FELTEXT                                                    
141900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
142000     END-IF                                                               
142100     .                                                                    
142200     EJECT                                                                
142300 S04-SEND-CLOSE SECTION.                                                  
142400                                                                          
142500     MOVE 'CLOSE' TO SEND-KDFUNC                                          
142600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
142700**FELHANTERING...                                                         
142800     IF SEND-KDRC > 0                                                     
142900      MOVE SEND-KDRC TO KDRC-DISPLAY                                      
143000      STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                     
143100       DELIMITED BY SIZE INTO FELTEXT                                     
143200       DISPLAY FELTEXT                                                    
143300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
143400     END-IF                                                               
143500                                                                          
143600     ADD +1                           TO WS-ANTAL-SEND                    
143700     .                                                                    
143800     EJECT                                                                
143900 S06-SEND-TO-WF0204-OPEN SECTION.                                         
144000     MOVE 'OPEN' TO SEND-KDFUNC                                           
144100     MOVE 'CARPARTS.BILLIT.STARTPROCESSING' TO SEND-ADDISPABS             
144200                                                                          
144300     CALL WZ01SEND USING           SEND-CONTROL-AREA                      
144400                                   SEND-OPEN-AREA                         
144500********              ...FELHANTERING...                                  
144600     IF SEND-KDRC > 0                                                     
144700      MOVE SEND-KDRC TO KDRC-DISPLAY                                      
144800      STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                      
144900      DELIMITED BY SIZE INTO FELTEXT                                      
145000      DISPLAY FELTEXT                                                     
145100      CALL ABEND USING RKOD-ABEND-MED-DUMP                                
145200     END-IF                                                               
145300     .                                                                    
145400     EJECT                                                                
145500 S07-SEND-TO-WF0204-PUT SECTION.                                          
145600                                                                          
145700     MOVE 'W418'  TO SEND-AREA(1:4)                                       
145800                                                                          
145900     MOVE 'PUT'                           TO SEND-KDFUNC                  
146000     COMPUTE SEND-KVDLEN = LENGTH OF SEND-AREA + 4                        
146100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
146200                         SEND-KVDLEN                                      
146300                         SEND-AREA                                        
146400**FELHANTERING...                                                         
146500     IF SEND-KDRC > 1                                                     
146600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
146700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
146800       DELIMITED BY SIZE INTO FELTEXT                                     
146900       DISPLAY FELTEXT                                                    
147000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
147100     END-IF                                                               
147200     .                                                                    
147300     EJECT                                                                
147400 S08-SEND-TO-WF0204-CLOSE SECTION.                                        
147500                                                                          
147600     MOVE 'CLOSE' TO SEND-KDFUNC                                          
147700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
147800**FELHANTERING...                                                         
147900     IF SEND-KDRC > 0                                                     
148000      MOVE SEND-KDRC TO KDRC-DISPLAY                                      
148100      STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                     
148200       DELIMITED BY SIZE INTO FELTEXT                                     
148300       DISPLAY FELTEXT                                                    
148400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
148500     END-IF                                                               
148600     .                                                                    
148700     EJECT                                                                
148800 S05-SKAPA-TKOST-BILLIT SECTION.                                          
148900                                                                          
149000     MOVE 1                            TO UT-REQU-IDMSGVER                
149100     MOVE SPACE                        TO UT-REQU-KDPGMACT                
149200     MOVE 'W4183100'                   TO UT-REQU-IDUSER                  
149300                                                                          
149400     MOVE IN31B-IDDISTR TO TEST-IDDISTR                                   
149500                                                                          
149600     MOVE 'VCCS'                       TO UT-IDLEGSEL                     
149700     MOVE WS-IDBUNDLE                  TO UT-IDBUNDLE (1:8)               
149800     MOVE ' W418AI'                    TO UT-IDBUNDLE (9:7)               
149900     MOVE SPACE                        TO UT-IDREF                        
150000                                                                          
150100     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
150200     MOVE IN31B-IDRAPPNR       TO CIA-IDARTBET-IN                         
150300     CALL W009CIA USING           CIA-W009CIA                             
150400     MOVE CIA-IDARTBET-UT      TO UT-IDREF                                
150500                                                                          
150600     MOVE IN31B-DALEVANM               TO UT-DAREFDAT                     
150700                                                                          
150800     ADD +1                            TO W-KVPOST-IN                     
150900     MOVE W-KVPOST-IN                  TO UT-IDREFRAD                     
151000     MOVE SPACE                        TO UT-BEVOLREF                     
151100                                                                          
151200     IF IN31B-IDDC NOT = DCS-IDDC                                         
151300        MOVE IN31B-IDDC TO W-IDDC-B6                                      
151400        PERFORM IMS-GU-WDB601                                             
151500     END-IF                                                               
151600     MOVE DCS-IDLANDX2 TO UT-IDLANDX3-SEND                                
151700     IF UT-IDLANDX3-SEND = SPACE                                          
151800       STRING 'SÄNDANDE DC SAKNAR ISO-LANDKOD' IN31B-IDDC                 
151900            DELIMITED BY SIZE INTO FELTEXT                                
152000       DISPLAY FELTEXT                                                    
152100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
152200     END-IF                                                               
152300                                                                          
152400     MOVE SPACE                        TO UT-IDLANDX3-REC                 
152500     MOVE IN31B-IDPARTNR               TO UT-IDPARTNR                     
152600     MOVE SPACE                        TO UT-IDEXCUST (1)                 
152700                                                                          
152800     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
152900     MOVE IN31B-IDDISTR        TO CIA-IDARTBET-IN                         
153000     CALL W009CIA USING           CIA-W009CIA                             
153100     MOVE CIA-IDARTBET-UT      TO UT-IDEXCUST (1)                         
153200                                                                          
153300     MOVE SPACE                        TO UT-IDEXCUST (2)                 
153400                                                                          
153500     IF IN31B-IDKUNDNR = ZERO                                             
153600       CONTINUE                                                           
153700     ELSE                                                                 
153800       MOVE 'VO'               TO CIA-IDARTPRE-IN                         
153900       MOVE IN31B-IDKUNDNR     TO CIA-IDARTBET-IN                         
154000       CALL W009CIA USING         CIA-W009CIA                             
154100       MOVE CIA-IDARTBET-UT    TO UT-IDEXCUST (2)                         
154200     END-IF                                                               
154300                                                                          
154400     MOVE SPACE                        TO UT-IDEXCUST (3)                 
154500     MOVE SPACE                        TO UT-IDOPTION (3)                 
154600                                          UT-IDOPTION (4)                 
154700                                          UT-IDOPTION (5)                 
154800     MOVE SPACE                        TO UT-IDAPPEND                     
154900     MOVE SPACE                        TO UT-IDARTNR-FINANCE              
155000     MOVE ZERO                         TO UT-IDSTATNR                     
155100                                                                          
155200     MOVE ZERO                         TO UT-VKARTNTO                     
155300     MOVE ZERO                         TO UT-REARTRAB                     
155400     MOVE ZERO                         TO UT-KVBEART                      
155500     MOVE 1                            TO UT-KVLEVART                     
155600                                                                          
155700*- ENLIGT BILLIT SÅ KAN MAN ALLTID GE NEJ HÄR. DET SKALL INTE VARA        
155800*- SEPARAT KREDITNOTA FÖR SW. LIKT DAGENS LÄGE: 2002-10-18                
155900     MOVE 'N'                          TO UT-FLSOFT                       
156000                                                                          
156100     MOVE NEJ                          TO UT-FLSPECPR                     
156200     MOVE IN31B-FLFREE                 TO UT-FLFREE                       
156300     IF UT-FLFREE = 'Y'                                                   
156400       MOVE 'J'                        TO UT-FLFREE                       
156500     END-IF                                                               
156600                                                                          
156700     MOVE IN31B-KDVAT                  TO UT-KDVAT                        
156800* START FIX INLAGD 031230                                                 
156900     IF UT-IDLANDX3-SEND = 'IT' AND                                       
157000        UT-KDVAT         = '70'                                           
157100       MOVE 'ID'                       TO UT-KDVAT                        
157200     END-IF                                                               
157300* SLUT  FIX INLAGD 031230                                                 
157400                                                                          
157500     IF DIST79-DEALER-PRICE OR                                            
157700        DIST79-ECOM-PRICE                                                 
157800       MOVE IN31B-KDVALISO             TO UT-KDVALISO                     
157900     ELSE                                                                 
158000       MOVE 'SEK'                      TO UT-KDVALISO                     
158100     END-IF                                                               
158200                                                                          
158300     MOVE 'NOW'                        TO UT-KDINVFRQ                     
158400     MOVE 'CR'                         TO UT-KDFINDOC                     
158500     MOVE IN31B-IDRAPPNR               TO UT-IDBREAK (1)                  
158600     MOVE IN31B-IDDC                   TO UT-IDBREAK (2)                  
158700     MOVE IN31B-IDKUNDNR               TO HELP-IDKUNDNR                   
158800     MOVE HELP-IDKUNDNR                TO WS-IDBREAK(3:6)                 
158900     MOVE WS-IDBREAK                   TO UT-IDBREAK (2)                  
159000     MOVE SPACE                        TO UT-IDSEQ (3)                    
159100     MOVE SPACE                        TO UT-KDARTURS                     
159200     MOVE SPACE                        TO UT-KDANMORS                     
159300     MOVE SPACE                        TO UT-IDLEVNR                      
159400*    MOVE ZERO                         TO UT-IDFAKREF                     
159500     MOVE IN31B-IDFAKREF               TO UT-IDFAKREF                     
159600*    MOVE ZERO                         TO UT-DAFAKREF                     
159700     MOVE IN31B-DAFAKREF               TO UT-DAFAKREF                     
159800**** WILL USE IDLEVNR GET THE RIGHT CURRENCY RATE IN BILL-IT              
159900     MOVE IN31B-DAFAKREF(3:4)          TO UT-IDLEVNR                      
160000     MOVE IN31B-IDDC                   TO UT-IDDC                         
160100     MOVE LEV-KDFRAKT                  TO UT-KDFRAKT                      
160200                                                                          
160300*-ENLIGT BOSSE H SKALL MAN ANGE LEV.VILLKOR FÖR RETURER(INTRASTAT)        
160400     IF OKOD-FL-RETILL = JA                                               
160500       IF IN31B-IDDC-RET = '11'                                           
160600         MOVE 'CIP             GOTHENBURG         '                       
160700                                       TO UT-BELEVVIL                     
160800       ELSE                                                               
160900         MOVE 'CIP             (INCOTERMS 2010)   '                       
161000                                       TO UT-BELEVVIL                     
161100       END-IF                                                             
161200                                                                          
161300       IF IN31B-IDDC-RET NOT = RET-DCS-IDDC                               
161400          MOVE IN31B-IDDC-RET TO W-IDDC-B6-RET                            
161500          PERFORM IMS-GU-WDB601-RET                                       
161600       END-IF                                                             
161700       MOVE 'W41X'                     TO UT-IDSYSTEM-SEND                
161800                                                                          
161900       IF IN31B-KDANMORS = '72' AND                                       
162000         DCS-IDLANDX2 = 'IT' AND RET-DCS-IDLANDX2 = 'IT'                  
162100         MOVE 'W418'                   TO UT-IDSYSTEM-SEND                
162200       END-IF                                                             
162300                                                                          
162400       IF DCS-IDLANDX2 = 'IT' AND RET-DCS-IDLANDX2 = 'IT'                 
162500          MOVE 'W418'                  TO UT-IDSYSTEM-SEND                
162600       END-IF                                                             
162700                                                                          
162800     ELSE                                                                 
162900       MOVE SPACE                      TO UT-BELEVVIL                     
163000       MOVE 'W418'                     TO UT-IDSYSTEM-SEND                
163100     END-IF                                                               
163200                                                                          
163300*** SEPARATE ECOM CREDIT FROM NORMAL CREDIT                               
163400     IF DIST79-ECOM-PRICE                                                 
163500       MOVE 'ECOM'                     TO UT-IDSYSTEM-SEND                
163600     END-IF                                                               
163700***                                                                       
163800                                                                          
163900     MOVE SPACE                        TO UT-IDACCNT (1)                  
164000     MOVE SPACE                        TO UT-IDACCNT (2)                  
164100     MOVE SPACE                        TO UT-IDACCNT (3)                  
164200     MOVE SPACE                        TO UT-IDACCNT (4)                  
164300     MOVE 'W418'                       TO UT-IDSYSTEM-REC                 
164400     MOVE IN31B-BEANST                 TO UT-BEANST                       
164500     MOVE IN31B-IDUSER-ADM             TO UT-IDUSER                       
164600     MOVE SPACE                        TO UT-BETEXT                       
164700                                          UT-BETEXT-CRE                   
164800     MOVE WS-GODK-GRP (1)              TO UT-BETEXT (1:41)                
164900     MOVE WS-GODK-GRP (2)              TO UT-BETEXT (42:41)               
165000     MOVE WS-GODK-GRP (3)              TO UT-BETEXT (83:41)               
165100     MOVE WS-GODK-GRP (4)              TO UT-BETEXT-CRE (1:41)            
165200     MOVE WS-GODK-GRP (5)              TO UT-BETEXT-CRE (42:41)           
165300     .                                                                    
165400     EJECT                                                                
165500 X-TAG-CHECKPOINT   SECTION.                                              
165600                                                                          
165700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
165800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
165900     PERFORM IMS-CHECKPOINT                                               
166000     MOVE ZERO TO CHKP-ANT                                                
166100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
166200     .                                                                    
166300     EJECT                                                                
166400* --- IMS SEKTIONER ---                                                   
166500                                                                          
166600     EJECT                                                                
166700 IMS-GHU-WDA201 SECTION.                                                  
166800                                                                          
166900     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
167000          DELIMITED BY SIZE INTO SSA1                                     
167100     MOVE '  GE' TO GODK-STATUSKODER                                      
167200     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA201 SSA1                   
167300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600     SKIP3                                                                
167700 IMS-REPL-WDA201 SECTION.                                                 
167800                                                                          
167900     MOVE '  ' TO GODK-STATUSKODER                                        
168000     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA201                       
168100     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
168200     PERFORM IMS-STATUSKONTROLL                                           
168300     .                                                                    
168400     EJECT                                                                
168500 IMS-GHU-WDA211 SECTION.                                                  
168600                                                                          
168700     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
168800          DELIMITED BY SIZE INTO SSA1                                     
168900     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
169000          DELIMITED BY SIZE INTO SSA2                                     
169100     MOVE '    ' TO GODK-STATUSKODER                                      
169200     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA211 SSA1 SSA2              
169300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
169400     PERFORM IMS-STATUSKONTROLL                                           
169500     .                                                                    
169600     SKIP3                                                                
169700 IMS-REPL-WDA211 SECTION.                                                 
169800                                                                          
169900     MOVE '  ' TO GODK-STATUSKODER                                        
170000     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA211                       
170100     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
170200     PERFORM IMS-STATUSKONTROLL                                           
170300     .                                                                    
170400     EJECT                                                                
170500 IMS-GU-WDD301 SECTION.                                                   
170600                                                                          
170700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
170800          DELIMITED BY SIZE INTO SSA1                                     
170900     MOVE '  GE' TO GODK-STATUSKODER                                      
171000     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
171100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
171200     PERFORM IMS-STATUSKONTROLL                                           
171300     .                                                                    
171400     EJECT                                                                
171500 IMS-GNP-WDD311 SECTION.                                                  
171600                                                                          
171700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
171800          DELIMITED BY SIZE INTO SSA1                                     
171900     MOVE '  GE' TO GODK-STATUSKODER                                      
172000     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
172100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
172200     PERFORM IMS-STATUSKONTROLL                                           
172300     .                                                                    
172400     EJECT                                                                
172500 IMS-GU-WDGX4103 SECTION.                                                 
172600                                                                          
172700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
172800          DELIMITED BY SIZE INTO SSA1                                     
172900     MOVE '  GE' TO GODK-STATUSKODER                                      
173000     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
173100     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
173200     PERFORM IMS-STATUSKONTROLL                                           
173300     .                                                                    
173400     EJECT                                                                
173500 IMS-GNP-WDGX4104 SECTION.                                                
173600                                                                          
173700     STRING 'WDGX4104*F(KEY4104  =' W-WDGXKEY-4104-X ')'                  
173800          DELIMITED BY SIZE INTO SSA1                                     
173900     MOVE '  GE' TO GODK-STATUSKODER                                      
174000     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4104 SSA1                 
174100     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
174200     PERFORM IMS-STATUSKONTROLL                                           
174300     .                                                                    
174400     EJECT                                                                
174500 IMS-GNP-WDGX4106 SECTION.                                                
174600                                                                          
174700     STRING 'WDGX4104(KEY4104  =' W-WDGXKEY-4104-X ')'                    
174800          DELIMITED BY SIZE INTO SSA1                                     
174900     MOVE 'WDGX4106 ' TO SSA2                                             
175000     MOVE '  GE' TO GODK-STATUSKODER                                      
175100     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4106 SSA1 SSA2            
175200     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
175300     PERFORM IMS-STATUSKONTROLL                                           
175400     .                                                                    
175500     EJECT                                                                
175600 IMS-RESTART SECTION.                                                     
175700     SKIP2                                                                
175800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
175900     MOVE '  ' TO GODK-STATUSKODER                                        
176000     CALL CBLTDLI USING XRST MSG-PCB                                      
176100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
176200                        CHKP-AREA-LENGTH CHKP-AREA                        
176300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
176400     PERFORM IMS-STATUSKONTROLL                                           
176500     .                                                                    
176600     SKIP3                                                                
176700 IMS-CHECKPOINT SECTION.                                                  
176800     SKIP2                                                                
176900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
177000     MOVE '  XD' TO GODK-STATUSKODER                                      
177100     CALL CBLTDLI USING CHKP MSG-PCB                                      
177200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
177300                        CHKP-AREA-LENGTH CHKP-AREA                        
177400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
177500     PERFORM IMS-STATUSKONTROLL                                           
177600                                                                          
177700     IF IMS-EJ-OK                                                         
177800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
177900       DISPLAY FELTEXT                                                    
178000       CALL FELLOG                                                        
178100     END-IF                                                               
178200     .                                                                    
178300     EJECT                                                                
178400 IMS-GU-WDB601    SECTION.                                                
178500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
178600          DELIMITED BY SIZE INTO SSA1                                     
178700     MOVE '  GE' TO GODK-STATUSKODER                                      
178800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
178900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
179000     PERFORM IMS-STATUSKONTROLL                                           
179100     IF SEGMENT-SAKNAS                                                    
179200         MOVE SPACE TO DCS-KDDC                                           
179300                       DCS-IDLANDX2                                       
179400     END-IF                                                               
179500     .                                                                    
179600 IMS-GU-WDB601-RET    SECTION.                                            
179700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-RET-X ')'                     
179800          DELIMITED BY SIZE INTO SSA1                                     
179900     MOVE '  GE' TO GODK-STATUSKODER                                      
180000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-RET SSA1             
180100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
180200     PERFORM IMS-STATUSKONTROLL                                           
180300     IF SEGMENT-SAKNAS                                                    
180400         MOVE SPACE TO RET-DCS-KDDC                                       
180500                       RET-DCS-IDLANDX2                                   
180600     END-IF                                                               
180700     .                                                                    
180800 IMS-GU-WDF502 SECTION.                                                   
180900     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-WDF502-X ')'                  
181000            DELIMITED BY SIZE INTO SSA1                                   
181100     MOVE 'WDF502'         TO SSA2                                        
181200     MOVE '  GE'                TO GODK-STATUSKODER                       
181300     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
181400     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
181500     PERFORM IMS-STATUSKONTROLL                                           
181600     .                                                                    
181700 IMS-GU-WDK601 SECTION.                                                   
181800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
181900          DELIMITED BY SIZE INTO SSA1                                     
182000     MOVE '  GE'              TO GODK-STATUSKODER                         
182100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
182200     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
182300     PERFORM IMS-STATUSKONTROLL                                           
182400     .                                                                    
182500     SKIP3                                                                
182600 IMS-GNP-WDK611 SECTION.                                                  
182700     MOVE 'WDK611 '           TO SSA1                                     
182800     MOVE '  GE'              TO GODK-STATUSKODER                         
182900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
183000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
183100     PERFORM IMS-STATUSKONTROLL                                           
183200     .                                                                    
183300 IMS-STATUSKONTROLL SECTION.                                              
183400     SKIP2                                                                
183500     SET STATUS-IX TO 1                                                   
183600     SEARCH GODK-STATUS                                                   
183700       AT END                                                             
183800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
183900           DELIMITED BY SIZE INTO FELTEXT                                 
184000         DISPLAY FELTEXT                                                  
184100         CALL FELLOG                                                      
184200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
184300         CONTINUE                                                         
184400     END-SEARCH                                                           
184500     .                                                                    
