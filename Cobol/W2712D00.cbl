000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2712D00.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   11/05/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SKAPAR AUTOMATISKA SKROTORDER FÖR OLIKA DC.           
001000*        KOPIA PÅ W2616900 (CDC) OCH W6032200.                            
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK711                                     
001300*                              6324 (WDR5)  HTYP 6321                     
001400*                              6326 (WDR5)  HTYP 6321                     
001500*                              2402 (WDR5)  HTYP 2401                     
001600*                              WDR4  ÅTERSTARTSREGISTER                   
001700*                                    HTYP=4579, SEGMENT=WDGX4580          
001800*                                                                         
001900*        STARTAR RUTIN W216S1 I SOP.                                      
002000*                                                                         
002100*                                                                         
002200*    INDATA.                                                              
002300*     INFIL        W271.W271D4.W2712D(+0)                                 
002400*                  W271.W271D4.W27116(+0)                                 
002500*                                                                         
002600*                                                                         
002700* 2011-05-30  SO  E'TRACKER 9399577 DESTOCKING - SCRAP 2                  
002800* 2016-06     GK  E'TRACKER 10221103 AUTOMATIC SCRAP AREA 98              
002900*                 IDPTYP TILLAGD PÅ INFILEN FÖR ATT SKILJA                
003000*                 URSPRUNGLIG PASSIVSKROT '2D' FIL W2712D                 
003100*                 FRÅN NY SCRAP 98 '16' FIL W27116                        
003200* IN                                                                      
003300*                                                                         
003400*    ABENDKODER:                                                          
003500*        U0016 -  . . . .                                                 
003600*        U1000 -  . . . .                                                 
003700*                                                                         
003800*                                                                         
003900******************************************************************        
004000****                      C H A N G E L O G                               
004100******************************************************************        
004200*                                                                         
004300*                                                                         
004400*                                                                         
004500                                                                          
004600     SKIP3                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800     SKIP2                                                                
004900 INPUT-OUTPUT SECTION.                                                    
005000                                                                          
005100 FILE-CONTROL.                                                            
005200     SKIP2                                                                
005300*          --- ARTIKLAR ATT SKROTA AUTOMATISKT                            
005400     SELECT W2712D                     ASSIGN TO W2712DD1.                
005500     EJECT                                                                
005600 DATA DIVISION.                                                           
005700     SKIP3                                                                
005800 FILE SECTION.                                                            
005900     SKIP3                                                                
006000 FD  W2712D                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  -COPY W2712D      -L.                                                
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800 77  IDPGM                       PIC X(8)    VALUE 'W2712D00'.            
006900 01  CHKP-VAR.                                                            
007000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
007100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
007200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
007300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
007400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
007500     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
007600 77  JA                          PIC X       VALUE 'J'.                   
007700 77  NEJ                         PIC X       VALUE 'N'.                   
007800 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
007900 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
008000 77  W-KVPOST-IN                 PIC S9(9)   VALUE +0   COMP-3.           
008100 77  WS-IDUSER-AUTO              PIC X(8)    VALUE 'W2712D00'.            
008200 77  WS-IDUSER-AUTO-98           PIC X(8)    VALUE 'W2712D98'.            
008300 77  WS-DASKROT9-BEORD           PIC 9(8)    VALUE ZERO.                  
008400 77  WS-6322-DASKROT9            PIC 9(8)    VALUE ZERO.                  
008500 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
008600 77  WS-SPAR-BEANST-GODK         PIC X(25)   VALUE SPACE.                 
008700 77  WS-ART-KDPRODSL             PIC 9(2)    VALUE ZERO.                  
008800 77  WS-MAX-SUBEL                PIC S9(7)   VALUE ZERO COMP-3.           
008900 77  SW-AUT-GODK                 PIC X       VALUE 'N'.                   
009000 77  WS-KVOKS-CDC                PIC S9(6)   VALUE ZERO.                  
009100 77  WS-TEMEMO                   PIC X(66)   VALUE SPACE.                 
009200 77  IX-RAD                      PIC S9(3)   VALUE ZERO COMP-3.           
009300 77  WS-TISKROT-AUTO-DISP        PIC 9(6)    VALUE ZERO.                  
009400                                                                          
009500 01  WS-SLAG-FLORDSP             PIC X       VALUE SPACE.                 
009600 01  WS-SLAG-FLSPBULK            PIC X       VALUE SPACE.                 
009700 01  WS-SLAG-TISKROT-AUTO        PIC S9(7)   VALUE ZERO COMP-3.           
009800 01  WS-SLAG-KVSPARR-KVAL        PIC S9(7)   VALUE ZERO COMP-3.           
009900 01  WS-SLAG-KDLEVSP             PIC S9(3)   VALUE ZERO COMP-3.           
010000                                                                          
010100 01  WS-SDC-KVLS                 PIC S9(7)   VALUE ZERO COMP-3.           
010200 01  WS-SDC-KVAKS                PIC S9(7)   VALUE ZERO COMP-3.           
010300 01  WS-SDC-KVOKS                PIC S9(7)   VALUE ZERO COMP-3.           
010400                                                                          
010500 01  WS-CLAG-KDERS               PIC S9(3)   VALUE ZERO COMP-3.           
010600 01  WS-CLAG-KVLS                PIC S9(7)   VALUE ZERO COMP-3.           
010700 01  WS-CLAG-KVRESS              PIC S9(7)   VALUE ZERO COMP-3.           
010800 01  WS-CLAG-KVROS               PIC S9(7)   VALUE ZERO COMP-3.           
010900 01  WS-CLAG-KVAKS-CDC           PIC S9(7)   VALUE ZERO COMP-3.           
011000 01  WS-CLAG-KVAKS-PAV           PIC S9(7)   VALUE ZERO COMP-3.           
011100 01  WS-CLAG-KVAKS-T             PIC S9(7)   VALUE ZERO COMP-3.           
011200 01  WS-CLAG-KVSPARR-KVAL        PIC S9(7)   VALUE ZERO COMP-3.           
011300 01  WS-CLAG-KDLEVSP             PIC S9(3)   VALUE ZERO COMP-3.           
011400                                                                          
011500 77  BEEMB-IX                    PIC 9(3)    VALUE ZERO.                  
011600 77  IX-BEEMB                    PIC S9(3)   VALUE ZERO  COMP-3.          
011700 77  URV-IX                      PIC S9(3)   VALUE ZERO  COMP-3.          
011800 77  URV-IX-MAX                  PIC S9(3)   VALUE +12   COMP-3.          
011900                                                                          
012000 01  SPAR-IDKUNDNR               PIC S9(9)   VALUE ZERO COMP-3.           
012100 01  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
012200 01  SPAR-IDPTYP                 PIC X(3)    VALUE SPACE.                 
012300                                                                          
012400 77  6328-IDUSER-GODK-SW         PIC X       VALUE 'N'.                   
012500     88 IDUSER-GODK-FINNS                    VALUE 'J'.                   
012600     88 IDUSER-GODK-SAKNAS                   VALUE 'N'.                   
012700                                                                          
012800 01  WS-TIDATETIME               PIC X(14).                               
012900 01  FILLER REDEFINES WS-TIDATETIME.                                      
013000     03  WS-DATUM                PIC 9(8).                                
013100     03  WS-TIDHHMMSS            PIC 9(6).                                
013200                                                                          
013300 01  WS-TID                      PIC 9(8) VALUE ZERO.                     
013400 01  WS-DAREGDAT                 PIC 9(8).                                
013500                                                                          
013600     SKIP2                                                                
013700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
013800 01  FELTEXT.                                                             
013900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014100                                                                          
014200 77  W2712D-EOF-SW               PIC X       VALUE 'N'.                   
014300     88  END-OF-W2712D                       VALUE 'J'.                   
014400     EJECT                                                                
014500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014600 01  FILLER REDEFINES DAGENS-DATUM.                                       
014700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015000     EJECT                                                                
015100                                                                          
015200 01  WS-BC-PARAMETRAR.                                                    
015300     03  WS-URVAL.                                                        
015400         05  URV-FLKLAR       PIC X     VALUE SPACE.                      
015500         05  URV-KDARBTYP     PIC X(8)  VALUE SPACE.                      
015600     03  URV-TABELL.                                                      
015700         05 URV-TAB-RAD OCCURS 12.                                        
015800            07  URV-IDDC             PIC X(2).                            
015900            07  URV-IDARTNR          PIC 9(9).                            
016000            07  URV-DASKROT9-BEORD   PIC 9(8).                            
016100                                                                          
016200     EJECT                                                                
016300 01  DYNAMISKA-SUBPROGRAM.                                                
016400*                                                                         
016500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
016800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016900     EJECT                                                                
017000*    --- PARAMETRAR TILL POSTSUM                                          
017100*                                                                         
017200*01  -COPY W0005   -PRE  POSTSUM-                                         
017300     EJECT                                                                
017400*    --- PARAMETRAR TILL ABEND                                            
017500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017700                                                                          
017800     EJECT                                                                
017900                                                                          
018000 01  PROG-TO-PROG-SW.                                                     
018100*    03  -COPY WMSGSOP                                                    
018200                                                                          
018300     EJECT                                                                
018400 01  IN-AREA-START               PIC X(24)   VALUE                        
018500                                             'IN-AREA-START'.             
018600     SKIP2                                                                
018700                                                                          
018800*01  AREA -COPY W2712D     -PRE IN-                                       
018900*                                                                         
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019200     SKIP3                                                                
019300 01  NYCKLAR-TILL-DLI.                                                    
019400     03  W-IDARTNR-X.                                                     
019500         05  W-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.             
019600                                                                          
019700     03  W-IDDC-X.                                                        
019800         05  W-IDDC            PIC X(2)    VALUE SPACE.                   
019900                                                                          
020000     03  W-KDSEGKEY-X.                                                    
020100         05  W-KDSEGKEY        PIC X(1)    VALUE '1'.                     
020200                                                                          
020300     03  W-KDARBTYP-X.                                                    
020400         05  W-KDARBTYP        PIC X(8)   VALUE 'ESC     '.               
020500                                                                          
020600     03  W-DASKROT9-X.                                                    
020700         05  W-DASKROT9-BEORD  PIC 9(8)   VALUE ZERO.                     
020800                                                                          
020900     03  W-IDDC-6324-X.                                                   
021000         05  W-IDDC-6324       PIC X(2)   VALUE SPACE.                    
021100                                                                          
021200     03  W-KDSTASKR-X.                                                    
021300         05  W-KDSTASKR        PIC S9     VALUE 1 COMP-3.                 
021400                                                                          
021500     03  W-WDGXKEY-6321.                                                  
021600         05  W-6321-IDHTYP     PIC X(4)   VALUE '6321'.                   
021700         05  W-6321-KDARBTYP   PIC X(8)   VALUE SPACE.                    
021800         05  W-6321-LOWVALUE   PIC X(18)  VALUE LOW-VALUE.                
021900                                                                          
022000     03  W-WDGXKEY-6327.                                                  
022100         05  W-6327-IDHTYP     PIC X(4)   VALUE '6327'.                   
022200         05  W-6327-KDARBTYP   PIC X(8)   VALUE SPACE.                    
022300         05  W-6327-IDDC       PIC X(2)   VALUE SPACE.                    
022400         05  W-6327-LOWVALUE   PIC X(16)  VALUE LOW-VALUE.                
022500                                                                          
022600     03  W-IDUSER-GODK-X.                                                 
022700         05  W-IDUSER-GODK     PIC X(8)   VALUE SPACE.                    
022800                                                                          
022900     03  W-SUBEL-MIN-X.                                                   
023000         05  W-SUBEL-MIN       PIC 9(7)  VALUE ZERO.                      
023100                                                                          
023200     03  W-SUBEL-MAX-X.                                                   
023300         05  W-SUBEL-MAX       PIC 9(7)  VALUE 9999999.                   
023400                                                                          
023500     03 W-2401-KEY-X.                                                     
023600         05 FILLER             PIC X(4)    VALUE '2401'.                  
023700         05 FILLER             PIC X(26)   VALUE LOW-VALUE.               
023800                                                                          
023900     03  W-WDGXKEY-X.                                                     
024000         05  W-IDHTYP            PIC X(4)    VALUE '4579'.                
024100         05  W-IDPGM             PIC X(8)    VALUE 'W2712D00'.            
024200         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
024300                                                                          
024400                                                                          
024500     SKIP2                                                                
024600*    --- STATUS-KOD FRÅN IMS                                              
024700 01  STATUS-WS                 PIC XX.                                    
024800     88  SEGMENT-FINNS                       VALUE '  '.                  
024900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
025200     88  IMS-EJ-OK                           VALUE 'XD'.                  
025300     SKIP2                                                                
025400 01  GODK-STATUSKODER.                                                    
025500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025600     SKIP3                                                                
025700 01  SSA1                        PIC X(128).                              
025800 01  SSA2                        PIC X(256).                              
025900 01  SSA3                        PIC X(256).                              
026000 01  SSA4                        PIC X(256).                              
026100                                                                          
026200     EJECT                                                                
026300*    --- IMS FUNKTIONSKODER                                               
026400*01  -COPY W0003                                                          
026500     EJECT                                                                
026600*    ---  DLI INPUT-OUTPUT AREA                                           
026700                                                                          
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
026900 01  DLI-IO-WDK601.                                                       
027000*    03  -COPY WDK601                                                     
027100     EJECT                                                                
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
027300 01  DLI-IO-WDK611.                                                       
027400*    03  -COPY WDK611                                                     
027500     EJECT                                                                
027600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
027700 01  DLI-IO-WDK701.                                                       
027800*    03  -COPY WDK701                                                     
027900     EJECT                                                                
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
028100 01  DLI-IO-WDK711.                                                       
028200*    03  -COPY WDK711                                                     
028300     EJECT                                                                
028400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
028500 01  DLI-IO-WDK901.                                                       
028600*    03  -COPY WDK901                                                     
028700     EJECT                                                                
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN601'.                      
028900 01  DLI-IO-WDN601.                                                       
029000*    03  -COPY WDN601                                                     
029100     EJECT                                                                
029200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDN611'.                      
029300 01  DLI-IO-WDN611.                                                       
029400*    03  -COPY WDN611                                                     
029500     EJECT                                                                
029600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR5-6321'.                   
029700 01  DLI-IO-WDR5-6321.                                                    
029800*    03  -COPY WDGX6321                                                   
029900     EJECT                                                                
030000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
030100 01  DLI-IO-WDGX6322.                                                     
030200*    03  -COPY WDGX6322                                                   
030300     EJECT                                                                
030400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
030500 01  DLI-IO-WDGX6324.                                                     
030600*    03  -COPY WDGX6324                                                   
030700     EJECT                                                                
030800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6325'.                    
030900 01  DLI-IO-WDGX6325.                                                     
031000*    03  -COPY WDGX6325                                                   
031100     EJECT                                                                
031200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6326'.                    
031300 01  DLI-IO-WDGX6326.                                                     
031400*    03  -COPY WDGX6326                                                   
031500     EJECT                                                                
031600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR5-6327'.                   
031700 01  DLI-IO-WDR5-6327.                                                    
031800*    03  -COPY WDGX6327                                                   
031900     EJECT                                                                
032000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
032100 01  DLI-IO-WDGX6328.                                                     
032200*    03  -COPY WDGX6328                                                   
032300     EJECT                                                                
032400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR5-2401'.                   
032500 01  DLI-IO-WDR5-2401.                                                    
032600*    03  -COPY WDGX2402                                                   
032700     EJECT                                                                
032800                                                                          
032900*-ÅTERSTARTSREGISTER WDR4                                                 
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
033100 01  DLI-IO-WDGX4580.                                                     
033200*    03  -COPY WDGX4580                                                   
033300                                                                          
033400     EJECT                                                                
033500 LINKAGE SECTION.                                                         
033600                                                                          
033700*01  -COPY W0009   -PRE MSG-                                              
033800     EJECT                                                                
033900*01  -COPY W0009   -PRE ALT-                                              
034000     EJECT                                                                
034100                                                                          
034200*01  -COPY W0008  -PRE WDK6-                                              
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500*01  -COPY W0008  -PRE WDK7-                                              
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008  -PRE WDK9-                                              
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008  -PRE WDN6-                                              
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE 6321-                                              
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE 6327-                                              
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008  -PRE 2401-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE 4579-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDK6-PCB WDK7-PCB              
036700                           WDK9-PCB WDN6-PCB 6321-PCB 6327-PCB            
036800                           2401-PCB 4579-PCB.                             
036900 MAIN SECTION.                                                            
037000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDK6-PCB WDK7-PCB              
037100                           WDK9-PCB WDN6-PCB 6321-PCB 6327-PCB            
037200                           2401-PCB 4579-PCB.                             
037300                                                                          
037400     SKIP2                                                                
037500     PERFORM A-INIT                                                       
037600                                                                          
037700     PERFORM IMS-LAS-ATERSTART                                            
037800     IF 4580-KVPOST > +0                                                  
037900       PERFORM S11-LAS-FRAM-TILL-CHKPOINT                                 
038000     ELSE                                                                 
038100       PERFORM S10-LAES-W2712D                                            
038200     END-IF                                                               
038300                                                                          
038400     MOVE SPACE       TO SPAR-IDDC                                        
038500                                                                          
038600     PERFORM UNTIL END-OF-W2712D                                          
038700                                                                          
038800       IF IN-IDDC = SPAR-IDDC  AND                                        
038900          IN-IDPTYP = SPAR-IDPTYP                                         
039000          CONTINUE                                                        
039100       ELSE                                                               
039200          PERFORM C-KOLLA-IDUSER-GODK                                     
039300          MOVE IN-IDDC     TO SPAR-IDDC                                   
039400          MOVE IN-IDPTYP   TO SPAR-IDPTYP                                 
039500       END-IF                                                             
039600                                                                          
039700       IF IDUSER-GODK-FINNS                                               
039800         PERFORM G-SKAPA-HANDELSE-6321                                    
039900                                                                          
040000         IF SW-AUT-GODK = JA                                              
040100           PERFORM H-UPPDATERA                                            
040200                                                                          
040300           PERFORM S10-LAES-W2712D                                        
040400                                                                          
040500           ADD +1           TO URV-IX                                     
040600           IF URV-IX > URV-IX-MAX OR                                      
040700              END-OF-W2712D                                               
040800              PERFORM UNTIL URV-IX > URV-IX-MAX                           
040900                 MOVE SPACE TO URV-IDDC           (URV-IX)                
041000                 MOVE ZERO  TO URV-IDARTNR        (URV-IX)                
041100                 MOVE ZERO  TO URV-DASKROT9-BEORD (URV-IX)                
041200                 ADD +1     TO URV-IX                                     
041300              END-PERFORM                                                 
041400              PERFORM S02-STARTA-URV-TRANS                                
041500              PERFORM X-TAG-CHECKPOINT                                    
041600              MOVE +1       TO URV-IX                                     
041700           END-IF                                                         
041800         ELSE                                                             
041900           PERFORM S10-LAES-W2712D                                        
042000         END-IF                                                           
042100       ELSE                                                               
042200         PERFORM S10-LAES-W2712D                                          
042300       END-IF                                                             
042400                                                                          
042500     END-PERFORM                                                          
042600                                                                          
042700                                                                          
042800*-- TAR HAND OM SISTA POSTEN.                                             
042900     IF END-OF-W2712D                                                     
043000       IF URV-IX > +1                                                     
043100         PERFORM UNTIL URV-IX > URV-IX-MAX                                
043200            MOVE SPACE TO URV-IDDC           (URV-IX)                     
043300            MOVE ZERO  TO URV-IDARTNR        (URV-IX)                     
043400            MOVE ZERO  TO URV-DASKROT9-BEORD (URV-IX)                     
043500            ADD +1     TO URV-IX                                          
043600         END-PERFORM                                                      
043700         PERFORM S02-STARTA-URV-TRANS                                     
043800       END-IF                                                             
043900     END-IF                                                               
044000                                                                          
044100     PERFORM Z-FINIT                                                      
044200                                                                          
044300     MOVE ZERO TO RETURN-CODE                                             
044400     GOBACK                                                               
044500     .                                                                    
044600     EJECT                                                                
044700 A-INIT SECTION.                                                          
044800     SKIP2                                                                
044900                                                                          
045000     PERFORM IMS-RESTART                                                  
045100                                                                          
045200     OPEN INPUT W2712D                                                    
045300                                                                          
045400     ACCEPT DAGENS-DATUM FROM DATE                                        
045500                                                                          
045600     MOVE +0             TO CHKP-ANT                                      
045700                            W-KVPOST-IN                                   
045800                                                                          
045900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
046000                                                                          
046100     MOVE +1 TO URV-IX                                                    
046200                                                                          
046300     .                                                                    
046400     EJECT                                                                
046500 C-KOLLA-IDUSER-GODK  SECTION.                                            
046600     MOVE 'C-KOLLA-IDUSER-GODK '  TO CURRENT-SECTION                      
046700                                                                          
046800     MOVE JA                     TO 6328-IDUSER-GODK-SW                   
046900     MOVE ZERO                   TO WS-MAX-SUBEL                          
047000     MOVE IN-IDDC                TO W-6327-IDDC                           
047100     MOVE W-KDARBTYP             TO W-6327-KDARBTYP                       
047200     MOVE SPACE                  TO WS-SPAR-BEANST-GODK                   
047300     PERFORM IMS-GU-WDR501-6327                                           
047400     IF SEGMENT-FINNS                                                     
047500       IF IN-IDPTYP = '2D'                                                
047600          MOVE WS-IDUSER-AUTO TO W-IDUSER-GODK                            
047700       ELSE                                                               
047800          MOVE WS-IDUSER-AUTO-98                                          
047900                              TO W-IDUSER-GODK                            
048000       END-IF                                                             
048100       PERFORM IMS-GNP-WDGX6328                                           
048200                                                                          
048300       IF SEGMENT-FINNS                                                   
048400          MOVE 6328-BEANST-GODK  TO WS-SPAR-BEANST-GODK                   
048500          MOVE 6328-SUBEL        TO WS-MAX-SUBEL                          
048600       ELSE                                                               
048700         MOVE NEJ                TO 6328-IDUSER-GODK-SW                   
048800       END-IF                                                             
048900     ELSE                                                                 
049000       MOVE NEJ                  TO 6328-IDUSER-GODK-SW                   
049100     END-IF                                                               
049200                                                                          
049300     .                                                                    
049400     EJECT                                                                
049500 G-SKAPA-HANDELSE-6321 SECTION.                                           
049600     MOVE 'G-SKAPA-HANDELSE-6321'   TO CURRENT-SECTION                    
049700                                                                          
049800     MOVE JA  TO SW-AUT-GODK                                              
049900                                                                          
050000     PERFORM GA-UPPD-SKROTSPARR-WDK7                                      
050100     PERFORM GB-SKAPA-HANDELSETR-6321                                     
050200     IF IN-IDPTYP = '2D'                                                  
050300        PERFORM GC-SKAPA-TEMEMO-AUTO-SCRAP                                
050400     ELSE                                                                 
050500        PERFORM GD-SKAPA-TEMEMO-98-SCRAP                                  
050600     END-IF                                                               
050700                                                                          
050800                                                                          
050900     IF IN-SUARTSTD > WS-MAX-SUBEL                                        
051000       MOVE NEJ  TO SW-AUT-GODK                                           
051100     END-IF                                                               
051200                                                                          
051300     .                                                                    
051400     EJECT                                                                
051500 GA-UPPD-SKROTSPARR-WDK7 SECTION.                                         
051600     MOVE 'GA-UPPD-SKROTSPARR-WDK7'  TO CURRENT-SECTION                   
051700                                                                          
051800     MOVE SPACE                    TO WS-SLAG-FLORDSP                     
051900     MOVE SPACE                    TO WS-SLAG-FLSPBULK                    
052000     MOVE ZERO                     TO WS-SLAG-TISKROT-AUTO                
052100     MOVE ZERO                     TO WS-SLAG-KVSPARR-KVAL                
052200     MOVE ZERO                     TO WS-SLAG-KDLEVSP                     
052300                                                                          
052400     MOVE IN-IDARTNR          TO W-IDARTNR                                
052500     MOVE IN-IDDC             TO W-IDDC                                   
052600     PERFORM IMS-GHU-WDK711                                               
052700     IF SEGMENT-FINNS                                                     
052800       MOVE JA                     TO SLAG-FLSKROT-BEORD                  
052900       MOVE NEJ                    TO SLAG-FLSKROT-AUTO                   
053000       MOVE DAGENS-DATUM           TO SLAG-TISKROT-BEORD                  
053100       MOVE SLAG-FLORDSP           TO WS-SLAG-FLORDSP                     
053200       MOVE SLAG-FLSPBULK          TO WS-SLAG-FLSPBULK                    
053300       MOVE SLAG-TISKROT-AUTO      TO WS-SLAG-TISKROT-AUTO                
053400       MOVE SLAG-KVSPARR-KVAL      TO WS-SLAG-KVSPARR-KVAL                
053500       MOVE SLAG-KDLEVSP           TO WS-SLAG-KDLEVSP                     
053600                                                                          
053700       PERFORM IMS-REPL-WDK711                                            
053800     END-IF                                                               
053900                                                                          
054000     .                                                                    
054100     EJECT                                                                
054200 GB-SKAPA-HANDELSETR-6321 SECTION.                                        
054300     MOVE 'GB-SKAPA-HANDELSETR-6321'  TO CURRENT-SECTION                  
054400                                                                          
054500     MOVE W-KDARBTYP        TO W-6321-KDARBTYP                            
054600     PERFORM IMS-GU-WDR501-6321                                           
054700     IF SEGMENT-SAKNAS                                                    
054800        MOVE '6321'         TO 6321-IDHTYP                                
054900        MOVE W-KDARBTYP     TO 6321-KDARBTYP                              
055000        MOVE LOW-VALUE      TO 6321-LOW-VALUE                             
055100        PERFORM IMS-ISRT-WDR501-6321                                      
055200     END-IF                                                               
055300                                                                          
055400     COMPUTE W-DASKROT9-BEORD = 99999999 - IN-DADATUM                     
055500     MOVE W-DASKROT9-BEORD  TO 6322-DASKROT9-BEORD                        
055600     PERFORM IMS-ISRT-WDGX6322                                            
055700                                                                          
055800     PERFORM GBA-LAS-FLYTTA-WDK6                                          
055900                                                                          
056000     MOVE IN-IDARTNR        TO 6324-IDARTNR                               
056100     MOVE IN-IDDC           TO 6324-IDDC                                  
056200     MOVE 1                 TO 6324-KDSTASKR                              
056300     MOVE NEJ               TO 6324-FLSKROT-GODK                          
056400     MOVE 'AUTOMATBEORDRAD' TO 6324-BELAGINS-DEL                          
056500     MOVE SPACE             TO 6324-IDKST                                 
056600                                                                          
056700***-SAMMA KONTO SOM FÖR CDC-AUTO-SKR ENLIGT PATRIK 20110816.              
056800     IF WS-CLAG-KDERS = 22 OR 23 OR 25 OR 26                              
056900        PERFORM GBD-KONTO-ANALYS                                          
057000     ELSE                                                                 
057100        MOVE SPACE          TO 6324-IDANALYS                              
057200        MOVE ZERO           TO 6324-IDKONTO                               
057300     END-IF                                                               
057400                                                                          
057500     MOVE IN-IDDISTR        TO 6324-IDDISTR                               
057600     MOVE IN-IDKUNDNR       TO 6324-IDKUNDNR                              
057700     MOVE ZERO              TO 6324-IDPERSON                              
057800     IF IN-IDPTYP = '2D'                                                  
057900        MOVE WS-IDUSER-AUTO TO 6324-IDUSER                                
058000     ELSE                                                                 
058100        MOVE WS-IDUSER-AUTO-98                                            
058200                            TO 6324-IDUSER                                
058300     END-IF                                                               
058400     MOVE ZERO              TO 6324-KDFRAKT                               
058500     MOVE 1                 TO 6324-KDORDKL                               
058600     MOVE IN-KVSKROT        TO 6324-KVSKROT-BEORD                         
058700     MOVE ZERO              TO 6324-KVSKROT-KVAR                          
058800     MOVE SPACE             TO 6324-FLJUSTBUFF                            
058900     MOVE WS-SPAR-BEANST-GODK                                             
059000                            TO 6324-BEANST                                
059100     MOVE ZERO              TO 6324-KVSKROT-ONDEM                         
059200                                                                          
059300     MOVE WS-CLAG-KDERS     TO 6324-KDERS-UTG                             
059400                                                                          
059500     PERFORM GBB-LAS-FLYTTA-WDK7                                          
059600     PERFORM IMS-GU-WDK901                                                
059700     IF SEGMENT-FINNS                                                     
059800        MOVE ART-SUTPO-TOT        TO 6324-SUTPO-TOT                       
059900                                                                          
060000        COMPUTE WS-KVOKS-CDC = ART-KVOKS-BULK       +                     
060100                               ART-KVOKS-DAG        +                     
060200                               ART-KVOKS-VOR                              
060300                                                                          
060400     ELSE                                                                 
060500        MOVE ZERO                 TO 6324-SUTPO-TOT                       
060600                                     WS-KVOKS-CDC                         
060700     END-IF                                                               
060800     COMPUTE 6324-KVTILLG-CDC ROUNDED =                                   
060900            WS-CLAG-KVLS     - WS-CLAG-KVRESS                             
061000                             - WS-CLAG-KVROS                              
061100                             - WS-KVOKS-CDC                               
061200                                                                          
061300     COMPUTE 6324-KVTILLG-SDC ROUNDED =                                   
061400            WS-SDC-KVLS - WS-SDC-KVOKS                                    
061500                                                                          
061600     COMPUTE 6324-KVAKS-CDC ROUNDED =                                     
061700            WS-CLAG-KVAKS-CDC  + WS-CLAG-KVAKS-PAV                        
061800                               + WS-CLAG-KVAKS-T                          
061900                                                                          
062000     COMPUTE 6324-KVAKS-SDC ROUNDED =                                     
062100            WS-SDC-KVAKS                                                  
062200                                                                          
062300     PERFORM GBC-LAS-FLYTTA-WDN6                                          
062400     PERFORM IMS-ISRT-WDGX6324                                            
062500                                                                          
062600     .                                                                    
062700     EJECT                                                                
062800 GBA-LAS-FLYTTA-WDK6 SECTION.                                             
062900     MOVE 'GBA-LAS-FLYTTA-WDK6 '  TO CURRENT-SECTION                      
063000                                                                          
063100     MOVE ZERO                 TO WS-CLAG-KDERS                           
063200                                  WS-CLAG-KVLS                            
063300                                  WS-CLAG-KVRESS                          
063400                                  WS-CLAG-KVROS                           
063500                                  WS-CLAG-KVAKS-CDC                       
063600                                  WS-CLAG-KVAKS-PAV                       
063700                                  WS-CLAG-KVAKS-T                         
063800                                  WS-CLAG-KVSPARR-KVAL                    
063900                                  WS-CLAG-KDLEVSP                         
064000                                                                          
064100     PERFORM IMS-GU-WDK601                                                
064200     PERFORM IMS-GNP-WDK611                                               
064300     IF SEGMENT-FINNS                                                     
064400        MOVE CLAG-KDERS        TO WS-CLAG-KDERS                           
064500        MOVE CLAG-KVLS         TO WS-CLAG-KVLS                            
064600        MOVE CLAG-KVRESS       TO WS-CLAG-KVRESS                          
064700        MOVE CLAG-KVROS        TO WS-CLAG-KVROS                           
064800        MOVE CLAG-KVAKS-CDC    TO WS-CLAG-KVAKS-CDC                       
064900        MOVE CLAG-KVAKS-PAV    TO WS-CLAG-KVAKS-PAV                       
065000        MOVE CLAG-KVAKS-T      TO WS-CLAG-KVAKS-T                         
065100        MOVE CLAG-KVSPARR-KVAL TO WS-CLAG-KVSPARR-KVAL                    
065200        MOVE CLAG-KDLEVSP      TO WS-CLAG-KDLEVSP                         
065300     END-IF                                                               
065400                                                                          
065500     .                                                                    
065600     EJECT                                                                
065700 GBB-LAS-FLYTTA-WDK7 SECTION.                                             
065800     MOVE 'GBB-LAS-FLYTTA-WDK7 '  TO CURRENT-SECTION                      
065900                                                                          
066000     MOVE ZERO                  TO WS-SDC-KVLS                            
066100                                   WS-SDC-KVAKS                           
066200                                   WS-SDC-KVOKS                           
066300                                                                          
066400     PERFORM IMS-GU-WDK701                                                
066500     IF SEGMENT-FINNS                                                     
066600        PERFORM IMS-GNP-WDK711                                            
066700        PERFORM UNTIL SEGMENT-SAKNAS                                      
066800            ADD SLAG-KVLS       TO WS-SDC-KVLS                            
066900            ADD SLAG-KVAKS-SDC  TO WS-SDC-KVAKS                           
067000            ADD SLAG-KVAKS-PAV  TO WS-SDC-KVAKS                           
067100            ADD SLAG-KVOKS-DAG  TO WS-SDC-KVOKS                           
067200            ADD SLAG-KVOKS-BULK TO WS-SDC-KVOKS                           
067300          PERFORM IMS-GNP-WDK711                                          
067400        END-PERFORM                                                       
067500     END-IF                                                               
067600     .                                                                    
067700     EJECT                                                                
067800 GBC-LAS-FLYTTA-WDN6 SECTION.                                             
067900     MOVE 'GBC-LAS-FLYTTA-WDN6 '  TO CURRENT-SECTION                      
068000                                                                          
068100     MOVE +1 TO BEEMB-IX                                                  
068200     PERFORM UNTIL BEEMB-IX > 20                                          
068300        MOVE SPACE        TO 6324-BEEMBLEM (BEEMB-IX)                     
068400        ADD +1        TO BEEMB-IX                                         
068500     END-PERFORM                                                          
068600     PERFORM IMS-GU-WDN601                                                
068700     IF SEGMENT-FINNS                                                     
068800        PERFORM IMS-GNP-WDN611                                            
068900        MOVE +1 TO BEEMB-IX                                               
069000        PERFORM UNTIL BEEMB-IX > 20 OR SEGMENT-SAKNAS                     
069100           MOVE KAT-BEEMBLEM TO 6324-BEEMBLEM (BEEMB-IX)                  
069200           ADD +1        TO BEEMB-IX                                      
069300           PERFORM IMS-GNP-WDN611                                         
069400        END-PERFORM                                                       
069500        IF SEGMENT-FINNS                                                  
069600           MOVE 'MORE' TO 6324-BEEMBLEM (20)                              
069700        END-IF                                                            
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 GBD-KONTO-ANALYS SECTION.                                                
070200     MOVE 'GBD-KONTO-ANALYS '  TO CURRENT-SECTION                         
070300                                                                          
070400     MOVE SPACE                TO 6324-IDANALYS                           
070500     MOVE ZERO                 TO 6324-IDKONTO                            
070600     IF ART-KDPRODSL = 11                                                 
070700        MOVE '158600002002'    TO 6324-IDANALYS                           
070800        MOVE 481180            TO 6324-IDKONTO                            
070900     END-IF                                                               
071000     IF ART-KDPRODSL = 13                                                 
071100        MOVE '158600002003'    TO 6324-IDANALYS                           
071200        MOVE 481180            TO 6324-IDKONTO                            
071300     END-IF                                                               
071400     IF ART-KDPRODSL = 14                                                 
071500        MOVE '158600002004'    TO 6324-IDANALYS                           
071600        MOVE 481180            TO 6324-IDKONTO                            
071700     END-IF                                                               
071800     IF ART-KDPRODSL = 15                                                 
071900        MOVE '158600002005'    TO 6324-IDANALYS                           
072000        MOVE 481180            TO 6324-IDKONTO                            
072100     END-IF                                                               
072200     IF ART-KDPRODSL = 16                                                 
072300        MOVE '158600002006'    TO 6324-IDANALYS                           
072400        MOVE 481180            TO 6324-IDKONTO                            
072500     END-IF                                                               
072600     IF ART-KDPRODSL = 17                                                 
072700        MOVE '158600002007'    TO 6324-IDANALYS                           
072800        MOVE 481180            TO 6324-IDKONTO                            
072900     END-IF                                                               
073000     IF ART-KDPRODSL = 18                                                 
073100        MOVE '158600002008'    TO 6324-IDANALYS                           
073200        MOVE 481180            TO 6324-IDKONTO                            
073300     END-IF                                                               
073400     IF ART-KDPRODSL = 19                                                 
073500        MOVE '158600002009'    TO 6324-IDANALYS                           
073600        MOVE 481180            TO 6324-IDKONTO                            
073700     END-IF                                                               
073800     IF ART-KDPRODSL > 20 AND ART-KDPRODSL < 30                           
073900        MOVE '158600002010'    TO 6324-IDANALYS                           
074000        MOVE 481180            TO 6324-IDKONTO                            
074100     END-IF                                                               
074200                                                                          
074300     .                                                                    
074400     EJECT                                                                
074500 GC-SKAPA-TEMEMO-AUTO-SCRAP SECTION.                                      
074600     MOVE 'GC-MEMO-AUTO-SCR'   TO CURRENT-SECTION                         
074700                                                                          
074800     MOVE SPACE                      TO WS-TEMEMO                         
074900     MOVE ZERO                       TO IX-RAD                            
075000                                                                          
075100     MOVE ART-KDPRODSL   TO WS-ART-KDPRODSL                               
075200     IF WS-ART-KDPRODSL(2:1) = 9                                          
075300        ADD +1                       TO IX-RAD                            
075400        MOVE SPACE                   TO WS-TEMEMO                         
075500        MOVE 'EMBALLAGE'             TO WS-TEMEMO                         
075600        PERFORM GCA-ISRT-6325                                             
075700        MOVE NEJ TO SW-AUT-GODK                                           
075800     END-IF                                                               
075900                                                                          
076000     IF WS-ART-KDPRODSL(1:1) = 9                                          
076100        ADD +1                       TO IX-RAD                            
076200        MOVE SPACE                   TO WS-TEMEMO                         
076300        MOVE 'LOCAL'                 TO WS-TEMEMO                         
076400        PERFORM GCA-ISRT-6325                                             
076500        MOVE NEJ TO SW-AUT-GODK                                           
076600     END-IF                                                               
076700                                                                          
076800     IF ART-KDPRODSL = 14                                                 
076900        ADD +1                       TO IX-RAD                            
077000        MOVE SPACE                   TO WS-TEMEMO                         
077100        MOVE 'EXCHANGE'              TO WS-TEMEMO                         
077200        PERFORM GCA-ISRT-6325                                             
077300        MOVE NEJ TO SW-AUT-GODK                                           
077400     END-IF                                                               
077500                                                                          
077600     IF WS-SLAG-TISKROT-AUTO > ZERO                                       
077700       MOVE WS-SLAG-TISKROT-AUTO   TO WS-TISKROT-AUTO-DISP                
077800                                                                          
077900       IF WS-TISKROT-AUTO-DISP < DAGENS-DATUM                             
078000         ADD +1                       TO IX-RAD                           
078100         MOVE SPACE                   TO WS-TEMEMO                        
078200         MOVE 'PASSED BLOCKDATE FOR AUTO SCRAP' TO WS-TEMEMO              
078300         PERFORM GCA-ISRT-6325                                            
078400         MOVE NEJ TO SW-AUT-GODK                                          
078500       END-IF                                                             
078600     END-IF                                                               
078700                                                                          
078800     IF (WS-SLAG-FLORDSP = 'Y' OR 'J' ) OR                                
078900        (WS-SLAG-FLSPBULK= 'Y' OR 'J' )                                   
079000                                                                          
079100       ADD +1                       TO IX-RAD                             
079200       MOVE SPACE                   TO WS-TEMEMO                          
079300       MOVE 'ORDER BLOCKED '        TO WS-TEMEMO                          
079400       PERFORM GCA-ISRT-6325                                              
079500       MOVE NEJ TO SW-AUT-GODK                                            
079600     END-IF                                                               
079700                                                                          
079800     IF (WS-SLAG-KDLEVSP > ZERO)      OR                                  
079900        (WS-SLAG-KVSPARR-KVAL > ZERO) OR                                  
080000        (WS-CLAG-KDLEVSP > ZERO)      OR                                  
080100        (WS-CLAG-KVSPARR-KVAL > ZERO)                                     
080200                                                                          
080300       ADD +1                       TO IX-RAD                             
080400       MOVE SPACE                   TO WS-TEMEMO                          
080500       MOVE 'QUALITY BLOCKED '      TO WS-TEMEMO                          
080600       PERFORM GCA-ISRT-6325                                              
080700       MOVE NEJ TO SW-AUT-GODK                                            
080800     END-IF                                                               
080900                                                                          
081000     IF WS-CLAG-KDERS    > 20                                             
081100        ADD +1                       TO IX-RAD                            
081200        MOVE SPACE                   TO WS-TEMEMO                         
081300        MOVE 'REPLACED'              TO WS-TEMEMO                         
081400        PERFORM GCA-ISRT-6325                                             
081500        MOVE NEJ TO SW-AUT-GODK                                           
081600     END-IF                                                               
081700                                                                          
081800     .                                                                    
081900     EJECT                                                                
082000 GCA-ISRT-6325      SECTION.                                              
082100     MOVE 'GCA-ISRT-6325 '     TO CURRENT-SECTION                         
082200                                                                          
082300     MOVE IX-RAD                  TO 6325-IDRADNR                         
082400     MOVE WS-TEMEMO               TO 6325-TEMEMO                          
082500     MOVE 6321-KDARBTYP           TO W-6321-KDARBTYP                      
082600     MOVE 6322-DASKROT9-BEORD     TO W-DASKROT9-BEORD                     
082700     MOVE 6324-IDARTNR            TO W-IDARTNR                            
082800     MOVE 6324-KDSTASKR           TO W-KDSTASKR                           
082900     MOVE 6324-IDDC               TO W-IDDC-6324                          
083000                                                                          
083100     PERFORM IMS-ISRT-WDGX6325                                            
083200                                                                          
083300     .                                                                    
083400     EJECT                                                                
083500 GD-SKAPA-TEMEMO-98-SCRAP  SECTION.                                       
083600     MOVE 'GD-TEMEMO-98-SCR'   TO CURRENT-SECTION                         
083700                                                                          
083800     MOVE '98 SCRAP'                 TO WS-TEMEMO                         
083900     MOVE +1                         TO IX-RAD                            
084000     PERFORM GDA-ISRT-6325                                                
084100     MOVE NEJ TO SW-AUT-GODK                                              
084200                                                                          
084300     MOVE ART-KDPRODSL   TO WS-ART-KDPRODSL                               
084400     IF WS-ART-KDPRODSL(1:1) = 9                                          
084500        ADD +1                       TO IX-RAD                            
084600        MOVE SPACE                   TO WS-TEMEMO                         
084700        MOVE 'LOCAL'                 TO WS-TEMEMO                         
084800        PERFORM GDA-ISRT-6325                                             
084900        MOVE NEJ TO SW-AUT-GODK                                           
085000     END-IF                                                               
085100                                                                          
085200     IF WS-SLAG-TISKROT-AUTO > ZERO                                       
085300       MOVE WS-SLAG-TISKROT-AUTO   TO WS-TISKROT-AUTO-DISP                
085400                                                                          
085500       IF WS-TISKROT-AUTO-DISP < DAGENS-DATUM                             
085600         ADD +1                       TO IX-RAD                           
085700         MOVE SPACE                   TO WS-TEMEMO                        
085800         MOVE 'PASSED BLOCKDATE FOR AUTO SCRAP' TO WS-TEMEMO              
085900         PERFORM GDA-ISRT-6325                                            
086000         MOVE NEJ TO SW-AUT-GODK                                          
086100       END-IF                                                             
086200     END-IF                                                               
086300                                                                          
086400     IF (WS-SLAG-FLORDSP = 'Y' OR 'J' ) OR                                
086500        (WS-SLAG-FLSPBULK= 'Y' OR 'J' )                                   
086600                                                                          
086700       ADD +1                       TO IX-RAD                             
086800       MOVE SPACE                   TO WS-TEMEMO                          
086900       MOVE 'ORDER BLOCKED '        TO WS-TEMEMO                          
087000       PERFORM GDA-ISRT-6325                                              
087100       MOVE NEJ TO SW-AUT-GODK                                            
087200     END-IF                                                               
087300                                                                          
087400     IF WS-CLAG-KDERS    > 20                                             
087500        ADD +1                       TO IX-RAD                            
087600        MOVE SPACE                   TO WS-TEMEMO                         
087700        MOVE 'REPLACED'              TO WS-TEMEMO                         
087800        PERFORM GDA-ISRT-6325                                             
087900        MOVE NEJ TO SW-AUT-GODK                                           
088000     END-IF                                                               
088100                                                                          
088200     .                                                                    
088300     EJECT                                                                
088400 GDA-ISRT-6325      SECTION.                                              
088500     MOVE 'GDA-ISRT-6325 '     TO CURRENT-SECTION                         
088600                                                                          
088700     MOVE IX-RAD                  TO 6325-IDRADNR                         
088800     MOVE WS-TEMEMO               TO 6325-TEMEMO                          
088900     MOVE 6321-KDARBTYP           TO W-6321-KDARBTYP                      
089000     MOVE 6322-DASKROT9-BEORD     TO W-DASKROT9-BEORD                     
089100     MOVE 6324-IDARTNR            TO W-IDARTNR                            
089200     MOVE 6324-KDSTASKR           TO W-KDSTASKR                           
089300     MOVE 6324-IDDC               TO W-IDDC-6324                          
089400                                                                          
089500     PERFORM IMS-ISRT-WDGX6325                                            
089600                                                                          
089700     .                                                                    
089800     EJECT                                                                
089900 H-UPPDATERA SECTION.                                                     
090000     MOVE 'H-UPPDATERA '   TO CURRENT-SECTION                             
090100                                                                          
090200     PERFORM HC-SKAPA-EN-SKROTORDER                                       
090300                                                                          
090400     .                                                                    
090500     EJECT                                                                
090600 HC-SKAPA-EN-SKROTORDER  SECTION.                                         
090700     MOVE 'HC-SKAPA-EN-SKROTORDER '  TO CURRENT-SECTION                   
090800                                                                          
090900     MOVE IN-IDARTNR          TO W-IDARTNR                                
091000     MOVE IN-IDDC             TO W-IDDC                                   
091100                                 W-IDDC-6324                              
091200                                 W-6327-IDDC                              
091300                                                                          
091400     MOVE W-KDARBTYP          TO W-6321-KDARBTYP                          
091500                                 W-6327-KDARBTYP                          
091600     COMPUTE WS-DASKROT9-BEORD = 99999999 - IN-DADATUM                    
091700     MOVE WS-DASKROT9-BEORD TO W-DASKROT9-BEORD                           
091800                               WS-6322-DASKROT9                           
091900     MOVE ZERO                TO 6324-KVSKROT-BEORD                       
092000                                                                          
092100     PERFORM IMS-GHU-WDR501-6321                                          
092200     IF SEGMENT-FINNS                                                     
092300        PERFORM IMS-GHNP-WDGX6324                                         
092400        IF SEGMENT-FINNS                                                  
092500           MOVE 'J' TO 6324-FLSKROT-GODK                                  
092600           PERFORM IMS-REPL-WDGX6324                                      
092700        END-IF                                                            
092800     END-IF                                                               
092900     PERFORM HE-UPDATERA-6326                                             
093000     MOVE 'J' TO URV-FLKLAR                                               
093100     PERFORM S01-SKAPA-URV-TRANS                                          
093200     PERFORM S15-SKAPA-WDGX2402                                           
093300                                                                          
093400*-------------                                                            
093500                                                                          
093600     PERFORM IMS-GHU-WDK711                                               
093700     IF SEGMENT-FINNS                                                     
093800        COMPUTE SLAG-KVSPARR-KVAL =                                       
093900                SLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD                    
094000                                                                          
094100        IF SLAG-KVSPARR-KVAL < ZERO                                       
094200           MOVE ZERO    TO SLAG-KVSPARR-KVAL                              
094300        END-IF                                                            
094400                                                                          
094500        MOVE WS-IDUSER-AUTO  TO SLAG-IDUSER-SPKVAL                        
094600        MOVE DAGENS-DATUM TO SLAG-TISPARR-KVAL                            
094700                                                                          
094800        MOVE 'N' TO SLAG-FLSKROT-BEORD                                    
094900        MOVE 'J' TO SLAG-FLSKROT-AUTO                                     
095000        MOVE DAGENS-DATUM TO SLAG-TISKROT                                 
095100                                                                          
095200        PERFORM IMS-REPL-WDK711                                           
095300     END-IF                                                               
095400                                                                          
095500     .                                                                    
095600     EJECT                                                                
095700 HE-UPDATERA-6326  SECTION.                                               
095800     MOVE 'HE-UPDATERA-6326 '   TO CURRENT-SECTION                        
095900                                                                          
096000     ACCEPT WS-TID FROM TIME                                              
096100     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATUM                          
096200     MOVE WS-TID (1:6)               TO WS-TIDHHMMSS                      
096300     MOVE WS-TIDATETIME              TO 6326-TIDATETIME                   
096400     MOVE WS-IDUSER-AUTO             TO 6326-IDUSER-GODK                  
096500     MOVE WS-IDUSER-AUTO             TO W-IDUSER-GODK                     
096600     PERFORM IMS-GU-WDGX6328                                              
096700     IF SEGMENT-FINNS                                                     
096800       MOVE 6328-BEANST-GODK         TO 6326-BEANST-GODK                  
096900     ELSE                                                                 
097000       MOVE SPACE                    TO 6326-BEANST-GODK                  
097100     END-IF                                                               
097200     PERFORM IMS-ISRT-WDGX6326                                            
097300                                                                          
097400     .                                                                    
097500     EJECT                                                                
097600 Z-FINIT SECTION.                                                         
097700                                                                          
097800                                                                          
097900     CLOSE W2712D                                                         
098000                                                                          
098100     PERFORM S12-NOLLA-ATERSTART                                          
098200                                                                          
098300     SKIP2                                                                
098400     MOVE 'S' TO POSTSUM-OPKOD                                            
098500     CALL POSTSUM USING POSTSUM-PARM                                      
098600     .                                                                    
098700     EJECT                                                                
098800 S10-LAES-W2712D  SECTION.                                                
098900     MOVE 'S10-LAES-W2712D '   TO CURRENT-SECTION.                        
099000     SKIP2                                                                
099100     READ W2712D INTO IN-AREA                                             
099200     AT END                                                               
099300        SET END-OF-W2712D TO TRUE                                         
099400                                                                          
099500     NOT AT END                                                           
099600        MOVE 'W2712D'     TO POSTSUM-FDNAMN                               
099700        MOVE 'W2712DD1'   TO POSTSUM-DDNAMN2                              
099800        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
099900        CALL POSTSUM USING POSTSUM-PARM                                   
100000                                                                          
100100        ADD +1            TO W-KVPOST-IN                                  
100200     END-READ                                                             
100300     .                                                                    
100400     EJECT                                                                
100500 S11-LAS-FRAM-TILL-CHKPOINT SECTION.                                      
100600     MOVE 'S11-LAS-FRAM-TILL-CHKPOINT' TO CURRENT-SECTION                 
100700                                                                          
100800     PERFORM S10-LAES-W2712D                                              
100900                                                                          
101000     PERFORM UNTIL END-OF-W2712D OR                                       
101100                    W-KVPOST-IN = 4580-KVPOST                             
101200        PERFORM S10-LAES-W2712D                                           
101300     END-PERFORM                                                          
101400                                                                          
101500     IF END-OF-W2712D                                                     
101600        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
101700                                       TO FELTEXT-STR                     
101800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
101900     END-IF                                                               
102000                                                                          
102100     .                                                                    
102200     EJECT                                                                
102300 S12-NOLLA-ATERSTART SECTION.                                             
102400     MOVE 'S12-NOLLA-ATERSTART'  TO CURRENT-SECTION                       
102500                                                                          
102600     PERFORM IMS-LAS-ATERSTART                                            
102700                                                                          
102800     MOVE +0                           TO 4580-KVPOST                     
102900     MOVE DAGENS-DATUM                 TO 4580-TIUPPDAT                   
103000     ACCEPT 4580-TIUPPTID FROM TIME                                       
103100                                                                          
103200     PERFORM IMS-REPL-ATERSTART                                           
103300                                                                          
103400     .                                                                    
103500     EJECT                                                                
103600 S01-SKAPA-URV-TRANS  SECTION.                                            
103700     MOVE 'S01-SKAPA-URV-TRANS '  TO CURRENT-SECTION                      
103800                                                                          
103900     MOVE W-KDARBTYP           TO URV-KDARBTYP                            
104000                                                                          
104100     MOVE IN-IDARTNR           TO URV-IDARTNR(URV-IX)                     
104200     MOVE IN-IDDC              TO URV-IDDC(URV-IX)                        
104300                                                                          
104400     COMPUTE WS-DASKROT9-BEORD = 999999999 - IN-DADATUM                   
104500     MOVE WS-DASKROT9-BEORD    TO URV-DASKROT9-BEORD(URV-IX)              
104600     .                                                                    
104700     EJECT                                                                
104800 S02-STARTA-URV-TRANS SECTION.                                            
104900     MOVE 'S02-STARTA-URV-TRANS '  TO CURRENT-SECTION                     
105000                                                                          
105100     MOVE '2712'   TO MSGSOP-IDTRANS                                      
105200     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
105300     MOVE 'W216S1' TO MSGSOP-IDPROCESS                                    
105400     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
105500                                                                          
105600     STRING 'URVAL1(' WS-URVAL ') '                                       
105700            'URVAL2(' URV-TAB-RAD (1) ') '                                
105800            'URVAL3(' URV-TAB-RAD (2) ') '                                
105900            'URVAL4(' URV-TAB-RAD (3) ') '                                
106000            'URVAL5(' URV-TAB-RAD (4) ') '                                
106100            'URVAL6(' URV-TAB-RAD (5) ') '                                
106200            'URVAL7(' URV-TAB-RAD (6) ') '                                
106300            'URVAL8(' URV-TAB-RAD (7) ') '                                
106400            'URVAL9(' URV-TAB-RAD (8) ') '                                
106500            'URVAL10(' URV-TAB-RAD (9) ') '                               
106600            'URVAL11(' URV-TAB-RAD (10) ') '                              
106700            'URVAL12(' URV-TAB-RAD (11) ') '                              
106800            'URVAL13(' URV-TAB-RAD (12) ')'                               
106900              DELIMITED BY SIZE INTO MSGSOP-TESYMBV                       
107000                                                                          
107100     PERFORM IMS-INSERT-ALTMSG                                            
107200                                                                          
107300     .                                                                    
107400     EJECT                                                                
107500 S15-SKAPA-WDGX2402 SECTION.                                              
107600     MOVE 'S15-SKAPA-WDGX2402 '  TO CURRENT-SECTION                       
107700                                                                          
107800     MOVE 6324-IDARTNR      TO 2402-IDARTNR                               
107900     MOVE 6324-IDANALYS     TO 2402-IDANALYS                              
108000     MOVE 6324-IDDC         TO 2402-IDDC                                  
108100     MOVE 6324-IDDISTR      TO 2402-IDDISTR                               
108200     MOVE 6324-IDKONTO      TO 2402-IDKONTO                               
108300     MOVE 6324-IDKST        TO 2402-IDKST                                 
108400     MOVE 6324-IDPERSON     TO 2402-IDPERSON                              
108500     MOVE 6321-KDARBTYP     TO 2402-KDARBTYP                              
108600     MOVE 6324-KVSKROT-BEORD TO 2402-KVSKROT-BEORD                        
108700     MOVE 6324-KVSKROT-ONDEM TO 2402-KVSKROT-KVAR                         
108800     MOVE 6324-IDUSER       TO 2402-IDUSER                                
108900     MOVE 6324-BEANST       TO 2402-BEANST                                
109000     MOVE WS-6322-DASKROT9  TO 2402-DASKROT9-BEORD                        
109100     MOVE 6326-TIDATETIME   TO 2402-TIDATETIME(1)                         
109200     MOVE 6326-IDUSER-GODK  TO 2402-IDUSER-GODK(1)                        
109300     MOVE 6326-BEANST-GODK  TO 2402-BEANST-GODK(1)                        
109400     MOVE 6324-IDKUNDNR     TO 2402-IDKUNDNR                              
109500     MOVE 6324-KDERS-UTG    TO 2402-KDERS-UTG                             
109600     MOVE 6324-KVTILLG-CDC  TO 2402-KVTILLG-CDC                           
109700     MOVE 6324-KVTILLG-SDC  TO 2402-KVTILLG-SDC                           
109800     MOVE 6324-KVAKS-CDC    TO 2402-KVAKS-CDC                             
109900     MOVE 6324-KVAKS-SDC    TO 2402-KVAKS-SDC                             
110000                                                                          
110100     MOVE +1     TO IX-BEEMB                                              
110200     PERFORM UNTIL IX-BEEMB > 20                                          
110300       MOVE 6324-BEEMBLEM(IX-BEEMB) TO 2402-BEEMBLEM(IX-BEEMB)            
110400       ADD +1 TO IX-BEEMB                                                 
110500     END-PERFORM                                                          
110600     MOVE 6324-SUTPO-TOT    TO 2402-SUTPO-TOT                             
110700     PERFORM IMS-ISRT-WDGX2402                                            
110800     .                                                                    
110900     EJECT                                                                
111000 X-TAG-CHECKPOINT   SECTION.                                              
111100     MOVE 'X-TAG-CHECKPOINT '   TO CURRENT-SECTION.                       
111200                                                                          
111300* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
111400* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
111500* --- LÄS OM DATABAS OM DET BEHÖVS                                        
111600                                                                          
111700***  UPPDATERA ÅTERSTARTREGISTRET                                         
111800     PERFORM IMS-LAS-ATERSTART                                            
111900                                                                          
112000     MOVE W-KVPOST-IN    TO 4580-KVPOST                                   
112100     ACCEPT 4580-TIUPPDAT FROM DATE                                       
112200     ACCEPT 4580-TIUPPTID FROM TIME                                       
112300                                                                          
112400     PERFORM IMS-REPL-ATERSTART                                           
112500                                                                          
112600     PERFORM IMS-CHECKPOINT                                               
112700     ADD +1              TO CHKP-ANT                                      
112800     .                                                                    
112900     EJECT                                                                
113000* --- IMS SEKTIONER ---                                                   
113100                                                                          
113200     EJECT                                                                
113300 IMS-INSERT-ALTMSG SECTION.                                               
113400     MOVE 'IMS-INSERT-ALTMSG '  TO DBS-SECTION                            
113500                                                                          
113600     MOVE SPACE TO GODK-STATUSKODER                                       
113700     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
113800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
113900     PERFORM IMS-STATUSKONTROLL                                           
114000     .                                                                    
114100     EJECT                                                                
114200 IMS-GU-WDK601 SECTION.                                                   
114300     MOVE 'IMS-GU-WDK601 '      TO DBS-SECTION                            
114400                                                                          
114500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
114600          DELIMITED BY SIZE INTO SSA1                                     
114700     MOVE '    ' TO GODK-STATUSKODER                                      
114800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
114900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
115000     PERFORM IMS-STATUSKONTROLL                                           
115100     .                                                                    
115200     EJECT                                                                
115300 IMS-GNP-WDK611 SECTION.                                                  
115400     MOVE 'IMS-GNP-WDK611 '      TO DBS-SECTION                           
115500                                                                          
115600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
115700          DELIMITED BY SIZE INTO SSA1                                     
115800     MOVE '  GE' TO GODK-STATUSKODER                                      
115900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
116000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
116100     PERFORM IMS-STATUSKONTROLL                                           
116200     .                                                                    
116300     EJECT                                                                
116400 IMS-GU-WDK611 SECTION.                                                   
116500     MOVE 'IMS-GU-WDK611 '      TO DBS-SECTION                            
116600                                                                          
116700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
116800          DELIMITED BY SIZE INTO SSA1                                     
116900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
117000          DELIMITED BY SIZE INTO SSA2                                     
117100     MOVE '  GE' TO GODK-STATUSKODER                                      
117200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
117300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
117400     PERFORM IMS-STATUSKONTROLL                                           
117500     .                                                                    
117600     EJECT                                                                
117700 IMS-GHU-WDK711 SECTION.                                                  
117800     MOVE 'IMS-GHU-WDK711 '      TO DBS-SECTION                           
117900                                                                          
118000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
118100          DELIMITED BY SIZE INTO SSA1                                     
118200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
118300          DELIMITED BY SIZE INTO SSA2                                     
118400     MOVE '  GE' TO GODK-STATUSKODER                                      
118500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
118600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
118700     PERFORM IMS-STATUSKONTROLL                                           
118800     .                                                                    
118900     SKIP3                                                                
119000 IMS-REPL-WDK711 SECTION.                                                 
119100     MOVE 'IMS-REPL-WDK711 '      TO DBS-SECTION                          
119200                                                                          
119300     MOVE '  ' TO GODK-STATUSKODER                                        
119400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
119500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
119600     PERFORM IMS-STATUSKONTROLL                                           
119700     .                                                                    
119800     EJECT                                                                
119900 IMS-GU-WDK701 SECTION.                                                   
120000     MOVE 'IMS-GU-WDK701 '   TO DBS-SECTION                               
120100                                                                          
120200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
120300          DELIMITED BY SIZE INTO SSA1                                     
120400     MOVE '  GE' TO GODK-STATUSKODER                                      
120500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
120600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
120700     PERFORM IMS-STATUSKONTROLL                                           
120800     .                                                                    
120900     SKIP3                                                                
121000 IMS-GNP-WDK711 SECTION.                                                  
121100     MOVE 'IMS-GNP-WDK711 '   TO DBS-SECTION                              
121200                                                                          
121300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
121400          DELIMITED BY SIZE INTO SSA1                                     
121500     MOVE '  GE' TO GODK-STATUSKODER                                      
121600     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
121700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
121800     PERFORM IMS-STATUSKONTROLL                                           
121900     .                                                                    
122000     SKIP3                                                                
122100 IMS-GU-WDK901 SECTION.                                                   
122200     MOVE 'IMS-GU-WDK901 '   TO DBS-SECTION                               
122300                                                                          
122400     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
122500          DELIMITED BY SIZE INTO SSA1                                     
122600     MOVE '  GE' TO GODK-STATUSKODER                                      
122700     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
122800     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
122900     PERFORM IMS-STATUSKONTROLL                                           
123000     .                                                                    
123100     EJECT                                                                
123200 IMS-GU-WDN601 SECTION.                                                   
123300     MOVE 'IMS-GU-WDN601 '   TO DBS-SECTION                               
123400                                                                          
123500     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
123600          DELIMITED BY SIZE INTO SSA1                                     
123700     MOVE '  GE' TO GODK-STATUSKODER                                      
123800     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
123900     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
124000     PERFORM IMS-STATUSKONTROLL                                           
124100     .                                                                    
124200     EJECT                                                                
124300 IMS-GNP-WDN611 SECTION.                                                  
124400     MOVE 'IMS-GNP-WDN611 '   TO DBS-SECTION                              
124500                                                                          
124600     STRING 'WDN611   '                                                   
124700          DELIMITED BY SIZE INTO SSA1                                     
124800     MOVE '  GE' TO GODK-STATUSKODER                                      
124900     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN611 SSA1                   
125000     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
125100     PERFORM IMS-STATUSKONTROLL                                           
125200     .                                                                    
125300     EJECT                                                                
125400 IMS-GU-WDR501-6321 SECTION.                                              
125500     MOVE 'IMS-GU-WDR501-6321 '      TO DBS-SECTION                       
125600                                                                          
125700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321 ')'                      
125800            DELIMITED BY SIZE INTO SSA1                                   
125900     MOVE 'GE  '                TO GODK-STATUSKODER                       
126000     CALL CBLTDLI USING GU   6321-PCB DLI-IO-WDR5-6321 SSA1               
126100     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
126200     PERFORM IMS-STATUSKONTROLL                                           
126300     .                                                                    
126400     EJECT                                                                
126500 IMS-ISRT-WDR501-6321 SECTION.                                            
126600     MOVE 'IMS-ISRT-WDR501-6321 '  TO DBS-SECTION                         
126700                                                                          
126800     STRING 'WDR501     '                                                 
126900            DELIMITED BY SIZE INTO SSA1                                   
127000     MOVE '  '                  TO GODK-STATUSKODER                       
127100     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDR5-6321 SSA1               
127200     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
127300     PERFORM IMS-STATUSKONTROLL                                           
127400     .                                                                    
127500     EJECT                                                                
127600 IMS-ISRT-WDGX6322 SECTION.                                               
127700     MOVE 'IMS-ISRT-WDGX6322 ' TO DBS-SECTION                             
127800                                                                          
127900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321 ')'                      
128000            DELIMITED BY SIZE INTO SSA1                                   
128100     MOVE 'WDGX6322'            TO SSA2                                   
128200     MOVE '  II'                TO GODK-STATUSKODER                       
128300     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2           
128400     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
128500     PERFORM IMS-STATUSKONTROLL                                           
128600     SKIP3                                                                
128700     .                                                                    
128800     SKIP3                                                                
128900 IMS-ISRT-WDGX6324 SECTION.                                               
129000     MOVE 'IMS-ISRT-WDGX6324 '  TO DBS-SECTION                            
129100                                                                          
129200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321 ')'                      
129300            DELIMITED BY SIZE INTO SSA1                                   
129400     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X ')'                        
129500            DELIMITED BY SIZE INTO SSA2                                   
129600     MOVE 'WDGX6324'            TO SSA3                                   
129700     MOVE '  '                  TO GODK-STATUSKODER                       
129800     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
129900                                      SSA1 SSA2 SSA3                      
130000     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
130100     PERFORM IMS-STATUSKONTROLL                                           
130200     .                                                                    
130300     EJECT                                                                
130400 IMS-GHU-WDR501-6321 SECTION.                                             
130500     MOVE 'IMS-GHU-WDR501-6321 '    TO DBS-SECTION                        
130600                                                                          
130700     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
130800          DELIMITED BY SIZE INTO SSA1                                     
130900     MOVE 'GE  ' TO GODK-STATUSKODER                                      
131000     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDR5-6321 SSA1                
131100     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
131200     PERFORM IMS-STATUSKONTROLL                                           
131300     .                                                                    
131400     EJECT                                                                
131500 IMS-GHNP-WDGX6324 SECTION.                                               
131600     MOVE 'IMS-GHNP-WDGX6324 '      TO DBS-SECTION                        
131700                                                                          
131800     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
131900          DELIMITED BY SIZE INTO SSA1                                     
132000     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
132100                    '&IDDC     =' W-IDDC-6324-X                           
132200                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
132300          DELIMITED BY SIZE INTO SSA2                                     
132400     MOVE '  GE' TO GODK-STATUSKODER                                      
132500     CALL CBLTDLI USING GHNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2           
132600     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
132700     PERFORM IMS-STATUSKONTROLL                                           
132800     .                                                                    
132900     SKIP3                                                                
133000 IMS-REPL-WDGX6324 SECTION.                                               
133100     MOVE 'IMS-REPL-WDGX6324 '      TO DBS-SECTION                        
133200                                                                          
133300     MOVE '  ' TO GODK-STATUSKODER                                        
133400     CALL CBLTDLI USING REPL 6321-PCB DLI-IO-WDGX6324                     
133500     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
133600     PERFORM IMS-STATUSKONTROLL                                           
133700     .                                                                    
133800     EJECT                                                                
133900 IMS-GU-WDR501-6327 SECTION.                                              
134000     MOVE 'IMS-GU-WDR501-6327 '    TO DBS-SECTION                         
134100                                                                          
134200     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6327 ')'                      
134300          DELIMITED BY SIZE INTO SSA1                                     
134400     MOVE 'GE  ' TO GODK-STATUSKODER                                      
134500     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDR5-6327 SSA1                 
134600     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
134700     PERFORM IMS-STATUSKONTROLL                                           
134800     .                                                                    
134900     EJECT                                                                
135000 IMS-GNP-WDGX6328 SECTION.                                                
135100     MOVE 'IMS-GNP-WDGX6328 '    TO DBS-SECTION                           
135200                                                                          
135300     STRING 'WDGX6328(IDUSERGK= ' W-IDUSER-GODK-X ')'                     
135400          DELIMITED BY SIZE INTO SSA1                                     
135500     MOVE 'GE  ' TO GODK-STATUSKODER                                      
135600     CALL CBLTDLI USING GNP 6327-PCB DLI-IO-WDGX6328 SSA1                 
135700     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     .                                                                    
136000     EJECT                                                                
136100 IMS-GU-WDGX6328 SECTION.                                                 
136200     MOVE 'IMS-GU-WDGX6328 '  TO DBS-SECTION                              
136300                                                                          
136400     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6327 ')'                      
136500          DELIMITED BY SIZE INTO SSA1                                     
136600     STRING 'WDGX6328(SUBEL   =>' W-SUBEL-MIN-X                           
136700                    '&SUBEL   =<' W-SUBEL-MAX-X                           
136800                    '&IDUSERGK= ' W-IDUSER-GODK-X ')'                     
136900          DELIMITED BY SIZE INTO SSA2                                     
137000     MOVE '  GE'                TO GODK-STATUSKODER                       
137100     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6328 SSA1 SSA2             
137200     MOVE 6327-STATUS-CODE      TO STATUS-WS                              
137300     PERFORM IMS-STATUSKONTROLL                                           
137400     .                                                                    
137500     EJECT                                                                
137600 IMS-ISRT-WDGX6326 SECTION.                                               
137700     MOVE 'IMS-ISRT-WDGX6326 '  TO DBS-SECTION                            
137800                                                                          
137900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321 ')'                      
138000            DELIMITED BY SIZE INTO SSA1                                   
138100     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X   ')'                      
138200            DELIMITED BY SIZE INTO SSA2                                   
138300     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
138400                    '&IDDC     =' W-IDDC-6324-X                           
138500                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
138600          DELIMITED BY SIZE INTO SSA3                                     
138700     MOVE 'WDGX6326'            TO SSA4                                   
138800     MOVE '  '                  TO GODK-STATUSKODER                       
138900     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6326                     
139000                                      SSA1 SSA2 SSA3 SSA4                 
139100     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     SKIP3                                                                
139500 IMS-ISRT-WDGX2402 SECTION.                                               
139600     MOVE 'IMS-ISRT-WDGX2402 '  TO DBS-SECTION.                           
139700                                                                          
139800     STRING 'WDR501  (WDGXKEY  =' W-2401-KEY-X ')'                        
139900            DELIMITED BY SIZE INTO SSA1                                   
140000     MOVE 'WDGX2402'            TO SSA2                                   
140100     MOVE '  '                  TO GODK-STATUSKODER                       
140200     CALL CBLTDLI USING ISRT 2401-PCB DLI-IO-WDR5-2401 SSA1 SSA2          
140300     MOVE 2401-STATUS-CODE      TO STATUS-WS                              
140400     PERFORM IMS-STATUSKONTROLL                                           
140500                                                                          
140600     .                                                                    
140700     EJECT                                                                
140800 IMS-ISRT-WDGX6325 SECTION.                                               
140900     MOVE 'IMS-ISRT-WDGX6325 '  TO DBS-SECTION.                           
141000                                                                          
141100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321 ')'                      
141200            DELIMITED BY SIZE INTO SSA1                                   
141300     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X ')'                        
141400            DELIMITED BY SIZE INTO SSA2                                   
141500     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
141600                    '&IDDC     =' W-IDDC-6324-X                           
141700                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
141800          DELIMITED BY SIZE INTO SSA3                                     
141900     MOVE 'WDGX6325 '           TO SSA4                                   
142000     MOVE '  II'                TO GODK-STATUSKODER                       
142100     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6325                     
142200                                      SSA1 SSA2 SSA3 SSA4                 
142300     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
142400     PERFORM IMS-STATUSKONTROLL                                           
142500     .                                                                    
142600     EJECT                                                                
142700 IMS-LAS-ATERSTART SECTION.                                               
142800     MOVE 'IMS-LAS-ATERSTART '  TO DBS-SECTION                            
142900                                                                          
143000     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
143100                    DELIMITED BY SIZE INTO SSA1                           
143200     MOVE 'WDR470 '      TO SSA2                                          
143300     MOVE '  '           TO GODK-STATUSKODER                              
143400     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
143500     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
143600     PERFORM IMS-STATUSKONTROLL                                           
143700     .                                                                    
143800     SKIP2                                                                
143900 IMS-REPL-ATERSTART SECTION.                                              
144000     MOVE 'IMS-REPL-ATERSTART ' TO DBS-SECTION                            
144100                                                                          
144200     MOVE '  '             TO GODK-STATUSKODER                            
144300     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
144400     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
144500     PERFORM IMS-STATUSKONTROLL                                           
144600     .                                                                    
144700     EJECT                                                                
144800 IMS-RESTART SECTION.                                                     
144900     SKIP2                                                                
145000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
145100     MOVE '  ' TO GODK-STATUSKODER                                        
145200     CALL CBLTDLI USING XRST MSG-PCB                                      
145300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
145400                        CHKP-AREA-LENGTH CHKP-AREA                        
145500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
145600     PERFORM IMS-STATUSKONTROLL                                           
145700     .                                                                    
145800     SKIP3                                                                
145900 IMS-CHECKPOINT SECTION.                                                  
146000     SKIP2                                                                
146100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
146200     MOVE '  XD' TO GODK-STATUSKODER                                      
146300     CALL CBLTDLI USING CHKP MSG-PCB                                      
146400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
146500                        CHKP-AREA-LENGTH CHKP-AREA                        
146600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
146700     PERFORM IMS-STATUSKONTROLL                                           
146800                                                                          
146900     IF IMS-EJ-OK                                                         
147000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
147100       DISPLAY FELTEXT                                                    
147200       CALL FELLOG                                                        
147300     END-IF                                                               
147400     .                                                                    
147500     EJECT                                                                
147600 IMS-STATUSKONTROLL SECTION.                                              
147700     SKIP2                                                                
147800     SET STATUS-IX TO 1                                                   
147900     SEARCH GODK-STATUS                                                   
148000       AT END                                                             
148100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
148200           DELIMITED BY SIZE INTO FELTEXT                                 
148300         DISPLAY FELTEXT                                                  
148400         CALL FELLOG                                                      
148500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
148600         CONTINUE                                                         
148700     END-SEARCH                                                           
148800     .                                                                    
