000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1029100.                                                
000400 AUTHOR.         HENRIK ARONSSON.                                         
000500 DATE-WRITTEN.   SEPTEMBER 1990.                                          
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        - PROGRAMMET ÄR ETT BAKGRUNDSPROGRAM TILL BILD 1221              
001000*          (STRUKTUR INGÅR I) OCH SKÖTER OM PRINT-FUNKTIONEN.             
001100*                                                                         
001200*        PROGRAMMET LÄSER WLSATB (WDJ1)                                   
001300*        PROGRAMMET LÄSER WLBENA (WDD3)                                   
001400*        PROGRAMMET LÄSER WLARTC (WDK6)                                   
001500*                                                                         
001600*                                                                         
001700*    SUBPROGRAM:                                                          
001800*        W006PRS1 - SKÖTER ALL SKRIVNING MOT IMS-PRINTER                  
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W1T291U                                             
002200*        MID:         W1I22101                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200*    -COPY WY2000W1                                                       
003300     SKIP3                                                                
003400 77  IDPGM                       PIC X(08)   VALUE 'W1022100'.            
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FÖR PRINTERRADER                                           
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  MAX-INDX                    PIC S9(4)  VALUE +31   COMP SYNC.        
004200                                                                          
004300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN BILD 1221              
004600 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
004700 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004800 77  WS-IDARTNR-2                PIC X(9)    VALUE SPACE.                 
004900 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
005000 77  WS-IDARTNR-KONVERTERAT      PIC 9(9)    VALUE ZERO.                  
005100 77  WS-BELEVART                 PIC X(30)   VALUE SPACE.                 
005200 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
005300 77  WS-1002-SATS                PIC X(1)    VALUE SPACE.                 
005400 77  WS-KDPRODSL                 PIC X(2)    VALUE SPACE.                 
005500 77  WS-KDPRODSL-NUM             PIC 9(2)    VALUE ZERO.                  
005600 77  WS-IDRADNR                  PIC X(4)    VALUE SPACE.                 
005700                                                                          
005800*01  -COPY WWPRODSL                                                       
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006500     88  NYCKLAR-OK                          VALUE 'J'.                   
006600     88  NYCKLAR-FEL                         VALUE 'N'.                   
006700                                                                          
006800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006900     88  ALLT-OK                             VALUE 'J'.                   
007000                                                                          
007100 77  RAD-SW                      PIC X       VALUE 'J'.                   
007200     88  RAD-OK                              VALUE 'J'.                   
007300     88  RAD-EJ-OK                           VALUE 'N'.                   
007400                                                                          
007500 77  SOEKNYCKELTYP-SW            PIC X       VALUE SPACE.                 
007600     88  SOEKNYCKEL-IDARTNR                  VALUE 'A'.                   
007700     88  SOEKNYCKEL-IDLEVNR-O-BELEVART       VALUE 'I'.                   
007800                                                                          
007900 77  STRUKTURNR-FINNS-PAA-WDK6-SW PIC X.                                  
008000     88 STRUKTURNR-FINNS-PAA-WDK6            VALUE 'J'.                   
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  EGEN-MID                            VALUE '1221'.                
008400     88  GODK-MID                            VALUE '1221'.                
008500     EJECT                                                                
008600*    --- DIVERSE VARIABLER                                                
008700                                                                          
008800 01  WS-MIN-IDARTNR-KONV         PIC S9(9)   VALUE +100000000.            
008900                                                                          
009000 01  IDARTNR                     PIC X       VALUE 'A'.                   
009100 01  IDLEVNR-O-BELEVART          PIC X       VALUE 'I'.                   
009200                                                                          
009300 01  WS-BEART                    PIC X(25)   VALUE SPACE.                 
009400 01  WS-TEARTNOT                 PIC X(40)   VALUE SPACE.                 
009500 01  WS-TEARTNOT-7               PIC X(40)   VALUE SPACE.                 
009600 01  WS-KDBENHOM                 PIC S9(1).                               
009700                                                                          
009800 77  SATS-I-STR-RA           PIC X(8)    VALUE '242     '.                
009900 77  SATS-I-STR-RB           PIC X(8)    VALUE '243     '.                
010000 77  SATS-I-STR-BERPV        PIC X(8)    VALUE '246     '.                
010100 77  SATS-I-STR-CARP         PIC X(8)    VALUE '241     '.                
010200                                                                          
010300 01  DAGENS-DATUM                PIC 9(6).                                
010400                                                                          
010500 01  FILLER REDEFINES DAGENS-DATUM.                                       
010600     03 AAR                      PIC 9(2).                                
010700     03 MAN                      PIC 9(2).                                
010800     03 DAG                      PIC 9(2).                                
010900                                                                          
011000 01  TID                         PIC 9(8).                                
011100                                                                          
011200 01  FILLER REDEFINES TID.                                                
011300     03 TIM                      PIC 9(2).                                
011400     03 MIN                      PIC 9(2).                                
011500     03 FILLER                   PIC 9(4).                                
011600                                                                          
011700 01  WS-SPAR-SPAERWDATUM         PIC 9(6).                                
011800                                                                          
011900 01  WS-PRINTER                  PIC X(8).                                
012000 01  WS-LIST-RAD                 PIC X(132)  VALUE SPACE.                 
012100 01  WS-DUMMY                    PIC X(132)  VALUE SPACE.                 
012200                                                                          
012300     EJECT                                                                
012400*    --- LISTRADER                                                        
012500                                                                          
012600 01  RUBRIK-RAD.                                                          
012700     05  FILLER        PIC X(4)    VALUE SPACE.                           
012800     05  FILLER        PIC X(10)   VALUE '1 2 2 1   '.                    
012900     05  FILLER        PIC X(16)   VALUE 'STRUKTUR INGÅR I'.              
013000     05  FILLER        PIC X(56)   VALUE SPACE.                           
013100     05  FILLER        PIC X(7)    VALUE 'DATUM  '.                       
013200     05  LIST-AAR      PIC X(2).                                          
013300     05  FILLER        PIC X       VALUE '/'.                             
013400     05  LIST-MAN      PIC X(2).                                          
013500     05  FILLER        PIC X       VALUE '/'.                             
013600     05  LIST-DAG      PIC X(2).                                          
013700     05  FILLER        PIC X(5)    VALUE SPACE.                           
013800     05  FILLER        PIC X(5)    VALUE 'TID  '.                         
013900     05  LIST-TIM      PIC X(2).                                          
014000     05  FILLER        PIC X       VALUE ':'.                             
014100     05  LIST-MIN      PIC X(2).                                          
014200                                                                          
014300                                                                          
014400 01  UNDER-RUBRIK1-RAD.                                                   
014500     05  FILLER        PIC X(4)    VALUE SPACE.                           
014600     05  FILLER        PIC X(6)    VALUE 'LEVNR '.                        
014700     05  LIST-IDLEVNR-IN PIC X(5).                                        
014800     05  FILLER        PIC X       VALUE SPACE.                           
014900     05  FILLER        PIC X(13)   VALUE 'ARTNR/LEVBET '.                 
015000     05  LIST-IDART-BE PIC X(30).                                         
015100     05  FILLER        PIC X       VALUE SPACE.                           
015200     05  FILLER        PIC X(6)    VALUE 'SPRAK '.                        
015300     05  LIST-IDSKYLT  PIC X(3).                                          
015400     05  FILLER        PIC X       VALUE SPACE.                           
015500     05  FILLER        PIC X(5)    VALUE '1002 '.                         
015600     05  LIST-1002     PIC X(3).                                          
015700     05  FILLER        PIC X       VALUE SPACE.                           
015800     05  FILLER        PIC X(3)    VALUE 'PS '.                           
015900     05  LIST-KDPRODSL-IN PIC Z(2) VALUE ZERO.                            
016000     05  FILLER        PIC X(32)   VALUE SPACE.                           
016100                                                                          
016200 01  UNDER-RUBRIK2-RAD.                                                   
016300     05  FILLER        PIC X(4)    VALUE SPACE.                           
016400     05  FILLER        PIC X(10)   VALUE 'BENÄMNING '.                    
016500     05  LIST-BEART-UT PIC X(25).                                         
016600     05  FILLER        PIC X(77)   VALUE SPACE.                           
016700                                                                          
016800 01  UNDER-RUBRIK3-RAD.                                                   
016900     05  FILLER        PIC X(4)    VALUE SPACE.                           
017000     05  FILLER        PIC X(10)   VALUE 'BERNOT    '.                    
017100     05  LIST-TEARTNOT PIC X(40).                                         
017200     05  FILLER        PIC X(7)    VALUE SPACE.                           
017300     05  LIST-TEARTN7  PIC X(40).                                         
017400     05  FILLER        PIC X(15)   VALUE SPACE.                           
017500                                                                          
017600 01  UNDER-RUBRIK4-RAD.                                                   
017700     05  FILLER        PIC X(4)    VALUE SPACE.                           
017800     05  FILLER        PIC X(7)    VALUE 'RAD'.                           
017900     05  FILLER        PIC X(13)   VALUE 'STRUKTURNR'.                    
018000     05  FILLER        PIC X(28)   VALUE 'BENÄMNING'.                     
018100     05  FILLER        PIC X(9)    VALUE 'ANTAL'.                         
018200     05  FILLER        PIC X(5)    VALUE 'ST'.                            
018300     05  FILLER        PIC X(5)    VALUE 'EK '.                           
018400     05  FILLER        PIC X(8)    VALUE 'LEVNR'.                         
018500     05  FILLER        PIC X(9)    VALUE 'PRODSL'.                        
018600     05  FILLER        PIC X(20)   VALUE 'FÖRSTA INLEVERANS'.             
018700     05  FILLER        PIC X(8)    VALUE 'BEREDARE'.                      
018800                                                                          
018900 01  LIST-RAD.                                                            
019000     05  FILLER        PIC X(4)    VALUE SPACE.                           
019100     05  LIST-IDRADNR  PIC Z(3)9.                                         
019200     05  FILLER        PIC X(3)    VALUE SPACE.                           
019300     05  LIST-IDARTNR  PIC Z(8)9.                                         
019400     05  FILLER        PIC X(4)    VALUE SPACE.                           
019500     05  LIST-BEART    PIC X(25).                                         
019600     05  FILLER        PIC X(3)    VALUE SPACE.                           
019700     05  LIST-REANTPSA PIC Z9.9(3).                                       
019800     05  FILLER        PIC X(4)    VALUE SPACE.                           
019900     05  LIST-IDSTRTYP PIC X.                                             
020000     05  FILLER        PIC X(3)    VALUE SPACE.                           
020100     05  LIST-KDERS    PIC Z9.                                            
020200     05  FILLER        PIC X(3)    VALUE SPACE.                           
020300     05  LIST-IDLEVNR  PIC X(5).                                          
020400     05  FILLER        PIC X(3)    VALUE SPACE.                           
020500     05  LIST-KDPRODSL PIC Z9.                                            
020600     05  FILLER        PIC X(7)    VALUE SPACE.                           
020700     05  LIST-TIFINLV  PIC Z(4)9.                                         
020800     05  FILLER        PIC X(15)   VALUE SPACE.                           
020900     05  LIST-IDBERED  PIC Z(2)9.                                         
021000     05  FILLER        PIC X(5)    VALUE SPACE.                           
021100                                                                          
021200 01  BLANK-RAD.                                                           
021300     05  FILLER        PIC X(116)  VALUE SPACE.                           
021400                                                                          
021500     EJECT                                                                
021600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021700 01  GENERELLA-SUBPROGRAM.                                                
021800     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
021900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
022000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
022100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
022200     SKIP3                                                                
022300*   -COPY WMEDAREA                                                        
022400     EJECT                                                                
022500     SKIP3                                                                
022600*   -COPY WWLAND03                                                        
022700     EJECT                                                                
022800     SKIP3                                                                
022900     SKIP3                                                                
023000*   -COPY W006PRAR                                                        
023100     EJECT                                                                
023200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023300*                                                                         
023400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023500     SKIP3                                                                
023600*01  MID -COPY W1I22101                                                   
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023900     SKIP3                                                                
024000*01  -COPY WMSGAREA                                                       
024100     EJECT                                                                
024200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024300     SKIP3                                                                
024400*01  -COPY WMFSAREA                                                       
024500     EJECT                                                                
024600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024700*                                                                         
024800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024900     SKIP3                                                                
025000 01  NYCKLAR-TILL-DLI.                                                    
025100     03  W-WDJ1CSEQ-X.                                                    
025200         05  W-IDLEVNR-X.                                                 
025300             07   W-IDLEVNR      PIC X(5)  VALUE SPACE.                   
025400         05  W-BELEVART-X.                                                
025500             07   W-BELEVART     PIC X(30)  VALUE SPACE.                  
025600         05  W-IDARTNR-X.                                                 
025700             07   W-IDARTNR      PIC S9(9)  VALUE ZERO COMP-3.            
025800                                                                          
025900     03  W-WDJ111KY-X.                                                    
026000         05  W-KDSTRRAD          PIC X      VALUE SPACE.                  
026100         05  W-IDRADNR           PIC S9(5)  VALUE ZERO COMP-3.            
026200                                                                          
026300     03  W-IDARTNR-2-X.                                                   
026400         05  W-IDARTNR-2         PIC S9(9)   VALUE ZERO COMP-3.           
026500                                                                          
026600     03  W-BEART-X.                                                       
026700         05  W-BEART             PIC X(25)  VALUE SPACE.                  
026800                                                                          
026900     03  W-LOW-VALUE-X.                                                   
027000         05  W-LOW-VALUE         PIC X(4)   VALUE SPACE.                  
027100                                                                          
027200     03  W-IDSKYLT-X.                                                     
027300         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
027400                                                                          
027500     03  W-KDNOTTYP-X.                                                    
027600         05  W-KDNOTTYP          PIC S9(1)  VALUE ZERO COMP-3.            
027700*    --- STATUS-KOD FRÅN IMS                                              
027800 01  STATUS-WS                   PIC XX.                                  
027900     88  SEGMENT-FINNS                       VALUE '  '.                  
028000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028200     88  SEGMENT-HOEGRE                      VALUE 'GB'.                  
028300     SKIP2                                                                
028400 01  GODK-STATUSKODER.                                                    
028500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028600     SKIP3                                                                
028700 01  SSA1                        PIC X(96).                               
028800 01  SSA2                        PIC X(96).                               
028900 01  SSA3                        PIC X(64).                               
029000     EJECT                                                                
029100*    --- IMS FUNKTIONSKODER                                               
029200*01  -COPY W0003                                                          
029300     EJECT                                                                
029400*    ---  DLI INPUT-OUTPUT AREA                                           
029500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
029600     SKIP3                                                                
029700 01  DLI-IO-AREA.                                                         
029800     03  IO-AREA                 PIC X(928)  VALUE SPACE.                 
029900     SKIP3                                                                
030000     03  WLARTC01 REDEFINES IO-AREA.                                      
030100*        05  -COPY WDK601                                                 
030200     SKIP3                                                                
030300     03  WLARTC11 REDEFINES IO-AREA.                                      
030400*        05  -COPY WDK611                                                 
030500     SKIP3                                                                
030600     03  WLARTC11 REDEFINES IO-AREA.                                      
030700*        05  -COPY WDK625                                                 
030800     SKIP3                                                                
030900     03  WLBENA01 REDEFINES IO-AREA.                                      
031000*        05  -COPY WDD301     -PRE BENA01-                                
031100     SKIP3                                                                
031200     03  WLBENA11 REDEFINES IO-AREA.                                      
031300*        05  -COPY WDD311     -PRE BENA11-                                
031400                                                                          
031500     EJECT                                                                
031600*    ---  DLI INPUT-OUTPUT AREA-2                                         
031700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-2'.         
031800     SKIP3                                                                
031900 01  DLI-IO-AREA-2.                                                       
032000     03  IO-AREA-2               PIC X(350)  VALUE SPACE.                 
032100     SKIP3                                                                
032200     03  WLSATB-CSEQ REDEFINES IO-AREA-2.                                 
032300         05 WLSATB11.                                                     
032400*            07 -COPY WDJ111     -PRE SATB11C-                            
032500         05 WLSATB01.                                                     
032600*            07 -COPY WDJ101     -PRE SATB01C-                            
032700     EJECT                                                                
032800*    ---  DLI INPUT-OUTPUT AREA-3                                         
032900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-3'.         
033000     SKIP3                                                                
033100 01  DLI-IO-AREA-3.                                                       
033200     03  IO-AREA-3               PIC X(110)  VALUE SPACE.                 
033300     SKIP3                                                                
033400     03  WLARTC01 REDEFINES IO-AREA-3.                                    
033500*        05  -COPY WDK601     -PRE ARTC2-                                 
033600     SKIP3                                                                
033700     EJECT                                                                
033800 LINKAGE SECTION.                                                         
033900*01  -COPY W0009      -PRE MSG-                                           
034000                                                                          
034100*01  -COPY W0009      -PRE ALT-                                           
034200     EJECT                                                                
034300*01  -COPY W0008      -PRE ARTC2-                                         
034400     05  FILLER                  PIC X.                                   
034500                                                                          
034600*01  -COPY W0008      -PRE ARTC-                                          
034700     05  FILLER                  PIC X.                                   
034800     EJECT                                                                
034900*01  -COPY W0008      -PRE SATB-C-                                        
035000     05  FILLER                  PIC X.                                   
035100                                                                          
035200*01  -COPY W0008      -PRE BENA-A-                                        
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500*01  -COPY W0008      -PRE BENA-B-                                        
035600     05  FILLER                  PIC X.                                   
035700     EJECT                                                                
035800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ARTC2-PCB ARTC-PCB             
035900                                SATB-C-PCB BENA-A-PCB BENA-B-PCB.         
036000 MAIN SECTION.                                                            
036100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ARTC2-PCB ARTC-PCB             
036200                                SATB-C-PCB BENA-A-PCB BENA-B-PCB.         
036300                                                                          
036400     PERFORM IMS-GET-MSG                                                  
036500     IF SEGMENT-FINNS                                                     
036600       PERFORM A-INIT                                                     
036700       PERFORM B-KOLLA-NYCKLAR                                            
036800       IF NYCKLAR-OK                                                      
036900         PERFORM C-SKRIV-LISTA                                            
037000       END-IF                                                             
037100     END-IF                                                               
037200                                                                          
037300     MOVE ZERO TO RETURN-CODE                                             
037400     GOBACK                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 A-INIT SECTION.                                                          
037800                                                                          
037900     IF MSG-DUBBLA-TRANSKODER                                             
038000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I22101                 
038100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
038300     ELSE                                                                 
038400       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W1I22101                   
038500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
038600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038700     END-IF                                                               
038800                                                                          
038900     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
039000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039200                                                                          
039300     MOVE LOW-VALUE  TO MSG-AREA                                          
039400                                                                          
039500     ACCEPT TID          FROM TIME                                        
039600     ACCEPT DAGENS-DATUM FROM DATE                                        
039700     .                                                                    
039800     EJECT                                                                
039900 B-KOLLA-NYCKLAR SECTION.                                                 
040000                                                                          
040100     MOVE JA TO NYCKLAR-SW                                                
040200                                                                          
040300     MOVE MID-IDLEVNR-UT   TO WS-IDLEVNR                                  
040400     MOVE MID-BELEVART-UT  TO WS-BELEVART                                 
040500     MOVE MID-IDARTNR-UT   TO WS-IDARTNR                                  
040600     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
040700     MOVE MID-IDSKYLT-UT   TO WS-IDSKYLT                                  
040800     MOVE MID-1002-SATS-UT TO WS-1002-SATS                                
040900     MOVE MID-KDPRODSL-UT  TO WS-KDPRODSL                                 
041000     INSPECT WS-KDPRODSL REPLACING LEADING SPACE BY ZERO                  
041100                                                                          
041200     IF WS-1002-SATS = 'J' OR 'N' OR ' '                                  
041300       CONTINUE                                                           
041400     ELSE                                                                 
041500       MOVE NEJ TO NYCKLAR-SW                                             
041600     END-IF                                                               
041700                                                                          
041800     IF WS-KDPRODSL NUMERIC                                               
041900       IF WS-KDPRODSL = ZERO                                              
042000         CONTINUE                                                         
042100       ELSE                                                               
042200         MOVE WS-KDPRODSL TO TEST-KDPRODSL                                
042300         IF KDPRODSL-VOLVO-BIMA                                           
042400           CONTINUE                                                       
042500         ELSE                                                             
042600           MOVE NEJ TO NYCKLAR-SW                                         
042700         END-IF                                                           
042800       END-IF                                                             
042900     ELSE                                                                 
043000       MOVE NEJ TO NYCKLAR-SW                                             
043100     END-IF                                                               
043200                                                                          
043300     IF WS-IDSKYLT = SPACE                                                
043400       MOVE 'S' TO WS-IDSKYLT                                             
043500     END-IF                                                               
043600                                                                          
043700     SET WWLAND03-IX TO +1                                                
043800     SEARCH WWLAND03-IDSKYLT-RAD                                          
043900       AT END                                                             
044000         MOVE NEJ TO NYCKLAR-SW                                           
044100       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT                    
044200         CONTINUE                                                         
044300     END-SEARCH                                                           
044400                                                                          
044500**** IF WS-IDLEVNR(1:1) NOT = ' ' AND '0' AND '+'                         
044600       IF WS-IDLEVNR NOT = SPACE                                          
044700****   OM LEVNR > SPACE, ÄR SÖKNYCKELN IDLEVNR IHOP MED BELEVART          
044800         MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                      
044900         MOVE WS-IDLEVNR         TO W-IDLEVNR                             
045000         MOVE WS-BELEVART        TO W-BELEVART                            
045100         MOVE ZERO               TO W-IDARTNR                             
045200                                    WS-IDARTNR                            
045300       ELSE                                                               
045400         IF WS-IDLEVNR = SPACE                                            
045500******   OM IDLEVNR = SPACE, ÄR SÖKNYCKELN IDARTNR                        
045600           MOVE IDARTNR TO SOEKNYCKELTYP-SW                               
045700           IF (WS-IDARTNR NUMERIC) AND                                    
045800               (WS-IDARTNR > ZERO) AND                                    
045900               (WS-IDARTNR < WS-MIN-IDARTNR-KONV)                         
046000             MOVE WS-IDARTNR TO W-IDARTNR                                 
046100             MOVE SPACE      TO W-IDLEVNR                                 
046200                                W-BELEVART                                
046300                                WS-IDLEVNR                                
046400                                WS-BELEVART                               
046500           ELSE                                                           
046600             MOVE NEJ TO NYCKLAR-SW                                       
046700           END-IF                                                         
046800***      ELSE                                                             
046900***        MOVE NEJ TO NYCKLAR-SW                                         
047000         END-IF                                                           
047100       END-IF                                                             
047200***  ELSE                                                                 
047300***    MOVE NEJ                TO NYCKLAR-SW                              
047400***    MOVE IDLEVNR-O-BELEVART TO SOEKNYCKELTYP-SW                        
047500***  END-IF                                                               
047600                                                                          
047700     .                                                                    
047800     SKIP3                                                                
047900 C-SKRIV-LISTA SECTION.                                                   
048000                                                                          
048100     EVALUATE MID-KDPRTVAL                                                
048200       WHEN 'A'   MOVE SATS-I-STR-RA     TO WS-PRINTER                    
048300       WHEN 'B'   MOVE SATS-I-STR-RB     TO WS-PRINTER                    
048400       WHEN 'C'   MOVE SATS-I-STR-BERPV  TO WS-PRINTER                    
048500       WHEN 'D'   MOVE SATS-I-STR-CARP   TO WS-PRINTER                    
048600     END-EVALUATE                                                         
048700                                                                          
048800     PERFORM CA-LAES-RADDATA                                              
048900     IF RAD-OK                                                            
049000       PERFORM CB-OPEN-PRT                                                
049100       PERFORM CC-INITIERA-LISTA                                          
049200       PERFORM CD-REDIGERA-SKRIV-LISTRUBRIKER                             
049300       PERFORM UNTIL RAD-EJ-OK                                            
049400*******  TILLS RADER SAKNAS                                               
049500         PERFORM CE-REDIGERA-SKRIV-SIDRUBRIK                              
049600         MOVE +1 TO INDX                                                  
049700         PERFORM UNTIL INDX > MAX-INDX OR RAD-EJ-OK                       
049800           IF RAD-OK                                                      
049900             PERFORM CF-REDIGERA-SKRIV-RAD                                
050000             ADD 1 TO INDX                                                
050100             PERFORM CA-LAES-RADDATA                                      
050200           END-IF                                                         
050300         END-PERFORM                                                      
050400       END-PERFORM                                                        
050500       PERFORM CG-CLOSE-PRT                                               
050600     END-IF                                                               
050700     .                                                                    
050800     SKIP3                                                                
050900 CA-LAES-RADDATA SECTION.                                                 
051000                                                                          
051100     MOVE NEJ TO RAD-SW                                                   
051200                                                                          
051300     PERFORM IMS-GET-SATB-CSEQ-NEXT                                       
051400                                                                          
051500     PERFORM UNTIL SEGMENT-SAKNAS OR RAD-OK                               
051600                                                                          
051700       IF SEGMENT-FINNS                                                   
051800         IF SATB01C-STR-IDARTNR >= WS-MIN-IDARTNR-KONV                    
051900********** NÄR MAN 'FÅR IN' ETT KONVERTERAT STRUKTURNR                    
052000********** FINNS DET BARA KONV STRUKTURER KVAR OCH                        
052100********** MAN SÄTTER STATUS TILL SEGMENT-SAKNAS                          
052200           MOVE 'GE' TO STATUS-WS                                         
052300         ELSE                                                             
052400           IF SATB01C-STR-TIBORT > 0                                      
052500             PERFORM IMS-GET-SATB-CSEQ-NEXT                               
052600           ELSE                                                           
052700             MOVE SATB11C-RAD-TISTODAT   TO TMP1-YYMMDD                   
052800             MOVE DAGENS-DATUM           TO TMP2-YYMMDD                   
052900             PERFORM WY2000P1                                             
053000             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
053100               PERFORM CAA-KOLLA-OM-RAD-SKA-MED                           
053200               IF RAD-OK                                                  
053300                 CONTINUE                                                 
053400               ELSE                                                       
053500                 PERFORM IMS-GET-SATB-CSEQ-NEXT                           
053600               END-IF                                                     
053700             ELSE                                                         
053800               PERFORM IMS-GET-SATB-CSEQ-NEXT                             
053900             END-IF                                                       
054000           END-IF                                                         
054100         END-IF                                                           
054200       END-IF                                                             
054300                                                                          
054400     END-PERFORM                                                          
054500     .                                                                    
054600     SKIP3                                                                
054700 CAA-KOLLA-OM-RAD-SKA-MED SECTION.                                        
054800                                                                          
054900     MOVE NEJ         TO RAD-SW                                           
055000                         STRUKTURNR-FINNS-PAA-WDK6-SW                     
055100     MOVE WS-KDPRODSL TO WS-KDPRODSL-NUM                                  
055200                                                                          
055300     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR-2                              
055400     PERFORM IMS-GET-ARTC2-ARTC01                                         
055500     IF SEGMENT-FINNS                                                     
055600       MOVE JA TO STRUKTURNR-FINNS-PAA-WDK6-SW                            
055700     END-IF                                                               
055800                                                                          
055900     IF WS-1002-SATS = 'J'                                                
056000       IF SATB01C-STR-IDLEVNR = '1002 '                                   
056100         IF WS-KDPRODSL-NUM = 0                                           
056200           MOVE JA TO RAD-SW                                              
056300         ELSE                                                             
056400           IF STRUKTURNR-FINNS-PAA-WDK6                                   
056500             IF ARTC2-ART-KDPRODSL = WS-KDPRODSL-NUM                      
056600               MOVE JA TO RAD-SW                                          
056700             END-IF                                                       
056800           ELSE                                                           
056900             IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                    
057000               MOVE JA TO RAD-SW                                          
057100             END-IF                                                       
057200           END-IF                                                         
057300         END-IF                                                           
057400       END-IF                                                             
057500     ELSE                                                                 
057600       IF WS-1002-SATS = 'N'                                              
057700         IF SATB01C-STR-IDLEVNR NOT = '1002 '                             
057800           IF WS-KDPRODSL-NUM = 0                                         
057900             MOVE JA TO RAD-SW                                            
058000           ELSE                                                           
058100             IF STRUKTURNR-FINNS-PAA-WDK6                                 
058200               IF ARTC2-ART-KDPRODSL = WS-KDPRODSL-NUM                    
058300                 MOVE JA TO RAD-SW                                        
058400               END-IF                                                     
058500             ELSE                                                         
058600               IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                  
058700                 MOVE JA TO RAD-SW                                        
058800               END-IF                                                     
058900             END-IF                                                       
059000           END-IF                                                         
059100         END-IF                                                           
059200       ELSE                                                               
059300******** WS-1002-SATS = SPACE                                             
059400         IF WS-KDPRODSL-NUM = 0                                           
059500           MOVE JA TO RAD-SW                                              
059600         ELSE                                                             
059700           IF STRUKTURNR-FINNS-PAA-WDK6                                   
059800             IF ARTC2-ART-KDPRODSL = WS-KDPRODSL-NUM                      
059900               MOVE JA TO RAD-SW                                          
060000             END-IF                                                       
060100           ELSE                                                           
060200             IF SATB01C-STR-KDPRODSL = WS-KDPRODSL-NUM                    
060300               MOVE JA TO RAD-SW                                          
060400             END-IF                                                       
060500           END-IF                                                         
060600         END-IF                                                           
060700                                                                          
060800       END-IF                                                             
060900                                                                          
061000     END-IF                                                               
061100     .                                                                    
061200     EJECT                                                                
061300 CB-OPEN-PRT SECTION.                                                     
061400                                                                          
061500     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
061600                         PRT-OPEN                                         
061700                         WS-PRINTER                                       
061800                         ALT-PCB                                          
061900                         WS-DUMMY                                         
062000                         WS-DUMMY                                         
062100     .                                                                    
062200     SKIP3                                                                
062300 CC-INITIERA-LISTA SECTION.                                               
062400                                                                          
062500     MOVE AAR TO LIST-AAR                                                 
062600     MOVE MAN TO LIST-MAN                                                 
062700     MOVE DAG TO LIST-DAG                                                 
062800     MOVE TIM TO LIST-TIM                                                 
062900     MOVE MIN TO LIST-MIN                                                 
063000     .                                                                    
063100     SKIP3                                                                
063200 CD-REDIGERA-SKRIV-LISTRUBRIKER SECTION.                                  
063300                                                                          
063400***** SKRIV RUBRIK                                                        
063500     MOVE RUBRIK-RAD    TO WS-LIST-RAD                                    
063600     MOVE PRT-NYSIDA-RAD4    TO PRT-RADSKIP                               
063700     PERFORM S03-SKRIV-RAD                                                
063800                                                                          
063900***** SKRIV UNDERRUBRIK 1                                                 
064000     MOVE WS-IDLEVNR        TO LIST-IDLEVNR-IN                            
064100     IF SOEKNYCKEL-IDARTNR                                                
064200       INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE                 
064300       MOVE WS-IDARTNR      TO LIST-IDART-BE (1:9)                        
064400     ELSE                                                                 
064500       MOVE WS-BELEVART     TO LIST-IDART-BE                              
064600     END-IF                                                               
064700     MOVE WS-IDSKYLT        TO LIST-IDSKYLT                               
064800     MOVE WS-1002-SATS      TO LIST-1002                                  
064900     MOVE WS-KDPRODSL       TO LIST-KDPRODSL-IN                           
065000     MOVE UNDER-RUBRIK1-RAD TO WS-LIST-RAD                                
065100     MOVE PRT-AFTER-2       TO PRT-RADSKIP                                
065200     PERFORM S03-SKRIV-RAD                                                
065300                                                                          
065400     PERFORM CDA-HAEMTA-BEART-TEARTNOT                                    
065500                                                                          
065600***** SKRIV UNDERRUBRIK 2                                                 
065700     MOVE WS-BEART          TO LIST-BEART-UT                              
065800     MOVE UNDER-RUBRIK2-RAD TO WS-LIST-RAD                                
065900     MOVE PRT-AFTER-2       TO PRT-RADSKIP                                
066000     PERFORM S03-SKRIV-RAD                                                
066100                                                                          
066200***** SKRIV UNDERRUBRIK 3                                                 
066300     MOVE WS-TEARTNOT       TO LIST-TEARTNOT                              
066400     MOVE WS-TEARTNOT-7     TO LIST-TEARTN7                               
066500     MOVE UNDER-RUBRIK3-RAD TO WS-LIST-RAD                                
066600     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
066700     PERFORM S03-SKRIV-RAD                                                
066800     .                                                                    
066900     SKIP3                                                                
067000 CDA-HAEMTA-BEART-TEARTNOT SECTION.                                       
067100                                                                          
067200     MOVE WS-IDARTNR TO W-IDARTNR-2                                       
067300     PERFORM IMS-GET-ARTC-ARTC01                                          
067400     IF SEGMENT-FINNS                                                     
067500       MOVE 3 TO W-KDNOTTYP                                               
067600       PERFORM IMS-GET-ARTC-ARTC25                                        
067700       IF SEGMENT-FINNS                                                   
067800         MOVE NOT-TEARTNOT TO WS-TEARTNOT                                 
067900       ELSE                                                               
068000         MOVE SPACE        TO WS-TEARTNOT                                 
068100       END-IF                                                             
068200       MOVE 7 TO W-KDNOTTYP                                               
068300       PERFORM IMS-GET-ARTC-ARTC25                                        
068400       IF SEGMENT-FINNS                                                   
068500         MOVE NOT-TEARTNOT TO WS-TEARTNOT-7                               
068600       ELSE                                                               
068700         MOVE SPACE        TO WS-TEARTNOT-7                               
068800       END-IF                                                             
068900       PERFORM S01-HAEMTA-BEART-BSEQ                                      
069000     ELSE                                                                 
069100       IF WS-IDSKYLT = 'S  '                                              
069200         MOVE SATB11C-RAD-BEART-SVE TO WS-BEART                           
069300       ELSE                                                               
069400         MOVE SATB11C-RAD-BEART-SVE TO W-BEART                            
069500         MOVE SATB11C-RAD-KDBENHOM  TO WS-KDBENHOM                        
069600         PERFORM S02-HAEMTA-BEART-ASEQ                                    
069700       END-IF                                                             
069800                                                                          
069900       MOVE SPACE TO WS-TEARTNOT                                          
070000                     WS-TEARTNOT-7                                        
070100     END-IF                                                               
070200     .                                                                    
070300     SKIP3                                                                
070400 CE-REDIGERA-SKRIV-SIDRUBRIK SECTION.                                     
070500                                                                          
070600***** SKRIV UNDERRUBRIK 4                                                 
070700     MOVE UNDER-RUBRIK4-RAD TO WS-LIST-RAD                                
070800     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
070900     PERFORM S03-SKRIV-RAD                                                
071000                                                                          
071100     MOVE BLANK-RAD         TO WS-LIST-RAD                                
071200     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
071300     PERFORM S03-SKRIV-RAD                                                
071400     .                                                                    
071500     SKIP3                                                                
071600 CF-REDIGERA-SKRIV-RAD SECTION.                                           
071700                                                                          
071800     MOVE SATB01C-STR-IDARTNR  TO LIST-IDARTNR                            
071900     MOVE SATB01C-STR-IDSTRTYP TO LIST-IDSTRTYP                           
072000     MOVE SATB11C-RAD-IDRADNR  TO LIST-IDRADNR                            
072100     MOVE SATB11C-RAD-REANTPSA TO LIST-REANTPSA                           
072200                                                                          
072300     MOVE SATB01C-STR-IDARTNR TO W-IDARTNR-2                              
072400     PERFORM IMS-GET-ARTC-ARTC01                                          
072500     IF SEGMENT-FINNS                                                     
072600****** OM DEN HITTADE STRUKTUREN FINNS PÅ ARTREG                          
072700       MOVE ART-IDLEVNR   TO LIST-IDLEVNR                                 
072800       MOVE ART-KDERS-UTG TO LIST-KDERS                                   
072900       MOVE ART-TIFINLV   TO LIST-TIFINLV                                 
073000       PERFORM IMS-GET-ARTC-ARTC11                                        
073100       IF SEGMENT-FINNS                                                   
073200         MOVE CLAG-IDBERED TO LIST-IDBERED                                
073300         MOVE CLAG-KDERS   TO LIST-KDERS                                  
073400       ELSE                                                               
073500         MOVE 0            TO LIST-IDBERED                                
073600       END-IF                                                             
073700       IF STRUKTURNR-FINNS-PAA-WDK6                                       
073800******   ARTC2-KDPRODSL LIGGER I IO-AREA-3 EFTER LÄSNING I CAA-           
073900                                                                          
074000         MOVE ARTC2-ART-KDPRODSL  TO LIST-KDPRODSL                        
074100       ELSE                                                               
074200         MOVE ZERO                TO LIST-KDPRODSL                        
074300       END-IF                                                             
074400       PERFORM S01-HAEMTA-BEART-BSEQ                                      
074500     ELSE                                                                 
074600       MOVE SATB01C-STR-KDPRODSL TO LIST-KDPRODSL                         
074700       MOVE SPACE                TO LIST-IDLEVNR                          
074800       MOVE ZERO                 TO LIST-KDERS                            
074900                                    LIST-TIFINLV                          
075000                                    LIST-IDBERED                          
075100       IF WS-IDSKYLT = 'S  '                                              
075200         MOVE SATB01C-STR-BEART-SVE TO WS-BEART                           
075300       ELSE                                                               
075400         MOVE SATB01C-STR-BEART-SVE TO W-BEART                            
075500         MOVE SATB01C-STR-KDBENHOM  TO WS-KDBENHOM                        
075600         PERFORM S02-HAEMTA-BEART-ASEQ                                    
075700       END-IF                                                             
075800     END-IF                                                               
075900     MOVE WS-BEART TO LIST-BEART                                          
076000                                                                          
076100     MOVE LIST-RAD          TO WS-LIST-RAD                                
076200     MOVE PRT-AFTER-1       TO PRT-RADSKIP                                
076300     PERFORM S03-SKRIV-RAD                                                
076400     .                                                                    
076500     SKIP3                                                                
076600 CG-CLOSE-PRT SECTION.                                                    
076700                                                                          
076800     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
076900                         PRT-CLOSE                                        
077000                         WS-PRINTER                                       
077100                         ALT-PCB                                          
077200                         WS-DUMMY                                         
077300                         WS-DUMMY                                         
077400     .                                                                    
077500     SKIP3                                                                
077600 S01-HAEMTA-BEART-BSEQ SECTION.                                           
077700                                                                          
077800     PERFORM IMS-GET-BENA-BENA01-BSEQ                                     
077900     IF SEGMENT-FINNS                                                     
078000       MOVE WS-IDSKYLT TO W-IDSKYLT                                       
078100       PERFORM IMS-GET-BENA-BENA11-BSEQ                                   
078200       IF SEGMENT-FINNS                                                   
078300         MOVE BENA11-TEXT-BEART TO WS-BEART                               
078400       ELSE                                                               
078500         MOVE SPACE TO WS-BEART                                           
078600       END-IF                                                             
078700     ELSE                                                                 
078800       MOVE SPACE TO WS-BEART                                             
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200 S02-HAEMTA-BEART-ASEQ SECTION.                                           
079300                                                                          
079400     MOVE 'S' TO W-IDSKYLT                                                
079500     PERFORM IMS-GET-BENA-BENA01-ASEQ                                     
079600     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
079700                    WS-KDBENHOM = BENA01-BEN-KDHOMONYM                    
079800       IF SEGMENT-FINNS                                                   
079900         IF WS-KDBENHOM = BENA01-BEN-KDHOMONYM                            
080000           CONTINUE                                                       
080100         ELSE                                                             
080200           PERFORM IMS-GET-BENA-BENA01-ASEQ-NEXT                          
080300         END-IF                                                           
080400       END-IF                                                             
080500     END-PERFORM                                                          
080600                                                                          
080700     IF SEGMENT-FINNS                                                     
080800       MOVE WS-IDSKYLT TO W-IDSKYLT                                       
080900       PERFORM IMS-GET-BENA-BENA11-ASEQ                                   
081000       IF SEGMENT-FINNS                                                   
081100         MOVE BENA11-TEXT-BEART TO WS-BEART                               
081200       ELSE                                                               
081300         MOVE SPACE TO WS-BEART                                           
081400       END-IF                                                             
081500     ELSE                                                                 
081600       MOVE SPACE TO WS-BEART                                             
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000 S03-SKRIV-RAD SECTION.                                                   
082100                                                                          
082200     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
082300                         PRT-WRITE                                        
082400                         WS-PRINTER                                       
082500                         ALT-PCB                                          
082600                         PRT-RADSKIP                                      
082700                         WS-LIST-RAD                                      
082800      .                                                                   
082900      EJECT                                                               
083000* --- IMS SEKTIONER ---                                                   
083100     SKIP3                                                                
083200 IMS-GET-MSG SECTION.                                                     
083300                                                                          
083400     MOVE '  QC' TO GODK-STATUSKODER                                      
083500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
083600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083700     PERFORM IMS-STATUSKONTROLL                                           
083800     .                                                                    
083900     SKIP3                                                                
084000 IMS-GET-SATB-CSEQ-NEXT SECTION.                                          
084100                                                                          
084200     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
084300          DELIMITED BY SIZE INTO SSA1                                     
084400     MOVE 'WLSATB01 ' TO SSA2                                             
084500     MOVE '  GE' TO GODK-STATUSKODER                                      
084600     CALL CBLTDLI USING GN SATB-C-PCB DLI-IO-AREA-2 SSA1 SSA2             
084700     MOVE SATB-C-STATUS-CODE TO STATUS-WS                                 
084800     PERFORM IMS-STATUSKONTROLL                                           
084900     .                                                                    
085000     SKIP3                                                                
085100 IMS-GET-ARTC2-ARTC01 SECTION.                                            
085200                                                                          
085300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-2-X ')'                       
085400          DELIMITED BY SIZE INTO SSA1                                     
085500     MOVE '  GE' TO GODK-STATUSKODER                                      
085600     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA-3 SSA1                   
085700     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
085800     PERFORM IMS-STATUSKONTROLL                                           
085900     .                                                                    
086000     EJECT                                                                
086100 IMS-GET-ARTC-ARTC01 SECTION.                                             
086200                                                                          
086300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-2-X ')'                       
086400          DELIMITED BY SIZE INTO SSA1                                     
086500     MOVE '  GE' TO GODK-STATUSKODER                                      
086600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
086700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
086800     PERFORM IMS-STATUSKONTROLL                                           
086900     .                                                                    
087000     SKIP3                                                                
087100 IMS-GET-ARTC-ARTC11 SECTION.                                             
087200                                                                          
087300     MOVE 'WLARTC11 ' TO SSA1                                             
087400     MOVE '  GE' TO GODK-STATUSKODER                                      
087500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
087600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
087700     PERFORM IMS-STATUSKONTROLL                                           
087800     .                                                                    
087900     EJECT                                                                
088000 IMS-GET-ARTC-ARTC25 SECTION.                                             
088100                                                                          
088200     MOVE 'WLARTC11 ' TO SSA1                                             
088300     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
088400          DELIMITED BY SIZE INTO SSA2                                     
088500     MOVE '  GE' TO GODK-STATUSKODER                                      
088600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
088700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
088800     PERFORM IMS-STATUSKONTROLL                                           
088900     .                                                                    
089000     EJECT                                                                
089100 IMS-GET-BENA-BENA01-ASEQ SECTION.                                        
089200                                                                          
089300     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
089400                                  W-BEART-X ')'                           
089500          DELIMITED BY SIZE INTO SSA1                                     
089600     MOVE '  GE' TO GODK-STATUSKODER                                      
089700     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA SSA1                    
089800     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
089900     PERFORM IMS-STATUSKONTROLL                                           
090000     .                                                                    
090100     SKIP3                                                                
090200 IMS-GET-BENA-BENA01-ASEQ-NEXT SECTION.                                   
090300                                                                          
090400     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
090500                                  W-BEART-X ')'                           
090600          DELIMITED BY SIZE INTO SSA1                                     
090700     MOVE '  GE' TO GODK-STATUSKODER                                      
090800     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA SSA1                    
090900     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
091000     PERFORM IMS-STATUSKONTROLL                                           
091100     .                                                                    
091200     SKIP3                                                                
091300 IMS-GET-BENA-BENA11-ASEQ SECTION.                                        
091400                                                                          
091500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
091600          DELIMITED BY SIZE INTO SSA1                                     
091700     MOVE '  GE' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA SSA1                   
091900     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     EJECT                                                                
092300 IMS-GET-BENA-BENA01-BSEQ SECTION.                                        
092400                                                                          
092500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-2-X ')'                       
092600          DELIMITED BY SIZE INTO SSA1                                     
092700     MOVE '  GE' TO GODK-STATUSKODER                                      
092800     CALL CBLTDLI USING GU BENA-B-PCB DLI-IO-AREA SSA1                    
092900     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
093000     PERFORM IMS-STATUSKONTROLL                                           
093100     .                                                                    
093200     SKIP3                                                                
093300 IMS-GET-BENA-BENA11-BSEQ SECTION.                                        
093400                                                                          
093500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
093600          DELIMITED BY SIZE INTO SSA1                                     
093700     MOVE '  GE' TO GODK-STATUSKODER                                      
093800     CALL CBLTDLI USING GNP BENA-B-PCB DLI-IO-AREA SSA1                   
093900     MOVE BENA-B-STATUS-CODE TO STATUS-WS                                 
094000     PERFORM IMS-STATUSKONTROLL                                           
094100     .                                                                    
094200     EJECT                                                                
094300 IMS-STATUSKONTROLL SECTION.                                              
094400                                                                          
094500     SET STATUS-IX TO 1                                                   
094600     SEARCH GODK-STATUS                                                   
094700       AT END CALL FELLOG                                                 
094800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
094900     END-SEARCH                                                           
095000     .                                                                    
095100     EJECT                                                                
095200*    -COPY WY2000P1                                                       
