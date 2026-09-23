000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4079600.                                                
000400 AUTHOR.         EVA LUNDELL.                                             
000500 DATE-WRITTEN.   95/09/25.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BAKGRUNDS-MPP FÖR INTERNUPPACKNING.                              
001100*        SKAPAR LEVERANSANMÄRKNING KOD 97 VIA PROGRAM                     
001200*        4791 SOM LÄGGER UPP TRANSARNA PÅ WDA2 VIA                        
001300*        DISPATCHEN.                                                      
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDE4                                       
001600*        PROGRAMMET LÄSER      WDE6                                       
001700*        PROGRAMMET LÄSER      WDK6                                       
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T796                                              
002100*        MID:         W4I79601                                            
002200*                     W4I79101                                            
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)  VALUE 'W4079600'.             
003300 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
003400 77  JA                          PIC X      VALUE 'J'.                    
003500 77  NEJ                         PIC X      VALUE 'N'.                    
003600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
003700 77  RAD-IX                      PIC S9(3)  COMP-3 VALUE ZERO.            
003800 77  WS-RAD-IX                   PIC 9(3)   VALUE ZERO.                   
003900 77  4791IX                      PIC S9(3)  COMP-3 VALUE +0.              
004000 77  MAX-4791IX                  PIC S9(3)  COMP-3 VALUE +4.              
004100 77  MAX-IX                      PIC S9(3)  COMP-3 VALUE +13.             
004200 77  ANTAL-ISRT                  PIC S9(3)  COMP-3 VALUE ZERO.            
004300 77  MAX-ANTAL-ISRT              PIC S9(3)  COMP-3 VALUE +25.             
004400 77  WS-KOLLI-MIN                PIC S9(5)  COMP-3 VALUE ZERO.            
004500 77  WS-KOLLI-MAX                PIC S9(5)  COMP-3 VALUE ZERO.            
004600 77  SPARA-IDKOLLI               PIC 9(5)   VALUE ZERO.                   
004700 77  WS-IDPLKLST                 PIC 9(3)   VALUE ZERO.                   
004800 77  WS-IDPURAD                  PIC 9(5)   VALUE ZERO.                   
004900 77  WS-IDKOLLI                  PIC 9(5)   VALUE ZERO.                   
005000 77  WS-IDKOLLI-MID              PIC 9(5)   VALUE ZERO.                   
005100 77  WS-IDDISTR-MID              PIC 9(5)   VALUE ZERO.                   
005200 77  SPARA-IDPURAD               PIC S9(5)  COMP-3 VALUE ZERO.            
005300 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
005400 77  DAGENS-TID                  PIC 9(8)   VALUE ZERO.                   
005500 77  WS-KOLLI-TIFAKT             PIC 9(6)   VALUE ZERO.                   
005600 77  WS-PRARTBTO                 PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
005700 77  SPARA-PRARTBTO              PIC 9(7).9(2) VALUE ZERO.                
005800 77  SPARA-PRARTBTO-LOC          PIC 9(7).9(2) VALUE ZERO.                
005900 77  SPARA-IDARTNR               PIC 9(9)   VALUE ZERO.                   
006000 77  SPARA-KVLEVART              PIC 9(6)   VALUE ZERO.                   
006100 77  SPARA-PRARTSTD              PIC 9(7).9(2) VALUE ZERO.                
006200 77  SPARA-PRARTSJK              PIC 9(7).9(2) VALUE ZERO.                
006300                                                                          
006600 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
006800                                                                          
006900 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
007000     88  OMSTART                             VALUE 'J'.                   
007100                                                                          
007200 77  RADER-KVAR-SW               PIC X       VALUE 'J'.                   
007300     88  RADER-KVAR                          VALUE 'J'.                   
007400     88  INGA-RADER-KVAR                     VALUE 'N'.                   
007500                                                                          
007600 77  FLERA-KOLLI-SW              PIC X       VALUE 'J'.                   
007700     88  KOLLI-FINNS                         VALUE 'J'.                   
007800     88  KOLLI-SLUT                          VALUE 'N'.                   
007900     EJECT                                                                
008000*    --- HÄMTA MARKNADSBOLAGSVALUTA                                       
008400                                                                          
008500 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
008600*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
008700     EJECT                                                                
008800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008900 01  GENERELLA-SUBPROGRAM.                                                
009000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009600     03  W335COST                PIC X(8)    VALUE 'W335COST'.            
009700     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010000*01 -COPY WMEDAREA                                                        
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
010300*01 -COPY WDATAREA                                                        
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM W335COST                              
010600 01 FILLER                       PIC X(8)    VALUE 'W335COST'.            
010700*   -COPY W335COST                                                        
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
011000 01 FILLER                       PIC X(8)    VALUE 'W335CURR'.            
011100*   -COPY W335CURR                                                        
011200     EJECT                                                                
011300*01 -COPY WMSGINIT                                                        
011400     SKIP3                                                                
011500*    --- PROGRAMHOPP AREAOR.                                              
011600*                                                                         
011700 01  FILLER                      PIC X(16)  VALUE 'P-TO-P-AREA'.          
011800     SKIP3                                                                
011900 01  FILLER                      PIC X(16)  VALUE '4791-MSG-AREA'.        
012000 01  4791-MSG-IO-AREA.                                                    
012100     03  4791-LL                 PIC S9(4)  VALUE +0 COMP SYNC.           
012200     03  4791-Z1                 PIC X.                                   
012300     03  4791-Z2                 PIC X.                                   
012400     03  4791-TRANSKOD           PIC X(8)   VALUE 'W4T791X '.             
012500     03  4791-IDTRANS            PIC X(4)   VALUE '4791'.                 
012600     03  4791-SPRAK              PIC X.                                   
012700     03  FILLER.                                                          
012800*       05 -COPY W4I79101    -PRE 4791-.                                  
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
013100*01  -COPY WMSGKOM                                                        
013200*                                                                         
013300 01  FILLER                      PIC X(16)  VALUE '4796-MID-AREA'.        
013400     SKIP3                                                                
013500*01  MID -COPY W4I79601                                                   
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013800     SKIP3                                                                
013900*01  -COPY WMSGAREA                                                       
014000     EJECT                                                                
014100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014400     SKIP3                                                                
014500 01  NYCKLAR-TILL-DLI.                                                    
014600     03  W-WDE401KY-X.                                                    
014700         05  W-IDGMTREF-X.                                                
014800             07  W-IDDISTR       PIC S9(5) COMP-3.                        
014900             07  W-IDKUNDNR      PIC S9(7) COMP-3.                        
015000             07  W-IDORDNR5      PIC 9(5).                                
015100             07  FILLER          PIC X(5)  VALUE SPACE.                   
015200         05  W-IDPRODNR          PIC S9(7) COMP-3.                        
015300         05  W-IDPLKLST          PIC S9(3) COMP-3.                        
015400                                                                          
015500     03  W-WDE4F1KY-X.                                                    
015600         05  W-IDPRODNR-E4F      PIC S9(7) COMP-3.                        
015700         05  W-IDKOLLI-E4F       PIC S9(5) COMP-3.                        
015800         05  W-IDGMTREF-E4F.                                              
015900             07  W-IDDISTR-E4F   PIC S9(5) COMP-3.                        
016000             07  W-IDKUNDNR-E4F  PIC S9(7) COMP-3.                        
016100             07  W-IDORDNR5-E4F  PIC 9(5).                                
016200             07  FILLER          PIC X(5)  VALUE SPACE.                   
016300         05  W-IDPURAD-E4F       PIC S9(5) COMP-3.                        
016400         05  W-IDPLKLST-E4F      PIC S9(3) COMP-3.                        
016500                                                                          
016600     03  W-WDE4F1KY-MIN-X.                                                
016700         05  W-IDPRODNR-MIN      PIC S9(7) COMP-3.                        
016800         05  W-IDKOLLI-MIN       PIC S9(5) COMP-3.                        
016900         05  W-IDGMTREF-MIN.                                              
017000             07  W-IDDISTR-MIN   PIC S9(5) COMP-3.                        
017100             07  W-IDKUNDNR-MIN  PIC S9(7) COMP-3.                        
017200             07  W-IDORDNR5-MIN  PIC 9(5).                                
017300             07  FILLER          PIC X(5)  VALUE SPACE.                   
017400         05  FILLER              PIC X(5).                                
017500                                                                          
017600     03  W-WDE4F1KY-MAX-X.                                                
017700         05  W-IDPRODNR-MAX      PIC S9(7) COMP-3.                        
017800         05  W-IDKOLLI-MAX       PIC S9(5) COMP-3.                        
017900         05  W-IDGMTREF-MAX.                                              
018000             07  W-IDDISTR-MAX   PIC S9(5) COMP-3.                        
018100             07  W-IDKUNDNR-MAX  PIC S9(7) COMP-3.                        
018200             07  W-IDORDNR5-MAX  PIC 9(5).                                
018300             07  FILLER          PIC X(5)  VALUE SPACE.                   
018400         05  FILLER              PIC X(5).                                
018500                                                                          
018600     03  W-IDPURAD-X.                                                     
018700         05  W-IDPURAD           PIC S9(5) COMP-3 VALUE ZERO.             
018800                                                                          
018900     03  W-IDPRODNR-X.                                                    
019000         05  W-IDPRODNR-WDE6     PIC S9(7) COMP-3 VALUE ZERO.             
019100                                                                          
019200     03  W-IDKOLLI-X.                                                     
019300         05  W-IDKOLLI           PIC S9(5) COMP-3 VALUE ZERO.             
019400                                                                          
019500     03  W-IDARTNR-X.                                                     
019600         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
019700                                                                          
019800     03  W-KDSEGKEY-X.                                                    
019900         05  W-KDSEGKEY          PIC X(1)  VALUE '1'.                     
020000                                                                          
022000     03  W-WDB101KY-X.                                                    
022100         05 W-IDPARTNR           PIC X(9).                                
022200         05 W-IDFTG              PIC 9(2).                                
022300                                                                          
022400     03  W-IDGMT-X.                                                       
022500         05 W-IDDISTR-WDB2       PIC S9(5)   COMP-3.                      
022600         05 W-IDKUNDNR-WDB2      PIC S9(7)   COMP-3.                      
022700                                                                          
022800     03  W-IDGMT-MIN-X.                                                   
022900         05 W-IDDISTR-WDB2-MIN   PIC S9(5)    COMP-3.                     
023000         05 W-IDKUNDNR-WDB2-MIN  PIC S9(7)    COMP-3.                     
023100                                                                          
023200     03  W-IDGMT-MAX-X.                                                   
023300         05 W-IDDISTR-WDB2-MAX   PIC S9(5)    COMP-3.                     
023400         05 W-IDKUNDNR-WDB2-MAX  PIC S9(7)    COMP-3.                     
023500                                                                          
023600*    --- STATUS-KOD FRÅN IMS                                              
023700 01  STATUS-WS                   PIC XX.                                  
023800     88  SEGMENT-FINNS                       VALUE '  '.                  
023900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024000     88  BASEN-SLUT                          VALUE 'GB'.                  
024100     SKIP2                                                                
024200 01  GODK-STATUSKODER.                                                    
024300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024400     SKIP3                                                                
024500 01  SSA1                        PIC X(124).                              
024600 01  SSA2                        PIC X(64).                               
024700     EJECT                                                                
024800*    --- IMS FUNKTIONSKODER                                               
024900*01  -COPY W0003                                                          
025000     EJECT                                                                
025100*    ---  DLI INPUT-OUTPUT AREA                                           
025200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E4F1'.         
025300 01  DLI-IO-E4F1.                                                         
025400*    03  -COPY WDE4F1                                                     
025500     EJECT                                                                
025600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E411'.         
025700 01  DLI-IO-E411.                                                         
025800*    03  -COPY WDE411                                                     
025900     EJECT                                                                
026000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
026100 01  DLI-IO-E601.                                                         
026200*    03  -COPY WDE601                                                     
026300     EJECT                                                                
026400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E611'.         
026500 01  DLI-IO-E611.                                                         
026600*    03  -COPY WDE611                                                     
026700     EJECT                                                                
026800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK601'.         
026900 01  DLI-IO-WDK601.                                                       
027000*    03  -COPY WDK601                                                     
027100     EJECT                                                                
027200 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK611'.         
027300 01  DLI-IO-WDK611.                                                       
027400*    03  -COPY WDK611                                                     
027500     EJECT                                                                
028400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB101'.           
028500 01  DLI-IO-WDB101.                                                       
028600*    03  -COPY WDB101.                                                    
028700     EJECT                                                                
028800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
028900 01  DLI-IO-WDB201.                                                       
029000*    03  -COPY WDB201.                                                    
029100     EJECT                                                                
029200 LINKAGE SECTION.                                                         
029300                                                                          
029400*01  -COPY W0009   -PRE MSG-                                              
029500*01  -COPY W0009   -PRE 4796-                                             
029600*01  -COPY W0009   -PRE 4791-                                             
029700*01  -COPY W0009   -PRE DISP-                                             
029800*01  -COPY W0008   -PRE USEA-                                             
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030100*01  -COPY W0008  -PRE WDE4F-                                             
030200     05  FILLER                  PIC X.                                   
030300     EJECT                                                                
030400*01  -COPY W0008  -PRE WDE6-                                              
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008  -PRE WDE4-                                              
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000*01  -COPY W0008  -PRE WDK6-                                              
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008  -PRE KOMM-                                              
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
032200*01  -COPY W0008  -PRE WDB1-                                              
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE WDB2-                                              
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800 01  COST-WDK6-PCB               PIC X.                                   
032900 01  COST-WDK7-PCB               PIC X.                                   
033000 01  COST-WDF1-PCB               PIC X.                                   
033100 01  COST-9305-PCB               PIC X.                                   
033200 01  COST-WDK72-PCB              PIC X.                                   
033300 01  COST-WDB6-PCB               PIC X.                                   
033500 PROCEDURE DIVISION  USING MSG-PCB  4796-PCB 4791-PCB DISP-PCB            
033600                          USEA-PCB WDE4F-PCB WDE6-PCB WDE4-PCB            
033700                          WDK6-PCB KOMM-PCB                               
033800                          WDB1-PCB WDB2-PCB                               
033900                          COST-WDK6-PCB                                   
034000                          COST-WDK7-PCB                                   
034100                          COST-WDF1-PCB                                   
034200                          COST-9305-PCB                                   
034300                          COST-WDK72-PCB COST-WDB6-PCB.                   
034500     ENTRY 'DLITCBL' USING MSG-PCB  4796-PCB 4791-PCB DISP-PCB            
034600                          USEA-PCB WDE4F-PCB WDE6-PCB WDE4-PCB            
034700                          WDK6-PCB KOMM-PCB                               
034800                          WDB1-PCB WDB2-PCB                               
034900                          COST-WDK6-PCB                                   
035000                          COST-WDK7-PCB                                   
035100                          COST-WDF1-PCB                                   
035200                          COST-9305-PCB                                   
035300                          COST-WDK72-PCB COST-WDB6-PCB.                   
035500     PERFORM IMS-GET-MSG                                                  
035600     IF SEGMENT-FINNS                                                     
035700       PERFORM A-INIT                                                     
035800       PERFORM B-KOLLA-NYCKLAR                                            
035900       PERFORM F-LAES-SKAPA-RADER                                         
036000                                                                          
036100       IF ANTAL-ISRT = MAX-ANTAL-ISRT AND RADER-KVAR                      
036200           MOVE W-IDKOLLI           TO WS-IDKOLLI                         
036300           MOVE WS-IDKOLLI          TO MID-IDKOLLI                        
036400           MOVE W-IDPLKLST          TO WS-IDPLKLST                        
036500           MOVE WS-IDPLKLST         TO MID-IDPLKLST                       
036600           MOVE W-IDPURAD           TO WS-IDPURAD                         
036700           MOVE WS-IDPURAD          TO MID-IDPURAD                        
036800           MOVE RAD-IX              TO WS-RAD-IX                          
036900           MOVE WS-RAD-IX           TO MID-RAD-IX                         
037000           MOVE 'W4T796X '          TO MSG-KDTRANS-1                      
037100           MOVE '4796'              TO MSG-IDTRANS-1                      
037200           COMPUTE MSG-KVLL = LENGTH OF MID + 17                          
037300           MOVE MID                 TO MSG-INDATA-MINUS-1-TRANSKOD        
037400           PERFORM IMS-ISRT-ALT96                                         
037500       END-IF                                                             
037600     END-IF                                                               
037700                                                                          
037800     MOVE ZERO TO RETURN-CODE                                             
037900     GOBACK                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 A-INIT SECTION.                                                          
038300                                                                          
038400     IF MSG-DUBBLA-TRANSKODER                                             
038500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I79601                 
038600     ELSE                                                                 
038700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I79601                  
038800     END-IF                                                               
038900                                                                          
039000     MOVE LOW-VALUE                 TO MSG-AREA                           
039100     ACCEPT DAGENS-TID              FROM TIME                             
039200     ACCEPT DAGENS-DATUM            FROM DATE                             
039300     MOVE SPACE                     TO 4791-MID-W4I79101                  
039400                                                                          
039500     MOVE LOW-VALUE                 TO W-IDGMT-MIN-X                      
039600                                                                          
039700     MOVE HIGH-VALUE                TO W-IDGMT-MAX-X                      
040200     .                                                                    
040300     EJECT                                                                
040400 B-KOLLA-NYCKLAR SECTION.                                                 
040500                                                                          
040600     MOVE LOW-VALUE                 TO W-WDE4F1KY-MIN-X                   
040700     MOVE HIGH-VALUE                TO W-WDE4F1KY-MAX-X                   
040800                                                                          
040900     MOVE MID-IDDISTR               TO W-IDDISTR                          
041000                                       W-IDDISTR-E4F                      
041100                                       W-IDDISTR-MIN                      
041200                                       W-IDDISTR-MAX                      
041300                                       W-IDDISTR-WDB2                     
041400                                       W-IDDISTR-WDB2-MIN                 
041500                                       W-IDDISTR-WDB2-MAX                 
041600     MOVE MID-IDKUNDNR              TO W-IDKUNDNR                         
041700                                       W-IDKUNDNR-E4F                     
041800                                       W-IDKUNDNR-MIN                     
041900                                       W-IDKUNDNR-MAX                     
042000                                       W-IDKUNDNR-WDB2                    
042100     MOVE MID-IDORDNR5              TO W-IDORDNR5                         
042200                                       W-IDORDNR5-E4F                     
042300                                       W-IDORDNR5-MIN                     
042400                                       W-IDORDNR5-MAX                     
042500     MOVE MID-IDPRODNR              TO W-IDPRODNR                         
042600                                       W-IDPRODNR-WDE6                    
042700                                       W-IDPRODNR-E4F                     
042800                                       W-IDPRODNR-MIN                     
042900                                       W-IDPRODNR-MAX                     
043000                                                                          
043100     IF MID-OMSTART-NYCKLAR NOT = SPACE                                   
043200         MOVE MID-RAD-IX            TO RAD-IX                             
043300         MOVE MID-IDPLKLST          TO W-IDPLKLST                         
043400                                       W-IDPLKLST-E4F                     
043500         MOVE MID-IDPURAD           TO W-IDPURAD                          
043600                                       W-IDPURAD-E4F                      
043700         MOVE MID-IDKOLLI           TO W-IDKOLLI                          
043800                                       W-IDKOLLI-E4F                      
043900                                       W-IDKOLLI-MIN                      
044000                                       W-IDKOLLI-MAX                      
044100                                       WS-KOLLI-MIN                       
044200         MOVE JA                    TO OMSTART-SW                         
044300     ELSE                                                                 
044400         MOVE  +1                   TO RAD-IX                             
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 F-LAES-SKAPA-RADER SECTION.                                              
044900                                                                          
045000     PERFORM IMS-GU-WDE601                                                
045100                                                                          
045200     IF MID-IDKOLLI-FOM (1) NOT = SPACE                                   
045300         PERFORM FA-LAES-SKAPA-VALDA-RADER                                
045400     ELSE                                                                 
045500         PERFORM FB-LAES-SKAPA-ALLA-RADER                                 
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 FA-LAES-SKAPA-VALDA-RADER SECTION.                                       
046000                                                                          
046100     PERFORM UNTIL RAD-IX > MAX-IX                                        
046200                OR ANTAL-ISRT = MAX-ANTAL-ISRT                            
046300                                                                          
046400       IF MID-IDKOLLI-TOM (RAD-IX) NOT = SPACE                            
046500          PERFORM FAA-SKAPA-VALDA-FOM-TOM                                 
046600       ELSE                                                               
046700          IF MID-IDKOLLI-FOM (RAD-IX) NOT = SPACE                         
046800             PERFORM FAB-SKAPA-VALDA-FOM                                  
046900          END-IF                                                          
047000       END-IF                                                             
047100                                                                          
047200*- FÖR ATT INTE BYTA RAD VID OMSTART                                      
047300       IF SEGMENT-SAKNAS OR BASEN-SLUT                                    
047400         ADD +1                   TO RAD-IX                               
047500       END-IF                                                             
047600                                                                          
047700     END-PERFORM                                                          
047800                                                                          
047900     IF ANTAL-ISRT < MAX-ANTAL-ISRT                                       
048000         IF 4791IX > +0                                                   
048100           IF RADER-KVAR                                                  
048200             IF SEGMENT-SAKNAS                                            
048300                 MOVE JA            TO 4791-MID-LEVANM-KLAR               
048400                 MOVE '1'           TO 4791-MID-KDLEVANM                  
048500                 MOVE NEJ           TO RADER-KVAR-SW                      
048600             ELSE                                                         
048700                 MOVE NEJ           TO 4791-MID-LEVANM-KLAR               
048800                 MOVE '0'           TO 4791-MID-KDLEVANM                  
048900             END-IF                                                       
049000             PERFORM S02-UPPDATERA-DISPATCHEN                             
049100             ADD +1                 TO ANTAL-ISRT                         
049200           END-IF                                                         
049300         END-IF                                                           
049400     END-IF                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 FAA-SKAPA-VALDA-FOM-TOM SECTION.                                         
049800                                                                          
049900     IF NOT OMSTART                                                       
050000       MOVE MID-IDKOLLI-FOM (RAD-IX) TO WS-KOLLI-MIN                      
050100     END-IF                                                               
050200     MOVE MID-IDKOLLI-TOM (RAD-IX)   TO WS-KOLLI-MAX                      
050300                                                                          
050400     PERFORM UNTIL WS-KOLLI-MIN > WS-KOLLI-MAX                            
050500                OR ANTAL-ISRT = MAX-ANTAL-ISRT                            
050600                                                                          
050700       MOVE WS-KOLLI-MIN            TO W-IDKOLLI                          
050800       PERFORM IMS-GNP-WDE611-KVAL                                        
050900                                                                          
051000       IF OMSTART                                                         
051100          PERFORM IMS-GU-WDE4F-KVAL                                       
051200          MOVE NEJ                  TO OMSTART-SW                         
051300       ELSE                                                               
051400          MOVE WS-KOLLI-MIN         TO W-IDKOLLI-MIN                      
051500                                       W-IDKOLLI-MAX                      
051600          PERFORM IMS-GU-WDE4F                                            
051700       END-IF                                                             
051800                                                                          
051900       PERFORM UNTIL  SEGMENT-SAKNAS OR BASEN-SLUT                        
052000                  OR  ANTAL-ISRT = MAX-ANTAL-ISRT                         
052100                                                                          
052200         PERFORM FX-BEHANDLA-RADER                                        
052300                                                                          
052400         IF ANTAL-ISRT < MAX-ANTAL-ISRT                                   
052500            PERFORM IMS-GN-WDE4F                                          
052600         END-IF                                                           
052700       END-PERFORM                                                        
052800                                                                          
052900       IF SEGMENT-SAKNAS OR BASEN-SLUT                                    
053000           ADD +1                   TO WS-KOLLI-MIN                       
053100           MOVE WS-KOLLI-MIN        TO W-IDKOLLI                          
053200       END-IF                                                             
053300     END-PERFORM                                                          
053400     .                                                                    
053500     EJECT                                                                
053600 FAB-SKAPA-VALDA-FOM SECTION.                                             
053700                                                                          
053800     IF NOT OMSTART                                                       
053900       MOVE MID-IDKOLLI-FOM (RAD-IX) TO W-IDKOLLI                         
054000     END-IF                                                               
054100     PERFORM IMS-GNP-WDE611-KVAL                                          
054200                                                                          
054300     IF OMSTART                                                           
054400        PERFORM IMS-GU-WDE4F-KVAL                                         
054500        MOVE NEJ                  TO OMSTART-SW                           
054600     ELSE                                                                 
054700        MOVE W-IDKOLLI            TO W-IDKOLLI-MIN                        
054800                                     W-IDKOLLI-MAX                        
054900        PERFORM IMS-GU-WDE4F                                              
055000     END-IF                                                               
055100                                                                          
055200                                                                          
055300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
055400                OR (ANTAL-ISRT = MAX-ANTAL-ISRT)                          
055500                                                                          
055600       PERFORM FX-BEHANDLA-RADER                                          
055700                                                                          
055800       IF ANTAL-ISRT < MAX-ANTAL-ISRT                                     
055900          PERFORM IMS-GN-WDE4F                                            
056000       END-IF                                                             
056100     END-PERFORM                                                          
056200     .                                                                    
056300     EJECT                                                                
056400 FB-LAES-SKAPA-ALLA-RADER SECTION.                                        
056500                                                                          
056600     IF OMSTART                                                           
056700         PERFORM IMS-GNP-WDE611-KVAL                                      
056800     ELSE                                                                 
056900         PERFORM IMS-GNP-WDE611                                           
057000     END-IF                                                               
057100     PERFORM UNTIL (SEGMENT-SAKNAS AND KOLLI-SLUT)                        
057200                OR (ANTAL-ISRT = MAX-ANTAL-ISRT)                          
057300        IF OMSTART                                                        
057400           PERFORM IMS-GU-WDE4F-KVAL                                      
057500           MOVE NEJ               TO OMSTART-SW                           
057600        ELSE                                                              
057700           MOVE W-IDKOLLI         TO W-IDKOLLI-MIN                        
057800                                     W-IDKOLLI-MAX                        
057900           PERFORM IMS-GU-WDE4F                                           
058000        END-IF                                                            
058100                                                                          
058200        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
058300                   OR (ANTAL-ISRT = MAX-ANTAL-ISRT)                       
058400                                                                          
058500           PERFORM FX-BEHANDLA-RADER                                      
058600                                                                          
058700           IF ANTAL-ISRT < MAX-ANTAL-ISRT                                 
058800             PERFORM IMS-GN-WDE4F                                         
058900           END-IF                                                         
059000         END-PERFORM                                                      
059100         IF ANTAL-ISRT < MAX-ANTAL-ISRT                                   
059200             PERFORM IMS-GNP-WDE611                                       
059300             IF SEGMENT-SAKNAS                                            
059400                 MOVE NEJ           TO FLERA-KOLLI-SW                     
059500             END-IF                                                       
059600         END-IF                                                           
059700     END-PERFORM                                                          
059800                                                                          
059900     IF ANTAL-ISRT < MAX-ANTAL-ISRT                                       
060000         IF 4791IX > +0                                                   
060100             IF  SEGMENT-SAKNAS AND KOLLI-SLUT                            
060200                 MOVE JA            TO 4791-MID-LEVANM-KLAR               
060300                 MOVE '1'           TO 4791-MID-KDLEVANM                  
060400                 MOVE NEJ           TO RADER-KVAR-SW                      
060500             ELSE                                                         
060600                 MOVE NEJ           TO 4791-MID-LEVANM-KLAR               
060700                 MOVE '0'           TO 4791-MID-KDLEVANM                  
060800             END-IF                                                       
060900             PERFORM S02-UPPDATERA-DISPATCHEN                             
061000             ADD +1                 TO ANTAL-ISRT                         
061100         END-IF                                                           
061200     END-IF                                                               
061300     .                                                                    
061400     EJECT                                                                
061500 FX-BEHANDLA-RADER SECTION.                                               
061600                                                                          
061700     MOVE SEQF-IDPURAD          TO W-IDPURAD                              
061800     MOVE SEQF-IDPLKLST         TO W-IDPLKLST                             
061900                                                                          
062000     IF 4791IX = MAX-4791IX                                               
062100         PERFORM S02-UPPDATERA-DISPATCHEN                                 
062200         MOVE '0'               TO 4791-MID-KDLEVANM                      
062300         ADD +1                 TO ANTAL-ISRT                             
062400         MOVE +0                TO 4791IX                                 
062500     END-IF                                                               
062600                                                                          
062700     PERFORM IMS-GU-WDE411                                                
062800     ADD +1                     TO 4791IX                                 
062900     PERFORM FXA-REDIGERA-4791-MID                                        
063000     .                                                                    
063100     EJECT                                                                
063200 FXA-REDIGERA-4791-MID SECTION.                                           
063300     SKIP2                                                                
063400                                                                          
063500     MOVE 'INT '                     TO 4791-MID-IDSYSTEM                 
063600     MOVE MID-IDDISTR                TO 4791-MID-IDDISTR                  
063700                                        TEST-IDDISTR                      
063800     MOVE MID-IDKUNDNR               TO 4791-MID-IDKUNDNR                 
063900     MOVE MID-IDRAPPNR               TO 4791-MID-IDRAPPNR                 
064000                                                                          
064100     MOVE MID-PRFOERS                TO 4791-MID-PRFOERS                  
064200     MOVE ALL ZERO                   TO 4791-MID-REEMBHNT                 
064300     MOVE MID-PRFRAKT                TO 4791-MID-PRFRAKT                  
064400     MOVE MID-RELANDCO               TO 4791-MID-RELANDCO                 
064500     MOVE MID-PRLEGKST               TO 4791-MID-PRLEGKST                 
064600     MOVE DAGENS-DATUM               TO 4791-MID-TILEVANM                 
064700                                                                          
064800     IF ORAD-FLDIRLEV > +0                                                
064900         MOVE JA                     TO 4791-MID-FLDIRLEV (4791IX)        
065000     ELSE                                                                 
065100         MOVE NEJ                    TO 4791-MID-FLDIRLEV (4791IX)        
065200     END-IF                                                               
065300                                                                          
065400     MOVE SPACE                     TO 4791-MID-IDANALYS  (4791IX)        
065500     MOVE ZERO                      TO 4791-MID-IDKONTO   (4791IX)        
065600     MOVE SPACE                     TO 4791-MID-IDKST     (4791IX)        
065700     MOVE ORAD-IDARTNR              TO SPARA-IDARTNR                      
065800     MOVE SPARA-IDARTNR             TO 4791-MID-IDARTNR   (4791IX)        
065900     MOVE VORD-IDDC                 TO 4791-MID-IDDC      (4791IX)        
066000                                       4791-MID-IDDC-RET  (4791IX)        
066100                                                                          
066200     MOVE MID-IDFAKT               TO 4791-MID-IDFAKT     (4791IX)        
066300     MOVE ZERO                     TO 4791-MID-IDFAKT-LOC (4791IX)        
066400                                                                          
066500     MOVE 57                        TO 4791-MID-IDFTG     (4791IX)        
066600     MOVE W-IDKOLLI                 TO SPARA-IDKOLLI                      
066700     MOVE SPARA-IDKOLLI             TO 4791-MID-IDKOLLI   (4791IX)        
066800     MOVE SPACE                     TO 4791-MID-IDKUNDRF  (4791IX)        
066900     MOVE W-IDORDNR5                TO 4791-MID-IDORDNR7  (4791IX)        
067000     MOVE 97                        TO 4791-MID-KDANMORS  (4791IX)        
067100     MOVE ALL ZERO                  TO 4791-MID-IDRADNR   (4791IX)        
067200     MOVE ALL ZERO                  TO 4791-MID-KDEMBLEV  (4791IX)        
067300     MOVE VORD-KDFAKTYP             TO 4791-MID-KDFAKTYP  (4791IX)        
067400     MOVE ORAD-KDFRAKT              TO 4791-MID-KDFRAKT   (4791IX)        
067500     MOVE 'Y  '                     TO 4791-MID-KDKREBEH  (4791IX)        
067600     MOVE SEQF-KVLEVART             TO SPARA-KVLEVART                     
067700     MOVE SPARA-KVLEVART            TO 4791-MID-KVLEVANM  (4791IX)        
067800                                                                          
067900     MOVE KOLLI-TIFAKT              TO WS-KOLLI-TIFAKT                    
068000     MOVE WS-KOLLI-TIFAKT           TO 4791-MID-TIFAKT    (4791IX)        
068100     MOVE ZERO                      TO 4791-MID-TIFAKT-LOC(4791IX)        
068200                                                                          
068300     MOVE DAGENS-DATUM            TO 4791-MID-TILEVANM-RAD(4791IX)        
068400     MOVE ORAD-KDORDKL              TO 4791-MID-KDORDKL   (4791IX)        
068500     MOVE SPACE                    TO 4791-MID-IDUSER-PACK(4791IX)        
068600     MOVE NEJ                      TO 4791-MID-FLPRQUES   (4791IX)        
068700     MOVE ORAD-KDVAT               TO 4791-MID-KDVAT      (4791IX)        
068800     MOVE ORAD-BEART-VIPS          TO 4791-MID-BEART-VIPS (4791IX)        
068900                                                                          
069000     IF DIST79-DEALER-PRICE                                               
069200       MOVE ORAD-IDARTNR     TO W-IDARTNR                                 
069300       PERFORM IMS-GU-WDK611                                              
069400       IF SEGMENT-FINNS                                                   
069500         MOVE CLAG-PRARTSTD        TO SPARA-PRARTSTD                      
069600                                                                          
069700         MOVE '11'                 TO COST-IDDC                           
069800         MOVE ORAD-IDARTNR         TO COST-IDARTNR                        
069900         MOVE FUNCTION CURRENT-DATE(5:2) TO COST-TIMM                     
070000         CALL W335COST USING COST-W335COST COST-WDK6-PCB                  
070100                                           COST-WDK7-PCB                  
070200                                           COST-WDF1-PCB                  
070300                                           COST-9305-PCB                  
070400                                           COST-WDK72-PCB                 
070500                                           COST-WDB6-PCB                  
070700         MOVE COST-PRARTSJK-MON    TO SPARA-PRARTSJK                      
070800                                                                          
070900         PERFORM S03-HAMTA-MARK-BOLAG-VALUTA                              
071100         MOVE SPARA-PRARTSTD       TO 4791-MID-PRARTSTD (4791IX)          
071200         MOVE SPARA-PRARTSJK       TO 4791-MID-PRARTSJK (4791IX)          
071300       ELSE                                                               
071400         MOVE ZERO                 TO 4791-MID-PRARTSTD (4791IX)          
071500         MOVE ZERO                 TO 4791-MID-PRARTSJK (4791IX)          
071600       END-IF                                                             
071700       MOVE ORAD-PRARTNTO-LOC         TO SPARA-PRARTBTO-LOC               
071800       IF ORAD-PRARTNTO-LOC = ZERO                                        
071900         MOVE ORAD-PRARTNTO-LOCPREL TO SPARA-PRARTBTO-LOC                 
072000       END-IF                                                             
072600       MOVE SPARA-PRARTBTO-LOC TO 4791-MID-PRARTBTO-LOC   (4791IX)        
072700       MOVE ZERO                   TO 4791-MID-PRARTBTO   (4791IX)        
072800       MOVE ZERO               TO 4791-MID-PRARTBTO-LOCINV(4791IX)        
072900     ELSE                                                                 
073001       IF DIST79-ECOM-PRICE                                               
073010         MOVE ZERO                 TO 4791-MID-PRARTSTD   (4791IX)        
073100         MOVE ZERO                 TO 4791-MID-PRARTSJK   (4791IX)        
073200         MOVE ORAD-PRARTNTO-LOC    TO SPARA-PRARTBTO-LOC                  
073300         MOVE SPARA-PRARTBTO-LOC   TO                                     
073400                                   4791-MID-PRARTBTO-LOC (4791IX)         
073500         MOVE ZERO             TO 4791-MID-PRARTBTO-LOCINV(4791IX)        
073610       ELSE                                                               
073620         MOVE ZERO                 TO 4791-MID-PRARTSTD   (4791IX)        
073630         MOVE ZERO                 TO 4791-MID-PRARTSJK   (4791IX)        
073640         MOVE ORAD-PRARTNTO        TO SPARA-PRARTBTO                      
073650         MOVE SPARA-PRARTBTO       TO 4791-MID-PRARTBTO   (4791IX)        
073660         MOVE ZERO             TO 4791-MID-PRARTBTO-LOC   (4791IX)        
073670         MOVE ZERO             TO 4791-MID-PRARTBTO-LOCINV(4791IX)        
073680       END-IF                                                             
073690     END-IF                                                               
073700                                                                          
073800     IF 4791-MID-KDVALISO = SPACE                                         
073900       MOVE ORAD-KDVALISO          TO 4791-MID-KDVALISO                   
074000     END-IF                                                               
074100     .                                                                    
074200     EJECT                                                                
074300* --- IMS SEKTIONER ---                                                   
074400     SKIP3                                                                
074500 S02-UPPDATERA-DISPATCHEN SECTION.                                        
074600     SKIP2                                                                
074700     MOVE 4791IX                    TO 4791-MID-KVRADER                   
074800     PERFORM S02A-SKAPA-BUNTHUVUD                                         
074900     PERFORM S02B-UPPDATERA-DISPATCHEN                                    
075000     .                                                                    
075100     EJECT                                                                
075200 S02A-SKAPA-BUNTHUVUD SECTION.                                            
075300                                                                          
075400     MOVE +54                       TO MSG-KOM-KVLL                       
075500     MOVE LOW-VALUE                 TO MSG-KOM-KDZ1                       
075600     MOVE LOW-VALUE                 TO MSG-KOM-KDZ2                       
075700     MOVE SPACE                     TO MSG-KOM-KDTRANS                    
075800     MOVE 'W4I79101'                TO MSG-KOM-IDCPYTXT                   
075900     MOVE 'KREDIT  '                TO MSG-KOM-IDSNDNOD                   
076000     MOVE 'W4079600'                TO MSG-KOM-IDSNDJOB                   
076100     MOVE DAGENS-DATUM              TO MSG-KOM-TIREGDAT                   
076200     MOVE DAGENS-TID                TO MSG-KOM-TIKLOCK                    
076300     MOVE SPACE                     TO MSG-KOM-IDMFSMED                   
076400                                       MSG-KOM-KDSVAR                     
076500     .                                                                    
076600     EJECT                                                                
076700 S02B-UPPDATERA-DISPATCHEN SECTION.                                       
076800                                                                          
076900*4791-LL = ((ANTAL-RADER * RAD-LÄNGDEN) + ÖVRIGT DATA) + 17 FÖR           
077000*          P-TO-P-SW                                                      
077100*                                                                         
077200     COMPUTE 4791-LL        = ((4791IX * 221) + 77) + 17                  
077300                                                                          
077400     CALL W006KOM USING MSG-PCB                                           
077500                        DISP-PCB                                          
077600                        KOMM-PCB                                          
077700                        MSG-KOM-WMSGKOM                                   
077800                        4791-MSG-IO-AREA                                  
077900                                                                          
078000     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
078100        MOVE                                                              
078200        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
078300                                     TO FELTEXT                           
078400        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800 S03-HAMTA-MARK-BOLAG-VALUTA SECTION.                                     
078900                                                                          
079000     PERFORM IMS-GU-WDB201-UNIK                                           
079100                                                                          
079200     IF SEGMENT-FINNS                                                     
079300       MOVE GMT-IDPARTNR        TO W-IDPARTNR                             
079400       MOVE GMT-IDFTG           TO W-IDFTG                                
079500     ELSE                                                                 
079600       PERFORM IMS-GU-WDB201                                              
079700       IF SEGMENT-FINNS                                                   
079800         MOVE GMT-IDPARTNR      TO W-IDPARTNR                             
079900         MOVE GMT-IDFTG         TO W-IDFTG                                
080000       END-IF                                                             
080100     END-IF                                                               
080200                                                                          
080300     PERFORM IMS-GU-WDB101                                                
080400     IF SEGMENT-FINNS                                                     
080500       MOVE BET-IDMARKBO        TO WS-IDMARKBO                            
080600     ELSE                                                                 
080700       MOVE SPACE               TO WS-IDMARKBO                            
080800     END-IF                                                               
080900                                                                          
083000     .                                                                    
083100     EJECT                                                                
085900 IMS-GET-MSG SECTION.                                                     
086000                                                                          
086100     MOVE '  QC' TO GODK-STATUSKODER                                      
086200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
086300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086400     PERFORM IMS-STATUSKONTROLL                                           
086500     .                                                                    
086600     SKIP3                                                                
086700 IMS-GU-WDE411 SECTION.                                                   
086800                                                                          
086900     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
087000          DELIMITED BY SIZE INTO SSA1                                     
087100     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
087200          DELIMITED BY SIZE INTO SSA2                                     
087300     MOVE '  ' TO GODK-STATUSKODER                                        
087400     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E411 SSA1 SSA2                 
087500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
087600     PERFORM IMS-STATUSKONTROLL                                           
087700     .                                                                    
087800     EJECT                                                                
087900 IMS-GU-WDE601 SECTION.                                                   
088000                                                                          
088100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
088200          DELIMITED BY SIZE INTO SSA1                                     
088300     MOVE '  ' TO GODK-STATUSKODER                                        
088400     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
088500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
088600     PERFORM IMS-STATUSKONTROLL                                           
088700     .                                                                    
088800     EJECT                                                                
088900 IMS-GNP-WDE611 SECTION.                                                  
089000                                                                          
089100     MOVE 'WDE611 ' TO SSA1                                               
089200     MOVE '  GE' TO GODK-STATUSKODER                                      
089300     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
089400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
089500     PERFORM IMS-STATUSKONTROLL                                           
089600     .                                                                    
089700     EJECT                                                                
089800 IMS-GNP-WDE611-KVAL SECTION.                                             
089900                                                                          
090000     STRING 'WDE611  *F(IDKOLLI  =' W-IDKOLLI-X ')'                       
090100          DELIMITED BY SIZE INTO SSA1                                     
090200     MOVE '  ' TO GODK-STATUSKODER                                        
090300     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
090400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
090500     PERFORM IMS-STATUSKONTROLL                                           
090600     .                                                                    
090700     EJECT                                                                
090800 IMS-GU-WDE4F-KVAL SECTION.                                               
090900                                                                          
091000     STRING 'WDE4F1  (WDE4F1KY =' W-WDE4F1KY-X ')'                        
091100          DELIMITED BY SIZE INTO SSA1                                     
091200     MOVE '  GE' TO GODK-STATUSKODER                                      
091300     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
091400     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
091500     PERFORM IMS-STATUSKONTROLL                                           
091600     .                                                                    
091700     EJECT                                                                
091800 IMS-GU-WDE4F SECTION.                                                    
091900                                                                          
092000     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
092100                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
092200          DELIMITED BY SIZE INTO SSA1                                     
092300     MOVE '  GE' TO GODK-STATUSKODER                                      
092400     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
092500     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     .                                                                    
092800     SKIP3                                                                
092900 IMS-GN-WDE4F SECTION.                                                    
093000                                                                          
093100     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
093200                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
093300          DELIMITED BY SIZE INTO SSA1                                     
093400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
093500     CALL CBLTDLI USING GN WDE4F-PCB DLI-IO-E4F1 SSA1                     
093600     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     .                                                                    
093900     EJECT                                                                
094000 IMS-GU-WDK611 SECTION.                                                   
094100                                                                          
094200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
094300          DELIMITED BY SIZE INTO SSA1                                     
094400     MOVE 'WDK611   ' TO SSA2                                             
094500     MOVE '  GE' TO GODK-STATUSKODER                                      
094600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
094700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
094800     PERFORM IMS-STATUSKONTROLL                                           
094900     .                                                                    
095000     SKIP3                                                                
097500 IMS-GU-WDB101                 SECTION.                                   
097600                                                                          
097700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
097800            DELIMITED BY SIZE INTO SSA1                                   
097900     MOVE '  GE' TO GODK-STATUSKODER                                      
098000     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
098100     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
098200     PERFORM IMS-STATUSKONTROLL                                           
098300     .                                                                    
098400     EJECT                                                                
098500 IMS-GU-WDB201-UNIK             SECTION.                                  
098600                                                                          
098700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
098800            DELIMITED BY SIZE INTO SSA1                                   
098900     MOVE '  GE' TO GODK-STATUSKODER                                      
099000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
099100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
099200     PERFORM IMS-STATUSKONTROLL                                           
099300     .                                                                    
099400     EJECT                                                                
099500 IMS-GU-WDB201 SECTION.                                                   
099600                                                                          
099700     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
099800                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
099900            DELIMITED BY SIZE INTO SSA1                                   
100000                                                                          
100100     MOVE '  ' TO GODK-STATUSKODER                                        
100200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
100300     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
100400     PERFORM IMS-STATUSKONTROLL                                           
100500     .                                                                    
100600     EJECT                                                                
100700 IMS-ISRT-ALT96  SECTION.                                                 
100800     SKIP2                                                                
100900     MOVE SPACE TO GODK-STATUSKODER                                       
101000     CALL CBLTDLI USING ISRT 4796-PCB MSG-IO-AREA                         
101100     MOVE 4796-STATUS-CODE TO STATUS-WS                                   
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     EJECT                                                                
101500 IMS-STATUSKONTROLL SECTION.                                              
101600                                                                          
101700     SET STATUS-IX TO 1                                                   
101800     SEARCH GODK-STATUS                                                   
101900       AT END                                                             
102000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
102100         DELIMITED BY SIZE INTO FELTEXT                                   
102200         CALL FELLOG                                                      
102300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
102400         CONTINUE                                                         
102500     END-SEARCH                                                           
102600     .                                                                    
