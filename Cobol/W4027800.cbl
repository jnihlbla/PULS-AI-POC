001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4027800.                                                
001600 AUTHOR.         BODIL LINDAHL.                                           
001700 DATE-WRITTEN.   1 APRIL 2003.                                            
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        PROGRAMMET LÄSER VOR MESSAGES.                                   
002200*                                                                         
002310*        PROGRAMMET LÄSER      WDA6                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T278                                              
002700*        MID:         W4I27801                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O27801                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003710                                                                          
003800 77  IDPGM                       PIC X(08)   VALUE 'W4027800'.            
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-KVANTAL                  PIC S9(7)   VALUE ZERO COMP-3.           
004600 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
004700 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
004800 77  WS-TIREGDAT-URSP            PIC 9(6)    VALUE ZERO.                  
004900 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
005000 77  WS-TIREGTID-URSP            PIC 9(8)    VALUE ZERO.                  
005010 77  WS-TIREGDAT-AVV             PIC 9(6)    VALUE ZERO.                  
005020 77  WS-TIREGTID-AVV             PIC 9(8)    VALUE ZERO.                  
005030 77  SW-INFO-FINNS               PIC X       VALUE SPACE.                 
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '4278'.                
005800     88  GODK-MID                            VALUE '4228'.                
006400                                                                          
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
008000     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
008100     EJECT                                                                
009590*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009591*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009800*01  MID -COPY W4I27801                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400     03  MOD REDEFINES MSG-AREA.                                          
010500*      05  -COPY W4O27801                                                 
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010900*01  -COPY WMFSAREA                                                       
011000     EJECT                                                                
011100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011602     03  W-WDA601KY-X.                                                    
011603         05  W-IDDISTR           PIC S9(5)  COMP-3 VALUE ZERO.            
011604         05  W-IDKUNDNR          PIC S9(7)  COMP-3 VALUE ZERO.            
011605         05  W-IDKUNDRF          PIC X(10)  VALUE SPACE.                  
011606         05  W-TIREGDAT-URSP     PIC S9(7)  COMP-3 VALUE ZERO.            
011607         05  W-IDARTNR           PIC S9(9)  COMP-3 VALUE ZERO.            
011608         05  W-TIREGTID-URSP     PIC S9(9)  COMP-3 VALUE ZERO.            
011609         05  W-TIREGDAT-AVV      PIC S9(7)  COMP-3 VALUE ZERO.            
011610         05  W-TIREGTID-AVV      PIC S9(9)  COMP-3 VALUE ZERO.            
011810     EJECT                                                                
011820 01  W-PROG-TO-PROG-SW.                                                   
011830     03  M-SW-LL                 PIC S9(4)   VALUE +80 COMP SYNC.         
011840     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
011850     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W0T501  '.            
011860     03  M-SW-IDTRANS            PIC X(4)    VALUE '4278'.                
011870     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '1'.                   
011880     EJECT                                                                
011890*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
013602 01  DLI-IO-WDA601.                                                       
013610*    03  -COPY WDA601                                                     
013900     EJECT                                                                
013910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
013920 01  DLI-IO-WDA612.                                                       
013930*    03  -COPY WDA612                                                     
013940     EJECT                                                                
013950 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
013960 01  DLI-IO-WDA613.                                                       
013970*    03  -COPY WDA613                                                     
013980     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014300     EJECT                                                                
014310*01  -COPY W0009   -PRE ALT-                                              
014320     EJECT                                                                
014402*01  -COPY W0008   -PRE WDA6-                                             
014410     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014601 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDA6-PCB.                      
014602 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDA6-PCB.                      
014700                                                                          
014800     PERFORM IMS-GET-MSG                                                  
014810     IF SEGMENT-FINNS                                                     
014820        PERFORM A-INIT                                                    
014821        PERFORM B-KOLLA-NYCKLAR                                           
014830        IF EGEN-MID OR GODK-MID                                           
014894           PERFORM F-LAES-VISA-INFO                                       
014905           COMPUTE MSG-KVLL = LENGTH OF MOD-W4O27801-CTX + 4              
014906           PERFORM IMS-INSERT-MSG                                         
014908        ELSE                                                              
014909           PERFORM IMS-INSERT-ALT-MSG                                     
014910        END-IF                                                            
014911     END-IF                                                               
014912                                                                          
014913     MOVE ZERO TO RETURN-CODE                                             
014914     GOBACK                                                               
014915     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     IF MSG-DUBBLA-TRANSKODER                                             
017400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I27801-CTX             
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I27801-CTX              
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018600                                                                          
018700     MOVE LOW-VALUE TO MSG-AREA                                           
018800     MOVE 'W4O278N1' TO MFS-IDMOD                                         
018900     MOVE '4278' TO MOD-IDTRANS                                           
019000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF EGEN-MID OR GODK-MID                                              
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-NYCKLAR SECTION.                                                 
021700                                                                          
021710     MOVE 'SE ' TO MED-IDSKYLT                                            
021720                                                                          
021730     INSPECT MID-IDDISTR REPLACING LEADING SPACE BY ZERO                  
021740     IF MID-IDDISTR NUMERIC                                               
021750        MOVE MID-IDDISTR TO WS-IDDISTR                                    
021760        MOVE WS-IDDISTR TO W-IDDISTR                                      
021770     ELSE                                                                 
021780        MOVE ZERO TO W-IDDISTR                                            
021790                     WS-IDDISTR                                           
021791     END-IF                                                               
021792                                                                          
021793     INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO                 
021794     IF MID-IDKUNDNR NUMERIC                                              
021795        MOVE MID-IDKUNDNR TO WS-IDKUNDNR                                  
021796        MOVE WS-IDKUNDNR TO W-IDKUNDNR                                    
021797     ELSE                                                                 
021798        MOVE ZERO TO W-IDKUNDNR                                           
021799                     WS-IDKUNDNR                                          
021800     END-IF                                                               
021801                                                                          
021802     MOVE MID-IDORDNR7 TO W-IDKUNDRF                                      
021803                                                                          
021804     IF MID-TIREGDAT-URSP NUMERIC                                         
021805        MOVE MID-TIREGDAT-URSP TO W-TIREGDAT-URSP                         
021806                                  WS-TIREGDAT-URSP                        
021807     ELSE                                                                 
021808        MOVE ZERO TO W-TIREGDAT-URSP                                      
021809                     WS-TIREGDAT-URSP                                     
021810     END-IF                                                               
021811                                                                          
021812     INSPECT MID-IDARTNR REPLACING LEADING SPACE BY ZERO                  
021813     IF MID-IDARTNR NUMERIC                                               
021814        MOVE MID-IDARTNR TO WS-IDARTNR-NUM                                
021815        MOVE WS-IDARTNR-NUM TO W-IDARTNR                                  
021816     ELSE                                                                 
021817        MOVE ZERO TO W-IDARTNR                                            
021818                     WS-IDARTNR-NUM                                       
021819     END-IF                                                               
021820                                                                          
021821     IF MID-TIREGTID-URSP NUMERIC                                         
021822        MOVE MID-TIREGTID-URSP TO W-TIREGTID-URSP                         
021823                                  WS-TIREGTID-URSP                        
021824     ELSE                                                                 
021825        MOVE ZERO TO W-TIREGTID-URSP                                      
021826                     WS-TIREGTID-URSP                                     
021827     END-IF                                                               
021828                                                                          
021829     IF MID-TIREGDAT-AVV NUMERIC                                          
021830        MOVE MID-TIREGDAT-AVV TO W-TIREGDAT-AVV                           
021831                                 WS-TIREGDAT-AVV                          
021832     ELSE                                                                 
021833        MOVE ZERO TO W-TIREGDAT-AVV                                       
021834                     WS-TIREGDAT-AVV                                      
021835     END-IF                                                               
021836                                                                          
021837     IF MID-TIREGTID-AVV NUMERIC                                          
021838        MOVE MID-TIREGTID-AVV TO W-TIREGTID-AVV                           
021839                                 WS-TIREGTID-AVV                          
021840     ELSE                                                                 
021841        MOVE ZERO TO W-TIREGTID-AVV                                       
021842                     WS-TIREGTID-AVV                                      
021843     END-IF                                                               
021844                                                                          
021845     IF EGEN-MID OR GODK-MID                                              
021846        MOVE WS-IDDISTR       TO MOD-IDDISTR                              
021847        MOVE WS-IDKUNDNR      TO MOD-IDKUNDNR                             
021848        MOVE W-IDKUNDRF       TO MOD-IDORDNR7                             
021849        MOVE WS-TIREGDAT-URSP TO MOD-TIREGDAT-URSP                        
021850        MOVE WS-IDARTNR-NUM   TO MOD-IDARTNR                              
021851        MOVE WS-TIREGTID-URSP TO MOD-TIREGTID-URSP                        
021852        MOVE WS-TIREGDAT-AVV  TO MOD-TIREGDAT-AVV                         
021853        MOVE WS-TIREGTID-AVV  TO MOD-TIREGTID-AVV                         
021854     ELSE                                                                 
021855        PERFORM MFS-RENSA-DOLDA-NYCKLAR                                   
021856     END-IF                                                               
023000     .                                                                    
023200     EJECT                                                                
023400 F-LAES-VISA-INFO SECTION.                                                
023402                                                                          
023403     MOVE NEJ TO SW-INFO-FINNS                                            
023404                                                                          
023405     PERFORM IMS-GET-WDA601                                               
023406     IF SEGMENT-FINNS                                                     
023407        MOVE VOR-IDDISTR  TO MOD-IDDISTR-UT                               
023408        MOVE VOR-IDKUNDNR TO MOD-IDKUNDNR-UT                              
023409        MOVE VOR-IDKUNDRF TO MOD-IDKUNDRF-UT                              
023410        INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE           
023411        MOVE VOR-IDARTNR  TO MOD-IDARTNR-UT                               
023412        COMPUTE WS-KVANTAL = VOR-KVBEART-Q - VOR-KVPREAVB                 
023413        END-COMPUTE                                                       
023414        MOVE WS-KVANTAL   TO MOD-KVANTAL-UT                               
023415                                                                          
023416        PERFORM IMS-GET-WDA612                                            
023417        IF SEGMENT-FINNS                                                  
023418           MOVE VTE-TEVOREXT TO MOD-TEVOREXT                              
023419           MOVE JA TO SW-INFO-FINNS                                       
023420        ELSE                                                              
023421           MOVE MFS-RENSA-FAELT TO MOD-TEVOREXT                           
023422        END-IF                                                            
023423                                                                          
023424        PERFORM IMS-GET-WDA613                                            
023425        IF SEGMENT-FINNS                                                  
023426           MOVE VTS-TEVORSC TO MOD-TEVORSC                                
023427           MOVE JA TO SW-INFO-FINNS                                       
023428        ELSE                                                              
023429           MOVE MFS-RENSA-FAELT TO MOD-TEVORSC                            
023430        END-IF                                                            
023431                                                                          
023432        IF SW-INFO-FINNS = NEJ                                            
023433           MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                          
023434           CALL WMEDKONV USING MED-WMEDAREA                               
023435           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
023436        END-IF                                                            
023437     ELSE                                                                 
023438        MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                             
023439        CALL WMEDKONV USING MED-WMEDAREA                                  
023440        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023441     END-IF                                                               
023442     .                                                                    
023450     EJECT                                                                
023491 MFS-RENSA-DOLDA-NYCKLAR SECTION.                                         
023492                                                                          
023493     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR                                  
023494                             MOD-IDKUNDNR                                 
023495                             MOD-IDORDNR7                                 
023496                             MOD-TIREGDAT-URSP                            
023497                             MOD-IDARTNR                                  
023498                             MOD-TIREGTID-URSP                            
023499                             MOD-TIREGDAT-AVV                             
023500                             MOD-TIREGTID-AVV                             
023510     .                                                                    
023520     EJECT                                                                
030400* --- IMS SEKTIONER ---                                                   
030600     SKIP3                                                                
030700 IMS-GET-MSG SECTION.                                                     
030800     MOVE '  QC' TO GODK-STATUSKODER                                      
030900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300     SKIP3                                                                
031400 IMS-INSERT-MSG SECTION.                                                  
031600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031700     MOVE SPACE TO GODK-STATUSKODER                                       
031800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032000     PERFORM IMS-STATUSKONTROLL                                           
032100     .                                                                    
032200     SKIP3                                                                
032300 IMS-INSERT-ALT-MSG SECTION.                                              
032400     MOVE SPACE TO GODK-STATUSKODER                                       
032500     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
032600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
032700     PERFORM IMS-STATUSKONTROLL                                           
032800     .                                                                    
032900     EJECT                                                                
033000 IMS-GET-WDA601 SECTION.                                                  
033100     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
033200          DELIMITED BY SIZE INTO SSA1                                     
033300     MOVE '  GE' TO GODK-STATUSKODER                                      
033400     CALL CBLTDLI USING GU WDA6-PCB DLI-IO-WDA601 SSA1                    
033500     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
033600     PERFORM IMS-STATUSKONTROLL                                           
033700     .                                                                    
033800     SKIP3                                                                
034700 IMS-GET-WDA612 SECTION.                                                  
034800     MOVE 'WDA612  ' TO SSA1                                              
034900     MOVE '  GE' TO GODK-STATUSKODER                                      
035000     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA612 SSA1                   
035100     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
035200     PERFORM IMS-STATUSKONTROLL                                           
035300     .                                                                    
035400     SKIP3                                                                
035500 IMS-GET-WDA613 SECTION.                                                  
035600     MOVE 'WDA613  ' TO SSA1                                              
035700     MOVE '  GE' TO GODK-STATUSKODER                                      
035800     CALL CBLTDLI USING GNP WDA6-PCB DLI-IO-WDA613 SSA1                   
035900     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
036000     PERFORM IMS-STATUSKONTROLL                                           
036100     .                                                                    
036200     SKIP3                                                                
041500 IMS-STATUSKONTROLL SECTION.                                              
041600     SET STATUS-IX TO 1                                                   
041700     SEARCH GODK-STATUS                                                   
041800       AT END                                                             
041900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042000         DELIMITED BY SIZE INTO FELTEXT                                   
042100         CALL FELLOG                                                      
042200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042300         CONTINUE                                                         
042400     END-SEARCH                                                           
042500     .                                                                    
