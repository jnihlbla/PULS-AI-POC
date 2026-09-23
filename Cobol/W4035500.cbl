000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4035500.                                                
000300 AUTHOR.         GERRY CARMICHAEL.                                        
000400 DATE-WRITTEN.   98/04/07.                                                
000500                                                                          
000600*    FUNKTION:                                                            
000700*        UPPDATERINGSPROGRAM FÖR ANNULLERING AV MJUKVARU-                 
000800*        ARTIKLAR. DESSA BEHANDLAS SOM DIREKTLEVERANS-                    
000900*        ARTIKLAR, DVS SALDO PÅVERKAS EJ PÅ ARTIKELBASER.                 
001000*                                                                         
001100*        UPPDATERING SKER GENOM TRANS W4T355X FRÅN W46382                 
001200*                                                                         
001300*        OM UPPDATERINGEN GICK BRA SKICKAS OK-MEDDELANDE                  
001400*        TILL DISPATCHER ANNARS SKICKAS FELMEDDELANDE.                    
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T355X                                             
001800*        MID:         W4I35501 + WMSGKOM                                  
001900*                                                                         
002000*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
002010*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
002020                                                                          
002030                                                                          
002040 ENVIRONMENT DIVISION.                                                    
002050                                                                          
002060 DATA DIVISION.                                                           
002070                                                                          
002080     EJECT                                                                
002090 WORKING-STORAGE SECTION.                                                 
002100*    -- CHECKED BY WY2000                                                 
002200 77  IDPGM                       PIC X(08)   VALUE 'W4035500'.            
002300 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002400 77  WS-PGM-POSITION             PIC X(24)   VALUE SPACE.                 
002500 77  JA                          PIC X       VALUE 'J'.                   
002600 77  YES                         PIC X       VALUE 'Y'.                   
002700 77  NEJ                         PIC X       VALUE 'N'.                   
002800 77  RAETT                       PIC X       VALUE 'R'.                   
002900 77  FEL                         PIC X       VALUE 'F'.                   
003000 77  IX                          PIC S9(3)   VALUE +0   COMP-3.           
003100 77  IX-MAX                      PIC S9(3)   VALUE +7   COMP-3.           
003200 77  INDX                        PIC S9(3)   VALUE +0   COMP-3.           
003300 77  MAX-INDX                    PIC S9(3)   VALUE +10  COMP-3.           
003400 77  2109-IX                     PIC S9(3)   VALUE +0   COMP-3.           
003500 77  2109-IX-MAX                 PIC S9(3)   VALUE +18  COMP-3.           
003600 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
003700 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
003800 77  WS-IDRADNR                  PIC 9(4)    VALUE ZERO.                  
003900 77  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
004000 77  WS-IDKUNDNR                 PIC 9(7)    VALUE ZERO.                  
004100 77  WS-IDORDNR5                 PIC 9(5)    VALUE ZERO.                  
004200 77  WS-IDPRODNR                 PIC 9(7)    VALUE ZERO.                  
004300 77  WS-ANTAL-ANNULL-RADER       PIC S9(3)   VALUE ZERO  COMP-3.          
004400 77  W-KVKOLLI                   PIC S9(7)   VALUE ZERO  COMP-3.          
004500 77  W-KVKOLLI-FAKT              PIC S9(7)   VALUE ZERO  COMP-3.          
004600 77  W-KVKOLLI-LAST              PIC S9(7)   VALUE ZERO  COMP-3.          
004700 77  W-KVKOLLI-FL                PIC S9(7)   VALUE ZERO  COMP-3.          
004800 77  WS-KDORDSTA                 PIC X(2)    VALUE SPACE.                 
004900 77  WS-DATUM-9KOMPL             PIC 9(8).                                
005000 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
005100 77    WS-HHMMSSDD               PIC 9(8)   VALUE ZERO.                   
005200 01     WS-HHMMSSDD-RED.                                                  
005300   03   WS-HHMMSS                PIC  9(6).                               
005400   03   WS-DD                    PIC  9(2).                               
005500 77    WS-TIRFS                  PIC  9(12).                              
005600                                                                          
005700*    --- ÖVRIGA ARBETSFÄLT                                                
005800                                                                          
005900*01  -COPY WWBYT03                                                        
006000                                                                          
006100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006200     88  INDATA-OK                           VALUE 'J'.                   
006300     88  INDATA-FEL                          VALUE 'N'.                   
006400                                                                          
006500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006600     88  NYCKLAR-OK                          VALUE 'J'.                   
006700     88  NYCKLAR-FEL                         VALUE 'N'.                   
006800                                                                          
006900 77  WS-INDATA-TEST              PIC X(01).                               
007000     88  WS-INDATA-RATT                      VALUE 'R'.                   
007100                                                                          
007200 77  WS-IDTRANS                  PIC X(04).                               
007300     88  GODKAND-TRANS                       VALUE '4355'.                
007400                                                                          
007500 77  GODK-KDORDBEK               PIC X(2).                                
007600     88 GODK-KOD                             VALUE '30' '31' '32'         
007700                                                   '33' '34' '83'.        
007800                                                                          
007900 77    KDORDSTA-SW               PIC X(01).                               
008000   88  KDORDSTA-KLAR                         VALUE 'J'.                   
008100   88  KDORDSTA-EJ-KLAR                      VALUE 'N'.                   
008200                                                                          
008300 77    WS-KDMFSFOR               PIC X(1)    VALUE SPACE.                 
008400   88  SWEDISH-TEXT                          VALUE '1'.                   
008500   88  ENGLISH-TEXT                          VALUE '2'.                   
008600                                                                          
008700 01    WS-IDUSER                 PIC X(8).                                
008800                                                                          
008900 01    FILLER REDEFINES  WS-IDUSER.                                       
009000   03  FILLER                    PIC X(2).                                
009100   03  WS-IDANSTNR               PIC 9(5).                                
009200   03  FILLER                    PIC X(1).                                
009300*                                                                         
009400 01     FILLER                   PIC X(10)   VALUE 'SPAR-AREOR'.          
009500 01     SPAR-AREOR.                                                       
009600   03   SPAR-AREA.                                                        
009700     05 SPAR-VKORDNTO            PIC  9(6)V9(4)    VALUE ZERO.            
009800     05 SPAR-VLORDNTO            PIC  9(4)V9(7)    VALUE ZERO.            
009900     05 SPAR-SUORDV-LEVPL        PIC  S9(9)V9(2)   VALUE ZERO.            
010000     05 SPAR-SUORDV-LEVPL-AVG    PIC  S9(9)V9(2)   VALUE ZERO.            
010100     05 SPAR-SUORDV-LEVPL-LOC    PIC  S9(9)V9(2)   VALUE ZERO.            
010200     05 SPAR-SUORDV-LEVPL-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.           
010300                                                                          
010400     EJECT                                                                
010500                                                                          
010600 01     WS-IDKUNDRF-OLD.                                                  
010700   03   WS-IDORDNR5-OLD          PIC 9(5).                                
010800   03   FILLER                   PIC X(5)    VALUE SPACE.                 
010900                                                                          
011000 01     WS-IDKUNDRF-NEW.                                                  
011100   03   WS-IDORDNR7-NEW          PIC 9(7).                                
011200   03   FILLER                   PIC X(3)    VALUE SPACE.                 
011300                                                                          
011400 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
011500*                                                                         
011600                                                                          
011700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011800 01  GENERELLA-SUBPROGRAM.                                                
011900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
012000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
012100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012500                                                                          
012600 01    ABENDKODER.                                                        
012700     03 FILLER                   PIC X(16) VALUE 'ABENDKODER'.            
012800     03 RKOD-ABEND-UTAN-DUMP     PIC S9(4) COMP SYNC VALUE +16.           
012900     03 RKOD-ABEND-MED-DUMP      PIC S9(4) COMP SYNC VALUE +33.           
013000     03 RKOD-FELTEXT             PIC X(32) VALUE SPACE.                   
013100     SKIP2                                                                
013200                                                                          
013300 01  MESSAGE-CODES.                                                       
013400     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
013500     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
013600     03  ERR-FEL-KOD             PIC X(3)    VALUE '025'.                 
013700     03  ERR-LINES-MISSING       PIC X(3)    VALUE '029'.                 
013800     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '054'.                 
013900     03  ERR-CAN-IMPOSSIBLE      PIC X(3)    VALUE '066'.                 
014000     03  INF-OK-BEHANDLAD        PIC X(3)    VALUE '101'.                 
014100                                                                          
014200*01  -COPY W009CIA                                                        
014300*                            IMS FUNKTIONSKODER                           
014400 01  FILLER                      PIC X(16)   VALUE 'W005INIT '.           
014500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014600*01 -COPY WMSGINIT                                                        
014700*                            IMS FUNKTIONSKODER                           
014800*01    -COPY W0003                                                        
014900   03    ROLB                    PIC X(4)    VALUE 'ROLB'.                
015000     EJECT                                                                
015100*                            DLI INPUT-OUTPUT AREA                        
015200                                                                          
015300     EJECT                                                                
015400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015500*                                                                         
015600 01  FILLER                  PIC X(16)   VALUE 'MID-AREA'.                
015700                                                                          
015800*01  -COPY W4I35501                                                       
015900     EJECT                                                                
016000 01  FILLER                  PIC X(16)   VALUE 'MSG/MOD-AREA'.            
016100                                                                          
016200*01  -COPY WMSGAREA                                                       
016300     EJECT                                                                
016400 01  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.                
016500                                                                          
016600*01  -COPY WMFSAREA                                                       
016700     EJECT                                                                
016800*    --- AREA FÖR KOMMUNIKATION MED DISPATCHER                            
016900*                                                                         
017000 01  FILLER                  PIC X(16)   VALUE 'KOM-DISP-IO-AREA'.        
017100                                                                          
017200 01  KOM-IO-AREA.                                                         
017300*  03  -COPY WMSGKOM                                                      
017400     EJECT                                                                
017500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600                                                                          
017700 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
017800                                                                          
017900 01    NYCKLAR-TILL-DLI.                                                  
018000*                                                                         
018100   03    W-WDE401-X.                                                      
018200     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
018300     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
018400     05    W-401-IDKUNDRF.                                                
018500       07  W-401-IDORDNR         PIC 9(5)    VALUE ZERO.                  
018600       07  FILLER                PIC X(5)    VALUE SPACE.                 
018700     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
018800     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
018900*                                                                         
019000   03    W-WDE411-X.                                                      
019100     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
019200*                                                                         
019300   03    W-WDE601-X.                                                      
019400     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
019500*                                                                         
019600   03  W-IDORDER-X.                                                       
019700     05  W-IDORDER               PIC S9(7)   COMP-3.                      
019800                                                                          
019900   03  W-IDDC-X.                                                          
020000     05  W-IDDC                  PIC X(02)   VALUE SPACE.                 
020100*                                                                         
020200   03    W-Q301-KEY-X.                                                    
020300     05    W-Q301-IDORDER        PIC S9(7)   VALUE ZERO  COMP-3.          
020400     05    W-Q301-IDDC           PIC X(2).                                
020500     05    W-Q301-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.          
020600     05    W-Q301-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.          
020700                                                                          
020800   03  W-Q301-KEY-MIN-X.                                                  
020900         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
021000         05  W-Q301-MIN-IDDC     PIC X(2).                                
021100         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
021200         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
021300                                                                          
021400   03  W-Q301-KEY-MAX-X.                                                  
021500         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
021600         05  W-Q301-MAX-IDDC     PIC X(2).                                
021700         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
021800         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
021900                                                                          
022000     03  W-WDQ301KY-MIN.                                                  
022100         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
022200         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
022300         05  FILLER                  PIC X(6)   VALUE LOW-VALUE.          
022400                                                                          
022500     03  W-WDQ301KY-MAX.                                                  
022600         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
022700         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
022800         05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.         
022900                                                                          
023000     03  W-WDE4ESEQ-X.                                                    
023100         05  W-E4ESEQ-IDPRODNR       PIC S9(7) VALUE ZERO COMP-3.         
023200                                                                          
023300     03  W-KDODELST                  PIC X.                               
023400     EJECT                                                                
023500 01    IMS-WS.                                                            
023600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
023700     SKIP3                                                                
023800*                        **** STATUS-KOD FRÅN IMS                         
023900   03    STATUS-WS               PIC XX.                                  
024000     88    SEGMENT-FINNS                     VALUE '  '.                  
024100     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
024200     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
024300     88    SEGMENT-SLUT                      VALUE 'GB'.                  
024400     SKIP3                                                                
024500   03    GODK-STATUSKODER.                                                
024600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
024700     SKIP3                                                                
024800 01    SSA1                      PIC X(128).                              
024900 01    SSA2                      PIC X(64).                               
025000 01    SSA3                      PIC X(64).                               
025100 01    SSA4                      PIC X(64).                               
025200     EJECT                                                                
025300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OBKR'.             
025400 01  DLI-IO-AREA-OBKR.                                                    
025500*    03  WLORQM01   -COPY WDQ101                                          
025600     EJECT                                                                
025700                                                                          
025800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OHUV'.             
025900 01  DLI-IO-AREA-OHUV.                                                    
026000*    03  WLORQI01   -COPY WDQ201                                          
026100     EJECT                                                                
026101                                                                          
026102 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARB'.              
026103 01  DLI-IO-AREA-ARB.                                                     
026104*    03  WLORQI12   -COPY WDQ212                                          
026105     EJECT                                                                
026106                                                                          
026107 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQA'.             
026108 01  DLI-IO-AREA-ORQA.                                                    
026109*    03  WLORQA01   -COPY WDQ301                                          
026110     EJECT                                                                
026120                                                                          
026130 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE601'.           
026140 01  DLI-IO-WDE601.                                                       
026150*    03  -COPY WDE601                                                     
026160     EJECT                                                                
026170                                                                          
026180 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE401'.           
026190 01  DLI-IO-WDE401.                                                       
026200*    03  -COPY WDE401                                                     
026300     EJECT                                                                
026400                                                                          
026500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE411'.           
026600 01  DLI-IO-WDE411.                                                       
026700*    03  -COPY WDE411                                                     
026800     EJECT                                                                
026900                                                                          
027000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-LOGG'.             
027100 01    DLI-IO-WDGZ01.                                                     
027200   03    IO-WDGZ01               PIC X(200)  VALUE SPACE.                 
027300     SKIP3                                                                
027400*  03    WDGZ01   -COPY WDGZ01  -PRE LOGG- -RED IO-WDGZ01.                
027500     EJECT                                                                
027600                                                                          
027700 01    FILLER                 PIC X(17) VALUE 'ANNULLATIONSTRANS'.        
027800     SKIP3                                                                
027900*01      WDGZRY5  -COPY WDGZRY5.                                          
028000     EJECT                                                                
028100     SKIP3                                                                
028200*01      WDGZRY5  -COPY WDGZRY5S.                                         
028300     EJECT                                                                
028400                                                                          
028500*    MSG-AREA FÖR HOPP TILL W20109                                        
028600 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
028700 01  W-PROG-TO-PROG-SW-1.                                                 
028800     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
028900     03  2109-Z1                   PIC X.                                 
029000     03  2109-Z2                   PIC X.                                 
029100     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
029200     03  2109-IDTRANS              PIC X(4)  VALUE '4355'.                
029300     03  2109-KDMFSFOR             PIC X.                                 
029400*    03  -COPY W2I10902    -PRE 2109-                                     
029500     EJECT                                                                
029600 LINKAGE SECTION.                                                         
029700*01    -COPY W0009     -PRE MSG-                                          
029800     EJECT                                                                
029900*01    -COPY W0009     -PRE DISP-                                         
030000     EJECT                                                                
030100*01    -COPY W0009     -PRE 2109-                                         
030200     EJECT                                                                
030300*01    -COPY W0009     -PRE USEA-                                         
030400     EJECT                                                                
030500*01    -COPY W0008     -PRE ORQA-                                         
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800*01    -COPY W0008     -PRE ORQI-                                         
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01    -COPY W0008     -PRE ORQM-                                         
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400*01    -COPY W0008     -PRE WDE4-                                         
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700*01    -COPY W0008     -PRE WDE41-                                        
031800     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000*01    -COPY W0008     -PRE WDE6-                                         
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300*01    -COPY W0008     -PRE ZZAC-                                         
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600 PROCEDURE DIVISION USING   MSG-PCB  DISP-PCB 2109-PCB  USEA-PCB          
032700                           ORQA-PCB  ORQI-PCB ORQM-PCB  WDE4-PCB          
032800                          WDE41-PCB  WDE6-PCB ZZAC-PCB.                   
032900                                                                          
033000     ENTRY 'DLITCBL' USING  MSG-PCB  DISP-PCB 2109-PCB  USEA-PCB          
033100                           ORQA-PCB  ORQI-PCB ORQM-PCB  WDE4-PCB          
033200                          WDE41-PCB  WDE6-PCB ZZAC-PCB.                   
033400     PERFORM IMS-GET-MSG                                                  
033500     IF SEGMENT-FINNS                                                     
033600       PERFORM IMS-GN-KOM-AREA                                            
033700       PERFORM A-INIT                                                     
033800       PERFORM B-KOLLA-INDATA                                             
033810                                                                          
033900                                                                          
034000       IF WS-INDATA-RATT AND GODKAND-TRANS                                
034100          PERFORM D-BEHANDLA-RADER                                        
034200       END-IF                                                             
034300                                                                          
034400       PERFORM Z-DISPATCH-AVSLUT                                          
034500                                                                          
034600     END-IF                                                               
034700                                                                          
034800     MOVE ZERO TO RETURN-CODE                                             
034900     GOBACK                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 A-INIT             SECTION.                                              
035300     MOVE 'STA A-SEC'                     TO   WS-PGM-POSITION            
035400     IF MSG-DUBBLA-TRANSKODER                                             
035500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I35501               
035600       MOVE MSG-IDTRANS-2                 TO   WS-IDTRANS                 
035700       MOVE MSG-KDMFSFOR-2                TO   WS-KDMFSFOR                
035800                                               2109-KDMFSFOR              
035900     ELSE                                                                 
036000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I35501               
036100       MOVE MSG-IDTRANS-1                 TO   WS-IDTRANS                 
036200       MOVE MSG-KDMFSFOR-1                TO   WS-KDMFSFOR                
036300                                               2109-KDMFSFOR              
036400     END-IF                                                               
036500                                                                          
036600     PERFORM AA-FLYTTA-INDATA-MID                                         
036700                                                                          
036800     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
036900     ACCEPT WS-HHMMSSDD                   FROM TIME                       
037000                                                                          
037100     MOVE SPACE                       TO MSG-KOM-IDMFSMED                 
037200                                                                          
037300     MOVE ZERO                        TO SPAR-VKORDNTO                    
037400                                         SPAR-VLORDNTO                    
037500                                         SPAR-SUORDV-LEVPL                
037600                                         SPAR-SUORDV-LEVPL-AVG            
037700                                         SPAR-SUORDV-LEVPL-LOC            
037800                                         SPAR-SUORDV-LEVPL-LOCPREL        
037900                                         WS-ANTAL-ANNULL-RADER            
038000                                                                          
038100     MOVE 'SLUTA-SEC'                  TO WS-PGM-POSITION                 
038200     .                                                                    
038300     SKIP2                                                                
038400 AA-FLYTTA-INDATA-MID    SECTION.                                         
038500                                                                          
038600     MOVE MID-IDDISTR                  TO WS-IDDISTR                      
038700                                          TEST-IDDISTR                    
038800     MOVE MID-IDKUNDNR                 TO WS-IDKUNDNR                     
038900     MOVE MID-IDORDNR7(3:5)            TO WS-IDORDNR5                     
039000     MOVE MID-IDPRODNR                 TO WS-IDPRODNR                     
039100     .                                                                    
039200     SKIP2                                                                
039300 B-KOLLA-INDATA      SECTION.                                             
039400     MOVE 'STA B-SEC'                  TO WS-PGM-POSITION                 
039500     MOVE RAETT                        TO WS-INDATA-TEST                  
039700     MOVE WS-IDPRODNR          TO W-601-IDPRODNR                          
039800     PERFORM IMS-GU-WDE601                                                
039900     IF SEGMENT-FINNS                                                     
040000       IF VORD-KDORDSTA < +3                                              
040100         MOVE VORD-IDPRODNR    TO W-E4ESEQ-IDPRODNR                       
040200         PERFORM IMS-GU-WDE401-ESEQ                                       
040300                                                                          
040400         MOVE KORD-IDORDER     TO W-IDORDER                               
040500         MOVE KORD-IDUSER      TO WS-IDUSER                               
040600         MOVE KORD-IDDC        TO WS-IDDC                                 
040700                                                                          
040800         MOVE +1 TO INDX                                                  
040900         PERFORM UNTIL INDX > MAX-INDX OR                                 
041000                       MID-IDRADNR (INDX) = SPACE                         
041100                                                                          
041200           MOVE MID-IDRADNR(INDX) TO WS-IDRADNR                           
041300           MOVE WS-IDRADNR        TO W-411-IDPURAD                        
041400           PERFORM IMS-GNP-WDE411                                         
041500           IF SEGMENT-FINNS                                               
041600              PERFORM BA-KONTROLLERA-RADEN                                
041700           ELSE                                                           
041800             MOVE ERR-LINES-MISSING TO MSG-KOM-IDMFSMED                   
041900             MOVE '4'               TO MSG-KOM-KDSVAR                     
042000             MOVE FEL               TO WS-INDATA-TEST                     
042100           END-IF                                                         
042200           ADD +1                TO INDX                                  
042300         END-PERFORM                                                      
042400       ELSE                                                               
042500         MOVE ERR-CAN-IMPOSSIBLE TO MSG-KOM-IDMFSMED                      
042600         MOVE '4'                TO MSG-KOM-KDSVAR                        
042700         MOVE FEL                TO WS-INDATA-TEST                        
042800       END-IF                                                             
042900     ELSE                                                                 
043000       MOVE ERR-ORDER-MISSING    TO MSG-KOM-IDMFSMED                      
043100       MOVE '4'                  TO MSG-KOM-KDSVAR                        
043200       MOVE FEL                  TO WS-INDATA-TEST                        
043300     END-IF                                                               
043400     MOVE 'SLUT B-SEC'           TO WS-PGM-POSITION                       
043500     .                                                                    
043600     EJECT                                                                
043700 BA-KONTROLLERA-RADEN SECTION.                                            
043800                                                                          
043900     MOVE 'STA BA-SEC '         TO WS-PGM-POSITION                        
044000     MOVE MID-KDORDBEK (INDX)   TO GODK-KDORDBEK                          
044100     IF GODK-KOD                                                          
044200       MOVE MID-IDARTPRE(INDX)  TO CIA-IDARTPRE-IN                        
044300       MOVE MID-IDARTBET(INDX)  TO CIA-IDARTBET-IN                        
044400                                                                          
044500       CALL W009CIA USING CIA-W009CIA                                     
044600       IF CIA-KDSVAR = 'F'                                                
044700          MOVE ERR-PART-MISSING  TO MSG-KOM-IDMFSMED                      
044800          MOVE '4'               TO MSG-KOM-KDSVAR                        
044900          MOVE FEL               TO WS-INDATA-TEST                        
045000       ELSE                                                               
045100          MOVE CIA-IDARTNR TO WS-IDARTNR                                  
045200          IF ORAD-IDARTNR = CIA-IDARTNR                                   
045300            IF ORAD-KDRADSTA < +4                                         
045400              CONTINUE                                                    
045500            ELSE                                                          
045600              MOVE ERR-UPDATE-NOT-ALLOWED TO MSG-KOM-IDMFSMED             
045700              MOVE '4'              TO MSG-KOM-KDSVAR                     
045800              MOVE FEL              TO WS-INDATA-TEST                     
045900            END-IF                                                        
046000          ELSE                                                            
046100            MOVE ERR-PART-MISSING   TO MSG-KOM-IDMFSMED                   
046200            MOVE '4'                TO MSG-KOM-KDSVAR                     
046300            MOVE FEL                TO WS-INDATA-TEST                     
046400          END-IF                                                          
046500       END-IF                                                             
046600     ELSE                                                                 
046700       MOVE ERR-FEL-KOD             TO MSG-KOM-IDMFSMED                   
046800       MOVE '4'                     TO MSG-KOM-KDSVAR                     
046900       MOVE FEL                     TO WS-INDATA-TEST                     
047000     END-IF                                                               
047100     MOVE 'SLUT BA-SEC'             TO WS-PGM-POSITION                    
047200     .                                                                    
047300     EJECT                                                                
047400 D-BEHANDLA-RADER     SECTION.                                            
047500     MOVE 'STA D-SEC '              TO WS-PGM-POSITION                    
047600     MOVE KORD-IDDISTR         TO W-401-IDDISTR                           
047700     MOVE KORD-IDKUNDNR        TO W-401-IDKUNDNR                          
047800     MOVE KORD-IDORDNR5        TO W-401-IDORDNR                           
047900     MOVE KORD-IDPRODNR        TO W-401-IDPRODNR                          
048000     MOVE KORD-IDPLKLST        TO W-401-IDPLKLST                          
048200     PERFORM IMS-GU-WDQ201                                                
048300                                                                          
048400     MOVE +1 TO INDX                                                      
048500                                                                          
048600     PERFORM UNTIL INDX > MAX-INDX OR                                     
048700       MID-IDRADNR(INDX) = SPACE                                          
048800       MOVE MID-IDRADNR (INDX)      TO WS-IDRADNR                         
048900       MOVE WS-IDRADNR              TO W-411-IDPURAD                      
049000       PERFORM IMS-GHU-WDE411                                             
049100       PERFORM DA-UPPDATERA-E4-RAD                                        
049200       PERFORM DB-SKAPA-TRANSAR                                           
049300       ADD +1                       TO INDX                               
049400     END-PERFORM                                                          
049500                                                                          
049600     IF 2109-IX > ZERO                                                    
049700        COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17             
049800        PERFORM IMS-PURG-MSG-2109                                         
049900     END-IF                                                               
050000                                                                          
050100     PERFORM DC-UPPDATERA-E6                                              
050200     PERFORM DD-UPPDATERA-E4                                              
050300                                                                          
050400     IF VORD-KVORDRAD = VORD-KVORDRAD-PACK                                
050500       PERFORM DE-UPPDATERA-Q3                                            
050600       PERFORM DF-UPPDATERA-Q2                                            
050700     END-IF                                                               
050800                                                                          
050900     MOVE 'SLUT D-SEC '             TO WS-PGM-POSITION                    
051000     .                                                                    
051100     EJECT                                                                
051200 DA-UPPDATERA-E4-RAD         SECTION.                                     
051300     MOVE 'STA DA-SEC '             TO WS-PGM-POSITION                    
051400                                                                          
051500     MOVE ORAD-KVBEART  TO ORAD-KVANNANT                                  
051600     MOVE ZERO          TO ORAD-KVAVBART                                  
051700     MOVE +4            TO ORAD-KDRADSTA                                  
051800     ADD +1             TO WS-ANTAL-ANNULL-RADER                          
051900                                                                          
052000     PERFORM DAA-SPAR-UPPGIFTER                                           
052100                                                                          
052200     PERFORM IMS-REPL-WDE411                                              
052300                                                                          
052400     MOVE 'SLUT DA-SEC '            TO WS-PGM-POSITION                    
052500     .                                                                    
052600     EJECT                                                                
052700 DAA-SPAR-UPPGIFTER       SECTION.                                        
052800     COMPUTE SPAR-VKORDNTO ROUNDED = SPAR-VKORDNTO +                      
052900                 (ORAD-VKARTNTO * ORAD-KVANNANT)                          
053000                                                                          
053100     COMPUTE SPAR-VLORDNTO ROUNDED = SPAR-VLORDNTO +                      
053200                 (ORAD-VLARTNTO * ORAD-KVANNANT / 1000000)                
053300                                                                          
053400     COMPUTE SPAR-SUORDV-LEVPL-LOC ROUNDED =                              
053500                                         SPAR-SUORDV-LEVPL-LOC +          
053600                 (ORAD-PRARTNTO-LOC * ORAD-KVANNANT)                      
053700                                                                          
053800     COMPUTE SPAR-SUORDV-LEVPL-LOCPREL ROUNDED =                          
053900                                       SPAR-SUORDV-LEVPL-LOCPREL +        
054000                 (ORAD-PRARTNTO-LOCPREL * ORAD-KVANNANT)                  
054100                                                                          
054200     COMPUTE SPAR-SUORDV-LEVPL ROUNDED = SPAR-SUORDV-LEVPL +              
054300                 (ORAD-PRARTNTO * ORAD-KVANNANT)                          
054400                                                                          
054500     COMPUTE SPAR-SUORDV-LEVPL-AVG ROUNDED = SPAR-SUORDV-LEVPL-AVG        
054600               + (ORAD-PRAVCOST * ORAD-KVANNANT)                          
054700     .                                                                    
054800     EJECT                                                                
054900 DB-SKAPA-TRANSAR   SECTION.                                              
055000                                                                          
055100     MOVE 'STA DB-SEC '             TO WS-PGM-POSITION                    
055200     PERFORM DBA-GENERERA-ANNULL-TRANS                                    
055300                                                                          
055400     PERFORM DBB-UPDAT-ORDBEK-WDQ1                                        
055500                                                                          
055600     IF ORAD-KDOI NOT = SPACE                                             
055700       PERFORM DBC-GENERERA-2109-TRANS                                    
055800     END-IF                                                               
055900     MOVE 'SLUT DB-SEC '            TO WS-PGM-POSITION                    
056000     .                                                                    
056100     EJECT                                                                
056200 DBA-GENERERA-ANNULL-TRANS      SECTION.                                  
056300****                                                                      
056400****SOFT*TRANSEN BEHANDLAS I W09208 SOM SKAPAR SU3 TRANSAR                
056500****SOFT*FÖR VR/VIPS. KDORDBEK BLIR 83 I W09298 OAVSÄTT DET               
056600****SOFT*ÄR 30, 31, 32, 33 ELLER 34 PÅ WDQ1.                              
056700****                                                                      
056800     MOVE 'STA DBA-SEC '            TO WS-PGM-POSITION                    
056900                                                                          
057000     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
057100     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
057200     ADD +1                               TO  LOGG-IDLOGLOP               
057300     MOVE 'RY5'                           TO  RY5-IDPTYP                  
057400                                              LOGG-IDPTYP                 
057500     MOVE ORAD-BERADREF                   TO  RY5-BERADREF                
057600     MOVE ORAD-BEVOLREF                   TO  RY5-BEVOLREF                
057700     MOVE KORD-IDKUNDRF                   TO  RY5-IDKUNDRF                
057800     MOVE ORAD-IDARTNR                    TO  RY5-IDARTNR                 
057900     MOVE ORAD-FLRESTN                    TO  RY5-FLRESTN                 
058000     MOVE ORAD-FLDIRLEV                   TO  RY5-FLDIRLEV                
058100     MOVE KORD-FLLSBOK                    TO  RY5-FLLSBOK                 
058200     MOVE KORD-FLORDSPE                   TO  RY5-FLORDSPE                
058300     MOVE ORAD-IDKUNDRF-RO                TO  RY5-IDKUNDRF-RO             
058400                                                                          
058500     IF ORAD-FLTILLK = JA                                                 
058600         MOVE 1                           TO  RY5-KDARTERS                
058700     ELSE                                                                 
058800         MOVE ZERO                        TO  RY5-KDARTERS                
058900     END-IF                                                               
059000                                                                          
059100     MOVE KORD-IDDC                       TO  RY5-IDDC                    
059200     MOVE ORAD-KDDSP                      TO  RY5-KDDSP                   
059300     MOVE KORD-KDFAKTYP                   TO  RY5-KDFAKTYP                
059400     MOVE ORAD-KDFRAKT                    TO  RY5-KDFRAKT                 
059500     MOVE ORAD-KDORDING                   TO  RY5-KDORDING                
059600     MOVE ORAD-KDORDKL                    TO  RY5-KDORDKL                 
059700                                              RY5-KDORDKL-URS             
059800     MOVE ORAD-KDORDTYP                   TO  RY5-KDORDTYP                
059900     MOVE ORAD-KDKVBRYT                   TO  RY5-KDKVBRYT                
060000     MOVE ORAD-KDVRINFO                   TO  RY5-KDVRINFO                
060100     MOVE ORAD-KVBEART                    TO  RY5-KVBEART                 
060200     MOVE ORAD-KVAVBART                   TO  RY5-KVAVBART                
060300     MOVE ORAD-KVANNANT                   TO  RY5-KVANNANT                
060400                                              RY5-KVAVART                 
060500     MOVE ORAD-REKSIFFR                   TO  RY5-REKSIFFR                
060600     MOVE ORAD-TIUTSKR                    TO  RY5-TIORDREG                
060700     MOVE ORAD-TIRODAT                    TO  RY5-TIRODAT                 
060800     MOVE RY5-WDGZRY5                     TO  LOGG-LOGGPOST               
060900     MOVE KORD-IDDISTR                    TO  RY5S-IDDISTR                
061000     MOVE KORD-IDKUNDNR                   TO  RY5S-IDKUNDNR               
061100     IF  OHUV-FLVORKO = JA                                                
061200     OR  OHUV-FLVORKO = YES                                               
061300         MOVE JA                          TO  RY5S-FLVORKO                
061400     ELSE                                                                 
061500         MOVE OHUV-FLVORKO                TO  RY5S-FLVORKO                
061600     END-IF                                                               
061700     MOVE OHUV-FLFORBI                    TO  RY5S-FLFORBI                
061800     MOVE OHUV-FLOVRLEV                   TO  RY5S-FLOVRLEV               
061900     MOVE ORAD-IDSYSTEM                   TO  RY5S-IDSYSTEM               
062000     MOVE ORAD-KVSLATT                    TO  RY5S-KVSLATT                
062100     MOVE ORAD-KDPRODSL                   TO  RY5S-KDPRODSL               
062200     MOVE SPACE                           TO  RY5S-FILLERX5               
062300                                              RY5S-FILLERX10              
062400     MOVE ZERO                            TO  RY5S-KDTPOTYP               
062500     MOVE RY5S-WDGZRY5S-CTX               TO  LOGG-SORTPOST               
062600                                                                          
062700     PERFORM IMS-ISRT-ZZAC01                                              
062800     PERFORM UNTIL SEGMENT-FINNS                                          
062900       IF LOGG-IDLOGLOP = 9                                               
063000         MOVE ZERO              TO LOGG-IDLOGLOP                          
063100         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
063200       END-IF                                                             
063300       ADD +1                   TO LOGG-IDLOGLOP                          
063400       PERFORM IMS-ISRT-ZZAC01                                            
063500     END-PERFORM                                                          
063600     MOVE 'SLUT DBA-SEC '                 TO WS-PGM-POSITION              
063700     .                                                                    
063800     EJECT                                                                
063900 DBB-UPDAT-ORDBEK-WDQ1 SECTION.                                           
064000                                                                          
064100     MOVE 'STA  DBB-SEC '                 TO WS-PGM-POSITION              
064200     MOVE KORD-IDORDER                    TO OBKR-IDORDER                 
064300     MOVE ORAD-IDARTNR                    TO OBKR-IDARTNR                 
064400     MOVE 1                               TO OBKR-IDLOPNR                 
064500     MOVE 1                               TO OBKR-IDSEKVNR                
064600     MOVE KORD-IDDC                       TO OBKR-IDDC                    
064700     MOVE MID-KDORDBEK(INDX)              TO OBKR-KDORDBEK                
064800     MOVE IDPGM                           TO OBKR-IDPGM                   
064900     MOVE SPACE                           TO OBKR-BEERS                   
065000     MOVE OHUV-BEKUNDRF                   TO OBKR-BEKUNDRF                
065100     MOVE ORAD-BERADREF                   TO OBKR-BERADREF                
065200     MOVE ORAD-BEVOLREF                   TO OBKR-BEVOLREF                
065300     MOVE ORAD-IDKAMPRF                   TO OBKR-IDKAMPRF                
065400     MOVE 0                               TO OBKR-DIERS-KVOT              
065500     MOVE NEJ                             TO OBKR-FLAKPLOC                
065600     MOVE NEJ                             TO OBKR-FLSLATT                 
065700     MOVE ORAD-FLINVEST                   TO OBKR-FLINVEST                
065800     MOVE JA                              TO OBKR-FLOBOK                  
065900     MOVE NEJ                             TO OBKR-FLOBTRAN                
066000     MOVE NEJ                             TO OBKR-FLOBPRT                 
066100     MOVE ORAD-FLPRTILL                   TO OBKR-FLPRTILL                
066200     MOVE ORAD-FLRESTN                    TO OBKR-FLRESTN                 
066300     MOVE NEJ                             TO OBKR-FLTILLK                 
066400     MOVE 0                               TO OBKR-IDARTNR-TILLK           
066500     MOVE ORAD-IDDC-RO                    TO OBKR-IDDC-RO                 
066600     MOVE KORD-IDDISTR                    TO OBKR-IDDISTR                 
066700     MOVE KORD-IDKUNDNR                   TO OBKR-IDKUNDNR                
066800                                                                          
066900     MOVE KORD-IDKUNDRF                   TO WS-IDKUNDRF-OLD              
067000     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
067100     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF                
067200                                                                          
067300     MOVE ORAD-IDKUNDRF-RO                TO WS-IDKUNDRF-OLD              
067400     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
067500     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF-RO             
067600                                                                          
067700     MOVE ORAD-IDLEVNR                    TO OBKR-IDLEVNR                 
067800     MOVE ORAD-IDLOPNR-RO                 TO OBKR-IDLOPNR-RO              
067900     MOVE ORAD-IDSYSTEM                   TO OBKR-IDSYSTEM                
068000     MOVE ORAD-KDDSP                      TO OBKR-KDDSP                   
068100     MOVE 0                               TO OBKR-KDERS                   
068200     MOVE ORAD-KDOI                       TO OBKR-KDOI                    
068300     MOVE ORAD-CLEARGROUP                 TO OBKR-CLEARGROUP              
068400     MOVE ORAD-KDKVBRYT                   TO OBKR-KDKVBRYT                
068500     MOVE ORAD-KDPRTYP                    TO OBKR-KDPRTYP                 
068600     MOVE 0                               TO OBKR-KDTPOTYP                
068700     MOVE ORAD-KDVRINFO                   TO OBKR-KDVRINFO                
068800                                                                          
068900     MOVE ORAD-KVANNANT                   TO OBKR-KVANNANT                
069000     MOVE ORAD-KVAVBART                   TO OBKR-KVAVBART                
069100     MOVE ORAD-KVBEART                    TO OBKR-KVBEART                 
069200                                             OBKR-KVBEART-Q               
069300     MOVE 0                               TO OBKR-KVBEART-TILLK           
069400     MOVE 0                               TO OBKR-KVPREAVB                
069500     MOVE 0                               TO OBKR-KVPRERO                 
069600     MOVE ZERO                            TO OBKR-KVQPACK                 
069700     MOVE 0                               TO OBKR-KVRO                    
069800     MOVE ZERO                            TO OBKR-TIRODAT                 
069900     MOVE ORAD-KVSLATT                    TO OBKR-KVSLATT                 
070000     MOVE ORAD-DEAL-PR-LINE               TO OBKR-DEAL-PR-LINE            
070100     MOVE ORAD-PRARTNTO                   TO OBKR-PRARTNTO                
070200     MOVE 0                               TO OBKR-PRBPRIS                 
070300     MOVE ORAD-REKSIFFR                   TO OBKR-REKSIFFR                
070400     MOVE 0                               TO OBKR-REKSIFFR-TILLK          
070500     MOVE 0                               TO OBKR-RERF-RAD                
070600     MOVE +0                              TO OBKR-TIDISPIN                
070700     MOVE KORD-TIORDREG                   TO OBKR-TIORDREG                
070800                                             WS-DATUM-9KOMPL              
070900     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
071000     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
071100     END-COMPUTE                                                          
071200     MOVE ORAD-TIPRIS                     TO OBKR-TIPRIS                  
071300                                                                          
071400     MOVE WS-DAGENS-DATUM                 TO OBKR-TIREGDAT                
071500     MOVE WS-HHMMSS                       TO OBKR-TIREGTID                
071600     MOVE KORD-TIORDREG                   TO WS-DATUM-9KOMPL              
071700     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
071800                                                                          
071900     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
072000     END-COMPUTE                                                          
072100     MOVE 0                                TO OBKR-TITPO                  
072200     MOVE ORAD-KDFRAKT                     TO OBKR-KDFRAKT                
072300     MOVE ORAD-KDORDKL                     TO OBKR-KDORDKL                
072400     MOVE ORAD-IDBIL                       TO OBKR-IDBIL                  
072500                                                                          
072600     MOVE OHUV-KDORDTYP-LDC                TO OBKR-KDORDTYP-LDC           
072700     MOVE OHUV-TIREPDAT                    TO OBKR-TIREPDAT               
072800     MOVE ORAD-IDKUNDRF-WIP                TO OBKR-IDKUNDRF-WIP           
072900     MOVE ZERO                             TO OBKR-TIDLEVDAT              
073000     MOVE ORAD-PRAVCOST                    TO OBKR-PRAVCOST               
073100     MOVE ORAD-KDVALISO                    TO OBKR-KDVALISO               
073200                                                                          
073300     PERFORM IMS-ISRT-WDQ101                                              
073400     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
073500        ADD +1                             TO OBKR-IDLOPNR                
073600        PERFORM IMS-ISRT-WDQ101                                           
073700     END-PERFORM                                                          
073800     MOVE 'SLUT DBB-SEC '           TO WS-PGM-POSITION                    
073900     .                                                                    
074000     EJECT                                                                
074100 DBC-GENERERA-2109-TRANS  SECTION.                                        
074200                                                                          
074300     MOVE 'STA  DBC-SEC '           TO WS-PGM-POSITION                    
074400                                                                          
074500*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
074600     MOVE ORAD-IDARTNR          TO BYT03-IDARTNR                          
074700     IF NOT BYT03-OBJEKT                                                  
074800        ADD +1                  TO 2109-IX                                
074900        MOVE 2109-IX            TO 2109-MID2-KVANTART                     
075000        MOVE ORAD-IDARTNR       TO 2109-MID2-IDARTNR (2109-IX)            
075100        MOVE OHUV-IDDC-PRIM     TO 2109-MID2-IDDC (2109-IX)               
075200        MOVE ORAD-KDOI          TO 2109-MID2-KDOI (2109-IX)               
075300        MOVE ORAD-CLEARGROUP    TO 2109-MID2-CLEARGROUP(2109-IX)          
075400        MOVE '-'                TO 2109-MID2-KDTECKEN (2109-IX)           
075500        MOVE ORAD-KVANNANT      TO 2109-MID2-KVOI (2109-IX)               
075600        MOVE KORD-TIORDREG      TO 2109-MID2-TIUPPDAT (2109-IX)           
075700                                                                          
075800        IF 2109-IX > 2109-IX-MAX                                          
075900          PERFORM DBCA-STARTA-2109                                        
076000        END-IF                                                            
076100     END-IF                                                               
076200     MOVE 'SLUT DBC-SEC '           TO WS-PGM-POSITION                    
076300     .                                                                    
076400     EJECT                                                                
076500 DBCA-STARTA-2109 SECTION.                                                
076600                                                                          
076700     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
076800                                                                          
076900     PERFORM IMS-PURG-MSG-2109                                            
077000                                                                          
077100     MOVE SPACE              TO 2109-MID2-W2I10902                        
077200     MOVE +0                 TO 2109-IX                                   
077300     .                                                                    
077400     EJECT                                                                
077500 DC-UPPDATERA-E6                      SECTION.                            
077600                                                                          
077700     MOVE 'STA  DC-SEC '            TO WS-PGM-POSITION                    
077800     PERFORM IMS-GHU-WDE601                                               
077900                                                                          
078000     ADD WS-ANTAL-ANNULL-RADER   TO   VORD-KVORDRAD-PACK                  
078100     SUBTRACT SPAR-SUORDV-LEVPL-LOC FROM VORD-SUORDV-LOC                  
078200     SUBTRACT SPAR-SUORDV-LEVPL-LOCPREL                                   
078300                                    FROM VORD-SUORDV-LOCPREL              
078400     SUBTRACT SPAR-SUORDV-LEVPL FROM VORD-SUORDV                          
078500     SUBTRACT SPAR-SUORDV-LEVPL-AVG FROM VORD-SUORDV                      
078600     SUBTRACT SPAR-VKORDNTO      FROM VORD-VKORDNTO                       
078700     SUBTRACT SPAR-VLORDNTO      FROM VORD-VLORDNTO                       
078800                                                                          
078900     IF VORD-KVORDRAD-PACK = VORD-KVORDRAD                                
079000       IF VORD-KVORDRAD = +1                                              
079100         MOVE WS-DAGENS-DATUM    TO VORD-TIPACKN-SK                       
079200         MOVE +5                 TO VORD-KDORDSTA                         
079300       ELSE                                                               
079400         IF VORD-KVKOLLI = VORD-KVKOLLI-FAKT AND                          
079500            VORD-KVKOLLI = VORD-KVKOLLI-LAST                              
079600           MOVE +5               TO VORD-KDORDSTA                         
079700         ELSE                                                             
079800           IF VORD-KVKOLLI-FL = VORD-KVKOLLI                              
079900             MOVE +4               TO VORD-KDORDSTA                       
080000           ELSE                                                           
080100             MOVE +3               TO VORD-KDORDSTA                       
080200           END-IF                                                         
080300         END-IF                                                           
080400       END-IF                                                             
080500     END-IF                                                               
080600                                                                          
080700     PERFORM IMS-REPL-WDE601                                              
080800                                                                          
080900     MOVE 'SLUT DC-SEC '            TO WS-PGM-POSITION                    
081000     .                                                                    
081100     EJECT                                                                
081200                                                                          
081300 DD-UPPDATERA-E4                      SECTION.                            
081400                                                                          
081500     MOVE 'STA  DD-SEC '            TO WS-PGM-POSITION                    
081600                                                                          
081700     PERFORM IMS-GHU-WDE401                                               
081800     MOVE VORD-VKORDNTO             TO  KORD-VKORDNTO                     
081900     MOVE VORD-VLORDNTO             TO  KORD-VLORDNTO                     
082000     MOVE VORD-DEAL-PR-SUM          TO  KORD-DEAL-PR-SUM                  
082100     MOVE VORD-SUORDV               TO  KORD-SUORDV-LEVPL                 
082200     MOVE VORD-KVORDRAD-PACK        TO  KORD-KVORDRAD-PACK                
082300                                                                          
082400     PERFORM IMS-REPL-WDE401                                              
082500                                                                          
082600     MOVE 'SLUT DD-SEC '            TO WS-PGM-POSITION                    
082700     .                                                                    
082800     EJECT                                                                
082900 DE-UPPDATERA-Q3                         SECTION.                         
083000                                                                          
083100     MOVE 'STA  DE-SEC '             TO WS-PGM-POSITION                   
083200     MOVE KORD-IDORDER               TO W-Q301-IDORDER                    
083300     MOVE KORD-IDDC                  TO W-Q301-IDDC                       
083400     MOVE KORD-IDPRODNR              TO W-Q301-IDPRODNR                   
083500     MOVE KORD-IDPLKLST              TO W-Q301-IDPLKLST                   
083600     PERFORM IMS-GHU-WDQ301                                               
083700                                                                          
083800     MOVE KORD-KVORDRAD-PACK         TO ODEL-KVPACKRAD-OD                 
083900     MOVE 'P'                        TO ODEL-KDODELSTA                    
084000     MOVE WS-DAGENS-DATUM            TO ODEL-TIPACKN                      
084100     MOVE WS-HHMMSS                  TO ODEL-TIPACTID                     
084200     PERFORM IMS-REPL-WDQ301                                              
084300     MOVE 'SLUT DE-SEC '            TO WS-PGM-POSITION                    
084400     .                                                                    
084500     EJECT                                                                
084600 DF-UPPDATERA-Q2                         SECTION.                         
084700     MOVE 'STA  DF-SEC '            TO WS-PGM-POSITION                    
084900     MOVE NEJ                       TO KDORDSTA-SW                        
085000                                                                          
085100     MOVE KORD-IDORDER              TO W-Q301KY-MIN-IDORDER               
085200                                       W-Q301KY-MAX-IDORDER               
085300     MOVE WS-IDDC                   TO W-Q301KY-MIN-IDDC                  
085400                                       W-Q301KY-MAX-IDDC                  
085500                                                                          
085600     MOVE 'R'                       TO W-KDODELST                         
085700     PERFORM IMS-GU-WDQ301-STATUS                                         
085800     IF SEGMENT-FINNS                                                     
085900        MOVE 'R*'                   TO WS-KDORDSTA                        
086000        MOVE JA                     TO KDORDSTA-SW                        
086100     ELSE                                                                 
086200                                                                          
086300        MOVE 'U'                    TO W-KDODELST                         
086400        PERFORM IMS-GU-WDQ301-STATUS                                      
086500        IF SEGMENT-FINNS                                                  
086600           MOVE 'U*'                TO WS-KDORDSTA                        
086700           MOVE JA                  TO KDORDSTA-SW                        
086800        ELSE                                                              
086900           PERFORM DFA-KOLLA-KVKOLLI                                      
087000           IF KDORDSTA-KLAR                                               
087100               CONTINUE                                                   
087200            ELSE                                                          
087300               PERFORM DFB-TA-FRAM-KDORDSTA                               
087400           END-IF                                                         
087500        END-IF                                                            
087600     END-IF                                                               
087700                                                                          
087800     MOVE WS-IDDC                   TO W-IDDC                             
087900     PERFORM IMS-GHNP-WDQ212                                              
087910     IF SEGMENT-FINNS                                                     
088000        MOVE WS-KDORDSTA            TO ARB-KDORDSTA                       
088100        PERFORM IMS-REPL-WDQ212                                           
088200     END-IF                                                               
088300     MOVE 'SLUT DF-SEC '            TO WS-PGM-POSITION                    
088400     .                                                                    
088500     EJECT                                                                
088600 DFA-KOLLA-KVKOLLI SECTION.                                               
088700                                                                          
088800     MOVE 'STA  DFA-SEC '           TO WS-PGM-POSITION                    
088900                                                                          
089000     IF (VORD-KVKOLLI-LAST     >  0    OR                                 
089100        VORD-KVKOLLI-FL        >  0    OR                                 
089200        VORD-KVKOLLI-FAKT      >  0)   OR                                 
089300        VORD-KDORDSTA          =  5                                       
089400                                                                          
089500        COMPUTE W-KVKOLLI      =  W-KVKOLLI + VORD-KVKOLLI                
089600        COMPUTE W-KVKOLLI-FAKT =                                          
089700                       W-KVKOLLI-FAKT + VORD-KVKOLLI-FAKT                 
089800        COMPUTE W-KVKOLLI-LAST =                                          
089900                       W-KVKOLLI-LAST + VORD-KVKOLLI-LAST                 
090000        COMPUTE W-KVKOLLI-FL =                                            
090100                       W-KVKOLLI-FL + VORD-KVKOLLI-FL                     
090200     ELSE                                                                 
090300        MOVE 'P '              TO WS-KDORDSTA                             
090400        MOVE JA                TO KDORDSTA-SW                             
090500     END-IF                                                               
090600     MOVE 'SLUT DFA-SEC '           TO WS-PGM-POSITION                    
090700     .                                                                    
090800     EJECT                                                                
090900 DFB-TA-FRAM-KDORDSTA SECTION.                                            
091000     MOVE 'STA  DEB-SEC '           TO WS-PGM-POSITION                    
091100                                                                          
091200     IF VORD-KDORDSTA          <  4                                       
091300        MOVE 'P*'              TO WS-KDORDSTA                             
091400     ELSE                                                                 
091500       IF W-KVKOLLI-FL            = W-KVKOLLI                             
091600         IF W-KVKOLLI-FAKT = ZERO AND                                     
091700            W-KVKOLLI-LAST = ZERO                                         
091800           MOVE 'S'          TO WS-KDORDSTA                               
091900         ELSE                                                             
092000           IF W-KVKOLLI-FAKT     NOT = W-KVKOLLI  AND                     
092100              W-KVKOLLI-LAST     NOT = W-KVKOLLI                          
092200             MOVE 'S*'       TO WS-KDORDSTA                               
092300           ELSE                                                           
092400             MOVE 'SF'       TO WS-KDORDSTA                               
092500           END-IF                                                         
092600         END-IF                                                           
092700       END-IF                                                             
092800     END-IF                                                               
092900     MOVE 'SLUT DEB-SEC '           TO WS-PGM-POSITION                    
093000     .                                                                    
093100     EJECT                                                                
093200 Z-DISPATCH-AVSLUT     SECTION.                                           
093300                                                                          
093400*    SKRIV FEL/KLAR MEDDELANDE TILL MPP DISPATCHERN                       
093500     IF MSG-KOM-IDMFSMED = SPACE                                          
093600        MOVE INF-OK-BEHANDLAD  TO MSG-KOM-IDMFSMED                        
093700     END-IF                                                               
093800     PERFORM IMS-INSERT-DISP-MSG                                          
093900     .                                                                    
094000     EJECT                                                                
094100* IMS SEKTIONER                                                           
094200*                                                                         
094300 IMS-GET-MSG SECTION.                                                     
094400     SKIP2                                                                
094500     MOVE '  QC' TO GODK-STATUSKODER                                      
094600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
094700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094800     PERFORM IMS-STATUSKONTROLL                                           
094900     .                                                                    
095000     SKIP2                                                                
095100 IMS-GN-KOM-AREA SECTION.                                                 
095200                                                                          
095300     MOVE '  '   TO GODK-STATUSKODER                                      
095400     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
095500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095600     PERFORM IMS-STATUSKONTROLL                                           
095700     .                                                                    
095800     SKIP2                                                                
095900 IMS-INSERT-DISP-MSG SECTION.                                             
096000                                                                          
096100     MOVE SPACE TO GODK-STATUSKODER                                       
096200     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
096300     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
096400     PERFORM IMS-STATUSKONTROLL                                           
096500     .                                                                    
096600     SKIP2                                                                
096700 IMS-PURG-MSG-2109 SECTION.                                               
096800                                                                          
096900     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
097000     MOVE '  '  TO GODK-STATUSKODER                                       
097100     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
097200     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
097300     PERFORM IMS-STATUSKONTROLL                                           
097400     .                                                                    
097500     SKIP2                                                                
097600 IMS-GHU-WDE401 SECTION.                                                  
097700     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
097800            DELIMITED BY SIZE INTO SSA1                                   
097900     MOVE '    ' TO GODK-STATUSKODER                                      
098000     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-WDE401 SSA1                
098100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
098200     PERFORM IMS-STATUSKONTROLL                                           
098300     SKIP2                                                                
098400     .                                                                    
098500 IMS-REPL-WDE401 SECTION.                                                 
098600     MOVE '  '   TO GODK-STATUSKODER                                      
098700     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE401                       
098800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
098900     PERFORM IMS-STATUSKONTROLL                                           
099000     SKIP2                                                                
099100     .                                                                    
099200 IMS-GHU-WDE411    SECTION.                                               
099300     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
099400            DELIMITED BY SIZE INTO SSA1                                   
099500     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
099600            DELIMITED BY SIZE INTO SSA2                                   
099700     MOVE '    ' TO GODK-STATUSKODER                                      
099800     CALL CBLTDLI USING GHU  WDE4-PCB DLI-IO-WDE411 SSA1 SSA2             
099900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
100000     PERFORM IMS-STATUSKONTROLL                                           
100100     SKIP2                                                                
100200     .                                                                    
100300 IMS-REPL-WDE411        SECTION.                                          
100400     MOVE '    ' TO GODK-STATUSKODER                                      
100500     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-WDE411                       
100600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
100700     PERFORM IMS-STATUSKONTROLL                                           
100800     .                                                                    
100900     EJECT                                                                
101000 IMS-GHU-WDQ301   SECTION.                                                
101100     STRING 'WLORQA01(WDQ301KY =' W-Q301-KEY-X ')'                        
101200            DELIMITED BY SIZE INTO SSA1                                   
101300     MOVE '  ' TO GODK-STATUSKODER                                        
101400     CALL CBLTDLI USING GHU    ORQA-PCB ODEL-WDQ301 SSA1                  
101500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
101600     PERFORM IMS-STATUSKONTROLL                                           
101700     .                                                                    
101800 IMS-GU-WDQ301-STATUS  SECTION.                                           
101900                                                                          
102000     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
102100                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
102200                    '&KDODELST =' W-KDODELST     ')'                      
102300          DELIMITED BY SIZE INTO SSA1                                     
102400     MOVE '  GE' TO GODK-STATUSKODER                                      
102500     CALL CBLTDLI USING GU  ORQA-PCB ODEL-WDQ301 SSA1                     
102600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
102700     PERFORM IMS-STATUSKONTROLL                                           
102800     .                                                                    
102900                                                                          
103000 IMS-REPL-WDQ301        SECTION.                                          
103100     MOVE '    ' TO GODK-STATUSKODER                                      
103200     CALL CBLTDLI USING REPL ORQA-PCB ODEL-WDQ301                         
103300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
103400     PERFORM IMS-STATUSKONTROLL                                           
103500     .                                                                    
103600     EJECT                                                                
103700 IMS-ISRT-WDQ101     SECTION.                                             
103800     MOVE 'WLORQM01 ' TO SSA1                                             
103900     MOVE '  II'   TO GODK-STATUSKODER                                    
104000     CALL CBLTDLI USING ISRT ORQM-PCB OBKR-WDQ101 SSA1                    
104100     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
104200     PERFORM IMS-STATUSKONTROLL                                           
104300     .                                                                    
104400     EJECT                                                                
104500 IMS-GU-WDE601 SECTION.                                                   
104600     STRING 'WDE601  (IDPRODNR =' W-WDE601-X ')'                          
104700            DELIMITED BY SIZE INTO SSA1                                   
104800     MOVE '  GE' TO GODK-STATUSKODER                                      
104900     CALL CBLTDLI USING GU   WDE6-PCB DLI-IO-WDE601 SSA1                  
105000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
105100     PERFORM IMS-STATUSKONTROLL                                           
105200     SKIP2                                                                
105300     .                                                                    
105400 IMS-GHU-WDE601 SECTION.                                                  
105500     STRING 'WDE601  (IDPRODNR =' W-WDE601-X ')'                          
105600            DELIMITED BY SIZE INTO SSA1                                   
105700     MOVE '    ' TO GODK-STATUSKODER                                      
105800     CALL CBLTDLI USING GHU   WDE6-PCB DLI-IO-WDE601 SSA1                 
105900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
106000     PERFORM IMS-STATUSKONTROLL                                           
106100     SKIP2                                                                
106200     .                                                                    
106300 IMS-GU-WDE401-ESEQ SECTION.                                              
106400     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
106500            DELIMITED BY SIZE INTO SSA1                                   
106600     MOVE '    ' TO GODK-STATUSKODER                                      
106700     CALL CBLTDLI USING GU  WDE41-PCB DLI-IO-WDE401 SSA1                  
106800     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
106900     PERFORM IMS-STATUSKONTROLL                                           
107000     SKIP2                                                                
107100     .                                                                    
107200 IMS-GNP-WDE411    SECTION.                                               
107300     STRING 'WDE411  *F(IDPURAD  =' W-WDE411-X ')'                        
107400            DELIMITED BY SIZE INTO SSA2                                   
107500     MOVE '  GE' TO GODK-STATUSKODER                                      
107600     CALL CBLTDLI USING GNP  WDE41-PCB DLI-IO-WDE411 SSA1 SSA2            
107700     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
107800     PERFORM IMS-STATUSKONTROLL                                           
107900     SKIP2                                                                
108000     .                                                                    
108100 IMS-REPL-WDE601 SECTION.                                                 
108200     MOVE '    ' TO GODK-STATUSKODER                                      
108300     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE601                       
108400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
108500     PERFORM IMS-STATUSKONTROLL                                           
108600     SKIP2                                                                
108700     .                                                                    
108800     EJECT                                                                
108900 IMS-GU-WDQ201 SECTION.                                                   
109000                                                                          
109100     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
109200            DELIMITED BY SIZE INTO SSA1                                   
109300     MOVE '  '   TO GODK-STATUSKODER                                      
109400     CALL CBLTDLI USING GU     ORQI-PCB OHUV-WDQ201 SSA1                  
109500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
109600     PERFORM IMS-STATUSKONTROLL                                           
109700     SKIP2                                                                
109800     .                                                                    
109900                                                                          
110000 IMS-GHNP-WDQ212 SECTION.                                                 
110100                                                                          
110200     STRING 'WLORQI12(IDDC     =' W-IDDC-X    ')'                         
110300            DELIMITED BY SIZE INTO SSA1                                   
110400     MOVE '  GE' TO GODK-STATUSKODER                                      
110500     CALL CBLTDLI USING GHNP   ORQI-PCB ARB-WDQ212 SSA1                   
110600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
110700     PERFORM IMS-STATUSKONTROLL                                           
110800     SKIP2                                                                
110900     .                                                                    
111000                                                                          
111100 IMS-REPL-WDQ212 SECTION.                                                 
111200                                                                          
111300     MOVE '    ' TO GODK-STATUSKODER                                      
111400     CALL CBLTDLI USING REPL ORQI-PCB ARB-WDQ212                          
111500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
111600     PERFORM IMS-STATUSKONTROLL                                           
111700     .                                                                    
111800     EJECT                                                                
111900 IMS-ISRT-ZZAC01      SECTION.                                            
112000*    WDG601                                                               
112100     MOVE 'WLZZAC01'  TO SSA1                                             
112200     MOVE '  II' TO GODK-STATUSKODER                                      
112300     CALL CBLTDLI USING ISRT ZZAC-PCB IO-WDGZ01 SSA1                      
112400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
112500     PERFORM IMS-STATUSKONTROLL                                           
112600     .                                                                    
112700     EJECT                                                                
112800 IMS-STATUSKONTROLL SECTION.                                              
112900     SET STATUS-IX TO 1                                                   
113000     SEARCH GODK-STATUS AT END CALL FELLOG                                
113100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
113200     END-SEARCH                                                           
113300     .                                                                    
