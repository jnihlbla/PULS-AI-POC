000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4057500.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   95/11/03.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        FRÅGOR PÅ RESTORDER.                                             
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLORQM (WDQ1)                              
001200*        PROGRAMMET LÄSER      WLORDP (WDA5)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T575                                              
001600*        MID:         W4I57501                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O57501                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W4057500'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
003900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004000                                                                          
004100                                                                          
004200 77  WS-KDORDKL                  PIC X       VALUE SPACE.                 
004300 77  WS-TIORDREG                 PIC X(6)    VALUE SPACE.                 
004400 77  WS-TIORDREG-OMVAND          PIC S9(9)   VALUE ZERO COMP-3.           
004500                                                                          
004600 01  WS-IDORDNR7                 PIC X(7).                                
004700 01  FILLER REDEFINES WS-IDORDNR7.                                        
004800     03  WS-NOLL                 PIC X(2).                                
004900     03  WS-IDORDNR              PIC X(5).                                
005000                                                                          
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200                                                                          
005300 01  W-TIAAAAMMDD                PIC 9(8)    VALUE ZERO.                  
005400                                                                          
005500 01  FILLER  REDEFINES W-TIAAAAMMDD.                                      
005600     03  W-TIAA                  PIC 9(2).                                
005700     03  W-TIAAMMDD-DATE         PIC 9(6).                                
005800                                                                          
005900*  ---- FÄLT FÖR HOPP TILL ANDRA BILDER                                   
006000 77  ANNAN-BILD-SW               PIC X       VALUE 'N'.                   
006100     88  STARTA-ANNAN-BILD                   VALUE 'J'.                   
006200                                                                          
006300 01  BILD-HOPP-AREOR.                                                     
006400                                                                          
006500   03  W-BILD                    PIC X(4)    VALUE SPACE.                 
006600   03  W-HOPP-IDTRANS.                                                    
006700     05  FILLER                  PIC X(1)    VALUE 'W'.                   
006800     05  W-HOPP-IDTRANS-2        PIC X(1).                                
006900     05  FILLER                  PIC X(1)    VALUE 'T'.                   
007000     05  W-HOPP-IDTRANS-4-6      PIC X(3).                                
007100     05  FILLER                  PIC X(2)    VALUE SPACE.                 
007200                                                                          
007300                                                                          
007400   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
007500   03      P-TO-P-SW.                                                     
007600                                                                          
007700     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
007800     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
007900     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
008000     05  P-TO-P-KDTRANS          PIC X(8).                                
008100     05  P-TO-P-IDTRANS          PIC X(4).                                
008200     05  P-TO-P-KDMFSFOR         PIC X(1).                                
008300     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
008400                                                                          
008500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008600     88  NYCKLAR-OK                          VALUE 'J'.                   
008700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008800                                                                          
008900 77  LAES-SW                     PIC X       VALUE 'J'.                   
009000     88  LAES-WDQ1B1                         VALUE 'J'.                   
009100     88  LAES-WDQ1                           VALUE 'N'.                   
009200                                                                          
009300 77  TIME-SW                     PIC X       VALUE 'J'.                   
009400     88  FIRST-TIME                          VALUE 'J'.                   
009500     88  NEXT-TIME                           VALUE 'N'.                   
009600                                                                          
009700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009800     88  EGEN-MID                            VALUE '4575'.                
009900     88  GODK-MID                            VALUE '4571' '4572'          
010000                                                   '4573' '4574'          
010100                                                   '4575' '4576'          
010200                                                   '4577' '4578'          
010300                                                   '4579'.                
010400     88  HELP-MID                            VALUE '0551'.                
010500     EJECT                                                                
010600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010700 01  GENERELLA-SUBPROGRAM.                                                
010800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011500*01 -COPY WMEDAREA                                                        
011600     SKIP3                                                                
011700 01  MESSAGE-CODES.                                                       
011800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012200     03  ERR-BACKORDER-MISSING   PIC X(3)    VALUE '403'.                 
012300     EJECT                                                                
012400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012500*01 -COPY WDATAREA                                                        
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012800*                                                                         
012900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013000     SKIP3                                                                
013100*01 -COPY WMSGINIT                                                        
013200     SKIP3                                                                
013300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013600     SKIP3                                                                
013700*01  MID -COPY W4I57501                                                   
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014000     SKIP3                                                                
014100*01  -COPY WMSGAREA                                                       
014200     EJECT                                                                
014300     03  MOD REDEFINES MSG-AREA.                                          
014400*      05  -COPY W4O57501                                                 
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014700     SKIP3                                                                
014800*01  -COPY WMFSAREA                                                       
014900     EJECT                                                                
015000*    --- SPAR-AREOR FöR BLäDDRING                                         
015100*                                                                         
015200     SKIP3                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'SP-BLADD'.            
015400     SKIP3                                                                
015500 01  SPAR-AREA.                                                           
015600     03  SPAR-IDTRANS            PIC X(4)    VALUE SPACE.                 
015700     03  SPAR-BLADD-ENTER        PIC X(42)   VALUE SPACE.                 
015800     03  SPAR-BLADD-NEXT         PIC X(42)   VALUE SPACE.                 
015900     EJECT                                                                
016000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016100*                                                                         
016200     SKIP3                                                                
016300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016400     SKIP3                                                                
016500 01  NYCKLAR-TILL-BLAEDDRING.                                             
016600     03  W-MINKEY-IDDISTR    PIC S9(5)  VALUE ZERO COMP-3.                
016700     03  W-MINKEY-IDKUNDNR   PIC S9(7)  VALUE ZERO COMP-3.                
016800     03  W-MINKEY-TITIORDD   PIC S9(9)  VALUE ZERO COMP-3.                
016900     03  W-MINKEY-KDFRAKT    PIC S9(3)  VALUE ZERO COMP-3.                
017000     03  W-MINKEY-KDORDKL    PIC S9(1)  VALUE ZERO COMP-3.                
017100     03  W-MINKEY-IDKUNDRF   PIC X(10)  VALUE SPACE.                      
017200     03  W-MINKEY-IDARTNR    PIC S9(9)  VALUE ZERO COMP-3.                
017300     03  W-MINKEY-IDLOPNR    PIC S9(3)  VALUE ZERO COMP-3.                
017400     03  W-MINKEY-IDSEKVNR   PIC S9(3)  VALUE ZERO COMP-3.                
017500     03  W-MINKEY-IDDC       PIC X(2)   VALUE SPACE.                      
017600     03  W-MINKEY-IDORDER    PIC S9(7)  VALUE ZERO COMP-3.                
017700     03  W-MINKEY-KDORDBEK   PIC  9(2)  VALUE ZERO.                       
017800     SKIP3                                                                
017900 01  NYCKLAR-TILL-DLI.                                                    
018000     03  W-WDQ1BSEQ-NEXT-X.                                               
018100         05  W-IDDISTR-NX          PIC S9(5) VALUE ZERO COMP-3.           
018200         05  W-IDKUNDNR-NX         PIC S9(7) VALUE ZERO COMP-3.           
018300         05  W-TITIORDD-9KOMPL-NX  PIC S9(9) VALUE ZERO COMP-3.           
018400         05  W-KDFRAKT-NX          PIC S9(3) VALUE ZERO COMP-3.           
018500         05  W-KDORDKL-NX          PIC S9(1) VALUE ZERO COMP-3.           
018600         05  W-IDKUNDRF-NX         PIC X(10) VALUE SPACE.                 
018700         05  W-IDARTNR-NX          PIC S9(9) VALUE ZERO COMP-3.           
018800         05  W-IDLOPNR-NX          PIC S9(3) VALUE ZERO COMP-3.           
018900         05  W-IDSEKVNR-NX         PIC S9(3) VALUE ZERO COMP-3.           
019000         05  W-IDDC-NX             PIC X(2)  VALUE SPACE.                 
019100                                                                          
019200     03  W-WDQ1BSEQ-MAX-X.                                                
019300         05  W-IDDISTR-MAX         PIC S9(5) VALUE ZERO COMP-3.           
019400         05  W-IDKUNDNR-MAX        PIC S9(7) VALUE ZERO COMP-3.           
019500         05  W-TITIORDD-9KOMPL-MAX PIC S9(9) VALUE ZERO COMP-3.           
019600         05  FILLER                PIC X(24) VALUE HIGH-VALUE.            
019700                                                                          
019800     03  W-WDQ1B1KY-NEXT-X.                                               
019900         05  W-IDDISTR-B1-NX       PIC S9(5) VALUE ZERO COMP-3.           
020000         05  W-IDKUNDNR-B1-NX      PIC S9(7) VALUE ZERO COMP-3.           
020100         05  W-TITIORDD-9KOMPL-B1-NX PIC S9(9) VALUE ZERO COMP-3.         
020200         05  W-KDFRAKT-B1-NX       PIC S9(3) VALUE ZERO COMP-3.           
020300         05  W-KDORDKL-B1-NX       PIC S9(1) VALUE ZERO COMP-3.           
020400         05  W-IDKUNDRF-B1-NX      PIC X(10) VALUE SPACE.                 
020500         05  W-IDARTNR-B1-NX       PIC S9(9) VALUE ZERO COMP-3.           
020600         05  W-IDLOPNR-B1-NX       PIC S9(3) VALUE ZERO COMP-3.           
020700         05  W-IDSEKVNR-B1-NX      PIC S9(3) VALUE ZERO COMP-3.           
020800         05  W-IDDC-B1-NX          PIC X(2)  VALUE SPACE.                 
020900         05  W-IDORDER-B1-NX       PIC S9(7) VALUE ZERO COMP-3.           
021000         05  W-KDORDBEK-B1-NX      PIC 9(2)  VALUE ZERO.                  
021100                                                                          
021200     03  W-WDQ1B1KY-MAX-X.                                                
021300         05  W-IDDISTR-B1-MAX      PIC S9(5) VALUE ZERO COMP-3.           
021400         05  FILLER                PIC X(39) VALUE HIGH-VALUE.            
021500                                                                          
021600     03  W-WDQ101KY-X.                                                    
021700         05  W-IDORDER-Q101        PIC S9(7) VALUE ZERO COMP-3.           
021800         05  W-IDARTNR-Q101        PIC S9(9) VALUE ZERO COMP-3.           
021900         05  W-IDLOPNR-Q101        PIC S9(3) VALUE ZERO COMP-3.           
022000         05  W-IDSEKVNR-Q101       PIC S9(3) VALUE ZERO COMP-3.           
022100         05  W-IDDC-Q101           PIC X(2)  VALUE SPACE.                 
022200         05  W-KDORDBEK-Q101       PIC 9(2)  VALUE ZERO.                  
022300                                                                          
022400     03  W-TITIORD9-X.                                                    
022500         05  W-TITIORD9             PIC S9(9) VALUE ZERO COMP-3.          
022600                                                                          
022700     03  W-KDORDKL-X.                                                     
022800         05  W-KDORDKL             PIC S9    VALUE ZERO COMP-3.           
022900                                                                          
023000     03  W-WDA501KY-A5-MIN-X.                                             
023100         05  W-IDDISTR-A5-MIN      PIC S9(5) VALUE ZERO COMP-3.           
023200         05  W-IDKUNDNR-A5-MIN     PIC S9(7) VALUE ZERO COMP-3.           
023300         05  W-IDKUNDRF-A5-MIN     PIC X(10) VALUE SPACE.                 
023400         05  W-IDARTNR-A5-MIN      PIC S9(9) VALUE ZERO COMP-3.           
023500         05  FILLER                PIC X(2)  VALUE LOW-VALUE.             
023600                                                                          
023700     03  W-WDA501KY-A5-MAX-X.                                             
023800         05  W-IDDISTR-A5-MAX      PIC S9(5) VALUE ZERO COMP-3.           
023900         05  W-IDKUNDNR-A5-MAX     PIC S9(7) VALUE ZERO COMP-3.           
024000         05  W-IDKUNDRF-A5-MAX     PIC X(10) VALUE SPACE.                 
024100         05  W-IDARTNR-A5-MAX      PIC S9(9) VALUE ZERO COMP-3.           
024200         05  FILLER                PIC X(2)  VALUE HIGH-VALUE.            
024300                                                                          
024400     03  W-KDSTARAD-X.                                                    
024500         05  W-KDSTARAD            PIC X(1)  VALUE '2'.                   
024600     03  W-KDORDBEK-X.                                                    
024700         05  W-KDORDBEK            PIC 9(2)  VALUE 90.                    
024800     EJECT                                                                
024900*    --- STATUS-KOD FRÅN IMS                                              
025000 01  STATUS-WS                   PIC XX.                                  
025100     88  STATUS-OK                           VALUE '  '.                  
025200     88  SEGMENT-FINNS                       VALUE '  '.                  
025300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025500     88  BAS-SLUT                            VALUE 'GB'.                  
025600     88  TRANSKOD-FEL                        VALUE 'A1'.                  
025700     88  SECURITY-FEL                        VALUE 'A4'.                  
025800     SKIP2                                                                
025900 01  GODK-STATUSKODER.                                                    
026000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026100     SKIP3                                                                
026200 01  SSA1                        PIC X(196).                              
026300 01  SSA2                        PIC X(64).                               
026400     EJECT                                                                
026500*    --- IMS FUNKTIONSKODER                                               
026600*01  -COPY W0003                                                          
026700     EJECT                                                                
026800*    ---  DLI INPUT-OUTPUT AREA                                           
026900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027000     SKIP3                                                                
027100 01  DLI-IO-AREA.                                                         
027200     03  IO-AREA                 PIC X(400)  VALUE SPACE.                 
027300     SKIP3                                                                
027400     03  WLORQM01 REDEFINES IO-AREA.                                      
027500*        05  -COPY WDQ101  -PRE ORQM-                                     
027600     EJECT                                                                
027700 01  DLI-IO-AREA2.                                                        
027800     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
027900     SKIP3                                                                
028000     03  WLORDP01 REDEFINES IO-AREA2.                                     
028100*        05  -COPY WDA501  -PRE ORDP-                                     
028200     EJECT                                                                
028300 01  DLI-IO-AREA3.                                                        
028400     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
028500     SKIP3                                                                
028600     03  WLORQO01 REDEFINES IO-AREA3.                                     
028700*        05  -COPY WDQ1B1  -PRE ORQO-                                     
028800     EJECT                                                                
028900 LINKAGE SECTION.                                                         
029000                                                                          
029100*01  -COPY W0009   -PRE MSG-                                              
029200*01  -COPY W0009   -PRE ALT-                                              
029300*01  -COPY W0008   -PRE USEA-                                             
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE BSEQ-                                              
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE ORDP-                                              
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE ORQO-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE ORQM-                                              
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB BSEQ-PCB              
030900                           ORDP-PCB ORQO-PCB ORQM-PCB.                    
031000 MAIN SECTION.                                                            
031100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB BSEQ-PCB              
031200                           ORDP-PCB ORQO-PCB ORQM-PCB.                    
031300                                                                          
031400     PERFORM IMS-GET-MSG                                                  
031500     IF SEGMENT-FINNS                                                     
031600       PERFORM A-INIT                                                     
031700       PERFORM B-KOLLA-NYCKLAR                                            
031800       IF NYCKLAR-OK                                                      
031900           IF MFS-FIRST                                                   
032000             PERFORM C-FOERSTA-SIDA                                       
032100           ELSE                                                           
032200             IF MFS-NEXT                                                  
032300               PERFORM D-NAESTA-SIDA                                      
032400             ELSE                                                         
032500               PERFORM E-SAMMA-SIDA                                       
032600             END-IF                                                       
032700           END-IF                                                         
032800           IF STARTA-ANNAN-BILD                                           
032900             CONTINUE                                                     
033000           ELSE                                                           
033100             IF LAES-WDQ1                                                 
033200               PERFORM F-LAES-VISA-INFO                                   
033300             ELSE                                                         
033400               PERFORM G-LAES-VISA-INFO                                   
033500             END-IF                                                       
033600           END-IF                                                         
033700       END-IF                                                             
033800       IF STARTA-ANNAN-BILD                                               
033900         CONTINUE                                                         
034000       ELSE                                                               
034100         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57501 + 4                    
034200         PERFORM IMS-INSERT-MSG                                           
034300       END-IF                                                             
034400     END-IF                                                               
034500                                                                          
034600     MOVE ZERO TO RETURN-CODE                                             
034700     GOBACK                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 A-INIT SECTION.                                                          
035100                                                                          
035200     IF MSG-DUBBLA-TRANSKODER                                             
035300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I57501                 
035400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
035500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035600     ELSE                                                                 
035700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I57501                  
035800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
035900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036000     END-IF                                                               
036100                                                                          
036200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
036300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
036400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
036500                                                                          
036600     MOVE LOW-VALUE TO MSG-AREA                                           
036700     MOVE 'W4O575N1' TO MFS-IDMOD                                         
036800     MOVE '4575' TO MOD-IDTRANS                                           
036900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
037000                                                                          
037100*    --- OM SVAR TILL SKÄRM:        MSG-KVLL = MOD-LÄNGD + 4              
037200*    --- OM PROGRAM-TILL-PROGRAM-SWITCH:     = MOD-LÄNGD + 17             
037300     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57501 + 4                        
037400                                                                          
037500     IF EGEN-MID OR HELP-MID                                              
037600       CONTINUE                                                           
037700     ELSE                                                                 
037800       MOVE SPACE TO MFS-KDTRTYP                                          
037900       MOVE '7' TO MFS-IDPFK                                              
038000     END-IF                                                               
038100                                                                          
038200     MOVE LOW-VALUE TO W-WDQ1BSEQ-NEXT-X                                  
038300                       W-WDQ1B1KY-NEXT-X                                  
038400                                                                          
038500     MOVE HIGH-VALUE TO W-WDQ1BSEQ-MAX-X                                  
038600                        W-WDQ1B1KY-MAX-X                                  
038700                                                                          
038800     MOVE NEJ TO LAES-SW                                                  
038900     .                                                                    
039000     EJECT                                                                
039100 B-KOLLA-NYCKLAR SECTION.                                                 
039200                                                                          
039300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039400     MOVE '001'             TO MSGI-KDCALL                                
039500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039600     MOVE '4575'            TO MSGI-IDTRANS                               
039700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039800     IF GODK-MID                                                          
039900       MOVE MID-IDDISTR-IN       TO MSGI-IDDISTR                          
040000       MOVE MID-IDKUNDNR-IN      TO MSGI-IDKUNDNR                         
040100     END-IF                                                               
040200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040300     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
040400     MOVE MSGI-TILOKDAT TO DAGENS-DATUM                                   
040500                                                                          
040600     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
040700                                                                          
040800     MOVE JA TO NYCKLAR-SW                                                
040900                                                                          
041000                                                                          
041100*    -- KONTROLL AV IDDISTR                                               
041200     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
041300                                                                          
041400     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
041500       MOVE '7'         TO MFS-IDPFK                                      
041600       MOVE SPACE       TO MFS-KDTRTYP                                    
041700     END-IF                                                               
041800     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
041900     IF MSGI-IDDISTR NUMERIC                                              
042000       IF MSGI-IDDISTR > ZERO                                             
042100         MOVE MSGI-IDDISTR TO W-IDDISTR-NX                                
042200                              W-IDDISTR-MAX                               
042300                              W-IDDISTR-B1-NX                             
042400                              W-IDDISTR-B1-MAX                            
042500       ELSE                                                               
042600         MOVE NEJ TO NYCKLAR-SW                                           
042700       END-IF                                                             
042800     ELSE                                                                 
042900       MOVE NEJ TO NYCKLAR-SW                                             
043000     END-IF                                                               
043100                                                                          
043200*    -- KONTROLL AV IDKUNDNR                                              
043300     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
043400                                                                          
043500     IF MID-IDKUNDNR-IN  NOT = ALL '+'                                    
043600       MOVE '7'         TO MFS-IDPFK                                      
043700       MOVE SPACE       TO MFS-KDTRTYP                                    
043800     END-IF                                                               
043900     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
044000     IF MSGI-IDKUNDNR NUMERIC                                             
044100       IF MSGI-IDKUNDNR > ZERO                                            
044200         MOVE MSGI-IDKUNDNR TO W-IDKUNDNR-NX                              
044300                               W-IDKUNDNR-MAX                             
044400       END-IF                                                             
044500     ELSE                                                                 
044600       MOVE NEJ TO NYCKLAR-SW                                             
044700     END-IF                                                               
044800                                                                          
044900*    -- KONTROLL AV KDORDKL                                               
045000     MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-IN                               
045100                                                                          
045200     IF EGEN-MID                                                          
045300       IF MID-KDORDKL-IN = ALL '+'                                        
045400         MOVE MID-KDORDKL-UT TO WS-KDORDKL                                
045500         INSPECT WS-KDORDKL REPLACING LEADING SPACE BY ZERO               
045600       ELSE                                                               
045700         MOVE MID-KDORDKL-IN TO WS-KDORDKL                                
045800         MOVE '7'       TO MFS-IDPFK                                      
045900         MOVE SPACE     TO MFS-KDTRTYP                                    
046000       END-IF                                                             
046100     ELSE                                                                 
046200       MOVE ZERO TO WS-KDORDKL                                            
046300       MOVE '7'         TO MFS-IDPFK                                      
046400       MOVE SPACE       TO MFS-KDTRTYP                                    
046500     END-IF                                                               
046600                                                                          
046700     IF WS-KDORDKL  NUMERIC                                               
046800       IF WS-KDORDKL = +0                                                 
046900         MOVE +1    TO W-KDORDKL                                          
047000                       WS-KDORDKL                                         
047100       ELSE                                                               
047200         MOVE WS-KDORDKL TO W-KDORDKL                                     
047300       END-IF                                                             
047400     ELSE                                                                 
047500       MOVE NEJ TO NYCKLAR-SW                                             
047600     END-IF                                                               
047700                                                                          
047800*    -- KONTROLL AV TIORDREG                                              
047900     MOVE MFS-RENSA-FAELT TO MOD-TIORDREG-IN                              
048000                                                                          
048100     IF EGEN-MID                                                          
048200       IF MID-TIORDREG-IN = ALL '+'                                       
048300         IF MID-TIORDREG-UT = ZERO                                        
048400           MOVE DAGENS-DATUM  TO WS-TIORDREG                              
048500         ELSE                                                             
048600           MOVE MID-TIORDREG-UT TO WS-TIORDREG                            
048700         END-IF                                                           
048800         INSPECT WS-TIORDREG REPLACING LEADING SPACE BY ZERO              
048900       ELSE                                                               
049000         MOVE MID-TIORDREG-IN TO WS-TIORDREG                              
049100         MOVE '7'       TO MFS-IDPFK                                      
049200         MOVE SPACE     TO MFS-KDTRTYP                                    
049300       END-IF                                                             
049400     ELSE                                                                 
049500       MOVE DAGENS-DATUM TO WS-TIORDREG                                   
049600       MOVE '7'         TO MFS-IDPFK                                      
049700       MOVE SPACE       TO MFS-KDTRTYP                                    
049800     END-IF                                                               
049900                                                                          
050000     IF WS-TIORDREG NUMERIC                                               
050100       IF WS-TIORDREG > ZERO                                              
050200         MOVE 'AAMMDD' TO DAT-KDDATFORM                                   
050300         MOVE WS-TIORDREG TO DAT-I-TIDATUM                                
050400         CALL WDATKONV USING DAT-KDDATFORM                                
050500                             DAT-I-TIDATUM                                
050600                             DAT-O-TIDATUM                                
050700                             DAT-KDSVAR                                   
050800         IF DAT-KDSVAR-OK                                                 
050900           MOVE DAT-TIAAMMDD TO W-TIAAMMDD-DATE                           
051000           PERFORM S01-OMVAND-DATUM                                       
051100           MOVE WS-TIORDREG-OMVAND TO W-TITIORDD-9KOMPL-NX                
051200                                      W-TITIORDD-9KOMPL-MAX               
051300                                      W-TITIORD9                          
051400         ELSE                                                             
051500           MOVE NEJ TO NYCKLAR-SW                                         
051600         END-IF                                                           
051700       END-IF                                                             
051800     ELSE                                                                 
051900       MOVE NEJ TO NYCKLAR-SW                                             
052000     END-IF                                                               
052100                                                                          
052200     IF WS-TIORDREG > ZERO                                                
052300       IF MSGI-IDKUNDNR > ZERO                                            
052400         MOVE NEJ TO LAES-SW                                              
052500       ELSE                                                               
052600         MOVE JA TO LAES-SW                                               
052700       END-IF                                                             
052800     ELSE                                                                 
052900       MOVE NEJ TO LAES-SW                                                
053000     END-IF                                                               
053100                                                                          
053200     IF GODK-MID OR NYCKLAR-OK                                            
053300       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
053400       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
053500       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
053600       MOVE WS-TIORDREG          TO MOD-TIORDREG-UT                       
053700       MOVE WS-KDORDKL           TO MOD-KDORDKL-UT                        
053800       INSPECT MOD-KDORDKL-UT REPLACING LEADING ZERO BY SPACE             
053900     ELSE                                                                 
054000       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
054100                               MOD-IDKUNDNR-UT                            
054200                               MOD-KDORDKL-UT                             
054300                               MOD-TIORDREG-UT                            
054400     END-IF                                                               
054500                                                                          
054600     IF NYCKLAR-FEL                                                       
054700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
054800       CALL WMEDKONV USING MED-WMEDAREA                                   
054900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
055000       PERFORM MFS-RENSA-FAELT-IN                                         
055100       PERFORM MFS-RENSA-FAELT-UT                                         
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500 C-FOERSTA-SIDA SECTION.                                                  
055600                                                                          
055700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
055800     CALL WMEDKONV USING MED-WMEDAREA                                     
055900     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
056000                                                                          
056100*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
056200                                                                          
056300     PERFORM MFS-RENSA-FAELT-IN                                           
056400     .                                                                    
056500     EJECT                                                                
056600 D-NAESTA-SIDA SECTION.                                                   
056700                                                                          
056800     MOVE SPAR-BLADD-NEXT   TO NYCKLAR-TILL-BLAEDDRING                    
056900     IF MSGI-IDTRANS = '4575'                                             
057000       IF W-MINKEY-IDDISTR IS NUMERIC                                     
057100        IF LAES-WDQ1                                                      
057200         IF W-MINKEY-IDDISTR > ZERO                                       
057300           MOVE W-MINKEY-IDDISTR    TO W-IDDISTR-NX                       
057400           MOVE W-MINKEY-IDKUNDNR   TO W-IDKUNDNR-NX                      
057500           MOVE W-MINKEY-TITIORDD   TO W-TITIORDD-9KOMPL-NX               
057600           MOVE W-MINKEY-KDFRAKT    TO W-KDFRAKT-NX                       
057700           MOVE W-MINKEY-KDORDKL    TO W-KDORDKL-NX                       
057800           MOVE W-MINKEY-IDKUNDRF   TO W-IDKUNDRF-NX                      
057900           MOVE W-MINKEY-IDARTNR    TO W-IDARTNR-NX                       
058000           MOVE W-MINKEY-IDLOPNR    TO W-IDLOPNR-NX                       
058100           MOVE W-MINKEY-IDSEKVNR   TO W-IDSEKVNR-NX                      
058200           MOVE W-MINKEY-IDDC       TO W-IDDC-NX                          
058300         END-IF                                                           
058400         PERFORM MFS-RENSA-FAELT-IN                                       
058500        ELSE                                                              
058600         IF W-MINKEY-IDDISTR > ZERO                                       
058700           MOVE W-MINKEY-IDDISTR    TO W-IDDISTR-B1-NX                    
058800           MOVE W-MINKEY-IDKUNDNR   TO W-IDKUNDNR-B1-NX                   
058900           MOVE W-MINKEY-TITIORDD   TO W-TITIORDD-9KOMPL-B1-NX            
059000           MOVE W-MINKEY-KDFRAKT    TO W-KDFRAKT-B1-NX                    
059100           MOVE W-MINKEY-KDORDKL    TO W-KDORDKL-B1-NX                    
059200           MOVE W-MINKEY-IDKUNDRF   TO W-IDKUNDRF-B1-NX                   
059300           MOVE W-MINKEY-IDARTNR    TO W-IDARTNR-B1-NX                    
059400           MOVE W-MINKEY-IDLOPNR    TO W-IDLOPNR-B1-NX                    
059500           MOVE W-MINKEY-IDSEKVNR   TO W-IDSEKVNR-B1-NX                   
059600           MOVE W-MINKEY-IDDC       TO W-IDDC-B1-NX                       
059700           MOVE W-MINKEY-IDORDER    TO W-IDORDER-B1-NX                    
059800           MOVE W-MINKEY-KDORDBEK   TO W-KDORDBEK-B1-NX                   
059900         END-IF                                                           
060000        END-IF                                                            
060010       END-IF                                                             
060100       PERFORM MFS-RENSA-FAELT-IN                                         
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 E-SAMMA-SIDA SECTION.                                                    
060600                                                                          
060700     MOVE SPAR-BLADD-ENTER    TO NYCKLAR-TILL-BLAEDDRING                  
060800     PERFORM MFS-RENSA-FAELT-UT                                           
060900     IF EGEN-MID OR HELP-MID                                              
060910       IF W-MINKEY-IDDISTR IS NUMERIC                                     
061000        IF LAES-WDQ1                                                      
061100         IF W-MINKEY-IDDISTR > ZERO                                       
061200           MOVE W-MINKEY-IDDISTR    TO W-IDDISTR-NX                       
061300           MOVE W-MINKEY-IDKUNDNR   TO W-IDKUNDNR-NX                      
061400           MOVE W-MINKEY-TITIORDD   TO W-TITIORDD-9KOMPL-NX               
061500           MOVE W-MINKEY-KDFRAKT    TO W-KDFRAKT-NX                       
061600           MOVE W-MINKEY-KDORDKL    TO W-KDORDKL-NX                       
061700           MOVE W-MINKEY-IDKUNDRF   TO W-IDKUNDRF-NX                      
061800           MOVE W-MINKEY-IDARTNR    TO W-IDARTNR-NX                       
061900           MOVE W-MINKEY-IDLOPNR    TO W-IDLOPNR-NX                       
062000           MOVE W-MINKEY-IDSEKVNR   TO W-IDSEKVNR-NX                      
062100           MOVE W-MINKEY-IDDC       TO W-IDDC-NX                          
062200         END-IF                                                           
062300        ELSE                                                              
062400         IF W-MINKEY-IDDISTR > ZERO                                       
062500           MOVE W-MINKEY-IDDISTR    TO W-IDDISTR-B1-NX                    
062600           MOVE W-MINKEY-IDKUNDNR   TO W-IDKUNDNR-B1-NX                   
062700           MOVE W-MINKEY-TITIORDD   TO W-TITIORDD-9KOMPL-B1-NX            
062800           MOVE W-MINKEY-KDFRAKT    TO W-KDFRAKT-B1-NX                    
062900           MOVE W-MINKEY-KDORDKL    TO W-KDORDKL-B1-NX                    
063000           MOVE W-MINKEY-IDKUNDRF   TO W-IDKUNDRF-B1-NX                   
063100           MOVE W-MINKEY-IDARTNR    TO W-IDARTNR-B1-NX                    
063200           MOVE W-MINKEY-IDLOPNR    TO W-IDLOPNR-B1-NX                    
063300           MOVE W-MINKEY-IDSEKVNR   TO W-IDSEKVNR-B1-NX                   
063400           MOVE W-MINKEY-IDDC       TO W-IDDC-B1-NX                       
063500           MOVE W-MINKEY-IDORDER    TO W-IDORDER-B1-NX                    
063600           MOVE W-MINKEY-KDORDBEK   TO W-KDORDBEK-B1-NX                   
063700         END-IF                                                           
063800        END-IF                                                            
063810       END-IF                                                             
063900                                                                          
064000       MOVE +1 TO INDX                                                    
064100       PERFORM UNTIL INDX > MAX-INDX                                      
064200         IF MID-BILDNR (INDX) = ALL '+'                                   
064300           CONTINUE                                                       
064400         ELSE                                                             
064500           IF MID-BILDNR (INDX) NUMERIC                                   
064600             PERFORM EA-STARTA-ANNAN-BILD                                 
064700             MOVE JA TO ANNAN-BILD-SW                                     
064800             MOVE +9999    TO INDX                                        
064900           END-IF                                                         
065000         END-IF                                                           
065100         ADD +1 TO INDX                                                   
065200       END-PERFORM                                                        
065300                                                                          
065400     ELSE                                                                 
065500       PERFORM MFS-RENSA-FAELT-IN                                         
065600     END-IF                                                               
065700     .                                                                    
065800     EJECT                                                                
065900 EA-STARTA-ANNAN-BILD SECTION.                                            
066000                                                                          
066100     INSPECT MID-IDKUNDNR (INDX) REPLACING LEADING SPACE BY ZERO          
066200     MOVE MID-IDKUNDNR (INDX)  TO MSGI-IDKUNDNR                           
066300     INSPECT MID-IDORDNR  (INDX) REPLACING LEADING SPACE BY ZERO          
066400     MOVE '00'                 TO WS-NOLL                                 
066500     MOVE MID-IDORDNR (INDX)   TO WS-IDORDNR                              
066600     MOVE WS-IDORDNR7          TO MSGI-IDKUNDRF(1:7)                      
066700     INSPECT MID-IDARTNR (INDX) REPLACING LEADING SPACE BY ZERO           
066800     MOVE MID-IDARTNR (INDX)  TO MSGI-IDARTNR                             
066900     MOVE '001'               TO MSGI-KDCALL                              
067000     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
067100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
067200                                                                          
067300     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
067400     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
067500     MOVE MID-BILDNR(INDX) (1:1)  TO W-HOPP-IDTRANS-2                     
067600     MOVE MID-BILDNR(INDX) (2:3)  TO W-HOPP-IDTRANS-4-6                   
067700     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
067800     MOVE '4575'                 TO P-TO-P-IDTRANS                        
067900     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
068000                                                                          
068100     PERFORM S08-INSERT-ALTMSG                                            
068200     .                                                                    
068300     EJECT                                                                
068400 F-LAES-VISA-INFO SECTION.                                                
068500                                                                          
068600     PERFORM IMS-GU-WDQ1                                                  
068700                                                                          
068800     IF SEGMENT-SAKNAS                                                    
068900       MOVE ERR-BACKORDER-MISSING TO MED-IDMFSFEL                         
069000       CALL WMEDKONV USING MED-WMEDAREA                                   
069100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
069200       PERFORM MFS-RENSA-FAELT-UT                                         
069300     ELSE                                                                 
069400       MOVE JA TO TIME-SW                                                 
069500                                                                          
069600       MOVE +1 TO INDX                                                    
069700       PERFORM UNTIL INDX > MAX-INDX                                      
069800         IF SEGMENT-FINNS                                                 
069900           IF ORQM-OBKR-IDDISTR       = W-IDDISTR-A5-MIN   AND            
070000              ORQM-OBKR-IDKUNDNR      = W-IDKUNDNR-A5-MIN  AND            
070100            ORQM-OBKR-IDKUNDRF(3:5) = W-IDKUNDRF-A5-MIN(1:5) AND          
070200              ORQM-OBKR-IDARTNR       = W-IDARTNR-A5-MIN                  
070300             CONTINUE                                                     
070400           ELSE                                                           
070500             MOVE ORQM-OBKR-IDDISTR       TO W-IDDISTR-A5-MIN             
070600                                           W-IDDISTR-A5-MAX               
070700             MOVE ORQM-OBKR-IDKUNDNR      TO W-IDKUNDNR-A5-MIN            
070800                                           W-IDKUNDNR-A5-MAX              
070900             MOVE SPACE                   TO W-IDKUNDRF-A5-MIN            
071000                                           W-IDKUNDRF-A5-MAX              
071100             MOVE ORQM-OBKR-IDORDNR7(3:5) TO W-IDKUNDRF-A5-MIN            
071200                                             W-IDKUNDRF-A5-MAX            
071300             MOVE ORQM-OBKR-IDARTNR       TO W-IDARTNR-A5-MIN             
071400                                           W-IDARTNR-A5-MAX               
071500                                                                          
071600             PERFORM IMS-GU-WDA501                                        
071700             IF SEGMENT-FINNS                                             
071800               IF ORDP-RAD-KDORDKL = W-KDORDKL                            
071900                 IF FIRST-TIME                                            
072000                   MOVE ORQM-OBKR-IDDISTR     TO W-MINKEY-IDDISTR         
072100                   MOVE ORQM-OBKR-IDKUNDNR    TO W-MINKEY-IDKUNDNR        
072200                   MOVE ORQM-OBKR-TITIORDD-9KOMPL TO                      
072300                                            W-MINKEY-TITIORDD             
072400                   MOVE ORQM-OBKR-KDFRAKT     TO W-MINKEY-KDFRAKT         
072500                   MOVE ORQM-OBKR-KDORDKL     TO W-MINKEY-KDORDKL         
072600                   MOVE ORQM-OBKR-IDKUNDRF    TO W-MINKEY-IDKUNDRF        
072700                   MOVE ORQM-OBKR-IDARTNR     TO W-MINKEY-IDARTNR         
072800                   MOVE ORQM-OBKR-IDLOPNR     TO W-MINKEY-IDLOPNR         
072900                   MOVE ORQM-OBKR-IDSEKVNR    TO W-MINKEY-IDSEKVNR        
073000                   MOVE ORQM-OBKR-IDDC        TO W-MINKEY-IDDC            
073100                                                                          
073200                   MOVE NYCKLAR-TILL-BLAEDDRING TO                        
073300                                     SPAR-BLADD-ENTER                     
073400                   MOVE NEJ TO TIME-SW                                    
073500                 END-IF                                                   
073600                                                                          
073700                 MOVE ORQM-OBKR-IDKUNDNR TO MOD-IDKUNDNR (INDX)           
073800                 MOVE ORQM-OBKR-IDORDNR7 TO MOD-IDORDNR (INDX)            
073900                 MOVE ORQM-OBKR-IDARTNR TO MOD-IDARTNR (INDX)             
074000                 MOVE ORDP-RAD-KVART     TO MOD-KVRO (INDX)               
074100                 MOVE ORQM-OBKR-BERADREF TO MOD-BERADREF(INDX)            
074200                 MOVE ORQM-OBKR-TIORDREG TO MOD-TIORDREG(INDX)            
074300                 IF ORQM-OBKR-IDKUNDRF-RO > ZERO                          
074400                   MOVE ORQM-OBKR-IDKUNDRF-RO TO                          
074500                                         MOD-IDKUNDRF-RO(INDX)            
074600                 ELSE                                                     
074700                   MOVE SPACE TO MOD-IDKUNDRF-RO (INDX)                   
074800                 END-IF                                                   
074900                 ADD +1 TO INDX                                           
075000               END-IF                                                     
075100             END-IF                                                       
075200           END-IF                                                         
075300           PERFORM IMS-GN-WDQ1                                            
075400         ELSE                                                             
075500           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR    (INDX)                 
075600                                   MOD-IDORDNR     (INDX)                 
075700                                   MOD-IDARTNR     (INDX)                 
075800                                   MOD-KVRO        (INDX)                 
075900                                   MOD-BERADREF    (INDX)                 
076000                                   MOD-TIORDREG    (INDX)                 
076100                                   MOD-IDKUNDRF-RO (INDX)                 
076200           MOVE MFS-STAENG-FAELT TO MOD-BILDNR-ATTR (INDX)                
076300           ADD 1 TO INDX                                                  
076400         END-IF                                                           
076500       END-PERFORM                                                        
076600                                                                          
076700       IF SEGMENT-FINNS                                                   
076800         MOVE ORQM-OBKR-IDDISTR            TO W-MINKEY-IDDISTR            
076900         MOVE ORQM-OBKR-IDKUNDNR           TO W-MINKEY-IDKUNDNR           
077000         MOVE ORQM-OBKR-TITIORDD-9KOMPL TO                                
077100                                  W-MINKEY-TITIORDD                       
077200         MOVE ORQM-OBKR-KDFRAKT            TO W-MINKEY-KDFRAKT            
077300         MOVE ORQM-OBKR-KDORDKL            TO W-MINKEY-KDORDKL            
077400         MOVE ORQM-OBKR-IDKUNDRF           TO W-MINKEY-IDKUNDRF           
077500         MOVE ORQM-OBKR-IDARTNR            TO W-MINKEY-IDARTNR            
077600         MOVE ORQM-OBKR-IDLOPNR            TO W-MINKEY-IDLOPNR            
077700         MOVE ORQM-OBKR-IDSEKVNR           TO W-MINKEY-IDSEKVNR           
077800         MOVE ORQM-OBKR-IDDC               TO W-MINKEY-IDDC               
077900                                                                          
078000         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
078100         CALL WMEDKONV USING MED-WMEDAREA                                 
078200         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
078300                                                                          
078400       ELSE                                                               
078500         MOVE ZERO                         TO W-MINKEY-IDDISTR            
078600                                              W-MINKEY-IDKUNDNR           
078700                                              W-MINKEY-TITIORDD           
078800                                              W-MINKEY-KDFRAKT            
078900                                              W-MINKEY-KDORDKL            
079000                                              W-MINKEY-IDARTNR            
079100                                              W-MINKEY-IDLOPNR            
079200                                              W-MINKEY-IDSEKVNR           
079300                                                                          
079400         MOVE SPACE                        TO W-MINKEY-IDKUNDRF           
079500                                              W-MINKEY-IDDC               
079600                                                                          
079700       END-IF                                                             
079800                                                                          
079900       MOVE NYCKLAR-TILL-BLAEDDRING  TO SPAR-BLADD-NEXT                   
080000       MOVE '4575'                   TO SPAR-IDTRANS                      
080100       MOVE '002'                    TO MSGI-KDCALL                       
080200       MOVE SPAR-AREA                TO MSGI-SPAR-AREA                    
080300                                                                          
080400       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
080500     END-IF                                                               
080600     .                                                                    
080700     EJECT                                                                
080800 G-LAES-VISA-INFO SECTION.                                                
080900                                                                          
081000     PERFORM IMS-GU-WDQ1B1                                                
081100     IF SEGMENT-SAKNAS                                                    
081200       MOVE ERR-BACKORDER-MISSING TO MED-IDMFSFEL                         
081300       CALL WMEDKONV USING MED-WMEDAREA                                   
081400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
081500       PERFORM MFS-RENSA-FAELT-UT                                         
081600     ELSE                                                                 
081700       MOVE JA TO TIME-SW                                                 
081800                                                                          
081900       MOVE +1 TO INDX                                                    
082000       PERFORM UNTIL INDX > MAX-INDX                                      
082100         IF SEGMENT-FINNS                                                 
082200           IF ORQO-SEQB-IDORDER = W-IDORDER-Q101 AND                      
082300              ORQO-SEQB-IDARTNR = W-IDARTNR-Q101                          
082400             CONTINUE                                                     
082500           ELSE                                                           
082600             MOVE ORQO-SEQB-IDWDQ101      TO W-WDQ101KY-X                 
082700                                                                          
082800             PERFORM IMS-GU-WDQ101                                        
082900             IF SEGMENT-FINNS                                             
083000               MOVE ORQM-OBKR-IDDISTR         TO W-IDDISTR-A5-MIN         
083100                                               W-IDDISTR-A5-MAX           
083200               MOVE ORQM-OBKR-IDKUNDNR        TO W-IDKUNDNR-A5-MIN        
083300                                               W-IDKUNDNR-A5-MAX          
083400               MOVE SPACE                     TO W-IDKUNDRF-A5-MIN        
083500                                               W-IDKUNDRF-A5-MAX          
083600               MOVE ORQM-OBKR-IDORDNR7(3:5) TO W-IDKUNDRF-A5-MIN          
083700                                               W-IDKUNDRF-A5-MAX          
083800               MOVE ORQM-OBKR-IDARTNR         TO W-IDARTNR-A5-MIN         
083900                                               W-IDARTNR-A5-MAX           
084000                                                                          
084100               PERFORM IMS-GU-WDA501                                      
084200               IF SEGMENT-FINNS AND                                       
084300                 ORQO-SEQB-KDORDKL = W-KDORDKL                            
084400                 IF FIRST-TIME                                            
084500                   MOVE ORQO-SEQB-IDDISTR     TO W-MINKEY-IDDISTR         
084600                   MOVE ORQO-SEQB-IDKUNDNR    TO W-MINKEY-IDKUNDNR        
084700                   MOVE ORQO-SEQB-TITIORDD-9KOMPL TO                      
084800                                            W-MINKEY-TITIORDD             
084900                   MOVE ORQO-SEQB-KDFRAKT     TO W-MINKEY-KDFRAKT         
085000                   MOVE ORQO-SEQB-KDORDKL     TO W-MINKEY-KDORDKL         
085100                   MOVE ORQO-SEQB-IDKUNDRF    TO W-MINKEY-IDKUNDRF        
085200                   MOVE ORQO-SEQB-IDARTNR     TO W-MINKEY-IDARTNR         
085300                   MOVE ORQO-SEQB-IDLOPNR     TO W-MINKEY-IDLOPNR         
085400                   MOVE ORQO-SEQB-IDSEKVNR    TO W-MINKEY-IDSEKVNR        
085500                   MOVE ORQO-SEQB-IDDC        TO W-MINKEY-IDDC            
085600                   MOVE ORQO-SEQB-IDORDER     TO W-MINKEY-IDORDER         
085700                   MOVE ORQO-SEQB-KDORDBEK    TO W-MINKEY-KDORDBEK        
085800                                                                          
085900                   MOVE NYCKLAR-TILL-BLAEDDRING TO                        
086000                                     SPAR-BLADD-ENTER                     
086100                   MOVE NEJ TO TIME-SW                                    
086200                 END-IF                                                   
086300                                                                          
086400                 MOVE ORQM-OBKR-IDKUNDNR TO MOD-IDKUNDNR (INDX)           
086500                 MOVE ORQM-OBKR-IDORDNR7 TO MOD-IDORDNR (INDX)            
086600                 MOVE ORQM-OBKR-IDARTNR    TO MOD-IDARTNR (INDX)          
086700                 MOVE ORDP-RAD-KVART       TO MOD-KVRO (INDX)             
086800                 MOVE ORQM-OBKR-BERADREF TO MOD-BERADREF(INDX)            
086900                 MOVE ORQM-OBKR-TIORDREG TO MOD-TIORDREG(INDX)            
087000                 IF ORQM-OBKR-IDKUNDRF-RO > ZERO                          
087100                   MOVE ORQM-OBKR-IDKUNDRF-RO TO                          
087200                                         MOD-IDKUNDRF-RO(INDX)            
087300                 ELSE                                                     
087400                   MOVE SPACE TO MOD-IDKUNDRF-RO (INDX)                   
087500                 END-IF                                                   
087600                 ADD +1 TO INDX                                           
087700               END-IF                                                     
087800             END-IF                                                       
087900           END-IF                                                         
088000           PERFORM IMS-GN-WDQ1B1                                          
088100         ELSE                                                             
088200           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR    (INDX)                 
088300                                   MOD-IDORDNR     (INDX)                 
088400                                   MOD-IDARTNR     (INDX)                 
088500                                   MOD-KVRO        (INDX)                 
088600                                   MOD-BERADREF    (INDX)                 
088700                                   MOD-TIORDREG    (INDX)                 
088800                                   MOD-IDKUNDRF-RO (INDX)                 
088900           ADD 1 TO INDX                                                  
089000         END-IF                                                           
089100       END-PERFORM                                                        
089200                                                                          
089300       IF SEGMENT-FINNS                                                   
089400         MOVE ORQO-SEQB-IDDISTR            TO W-MINKEY-IDDISTR            
089500         MOVE ORQO-SEQB-IDKUNDNR           TO W-MINKEY-IDKUNDNR           
089600         MOVE ORQO-SEQB-TITIORDD-9KOMPL TO                                
089700                                  W-MINKEY-TITIORDD                       
089800         MOVE ORQO-SEQB-KDFRAKT            TO W-MINKEY-KDFRAKT            
089900         MOVE ORQO-SEQB-KDORDKL            TO W-MINKEY-KDORDKL            
090000         MOVE ORQO-SEQB-IDKUNDRF           TO W-MINKEY-IDKUNDRF           
090100         MOVE ORQO-SEQB-IDARTNR            TO W-MINKEY-IDARTNR            
090200         MOVE ORQO-SEQB-IDLOPNR            TO W-MINKEY-IDLOPNR            
090300         MOVE ORQO-SEQB-IDSEKVNR           TO W-MINKEY-IDSEKVNR           
090400         MOVE ORQO-SEQB-IDDC               TO W-MINKEY-IDDC               
090500         MOVE ORQO-SEQB-IDORDER            TO W-MINKEY-IDORDER            
090600         MOVE ORQO-SEQB-KDORDBEK           TO W-MINKEY-KDORDBEK           
090700                                                                          
090800         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
090900         CALL WMEDKONV USING MED-WMEDAREA                                 
091000         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
091100                                                                          
091200       ELSE                                                               
091300         MOVE ZERO                         TO W-MINKEY-IDDISTR            
091400                                              W-MINKEY-IDKUNDNR           
091500                                              W-MINKEY-TITIORDD           
091600                                              W-MINKEY-KDFRAKT            
091700                                              W-MINKEY-KDORDKL            
091800                                              W-MINKEY-IDARTNR            
091900                                              W-MINKEY-IDLOPNR            
092000                                              W-MINKEY-IDSEKVNR           
092100                                              W-MINKEY-IDORDER            
092200                                              W-MINKEY-KDORDBEK           
092300                                                                          
092400         MOVE SPACE                        TO W-MINKEY-IDKUNDRF           
092500                                              W-MINKEY-IDDC               
092600                                                                          
092700       END-IF                                                             
092800                                                                          
092900       MOVE NYCKLAR-TILL-BLAEDDRING  TO SPAR-BLADD-NEXT                   
093000       MOVE '4575'                   TO SPAR-IDTRANS                      
093100       MOVE '002'                    TO MSGI-KDCALL                       
093200       MOVE SPAR-AREA                TO MSGI-SPAR-AREA                    
093300                                                                          
093400       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
093500                                                                          
093600     END-IF                                                               
093700     .                                                                    
093800     EJECT                                                                
093900 S01-OMVAND-DATUM SECTION.                                                
094000                                                                          
094100     IF  W-TIAAMMDD-DATE > 500000                                         
094200         MOVE 19 TO W-TIAA                                                
094300     ELSE                                                                 
094400         MOVE 20 TO W-TIAA                                                
094500     END-IF                                                               
094600                                                                          
094700     COMPUTE WS-TIORDREG-OMVAND = 999999999 - W-TIAAAAMMDD                
094800     .                                                                    
094900     EJECT                                                                
095000 S08-INSERT-ALTMSG SECTION.                                               
095100                                                                          
095200     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
095300     PERFORM IMS-CHANGE-ALTMSG                                            
095400     IF STATUS-OK                                                         
095500       PERFORM IMS-INSERT-ALTMSG                                          
095600     ELSE                                                                 
095700       MOVE LOW-VALUE          TO MSG-AREA                                
095800       MOVE 'W4O57501'         TO MFS-IDMOD                               
095900       MOVE '4575'             TO MOD-IDTRANS                             
096000       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
096100       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
096200       IF SECURITY-FEL                                                    
096300         STRING 'NOT AUTHORIZED TO USE '                                  
096400                W-BILD                                                    
096500                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
096600       ELSE                                                               
096700         STRING 'WRONG PICTURE '                                          
096800                 W-BILD                                                   
096900                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
097000       END-IF                                                             
097100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
097200       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57501 + 4                      
097300       PERFORM IMS-INSERT-MSG                                             
097400     END-IF                                                               
097500     .                                                                    
097600     EJECT                                                                
097700                                                                          
097800 MFS-RENSA-FAELT-UT SECTION.                                              
097900                                                                          
098000*    --- ALLA UTDATA-FÄLT                                                 
098100*    --- INKL. BLÄDDRINGSNYCKLAR                                          
098200     MOVE +1 TO INDX                                                      
098300     PERFORM UNTIL INDX > MAX-INDX                                        
098400       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
098500       ADD +1 TO INDX                                                     
098600     END-PERFORM                                                          
098700     .                                                                    
098800     SKIP3                                                                
098900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
099000                                                                          
099100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
099200     MOVE MFS-RENSA-FAELT TO MOD-BILDNR      (INDX)                       
099300                             MOD-IDKUNDNR    (INDX)                       
099400                             MOD-IDORDNR     (INDX)                       
099500                             MOD-IDARTNR     (INDX)                       
099600                             MOD-KVRO        (INDX)                       
099700                             MOD-BERADREF    (INDX)                       
099800                             MOD-TIORDREG    (INDX)                       
099900                             MOD-IDKUNDRF-RO (INDX)                       
100000     .                                                                    
100100     SKIP3                                                                
100200 MFS-RENSA-FAELT-IN SECTION.                                              
100300                                                                          
100400*    --- ALLA INDATA-FÄLT                                                 
100500     MOVE +1 TO INDX                                                      
100600     PERFORM UNTIL INDX > MAX-INDX                                        
100700       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
100800       ADD +1 TO INDX                                                     
100900     END-PERFORM                                                          
101000     .                                                                    
101100     EJECT                                                                
101200 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
101300                                                                          
101400*    --- ALLA INDATA-FÄLT                                                 
101500     MOVE MFS-RENSA-FAELT TO MOD-BILDNR   (INDX)                          
101600                             MOD-IDKUNDNR (INDX)                          
101700                             MOD-IDORDNR  (INDX)                          
101800                             MOD-IDARTNR  (INDX)                          
101900     .                                                                    
102000     EJECT                                                                
102100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
102200                                                                          
102300*    --- ALLA UTDATA-FÄLT                                                 
102400*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
102500     MOVE +1 TO INDX                                                      
102600     PERFORM UNTIL INDX > MAX-INDX                                        
102700       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
102800       ADD +1 TO INDX                                                     
102900     END-PERFORM                                                          
103000     .                                                                    
103100     SKIP2                                                                
103200 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
103300                                                                          
103400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
103500     MOVE MFS-ROER-EJ-FAELT TO MOD-BILDNR      (INDX)                     
103600                               MOD-IDKUNDNR    (INDX)                     
103700                               MOD-IDORDNR     (INDX)                     
103800                               MOD-IDARTNR     (INDX)                     
103900                               MOD-KVRO        (INDX)                     
104000                               MOD-BERADREF    (INDX)                     
104100                               MOD-TIORDREG    (INDX)                     
104200                               MOD-IDKUNDRF-RO (INDX)                     
104300     .                                                                    
104400     SKIP3                                                                
104500*MFS-ROER-EJ-FAELT-IN  SECTION.                                           
104600*                                                                         
104700*    --- ALLA INDATA-FÄLT                                                 
104800*    MOVE +1 TO INDX                                                      
104900*    PERFORM UNTIL INDX > MAX-INDX                                        
105000*      PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
105100*      ADD +1 TO INDX                                                     
105200*    END-PERFORM                                                          
105300*    .                                                                    
105400*    SKIP2                                                                
105500*MFS-ROER-EJ-RAD-FAELT-IN  SECTION.                                       
105600*                                                                         
105700*    --- ALLA INDATA-FÄLT                                                 
105800*    MOVE MFS-ROER-EJ-FAELT TO MOD-BILDNR   (INDX)                        
105900*                              MOD-IDKUNDNR (INDX)                        
106000*                              MOD-IDORDNR  (INDX)                        
106100*                              MOD-IDARTNR  (INDX)                        
106200*    .                                                                    
106300*    EJECT                                                                
106400*MFS-FORM-ATTR SECTION.                                                   
106500*                                                                         
106600*    --- ALLA INDATA-FÄLT                                                 
106700*    MOVE MFS-FORMATETS-ATTR TO MOD-XXXXXXXX-ATTR                         
106800*                               MOD-XXXXXXXX-ATTR                         
106900*    .                                                                    
107000*    SKIP2                                                                
107100*MFS-LAES-IN-IGEN SECTION.                                                
107200*                                                                         
107300*    --- ALLA INDATA-FÄLT                                                 
107400*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-XXXXXXXX-ATTR                      
107500*                                  MOD-XXXXXXXX-ATTR                      
107600*    .                                                                    
107700     EJECT                                                                
107800* --- IMS SEKTIONER ---                                                   
107900     SKIP3                                                                
108000 IMS-GET-MSG SECTION.                                                     
108100                                                                          
108200     MOVE '  QC' TO GODK-STATUSKODER                                      
108300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
108400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
108500     PERFORM IMS-STATUSKONTROLL                                           
108600     .                                                                    
108700     SKIP3                                                                
108800 IMS-INSERT-MSG SECTION.                                                  
108900                                                                          
109000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
109100       MOVE '0' TO MFS-KDHUVOMR                                           
109200     END-IF                                                               
109300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
109400     MOVE SPACE TO GODK-STATUSKODER                                       
109500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
109600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109700     PERFORM IMS-STATUSKONTROLL                                           
109800     .                                                                    
109900     EJECT                                                                
110000 IMS-CHANGE-ALTMSG SECTION.                                               
110100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
110200     MOVE '  A1A4' TO GODK-STATUSKODER                                    
110300     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
110400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
110500     PERFORM IMS-STATUSKONTROLL                                           
110600     .                                                                    
110700     SKIP3                                                                
110800 IMS-INSERT-ALTMSG SECTION.                                               
110900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
111000     MOVE SPACE TO GODK-STATUSKODER                                       
111100     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
111200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
111300     PERFORM IMS-STATUSKONTROLL                                           
111400     .                                                                    
111500     EJECT                                                                
111600 IMS-GU-WDQ1 SECTION.                                                     
111700                                                                          
111800     STRING 'WLORQM01(WDQ1BSEQ>=' W-WDQ1BSEQ-NEXT-X                       
111900                    '&WDQ1BSEQ<=' W-WDQ1BSEQ-MAX-X                        
112000                    '&KDORDBEK =' W-KDORDBEK-X ')'                        
112100          DELIMITED BY SIZE INTO SSA1                                     
112200     MOVE '  GE' TO GODK-STATUSKODER                                      
112300     CALL CBLTDLI USING GU BSEQ-PCB DLI-IO-AREA SSA1                      
112400     MOVE BSEQ-STATUS-CODE TO STATUS-WS                                   
112500     PERFORM IMS-STATUSKONTROLL                                           
112600     .                                                                    
112700     SKIP2                                                                
112800 IMS-GN-WDQ1 SECTION.                                                     
112900                                                                          
113000     STRING 'WLORQM01(WDQ1BSEQ>=' W-WDQ1BSEQ-NEXT-X                       
113100                    '&WDQ1BSEQ<=' W-WDQ1BSEQ-MAX-X                        
113200                    '&KDORDBEK =' W-KDORDBEK-X ')'                        
113300          DELIMITED BY SIZE INTO SSA1                                     
113400     MOVE '  GE' TO GODK-STATUSKODER                                      
113500     CALL CBLTDLI USING GN BSEQ-PCB DLI-IO-AREA SSA1                      
113600     MOVE BSEQ-STATUS-CODE TO STATUS-WS                                   
113700     PERFORM IMS-STATUSKONTROLL                                           
113800     .                                                                    
113900     SKIP2                                                                
114000 IMS-GU-WDQ101 SECTION.                                                   
114100                                                                          
114200     STRING 'WLORQM01(WDQ101KY =' W-WDQ101KY-X ')'                        
114300          DELIMITED BY SIZE INTO SSA1                                     
114400     MOVE '    ' TO GODK-STATUSKODER                                      
114500     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA SSA1                      
114600     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
114700     PERFORM IMS-STATUSKONTROLL                                           
114800     .                                                                    
114900     SKIP2                                                                
115000 IMS-GU-WDQ1B1 SECTION.                                                   
115100                                                                          
115200     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-NEXT-X                       
115300                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
115400                    '&KDORDBEK =' W-KDORDBEK-X                            
115500                    '&TITIORD9 =' W-TITIORD9-X ')'                        
115600          DELIMITED BY SIZE INTO SSA1                                     
115700     MOVE '  GE' TO GODK-STATUSKODER                                      
115800     CALL CBLTDLI USING GU ORQO-PCB DLI-IO-AREA3 SSA1                     
115900     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
116000     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
116200     SKIP2                                                                
116300 IMS-GN-WDQ1B1 SECTION.                                                   
116400                                                                          
116500     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-NEXT-X                       
116600                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
116700                    '&KDORDBEK =' W-KDORDBEK-X                            
116800                    '&TITIORD9 =' W-TITIORD9-X ')'                        
116900          DELIMITED BY SIZE INTO SSA1                                     
117000     MOVE '  GE' TO GODK-STATUSKODER                                      
117100     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA3 SSA1                     
117200     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
117300     PERFORM IMS-STATUSKONTROLL                                           
117400     .                                                                    
117500     SKIP2                                                                
117600 IMS-GU-WDA501 SECTION.                                                   
117700                                                                          
117800     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-A5-MIN-X                     
117900                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
118000                    '&KDORDKL  =' W-KDORDKL-X                             
118100                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
118200          DELIMITED BY SIZE INTO SSA1                                     
118300     MOVE '  GE' TO GODK-STATUSKODER                                      
118400     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA2 SSA1                     
118500     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
118600     PERFORM IMS-STATUSKONTROLL                                           
118700     .                                                                    
118800     EJECT                                                                
118900 IMS-STATUSKONTROLL SECTION.                                              
119000                                                                          
119100     SET STATUS-IX TO 1                                                   
119200     SEARCH GODK-STATUS                                                   
119300       AT END                                                             
119400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
119500         DELIMITED BY SIZE INTO FELTEXT                                   
119600         CALL FELLOG                                                      
119700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
119800         CONTINUE                                                         
119900     END-SEARCH                                                           
120000     .                                                                    
