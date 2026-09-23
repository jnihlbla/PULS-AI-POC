000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2014300.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   04/06/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR PARAMETRAR VID BERÄKNING AV                                
000900*        SÄKERHETSLAGER OCH                                               
001000*        HEMTAGNINGSKVANTITET                                             
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*                              WDP6                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T143                                              
001700*        MID:         W2I14301                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O14301                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W2014300'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700                                                                          
003800                                                                          
003900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004000     88  NYCKLAR-OK                          VALUE 'J'.                   
004100     88  NYCKLAR-FEL                         VALUE 'N'.                   
004200                                                                          
004300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004400     88  EGEN-MID                            VALUE '2143'.                
004500     88  GODK-MID                            VALUE '2143'.                
004600     88  HELP-MID                            VALUE '0551'.                
004700     EJECT                                                                
004800 01  ARBETSAREOR.                                                         
004900     03  W-KDPRISKL-9        PIC 9.                                       
005000     03  W-ALLA-FREKKL       PIC X(7) VALUE 'ABCDEFG'.                    
005100     03  IX-LEV              PIC S9(9)   VALUE +0    COMP SYNC.           
005200     03  IX-OMR              PIC S9(9)   VALUE +0    COMP SYNC.           
005300     03  IX-BEFT             PIC S9(9)   VALUE +0    COMP SYNC.           
005400     03  WS-KVVECKOR-MINSL   PIC S9(2)V9             COMP-3.              
005500     03  WS-KVVECKOR-MAXSL   PIC S9(2)V9             COMP-3.              
005600     03  WS-KVVECKOR-LVAR    PIC S9(2)V9             COMP-3.              
005700     03  WS-REVKOST-OMR      PIC S9(2)V9(2)          COMP-3.              
005800     03  WS-REOKOST-OMR      PIC S9(2)V9(2)          COMP-3.              
005900     03  WS-REOKOST-LEV      PIC S9(2)V9(2)          COMP-3.              
006000     03  WS-REOKOST-BEFT     PIC S9(2)V9(2)          COMP-3.              
006100     03  WS-REOLAGK          PIC S9(1)V9(2)          COMP-3.              
006200     03  WS-RETARGET         PIC S9(1)V9(3)          COMP-3.              
006300     03  PRLAGK              PIC S9(7)V9(2) VALUE    ZERO COMP-3.         
006400     03  RELAGR              PIC S9(3)V9(2) VALUE    ZERO COMP-3.         
006500     03  REOLAGK             PIC S9(1)V9(2)          COMP-3.              
006600     03  PRORDSK             PIC S9(7)V9(2) VALUE    ZERO COMP-3.         
006700     03  WS-KDPRODSL         PIC S9(2).                                   
006800     03  FILLER  REDEFINES WS-KDPRODSL.                                   
006900         05  WS-KDPRODSL-POS1    PIC 9.                                   
007000         05  WS-KDPRODSL-POS2    PIC 9.                                   
007100     EJECT                                                                
007200*01 -COPY W221W030                                                        
007300     EJECT                                                                
007400*01 -COPY W221W034                                                        
007500     EJECT                                                                
007600*01 -COPY W221W035                                                        
007700     EJECT                                                                
007800*01 -COPY W221W036                                                        
007900     EJECT                                                                
008000*01 -COPY W221W097                                                        
008100     EJECT                                                                
008200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008300 01  GENERELLA-SUBPROGRAM.                                                
008400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008600     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     EJECT                                                                
009000*    --- LÄNKAREA TILL SUBPROGRAM W222PBTO  (KVPB-PLAN)                   
009100*01  -COPY W222PBTO                                                       
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009400*01 -COPY WMEDAREA                                                        
009500     SKIP3                                                                
009600 01  MESSAGE-CODES.                                                       
009700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010200     SKIP3                                                                
010300*01 -COPY WMSGINIT                                                        
010400     EJECT                                                                
010500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010600*                                                                         
010700 01  SPAR-AREA.                                                           
010800     03  SPAR-IDTRANS           PIC X(4)    VALUE '2143'.                 
010900     EJECT                                                                
011000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011300     SKIP3                                                                
011400*01  MID -COPY W2I14301                                                   
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011700     SKIP3                                                                
011800*01  -COPY WMSGAREA                                                       
011900     EJECT                                                                
012000     03  MOD REDEFINES MSG-AREA.                                          
012100*      05  -COPY W2O14301                                                 
012200     EJECT                                                                
012300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012400     SKIP3                                                                
012500*01  -COPY WMFSAREA                                                       
012600     EJECT                                                                
012700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012800*                                                                         
012900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013000     SKIP3                                                                
013100 01  NYCKLAR-TILL-DLI.                                                    
013200     03  W-IDARTNR-X.                                                     
013300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013400     03  W-KDSEGKEY-X.                                                    
013500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013600     03  W-KDPRODSL-X.                                                    
013700         05  W-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
013800     03  W-KDPRISKL-X.                                                    
013900         05  W-KDPRISKL          PIC X       VALUE SPACE.                 
014000     03  W-KDFREKKL-X.                                                    
014100         05  W-KDFREKKL          PIC X       VALUE SPACE.                 
014200     SKIP2                                                                
014300*    --- STATUS-KOD FRÅN IMS                                              
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FINNS                       VALUE '  '.                  
014600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014800     SKIP2                                                                
014900 01  GODK-STATUSKODER.                                                    
015000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(64).                               
015300 01  SSA2                        PIC X(64).                               
015400     EJECT                                                                
015500*    --- IMS FUNKTIONSKODER                                               
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*    ---  DLI INPUT-OUTPUT AREA                                           
015900                                                                          
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
016100 01  DLI-IO-WDK601.                                                       
016200*    03  -COPY WDK601                                                     
016300     EJECT                                                                
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
016500 01  DLI-IO-WDK611.                                                       
016600*    03  -COPY WDK611                                                     
016700     EJECT                                                                
016800                                                                          
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP601'.                      
017000 01  DLI-IO-WDP601.                                                       
017100*    03  -COPY WDP601                                                     
017200     EJECT                                                                
017300 LINKAGE SECTION.                                                         
017400*01  -COPY W0009   -PRE MSG-                                              
017500*01  -COPY W0008   -PRE WDP7-                                             
017600     05  FILLER                    PIC X.                                 
017700                                                                          
017800*01  -COPY W0008  -PRE WDK6-                                              
017900     05  FILLER                    PIC X.                                 
018000     EJECT                                                                
018100 01  PBTO-WDK6-PCB                 PIC X.                                 
018200 01  PBTO-ARTM-PCB                 PIC X.                                 
018300 01  PBTO-WDK7-PCB                 PIC X.                                 
018400 01  PBTO-2501-PCB                 PIC X.                                 
018500 01  PBTO-WDB6R-PCB                PIC X.                                 
018600 01  PBTO-WDK7R-PCB                PIC X.                                 
018700*01  -COPY W0008  -PRE WDP6-                                              
018800     05  FILLER                    PIC X.                                 
018900     EJECT                                                                
019000 01  PBTO-WDB6-PCB                 PIC X.                                 
019100 01  PBTO-WDD7-PCB                 PIC X.                                 
019200 01  PBTO-WDK7E-PCB                PIC X.                                 
019300 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
019400 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
019500 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
019600 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
019700 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
019800 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
019900 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
020000 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
020100     EJECT                                                                
020200 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB                      
020300                       PBTO-WDK6-PCB PBTO-ARTM-PCB PBTO-WDK7-PCB          
020400                       PBTO-2501-PCB PBTO-WDB6R-PCB PBTO-WDK7R-PCB        
020500                       WDP6-PCB      PBTO-WDB6-PCB                        
020600                       PBTO-WDD7-PCB PBTO-WDK7E-PCB                       
020700                       PBTO-W222-UTIL-WDK6-PCB                            
020800                       PBTO-W222-UTIL-WDK7-PCB                            
020900                       PBTO-W222-UTIL-WDB6-PCB                            
021000                       PBTO-W222-UTUP-WDK7-PCB                            
021100                       PBTO-W222-UTUP-WDB6-PCB                            
021200                       PBTO-W222-UTUP-UTIL-WDK6-PCB                       
021300                       PBTO-W222-UTUP-UTIL-WDK7-PCB                       
021400                       PBTO-W222-UTUP-UTIL-WDB6-PCB                       
021500                       .                                                  
021600 MAIN SECTION.                                                            
021700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB                      
021800                       PBTO-WDK6-PCB PBTO-ARTM-PCB PBTO-WDK7-PCB          
021900                       PBTO-2501-PCB PBTO-WDB6R-PCB PBTO-WDK7R-PCB        
022000                       WDP6-PCB      PBTO-WDB6-PCB                        
022100                       PBTO-WDD7-PCB PBTO-WDK7E-PCB                       
022200                       PBTO-W222-UTIL-WDK6-PCB                            
022300                       PBTO-W222-UTIL-WDK7-PCB                            
022400                       PBTO-W222-UTIL-WDB6-PCB                            
022500                       PBTO-W222-UTUP-WDK7-PCB                            
022600                       PBTO-W222-UTUP-WDB6-PCB                            
022700                       PBTO-W222-UTUP-UTIL-WDK6-PCB                       
022800                       PBTO-W222-UTUP-UTIL-WDK7-PCB                       
022900                       PBTO-W222-UTUP-UTIL-WDB6-PCB                       
023000                       .                                                  
023100                                                                          
023200     PERFORM IMS-GET-MSG                                                  
023300     IF SEGMENT-FINNS                                                     
023400       PERFORM A-INIT                                                     
023500       PERFORM B-KOLLA-NYCKLAR                                            
023600       IF NYCKLAR-OK                                                      
023700         PERFORM F-LAES-VISA-INFO                                         
023800       END-IF                                                             
023900       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O14301 + 4                      
024000       PERFORM IMS-INSERT-MSG                                             
024100     END-IF                                                               
024200                                                                          
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 A-INIT SECTION.                                                          
024800                                                                          
024900     IF MSG-DUBBLA-TRANSKODER                                             
025000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I14301                 
025100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025300     ELSE                                                                 
025400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I14301                  
025500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
025600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025700     END-IF                                                               
025800                                                                          
025900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026200                                                                          
026300     MOVE LOW-VALUE TO MSG-AREA                                           
026400     MOVE 'W2O143N1' TO MFS-IDMOD                                         
026500     MOVE '2143' TO MOD-IDTRANS                                           
026600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
026700                                                                          
026800     IF EGEN-MID OR HELP-MID                                              
026900       CONTINUE                                                           
027000     ELSE                                                                 
027100       MOVE SPACE TO MFS-KDTRTYP                                          
027200       MOVE '7' TO MFS-IDPFK                                              
027300     END-IF                                                               
027400                                                                          
027500     MOVE    0.15  TO RELAGR                                              
027600     MOVE    ZERO  TO REOLAGK                                             
027700     MOVE 1500.00  TO PRLAGK                                              
027800     MOVE   40.00  TO PRORDSK                                             
027900     .                                                                    
028000     EJECT                                                                
028100 B-KOLLA-NYCKLAR SECTION.                                                 
028200                                                                          
028300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028400     MOVE '001'             TO MSGI-KDCALL                                
028500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
028600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
028700     MOVE '2143'            TO MSGI-IDTRANS                               
028800     IF GODK-MID                                                          
028900         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
029000     END-IF                                                               
029100     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
029200     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
029300                                                                          
029400*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
029500     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
029600                                                                          
029700     MOVE JA TO NYCKLAR-SW                                                
029800                                                                          
029900                                                                          
030000*    -- KONTROLL AV IDARTNR                                               
030100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
030200                                                                          
030300     IF MID-IDARTNR-IN NOT = ALL '+'                                      
030400       MOVE '7'         TO MFS-IDPFK                                      
030500       MOVE SPACE       TO MFS-KDTRTYP                                    
030600     END-IF                                                               
030700     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
030800     IF MSGI-IDARTNR NUMERIC                                              
030900       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
031000     ELSE                                                                 
031100       MOVE NEJ TO NYCKLAR-SW                                             
031200     END-IF                                                               
031300                                                                          
031400     IF GODK-MID AND NYCKLAR-OK                                           
031500       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
031600     ELSE                                                                 
031700       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
031800     END-IF                                                               
031900                                                                          
032000     IF NYCKLAR-FEL                                                       
032100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
032200       CALL WMEDKONV USING MED-WMEDAREA                                   
032300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
032400***    PERFORM MFS-RENSA-FAELT-IN                                         
032500       PERFORM MFS-RENSA-FAELT-UT                                         
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 F-LAES-VISA-INFO SECTION.                                                
033000                                                                          
033100     PERFORM FA-LAES-GRUNDDATA                                            
033200                                                                          
033300     IF SEGMENT-SAKNAS                                                    
033400* FLYTTA LÄMPLIGT FELMEDDELANDE                                           
033500        CALL WMEDKONV USING MED-WMEDAREA                                  
033600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
033700        PERFORM MFS-RENSA-FAELT-UT                                        
033800        MOVE 'ARTIKEL SAKNAS' TO MOD-TEMFSFEL                             
033900     ELSE                                                                 
034000        PERFORM FB-FYLL-I-FAELT                                           
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 FA-LAES-GRUNDDATA SECTION.                                               
034500                                                                          
034600     PERFORM IMS-GET-WDK601                                               
034700                                                                          
034800     IF SEGMENT-FINNS                                                     
034900        PERFORM IMS-GET-WDK611                                            
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 FB-FYLL-I-FAELT   SECTION.                                               
035400                                                                          
035500     MOVE  ART-KDPRODSL         TO MOD-KDPRODSL                           
035600                                   W-KDPRODSL                             
035700     MOVE  ART-IDLEVNR          TO MOD-IDLEVNR                            
035800     MOVE  CLAG-KVVECKOR-LVAR   TO MOD-KVVECKOR-LVAR-ART                  
035900     MOVE  CLAG-KVEOQ           TO MOD-KVEOQ                              
036000     MOVE  CLAG-KVSLAGER        TO MOD-KVSLAGER                           
036100     MOVE  CLAG-KVSLAGER-OPT    TO MOD-KVSLAGER-OPT                       
036200     MOVE  CLAG-KDPRISKL        TO MOD-KDPRISKL                           
036300                                   W-KDPRISKL                             
036400     MOVE  CLAG-KDFREKKL        TO MOD-KDFREKKL                           
036500                                   W-KDFREKKL                             
036600     MOVE  CLAG-RESLJUST        TO MOD-RESLJUST                           
036700     MOVE  CLAG-BEFT            TO MOD-BEFT                               
036800     MOVE  CLAG-ADLAGOMR        TO MOD-ADLAGOMR                           
036900     MOVE  CLAG-PRORDSK         TO MOD-PRORDSK                            
037000     MOVE  CLAG-KVQ             TO MOD-KVQ                                
037100     MOVE  CLAG-KVULOAD         TO MOD-KVULOAD                            
037200     MOVE  CLAG-KVPALL          TO MOD-KVPALL                             
037300     MOVE  CLAG-KVMAD-TOT       TO MOD-KVMAD-TOT                          
037400     IF CLAG-FLNYBER = JA                                                 
037500        MOVE PRORDSK            TO MOD-PRORDSK                            
037600     END-IF                                                               
037700***  KOLLA FÖRST KDPRODSL OSV VILKA VÄRDEN...                             
037800     MOVE W-KDPRODSL     TO WS-KDPRODSL                                   
037900     IF WS-KDPRODSL-POS1 = 2                                              
038000        MOVE 1           TO WS-KDPRODSL-POS1                              
038100        MOVE WS-KDPRODSL TO W-KDPRODSL                                    
038200     END-IF                                                               
038300     PERFORM IMS-GET-WDP601                                               
038400     IF SEGMENT-FINNS                                                     
038500        MOVE SLB-RETARGET       TO WS-RETARGET                            
038600        COMPUTE MOD-RETARGET    =  WS-RETARGET * 100                      
038700        MOVE SLB-KVVECKOR-MINSL                                           
038800                                TO MOD-KVVECKOR-MINSL                     
038900        MOVE SLB-KVVECKOR-MAXSL                                           
039000                                TO MOD-KVVECKOR-MAXSL                     
039100        MOVE SLB-REOLAGK        TO WS-REOLAGK                             
039200        COMPUTE MOD-REOLAGK        =  WS-REOLAGK * 100                    
039300     ELSE                                                                 
039400        MOVE ZERO               TO MOD-RETARGET                           
039500                                   MOD-KVVECKOR-MINSL                     
039600                                   MOD-KVVECKOR-MAXSL                     
039700                                   MOD-REOLAGK                            
039800     END-IF                                                               
039900     MOVE  PRLAGK               TO MOD-PRLAGK                             
040000     COMPUTE MOD-RELAGR         =  RELAGR * 100                           
040100                                                                          
040200     PERFORM FBA-HAMTA-FAELT                                              
040300                                                                          
040400     MOVE  WS-REOKOST-LEV       TO MOD-REOKOST-LEV                        
040500     MOVE  WS-REOKOST-BEFT      TO MOD-REOKOST-BEFT                       
040600     MOVE  WS-REOKOST-OMR       TO MOD-REOKOST-OMR                        
040700     MOVE  WS-REVKOST-OMR       TO MOD-REVKOST-OMR                        
040800     MOVE  WS-KVVECKOR-LVAR     TO MOD-KVVECKOR-LVAR-LEV                  
040900                                                                          
041000     PERFORM FBB-HAMTA-KVPB-PLAN                                          
041100     .                                                                    
041200     EJECT                                                                
041300 FBA-HAMTA-FAELT   SECTION.                                               
041400                                                                          
041500     MOVE +1 TO IX-LEV                                                    
041600     MOVE ZERO TO WS-KVVECKOR-LVAR                                        
041700     PERFORM UNTIL IX-LEV > T30-MAXLEV-IX                                 
041800       IF ART-IDLEVNR = T30-IDLEVNR (IX-LEV)                              
041900          MOVE T30-KVVECKOR-LVAR (IX-LEV) TO WS-KVVECKOR-LVAR             
042000       END-IF                                                             
042100       ADD +1 TO IX-LEV                                                   
042200     END-PERFORM                                                          
042300                                                                          
042400     MOVE +1 TO IX-LEV                                                    
042500     MOVE 1 TO WS-REOKOST-LEV                                             
042600     PERFORM UNTIL IX-LEV > T34-MAXLEV-IX                                 
042700       IF ART-IDLEVNR = T34-IDLEVNR (IX-LEV)                              
042800          MOVE T34-REOKOST-LEV (IX-LEV) TO WS-REOKOST-LEV                 
042900       END-IF                                                             
043000       ADD +1 TO IX-LEV                                                   
043100     END-PERFORM                                                          
043200                                                                          
043300     MOVE +1 TO IX-OMR                                                    
043400     MOVE 1 TO WS-REOKOST-OMR                                             
043500     MOVE 1 TO WS-REVKOST-OMR                                             
043600     PERFORM UNTIL IX-OMR > T36-MAXADLAGOMR-IX                            
043700       IF CLAG-ADLAGOMR = T36-ADLAGOMR (IX-OMR)                           
043800          MOVE T36-REOKOST-OMR (IX-OMR) TO WS-REOKOST-OMR                 
043900          MOVE T36-REVKOST-OMR (IX-OMR) TO WS-REVKOST-OMR                 
044000       END-IF                                                             
044100       ADD +1 TO IX-OMR                                                   
044200     END-PERFORM                                                          
044300                                                                          
044400     MOVE +1 TO IX-BEFT                                                   
044500     MOVE 1 TO WS-REOKOST-BEFT                                            
044600     PERFORM UNTIL IX-BEFT > T35-MAXBEFT-IX                               
044700       IF CLAG-BEFT = T35-BEFT (IX-BEFT)                                  
044800          MOVE T35-REOKOST-BEFT (IX-BEFT) TO WS-REOKOST-BEFT              
044900       END-IF                                                             
045000       ADD +1 TO IX-BEFT                                                  
045100     END-PERFORM                                                          
045200                                                                          
045300     .                                                                    
045400     EJECT                                                                
045500 FBB-HAMTA-KVPB-PLAN SECTION.                                             
045600                                                                          
045700        IF CLAG-DAPBPLAN > ZERO                                           
045800*                                              MANUELL KVPB-PLAN          
045900           MOVE CLAG-KVPB-PLAN         TO MOD-KVPB-PLAN                   
046000        ELSE                                                              
046100*                                            MASKINELL KVPB-PLAN          
046200           MOVE W-IDARTNR   TO PBTO-IDARTNR                               
046300           CALL W222PBTO USING PBTO-W222PBTO                              
046400                               PBTO-WDK6-PCB                              
046500                               PBTO-WDK7-PCB                              
046600                               PBTO-ARTM-PCB                              
046700                               PBTO-2501-PCB                              
046800                               PBTO-WDB6R-PCB                             
046900                               PBTO-WDK7R-PCB                             
047000                               PBTO-WDB6-PCB                              
047100                               PBTO-WDD7-PCB                              
047200                               PBTO-WDK7E-PCB                             
047300                               PBTO-W222-UTIL-WDK6-PCB                    
047400                               PBTO-W222-UTIL-WDK7-PCB                    
047500                               PBTO-W222-UTIL-WDB6-PCB                    
047600                               PBTO-W222-UTUP-WDK7-PCB                    
047700                               PBTO-W222-UTUP-WDB6-PCB                    
047800                               PBTO-W222-UTUP-UTIL-WDK6-PCB               
047900                               PBTO-W222-UTUP-UTIL-WDK7-PCB               
048000                               PBTO-W222-UTUP-UTIL-WDB6-PCB               
048100                                                                          
048200           IF PBTO-KDSVAR = JA                                            
048300              MOVE PBTO-KVPB-PLAN      TO MOD-KVPB-PLAN                   
048400           ELSE                                                           
048500              MOVE ZERO                TO MOD-KVPB-PLAN                   
048600           END-IF                                                         
048700        END-IF                                                            
048800     .                                                                    
048900     EJECT                                                                
049000 MFS-RENSA-FAELT-UT SECTION.                                              
049100                                                                          
049200*    --- ALLA UTDATA-FÄLT                                                 
049300     MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL                                 
049400                             MOD-IDLEVNR                                  
049500                             MOD-KVVECKOR-LVAR-ART                        
049600                             MOD-KVEOQ                                    
049700                             MOD-KVSLAGER                                 
049800                             MOD-KVSLAGER-OPT                             
049900                             MOD-KDPRISKL                                 
050000                             MOD-KDFREKKL                                 
050100                             MOD-RESLJUST                                 
050200                             MOD-BEFT                                     
050300                             MOD-ADLAGOMR                                 
050400                             MOD-PRORDSK                                  
050500                             MOD-RETARGET                                 
050600                             MOD-KVVECKOR-MINSL                           
050700                             MOD-KVVECKOR-MAXSL                           
050800                             MOD-KVVECKOR-LVAR-LEV                        
050900                             MOD-REOKOST-LEV                              
051000                             MOD-REOKOST-BEFT                             
051100                             MOD-REOKOST-OMR                              
051200                             MOD-REVKOST-OMR                              
051300                             MOD-REOLAGK                                  
051400                             MOD-PRLAGK                                   
051500                             MOD-RELAGR                                   
051600                             MOD-KVQ                                      
051700                             MOD-KVULOAD                                  
051800                             MOD-KVPALL                                   
051900                             MOD-KVMAD-TOT                                
052000                             MOD-KVPB-PLAN                                
052100     .                                                                    
052200     SKIP3                                                                
052300*MFS-RENSA-FAELT-IN SECTION.                                              
052400*                                                                         
052500*    --- ALLA INDATA-FÄLT                                                 
052600*    MOVE MFS-RENSA-FAELT TO MOD-XXXXXXXX-IN                              
052700*                               MOD-XXXXXXXX-IN                           
052800*    .                                                                    
052900*    EJECT                                                                
053000*MFS-ROER-EJ-FAELT-UT  SECTION.                                           
053100*                                                                         
053200*    --- ALLA UTDATA-FÄLT                                                 
053300*    MOVE MFS-ROER-EJ-FAELT TO MOD-XXXXXXXX                               
053400*                                MOD-XXXXXXXX                             
053500*    .                                                                    
053600*    SKIP3                                                                
053700*MFS-ROER-EJ-FAELT-IN  SECTION.                                           
053800*                                                                         
053900*    --- ALLA INDATA-FÄLT                                                 
054000*    MOVE MFS-ROER-EJ-FAELT TO MOD-XXXXXXXX-IN                            
054100*                                MOD-XXXXXXXX-IN                          
054200*    .                                                                    
054300*    EJECT                                                                
054400*MFS-FORM-ATTR SECTION.                                                   
054500*                                                                         
054600*    --- ALLA INDATA-FÄLT                                                 
054700*    MOVE MFS-FORMATETS-ATTR TO MOD-XXXXXXXX-ATTR                         
054800*                               MOD-XXXXXXXX-ATTR                         
054900*    .                                                                    
055000*    SKIP2                                                                
055100*MFS-LAES-IN-IGEN SECTION.                                                
055200*                                                                         
055300*    --- ALLA INDATA-FÄLT                                                 
055400*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-XXXXXXXX-ATTR                      
055500*                                  MOD-XXXXXXXX-ATTR                      
055600*    .                                                                    
055700     EJECT                                                                
055800* --- IMS SEKTIONER ---                                                   
055900     SKIP3                                                                
056000 IMS-GET-MSG SECTION.                                                     
056100                                                                          
056200     MOVE '  QC' TO GODK-STATUSKODER                                      
056300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
056400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056500     PERFORM IMS-STATUSKONTROLL                                           
056600     .                                                                    
056700     SKIP3                                                                
056800 IMS-INSERT-MSG SECTION.                                                  
056900                                                                          
057000     IF MSGI-IDLAND-SPR = 'SE'                                            
057100       MOVE '0' TO MFS-KDHUVOMR                                           
057200     END-IF                                                               
057300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
057400     MOVE SPACE TO GODK-STATUSKODER                                       
057500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
057600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
057900     EJECT                                                                
058000 IMS-GET-WDK601 SECTION.                                                  
058100                                                                          
058200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
058300          DELIMITED BY SIZE INTO SSA1                                     
058400     MOVE '  GE' TO GODK-STATUSKODER                                      
058500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
058600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
058700     PERFORM IMS-STATUSKONTROLL                                           
058800     .                                                                    
058900     EJECT                                                                
059000 IMS-GET-WDK611 SECTION.                                                  
059100                                                                          
059200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
059300          DELIMITED BY SIZE INTO SSA1                                     
059400     MOVE '  GE' TO GODK-STATUSKODER                                      
059500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
059600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     .                                                                    
059900     EJECT                                                                
060000 IMS-GET-WDP601 SECTION.                                                  
060100     STRING 'WDP601  (KDPRODSL =' W-KDPRODSL-X                            
060200                    '&KDPRISKL =' W-KDPRISKL-X                            
060300                    '&KDFREKKL =' W-KDFREKKL-X ')'                        
060400          DELIMITED BY SIZE INTO SSA1                                     
060500     MOVE '  GE' TO GODK-STATUSKODER                                      
060600     CALL CBLTDLI USING GU WDP6-PCB DLI-IO-WDP601 SSA1                    
060700     MOVE WDP6-STATUS-CODE TO STATUS-WS                                   
060800     PERFORM IMS-STATUSKONTROLL                                           
060900     .                                                                    
061000     EJECT                                                                
061100 IMS-STATUSKONTROLL SECTION.                                              
061200                                                                          
061300     SET STATUS-IX TO 1                                                   
061400     SEARCH GODK-STATUS                                                   
061500       AT END                                                             
061600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
061700         DELIMITED BY SIZE INTO FELTEXT                                   
061800         CALL FELLOG                                                      
061900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
062000         CONTINUE                                                         
062100     END-SEARCH                                                           
062200     .                                                                    
