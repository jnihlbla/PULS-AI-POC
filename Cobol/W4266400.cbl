000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4266400.                                                
000400*AUTHOR.         GERRY CARMICHAEL.                                        
000500*DATE-WRITTEN.   92/08/25.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER W6G2 OCH SKRIVER EN FIL.                               
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR W6KODA (W6G2)                              
001300*        PROGRAMMET UPPDATERAR W6CKPE (W6G2)                              
001400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001500*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001600*        PROGRAMMET LÄSER      W6KVAH (W6D2)                              
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- UTFIL                                                      
003100     SELECT W42664                     ASSIGN TO W42664D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W42664                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100*01  POST -COPY W4266301 -PRE  UT-  -L.                                   
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500*    ---- ARBETSVARIABLER                                                 
004600*                                                                         
004700 77  IDPGM                       PIC X(8)    VALUE 'W4266400'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP SYNC VALUE ZERO.        
005200     SKIP2                                                                
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600*                                                                         
005700     EJECT                                                                
005800*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
005900     SKIP3                                                                
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
006200   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006300   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006400   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
006500   EJECT                                                                  
006600*  --- PARAMETRAR TILL POSTSUM                                            
006700*                                                                         
006800*01 -COPY W0005  -PRE POSTSUM-                                            
006900   EJECT                                                                  
007000                                                                          
007100 01  UT-AREA-START               PIC X(24)   VALUE                        
007200                                 'UT-AREA-START  '.                       
007300     SKIP2                                                                
007400                                                                          
007500*01  AREA -COPY W4266301     -PRE UT-                                     
007600     EJECT                                                                
007700*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
007800                                                                          
007900 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
008000     SKIP3                                                                
008100*    ---- STATUSKOD FRÅN IMS                                              
008200                                                                          
008300 01  STATUS-WS               PIC XX.                                      
008400     88  SEGMENT-SLUT                     VALUE 'GB'.                     
008500     88  SEGMENT-FINNS                    VALUE '  '.                     
008600     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
008700     88  IMS-EJ-OK                        VALUE 'XD'.                     
008800     SKIP3                                                                
008900 01  GODK-STATUSKODER.                                                    
009000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
009100     SKIP3                                                                
009200 01  SSA1                    PIC X(64).                                   
009300 01  SSA2                    PIC X(64).                                   
009400     EJECT                                                                
009500*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
009600                                                                          
009700 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
009800 01  NYCKLAR-TILL-DLI.                                                    
009900                                                                          
010000     03  W-IDARTNR-X.                                                     
010100         05 W-IDARTNR            PIC S9(9)  VALUE ZERO COMP-3.            
010200                                                                          
010300     03  W-IDSKYLT-KEY-X.                                                 
010400         05 W-IDSKYLT-KEY        PIC X(3)   VALUE SPACE.                  
010500                                                                          
010600     03  W-W6GXKEY-X.                                                     
010700         05 W-IDHTYP-6103        PIC X(04)  VALUE '6103'.                 
010800         05 FILLER               PIC X(26)  VALUE LOW-VALUE.              
010900                                                                          
011000     EJECT                                                                
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011500     SKIP3                                                                
011600 01  DLI-IO-AREA.                                                         
011700     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
011800     SKIP3                                                                
011900     03  WLARTC01 REDEFINES IO-AREA.                                      
012000*        05  -COPY WDK601    -PRE   ARTC-                                 
012100     SKIP3                                                                
012200     03  WLARTC11 REDEFINES IO-AREA.                                      
012300*        05  -COPY WDK611                                                 
012400     SKIP3                                                                
013700     03  WLBENA01 REDEFINES IO-AREA.                                      
013800*        05  -COPY WDD301                                                 
013900     SKIP3                                                                
014000     03  WLBENA11 REDEFINES IO-AREA.                                      
014100*        05  -COPY WDD311                                                 
014200     SKIP3                                                                
014300     03  W6KODA01 REDEFINES IO-AREA.                                      
014400*        05  -COPY W6GX01                                                 
014500     SKIP3                                                                
014600     03  W6KODA11 REDEFINES IO-AREA.                                      
014700*        05  -COPY W6GX6104                                               
014800     SKIP3                                                                
014900     03  W6KVAH01 REDEFINES IO-AREA.                                      
015000*        05  -COPY W6D201                                                 
015100     SKIP3                                                                
015200     03  W6KVAH12 REDEFINES IO-AREA.                                      
015300*        05  -COPY W6D212                                                 
015400     EJECT                                                                
015500 LINKAGE SECTION.                                                         
015600     SKIP2                                                                
015700*01  -COPY W0009      -PRE  MSG-                                          
015800     EJECT                                                                
015900*01  -COPY W0008      -PRE  KODA-                                         
016000       05  FILLER                PIC X.                                   
016100     EJECT                                                                
016200*01  -COPY W0008      -PRE  ARTC-                                         
016300       05  FILLER                PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008      -PRE  BENA-                                         
016600       05  FILLER                PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008      -PRE  KVAH-                                         
016900       05  FILLER                PIC X.                                   
017000     EJECT                                                                
017100 PROCEDURE DIVISION  USING MSG-PCB KODA-PCB ARTC-PCB                      
017200                                   BENA-PCB KVAH-PCB.                     
017300     ENTRY 'DLITCBL' USING MSG-PCB KODA-PCB ARTC-PCB                      
017400                                   BENA-PCB KVAH-PCB.                     
017500     PERFORM A-INIT                                                       
017600                                                                          
017700     PERFORM B-BEHANDLA-INDATA                                            
017800                                                                          
017900     PERFORM Z-FINIT                                                      
018000                                                                          
018100     MOVE ZERO TO RETURN-CODE                                             
018200     GOBACK                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 A-INIT SECTION.                                                          
018600     SKIP2                                                                
018700                                                                          
018800     OPEN OUTPUT W42664                                                   
018900                                                                          
019000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019100     .                                                                    
019200     EJECT                                                                
019300 B-BEHANDLA-INDATA SECTION.                                               
019400                                                                          
019500     PERFORM IMS-GHU-KODA-KODA01                                          
019600     PERFORM BA-LAES-BARN-SKRIV-POSTER                                    
019700     PERFORM IMS-GHU-KODA-KODA01                                          
019800     PERFORM IMS-DLET-KODA-KODA01                                         
019900     PERFORM IMS-ISRT-KODA-KODA01                                         
020000     .                                                                    
020100     EJECT                                                                
020200                                                                          
020300 BA-LAES-BARN-SKRIV-POSTER SECTION.                                       
020400                                                                          
020500     PERFORM IMS-GNP-KODA-KODA11                                          
020600     IF SEGMENT-FINNS                                                     
020700       PERFORM UNTIL SEGMENT-SAKNAS                                       
020800          PERFORM BAA-NOLLA-UTAREA                                        
020900          MOVE 6104-IDARTNR     TO  W-IDARTNR                             
021000                                                                          
021100          PERFORM BAC-LAES-ARTC                                           
021200          PERFORM BAD-LAES-BENA                                           
021300          PERFORM BAE-LAES-KVAH                                           
021400                                                                          
021500          PERFORM S01-SKRIV-W42664                                        
021600                                                                          
021700          PERFORM IMS-GNP-KODA-KODA11                                     
021800       END-PERFORM                                                        
021900     END-IF                                                               
022000                                                                          
022100     .                                                                    
022200     EJECT                                                                
022300 BAA-NOLLA-UTAREA SECTION.                                                
022400     MOVE ZERO                  TO  UT-IDARTNR                            
022500     MOVE SPACE                 TO  UT-ADKVAULG                           
022600                                    UT-BEART                              
022700     MOVE ZERO                  TO  UT-IDBERED                            
022800                                    UT-IDFKNGRP                           
023100                                    UT-KDERS                              
023200                                    UT-KDFARLIG                           
023300                                    UT-BEFT                               
023400                                    UT-KDPRODSL                           
023500     MOVE SPACE                 TO  UT-KDKVAKTL                           
023510                                    UT-IDLEVNR-OLD                        
023520                                    UT-IDLEVNR-NEW                        
023600     MOVE ZERO                  TO  UT-KDVVKL                             
023700                                    UT-KDYTBEH                            
023800                                    UT-PRARTSTD                           
023900     MOVE SPACE                 TO  UT-TEXT                               
024000     MOVE ZERO                  TO  UT-TIFINLV                            
024100                                    UT-TIKVASAK                           
024200     .                                                                    
024300     EJECT                                                                
024400 BAC-LAES-ARTC SECTION.                                                   
024500                                                                          
024600     PERFORM IMS-GU-ARTC-ARTC01                                           
024700     IF SEGMENT-FINNS                                                     
024800       MOVE ARTC-ART-IDARTNR    TO  UT-IDARTNR                            
024900       MOVE ARTC-ART-IDFKNGRP   TO  UT-IDFKNGRP                           
025000       MOVE ARTC-ART-IDLEVNR    TO  UT-IDLEVNR-OLD                        
025100                                    UT-IDLEVNR-NEW                        
025200       MOVE ARTC-ART-TIFINLV    TO  UT-TIFINLV                            
025300       MOVE ARTC-ART-KDPRODSL   TO  UT-KDPRODSL                           
025400                                                                          
025500       PERFORM IMS-GNP-ARTC-ARTC11                                        
025600       IF SEGMENT-FINNS                                                   
025700         IF CLAG-IDBERED > 19 OR CLAG-IDBERED = ZERO                      
025800           MOVE 'INLEV KKOD '      TO  UT-TEXT                            
025900           MOVE 'S  '              TO  W-IDSKYLT-KEY                      
026000         ELSE                                                             
026100           MOVE 'NO INSP. C.'      TO  UT-TEXT                            
026200           MOVE 'GB '              TO  W-IDSKYLT-KEY                      
026300         END-IF                                                           
026400         MOVE CLAG-IDANSK          TO  UT-IDANSK                          
026410         MOVE CLAG-IDBERED         TO  UT-IDBERED                         
026500         MOVE CLAG-KDYTBEH         TO  UT-KDYTBEH                         
026600         MOVE CLAG-KDVVKL          TO  UT-KDVVKL                          
026700         MOVE CLAG-PRARTSTD        TO  UT-PRARTSTD                        
026800         MOVE CLAG-KDFARLIG        TO  UT-KDFARLIG                        
026900         MOVE CLAG-BEFT            TO  UT-BEFT                            
027100         MOVE CLAG-KDERS           TO  UT-KDERS                           
027200       END-IF                                                             
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 BAD-LAES-BENA SECTION.                                                   
029200     PERFORM IMS-GU-BENA-BENA01                                           
029300     IF SEGMENT-FINNS                                                     
029400       PERFORM IMS-GNP-BENA-BENA11                                        
029500       IF SEGMENT-FINNS                                                   
029600         MOVE TEXT-BEART        TO  UT-BEART                              
029700       END-IF                                                             
029800     END-IF                                                               
029900     .                                                                    
030000     EJECT                                                                
030100 BAE-LAES-KVAH SECTION.                                                   
030200     PERFORM IMS-GU-KVAH-KVAH01                                           
030300     IF SEGMENT-FINNS                                                     
030400       MOVE ART-ADKVAULG        TO  UT-ADKVAULG                           
030500       MOVE ART-KDKVAKTL        TO  UT-KDKVAKTL                           
030600       PERFORM IMS-GNP-KVAH-KVAH12                                        
030700       IF SEGMENT-FINNS                                                   
030800         MOVE LEV-TIKVASAK      TO  UT-TIKVASAK                           
030900       END-IF                                                             
031000     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031300 S01-SKRIV-W42664 SECTION.                                                
031400     WRITE UT-POST FROM UT-AREA                                           
031500                                                                          
031600     MOVE SPACE        TO POSTSUM-TRANSTYP                                
031700     MOVE 'W42664 ' TO POSTSUM-FDNAMN                                     
031800     MOVE 'W42664D1' TO POSTSUM-DDNAMN2                                   
031900     CALL POSTSUM USING POSTSUM-PARM                                      
032000     .                                                                    
032100     EJECT                                                                
032200 Z-FINIT SECTION.                                                         
032300                                                                          
032400     CLOSE W42664                                                         
032500                                                                          
032600     MOVE 'S' TO POSTSUM-OPKOD                                            
032700     CALL POSTSUM USING POSTSUM-PARM                                      
032800     .                                                                    
032900     EJECT                                                                
033000                                                                          
033100*    ---- IMS SEKTIONER                                                   
033200                                                                          
033300 IMS-GHU-KODA-KODA01 SECTION.                                             
033400     STRING 'W6KODA01(W6GXKEY  =' W-W6GXKEY-X ')'                         
033500          DELIMITED BY SIZE INTO SSA1                                     
033600     MOVE '  ' TO GODK-STATUSKODER                                        
033700     CALL CBLTDLI USING GHU KODA-PCB DLI-IO-AREA SSA1                     
033800     MOVE KODA-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     .                                                                    
034100                                                                          
034200 IMS-GNP-KODA-KODA11 SECTION.                                             
034300     MOVE 'W6KODA11  ' TO SSA1                                            
034400     MOVE '  GE' TO GODK-STATUSKODER                                      
034500     CALL CBLTDLI USING GNP KODA-PCB DLI-IO-AREA SSA1                     
034600     MOVE KODA-STATUS-CODE TO STATUS-WS                                   
034700     PERFORM IMS-STATUSKONTROLL                                           
034800     .                                                                    
034900                                                                          
035000 IMS-DLET-KODA-KODA01 SECTION.                                            
035100                                                                          
035200     MOVE '  ' TO GODK-STATUSKODER                                        
035300     CALL CBLTDLI USING DLET KODA-PCB DLI-IO-AREA                         
035400     MOVE KODA-STATUS-CODE TO STATUS-WS                                   
035500     PERFORM IMS-STATUSKONTROLL                                           
035600     .                                                                    
035700                                                                          
035800 IMS-ISRT-KODA-KODA01 SECTION.                                            
035900                                                                          
036000     MOVE 'W6KODA01 '           TO SSA1                                   
036100     MOVE '  II' TO GODK-STATUSKODER                                      
036200     CALL CBLTDLI USING ISRT KODA-PCB DLI-IO-AREA SSA1                    
036300     MOVE KODA-STATUS-CODE TO STATUS-WS                                   
036400     PERFORM IMS-STATUSKONTROLL                                           
036500     .                                                                    
036600     EJECT                                                                
036700                                                                          
036800 IMS-GU-ARTC-ARTC01 SECTION.                                              
036900                                                                          
037000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
037100            DELIMITED BY SIZE INTO SSA1                                   
037200     MOVE '  GE' TO GODK-STATUSKODER                                      
037300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
037400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
037500     PERFORM IMS-STATUSKONTROLL                                           
037600     .                                                                    
037700     SKIP3                                                                
037800 IMS-GNP-ARTC-ARTC11 SECTION.                                             
037900                                                                          
038000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
038100     MOVE '  GE' TO GODK-STATUSKODER                                      
038200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
038300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
038400     PERFORM IMS-STATUSKONTROLL                                           
038500     .                                                                    
038600     SKIP3                                                                
041300 IMS-GU-BENA-BENA01 SECTION.                                              
041400                                                                          
041500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
041600     DELIMITED BY SIZE INTO SSA1                                          
041700     MOVE '  GE' TO GODK-STATUSKODER                                      
041800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
041900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200     EJECT                                                                
042300                                                                          
042400 IMS-GNP-BENA-BENA11 SECTION.                                             
042500                                                                          
042600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-KEY-X ')'                     
042700     DELIMITED BY SIZE INTO SSA1                                          
042800     MOVE '  GE' TO GODK-STATUSKODER                                      
042900     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
043000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     .                                                                    
043300     EJECT                                                                
043400 IMS-GU-KVAH-KVAH01 SECTION.                                              
043500     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
043600          DELIMITED BY SIZE INTO SSA1                                     
043700     MOVE '  GE' TO GODK-STATUSKODER                                      
043800     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA SSA1                      
043900     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200     SKIP3                                                                
044300 IMS-GNP-KVAH-KVAH12 SECTION.                                             
044400     MOVE 'W6KVAH12 ' TO SSA1                                             
044500     MOVE '  GE' TO GODK-STATUSKODER                                      
044600     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA SSA1                     
044700     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
044800     PERFORM IMS-STATUSKONTROLL                                           
044900     .                                                                    
045000     SKIP3                                                                
045100 IMS-STATUSKONTROLL SECTION.                                              
045200                                                                          
045300     SET STATUS-IX TO 1                                                   
045400     SEARCH GODK-STATUS                                                   
045500       AT END                                                             
045600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
045700         DELIMITED BY SIZE INTO FELTEXT                                   
045800         CALL FELLOG                                                      
045900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046000         CONTINUE                                                         
046100     END-SEARCH                                                           
046200     .                                                                    
