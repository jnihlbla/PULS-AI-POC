000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9043000.                                                
000400*AUTHOR.         ROS-MARIE CLASON - GUIDE DATAKONSULT AB.                 
000500*DATE-WRITTEN.   92/02/26.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM VISAR ANGIVEN ARTIKELS                  
001200*        FÖRBEHANDLINGS- OCH/ELLER FÖRPACKNINGSGRUPP.                     
001300*        OCH OLIKA ANSVARIGA FÖR ARTIKELN                                 
001400*                                                                         
001500*        THE PROGRAM READS     W6HANA (W6G1)                              
001600*        THE PROGRAM READS     W6PLAA (W6G1)                              
001700*        THE PROGRAM READS     WLARTC (WDK6)                              
001800*        THE PROGRAM READS     WLARTC (WDK7)                              
001900*        THE PROGRAM READS     WDP3                                       
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: W90430T                                             
002300*        MID:         W90430I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        MOD:         W90430O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W9043000'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003900                                                                          
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  SW-TRAEFF                   PIC X       VALUE SPACE.                 
004300                                                                          
004400 77  LANGUAGE-IX                 PIC S9(9)  VALUE +0    COMP SYNC.        
004500*   IF ANSWER TO SCREEN: MAX-MOD-LENGTH = MOD-LENGTH + 4                  
004600 77  MAX-MOD-LENGTH              PIC S9(4)  VALUE +578  COMP SYNC.        
004700                                                                          
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005100 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005200 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
005300 77  SPAR-IDFKNGRP               PIC S9(5)   VALUE ZERO COMP-3.           
005400 77  WS-IDPERSON                 PIC S9(3)   VALUE +0 COMP-3.             
005500 01  W-IDLEVNR                   PIC X(5)    VALUE SPACE.                 
005600                                                                          
005700 01  SPAR-IDBERED                PIC S9(3)   VALUE +0 COMP-3.             
005800 01  SPAR-IDANSK                 PIC S9(3)   VALUE +0 COMP-3.             
005900 01  SPAR-IDINK                  PIC S9(3)   VALUE +0 COMP-3.             
006000 01  SPAR-IDPERSON-ANSV          PIC S9(3)   VALUE +0 COMP-3.             
006100 01  SPAR-ADLAGOMR               PIC 9(2)    VALUE ZERO.                  
006200*      --- VALID IDDC CODES                                               
006300*                                                                         
006400*01    -COPY WWDCKONS                                                     
006500*01    -COPY WWDC99                                                       
006600*01    -COPY WWDC99 -PRE STYR-                                            
006700       EJECT                                                              
006800                                                                          
006900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007000     88  KEYS-OK                             VALUE 'Y'.                   
007100     88  KEYS-WRONG                          VALUE 'N'.                   
007200                                                                          
007300 77  ALL-SW                      PIC X       VALUE 'Y'.                   
007400     88  ALL-OK                              VALUE 'Y'.                   
007500                                                                          
007600 77  ARTNR-SW                    PIC X       VALUE 'N'.                   
007700     88  ARTNR-NY                            VALUE 'Y'.                   
007800                                                                          
007900 77  LEVNR-SW                    PIC X       VALUE 'N'.                   
008000     88  LEVNR-NY                            VALUE 'Y'.                   
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  OWN-MID                             VALUE '9430'.                
008400     88  GOOD-MID                            VALUE '9430'.                
008500     88  HELP-MID                            VALUE '0551'.                
008600     EJECT                                                                
008700                                                                          
008800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008900 01  GENERAL-SUBPROGRAM.                                                  
009000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
009400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009500     EJECT                                                                
009600                                                                          
009700*01 -COPY WMSGINIT                                                        
009800     SKIP3                                                                
009900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010000*01 -COPY WMEDAREA                                                        
010100     SKIP3                                                                
010200 01  MESSAGE-CODES.                                                       
010300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010400     03  ERR-ART-MISSING         PIC X(3)    VALUE '017'.                 
010500     03  ERR-OMR-MISSING         PIC X(3)    VALUE '706'.                 
010600     EJECT                                                                
010700                                                                          
010800*01  -COPY W611STYR                                                       
010900     EJECT                                                                
011000                                                                          
011100*    --- AREAS FOR MFS AND SCREENHANDLING                                 
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011400     SKIP3                                                                
011500*01  MID -COPY W90430I1                                                   
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011800     SKIP3                                                                
011900*01  -COPY WMSGAREA                                                       
012000     EJECT                                                                
012100                                                                          
012200     03  MOD REDEFINES MSG-AREA.                                          
012300*      05  -COPY W90430O1                                                 
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012600     SKIP3                                                                
012700*01  -COPY WMFSAREA                                                       
012800     EJECT                                                                
012900                                                                          
013000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
013100*                                                                         
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013400     SKIP3                                                                
013500 01  KEYS-TO-DLI.                                                         
013600*    --- D601                                                             
013700     03  W-IDARTNR-X.                                                     
013800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013900*    --- D661                                                             
014000     03  W-KDCLAGER-X.                                                    
014100         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
014200     03  W-KDARBTYP-X.                                                    
014300         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
014400     03  W-W6GXKEY-X.                                                     
014500         05  FILLER              PIC X(4)    VALUE '6005'.                
014600         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
014700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
014800     03  W-W6GXKEY-DC11-X.                                                
014900         05  FILLER              PIC X(4)    VALUE '6005'.                
015000         05  FILLER              PIC X(2)    VALUE '11'.                  
015100         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
015200     03  W-ADINLOMR-X.                                                    
015300         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
015400     03  W-IDPERSON-X.                                                    
015500         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
015600     03  W-IDDC-X.                                                        
015700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015800     03  W-WDP3A1-MIN.                                                    
015900       05  W-IDLANDA1-MIN        PIC X(2)    VALUE SPACE.                 
016000       05  W-IDARTNRF-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
016100       05  W-IDARTNRT-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
016200       05  W-KDARBTYP-A-MIN      PIC X(8)    VALUE SPACE.                 
016300     03  W-WDP3A1-MAX.                                                    
016310       05  W-IDLANDA1-MAX        PIC X(2)    VALUE SPACE.                 
016400       05  W-IDARTNRF-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
016500       05  W-IDARTNRT-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
016600       05  W-KDARBTYP-A-MAX      PIC X(8)    VALUE SPACE.                 
016700     03  W-WDP3B1-X.                                                      
016710         05  W-IDLAND-B           PIC X(2)   VALUE SPACE.                 
016800         05  W-IDLEVNR-B          PIC X(5)   VALUE SPACE.                 
016900         05  W-KDARBTYP-B         PIC X(8)   VALUE SPACE.                 
017000     03  W-WDP3C1-MIN.                                                    
017010       05  W-IDLANDC1-MIN        PIC X(2)    VALUE SPACE.                 
017100       05  W-IDFKNGRPF-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
017200       05  W-IDFKNGRPT-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
017300       05  W-KDARBTYP-C-MIN      PIC X(8)    VALUE SPACE.                 
017400     03  W-WDP3C1-MAX.                                                    
017410       05  W-IDLANDC1-MAX        PIC X(2)    VALUE SPACE.                 
017500       05  W-IDFKNGRPF-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
017600       05  W-IDFKNGRPT-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
017700       05  W-KDARBTYP-C-MAX      PIC X(8)    VALUE SPACE.                 
017800     SKIP2                                                                
017900*                                                                         
018000*    --- STATUS-KOD FRÅN IMS                                              
018100 01  STATUS-WS                   PIC XX.                                  
018200     88  SEGMENT-FOUND                       VALUE '  '.                  
018300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018400     88  SEGMENT-MISSING                     VALUE 'GE'                   
018500                                                   'GB'.                  
018600     SKIP2                                                                
018700 01  GOOD-STATUSCODES.                                                    
018800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018900     SKIP3                                                                
019000*                                                                         
019100 01  SSA1                        PIC X(128).                              
019200 01  SSA2                        PIC X(64).                               
019300 01  SSA3                        PIC X(64).                               
019400     EJECT                                                                
019500*                                                                         
019600*    --- IMS FUNCTION CODES                                               
019700*01  -COPY W0003                                                          
019800     EJECT                                                                
019900*                                                                         
020000*    ---  DLI INPUT-OUTPUT AREA                                           
020100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020200     SKIP3                                                                
020300*                                                                         
020400 01  DLI-IO-AREA-PLAA.                                                    
020500     03  IO-AREA-PLAA            PIC X(150)  VALUE SPACE.                 
020600     SKIP3                                                                
020700     03  W6PLAA11 REDEFINES IO-AREA-PLAA.                                 
020800*        05  -COPY W6GX6006  -PRE PLAA-                                   
020900     SKIP3                                                                
021000 01  DLI-IO-AREA.                                                         
021100     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
021200     SKIP3                                                                
021300     03  WLARTC01 REDEFINES IO-AREA.                                      
021400*        05  -COPY WDK601  -PRE K601-                                     
021500     SKIP3                                                                
021600     03  WLARTC11 REDEFINES IO-AREA.                                      
021700*        05  -COPY WDK611  -PRE K611-                                     
021800     EJECT                                                                
021900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-ARTS11'.        
022000*                                                                         
022100 01  DLI-IO-AREA-ARTS11.                                                  
022200*    03  -COPY WDK711 -PRE ARTS-                                          
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-P311'.          
022500*                                                                         
022600 01  DLI-IO-AREA-P311.                                                    
022700*    03  -COPY WDP311                                                     
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP3A'.         
023000 01  DLI-IO-AREA-WDP3A.                                                   
023100*    03  -COPY WDP3A1                                                     
023200     EJECT                                                                
023300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP3B'.         
023400 01  DLI-IO-AREA-WDP3B.                                                   
023500*    03  -COPY WDP3B1                                                     
023600     EJECT                                                                
023700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP3C'.         
023800 01  DLI-IO-AREA-WDP3C.                                                   
023900*    03  -COPY WDP3C1                                                     
024000     EJECT                                                                
024100 LINKAGE SECTION.                                                         
024200                                                                          
024300*01  -COPY W0009   -PRE MSG-                                              
024400     EJECT                                                                
024500*01  -COPY W0008  -PRE USEA-                                              
024600     05  FILLER                  PIC X.                                   
024700     EJECT                                                                
024800*01  -COPY W0008  -PRE HANA-                                              
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025100*01  -COPY W0008  -PRE PLAA-                                              
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400*01  -COPY W0008  -PRE WDP3-                                              
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008  -PRE ARTC-                                              
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*01  -COPY W0008  -PRE ARTS-                                              
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008  -PRE WDP3A-                                             
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01  -COPY W0008  -PRE WDP3B-                                             
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008  -PRE WDP3C-                                             
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200 PROCEDURE DIVISION  USING MSG-PCB                                        
027300                           USEA-PCB                                       
027400                           HANA-PCB                                       
027500                           PLAA-PCB                                       
027600                           WDP3-PCB                                       
027700                           ARTC-PCB                                       
027800                           ARTS-PCB                                       
027900                           WDP3A-PCB                                      
028000                           WDP3B-PCB                                      
028100                           WDP3C-PCB.                                     
028200                                                                          
028300     ENTRY 'DLITCBL' USING MSG-PCB                                        
028400                           USEA-PCB                                       
028500                           HANA-PCB                                       
028600                           PLAA-PCB                                       
028700                           WDP3-PCB                                       
028800                           ARTC-PCB                                       
028900                           ARTS-PCB                                       
029000                           WDP3A-PCB                                      
029100                           WDP3B-PCB                                      
029200                           WDP3C-PCB.                                     
029300                                                                          
029400     PERFORM IMS-GET-MSG                                                  
029500     IF SEGMENT-FOUND                                                     
029600        PERFORM A-INIT                                                    
029700        PERFORM B-CHECK-KEYS                                              
029800        IF KEYS-OK                                                        
029900          IF W-IDARTNR > +0                                               
030000            PERFORM FA-READ-BASICDATA-IDARTNR                             
030100          ELSE                                                            
030200            PERFORM FB-READ-BASICDATA-IDLEVNR                             
030300          END-IF                                                          
030400          IF KEYS-OK                                                      
030500            PERFORM FC-READ-NAME                                          
030600          END-IF                                                          
030700        END-IF                                                            
030800        MOVE MAX-MOD-LENGTH TO MSG-KVLL                                   
030900        PERFORM IMS-INSERT-MSG                                            
031000     END-IF                                                               
031100                                                                          
031200     MOVE ZERO TO RETURN-CODE                                             
031300     GOBACK                                                               
031400     .                                                                    
031500     EJECT                                                                
031600 A-INIT SECTION.                                                          
031700                                                                          
031800     IF MSG-DOUBLE-TRANSACTIONS                                           
031900       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W90430I1                 
032000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
032100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032200     ELSE                                                                 
032300       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W90430I1                  
032400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
032500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032600     END-IF                                                               
032700                                                                          
032800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
033000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033100                                                                          
033200     MOVE LOW-VALUE TO MSG-AREA                                           
033300     MOVE 'W90430O1' TO MFS-IDMOD                                         
033400     MOVE '9430' TO MOD-IDTRANS                                           
033500     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
033600                                                                          
033700     IF OWN-MID OR HELP-MID                                               
033800        CONTINUE                                                          
033900     ELSE                                                                 
034000       MOVE SPACE TO MFS-KDTRTYP                                          
034100       MOVE '7' TO MFS-IDPFK                                              
034200     END-IF                                                               
034300                                                                          
034400     MOVE LOW-VALUE  TO W-WDP3A1-MIN                                      
034500                        W-WDP3C1-MIN                                      
034600     MOVE HIGH-VALUE TO W-WDP3A1-MAX                                      
034700                        W-WDP3C1-MAX                                      
034710     MOVE 'SE'       TO W-IDLANDA1-MIN                                    
034720                        W-IDLANDA1-MAX                                    
034730                        W-IDLAND-B                                        
034740                        W-IDLANDC1-MIN                                    
034750                        W-IDLANDC1-MAX                                    
034800     .                                                                    
034900     EJECT                                                                
035000*----------------------------------------------------------------*        
035100 B-CHECK-KEYS SECTION.                                                    
035200                                                                          
035300     MOVE YES TO KEYS-SW                                                  
035400                                                                          
035500*    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
035600*                            MOD-IDLEVNR-IN                               
035700*                            MOD-IDDC-IN                                  
035800                                                                          
035900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
036000     MOVE '001'             TO MSGI-KDCALL                                
036100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036200                                    MSGI-IDLTERM-USER                     
036300     MOVE '9430'                 TO MSGI-IDTRANS                          
036400                                                                          
036500     IF MFS-IDTRANS = '9430'                                              
036600*        MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                            
036700         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
036800         MOVE MID-IDDC-IN      TO MSGI-IDDC                               
036900     ELSE                                                                 
037000       IF MID-IDARTNR-IN NUMERIC                                          
037100       AND MID-IDARTNR-IN > ZERO                                          
037200         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
037300         MOVE SPACE          TO MSGI-IDLEVNR                              
037400       END-IF                                                             
037500       MOVE SPACE          TO MSGI-IDDC                                   
037600     END-IF                                                               
037700                                                                          
037800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037900                                                                          
038000     IF MSGI-IDLAND-SPR = 'GB'                                            
038100       MOVE +2 TO LANGUAGE-IX                                             
038200       MOVE 'GB ' TO MED-IDSKYLT                                          
038300     ELSE                                                                 
038400       MOVE +1 TO LANGUAGE-IX                                             
038500       MOVE 'S  ' TO MED-IDSKYLT                                          
038600     END-IF                                                               
038700                                                                          
038800     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
038900     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
039000                                                                          
039100     IF MFS-IDTRANS = '9430'                                              
039200       IF MID-IDARTNR-IN         = ALL '+'                                
039300           CONTINUE                                                       
039400        ELSE                                                              
039500           MOVE '7'              TO MFS-IDPFK                             
039600           MOVE YES              TO ARTNR-SW                              
039700           MOVE SPACE            TO MFS-KDTRTYP                           
039800       END-IF                                                             
039900     ELSE                                                                 
040000       IF WS-IDARTNR > ZERO                                               
040100           MOVE '7'              TO MFS-IDPFK                             
040200           MOVE YES              TO ARTNR-SW                              
040300           MOVE SPACE            TO MFS-KDTRTYP                           
040400       END-IF                                                             
040500     END-IF                                                               
040600                                                                          
040700     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
040800         MOVE WS-IDARTNR TO W-IDARTNR                                     
040900     END-IF                                                               
041000                                                                          
041100     MOVE MSGI-IDLEVNR   TO WS-IDLEVNR                                    
041200                                                                          
041300     IF MFS-IDTRANS = '9430'                                              
041400*      IF MID-IDLEVNR-IN         = ALL '+'                                
041500*          CONTINUE                                                       
041600*       ELSE                                                              
041700*          MOVE '7'              TO MFS-IDPFK                             
041800*          MOVE YES              TO LEVNR-SW                              
041900*          MOVE SPACE            TO MFS-KDTRTYP                           
042000*      END-IF                                                             
042100       CONTINUE                                                           
042200     ELSE                                                                 
042300       IF WS-IDLEVNR NOT = SPACE                                          
042400           MOVE '7'              TO MFS-IDPFK                             
042500           MOVE YES              TO LEVNR-SW                              
042600           MOVE SPACE            TO MFS-KDTRTYP                           
042700       END-IF                                                             
042800     END-IF                                                               
042900                                                                          
043000     IF WS-IDLEVNR NOT = SPACE                                            
043100       MOVE WS-IDLEVNR TO W-IDLEVNR                                       
043200       IF LEVNR-NY                                                        
043300         IF ARTNR-NY                                                      
043400           IF WS-IDARTNR NUMERIC AND WS-IDARTNR > 0                       
043500             MOVE SPACE      TO W-IDLEVNR                                 
043600                                WS-IDLEVNR                                
043700           ELSE                                                           
043800             MOVE ZERO TO W-IDARTNR                                       
043900                          WS-IDARTNR                                      
044000           END-IF                                                         
044100         ELSE                                                             
044200           MOVE ZERO TO W-IDARTNR                                         
044300                        WS-IDARTNR                                        
044400         END-IF                                                           
044500       ELSE                                                               
044600         IF ARTNR-NY                                                      
044700           MOVE SPACE        TO W-IDLEVNR                                 
044800                                WS-IDLEVNR                                
044900         ELSE                                                             
045000           IF WS-IDARTNR NUMERIC AND WS-IDARTNR > 0                       
045100             MOVE SPACE      TO W-IDLEVNR                                 
045200                                WS-IDLEVNR                                
045300           ELSE                                                           
045400             MOVE ZERO TO W-IDARTNR                                       
045500                          WS-IDARTNR                                      
045600           END-IF                                                         
045700         END-IF                                                           
045800       END-IF                                                             
045900     END-IF                                                               
046000                                                                          
046100     IF W-IDARTNR = +0 AND W-IDLEVNR = SPACE                              
046200       MOVE NEJ TO KEYS-SW                                                
046300     END-IF                                                               
046400                                                                          
046500     IF MID-IDDC-IN            = ALL '+'                                  
046600         MOVE MSGI-IDDC        TO WS-IDDC                                 
046700         INSPECT WS-IDDC    REPLACING LEADING SPACE BY ZERO               
046800      ELSE                                                                
046900         IF MFS-IDTRANS = '9430'                                          
047000           MOVE MID-IDDC-IN      TO WS-IDDC                               
047100         ELSE                                                             
047200           MOVE MSGI-IDDC      TO WS-IDDC                                 
047300         END-IF                                                           
047400         INSPECT WS-IDDC    REPLACING LEADING SPACE BY ZERO               
047500         MOVE '7'              TO MFS-IDPFK                               
047600         MOVE SPACE            TO MFS-KDTRTYP                             
047700     END-IF                                                               
047800                                                                          
047900     IF GOOD-DC                                                           
048000         IF SDC                                                           
048100           MOVE WC-CDC-SE TO WS-IDDC                                      
048200         END-IF                                                           
048300         MOVE WS-IDDC    TO W-6005-IDDC                                   
048400                            W-IDDC                                        
048500     ELSE                                                                 
048600         MOVE NEJ TO KEYS-SW                                              
048700     END-IF                                                               
048800                                                                          
048900     IF CDC OR NDC                                                        
049000        CONTINUE                                                          
049100     ELSE                                                                 
049200        MOVE NEJ TO KEYS-SW                                               
049300     END-IF                                                               
049400                                                                          
049500*    MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
049600*    INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
049700*    MOVE WS-IDLEVNR       TO MOD-IDLEVNR-UT                              
049800*    MOVE WS-IDDC          TO MOD-IDDC-UT                                 
049900*    INSPECT MOD-IDDC-UT    REPLACING LEADING ZERO BY SPACE               
050000                                                                          
050100     IF KEYS-WRONG                                                        
050200       IF GOOD-MID                                                        
050300*-FEL - 401                                                               
050400         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
050500         CALL WMEDKONV USING MED-WMEDAREA                                 
050600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
050700         PERFORM MFS-ERASE-FIELD-OUT                                      
050800*        PERFORM MFS-ERASE-FIELD-IN                                       
050900       ELSE                                                               
051000         PERFORM MFS-ERASE-FIELD-OUT                                      
051100*        PERFORM MFS-ERASE-FIELD-IN                                       
051200       END-IF                                                             
051300     END-IF                                                               
051400     .                                                                    
051500     EJECT                                                                
051600*----------------------------------------------------------------*        
051700 FA-READ-BASICDATA-IDARTNR SECTION.                                       
051800                                                                          
051900     MOVE +1               TO W-KDCLAGER                                  
052000     MOVE ZERO            TO STYR-IDARTNR                                 
052100                             STYR-IDFKNGRP                                
052200                             STYR-BEFT                                    
052300     MOVE W-6005-IDDC     TO STYR-IDDC                                    
052400                                                                          
052500     PERFORM IMS-GU-ARTC-K601                                             
052600     IF NDC                                                               
052700        PERFORM IMS-GU-ARTS-K711                                          
052800     END-IF                                                               
052900                                                                          
053000     IF SEGMENT-FOUND                                                     
053100        MOVE K601-ART-IDARTNR  TO STYR-IDARTNR                            
053200        MOVE K601-ART-IDFKNGRP TO STYR-IDFKNGRP                           
053300                                  SPAR-IDFKNGRP                           
053400        MOVE K601-ART-IDLEVNR  TO STYR-IDLEVNR                            
053500                                  SPAR-IDLEVNR                            
053600                                                                          
053700        PERFORM IMS-GNP-ARTC-K611                                         
053800                                                                          
053900        IF SEGMENT-FOUND                                                  
054000          MOVE K611-CLAG-IDBERED   TO SPAR-IDBERED                        
054100          MOVE K611-CLAG-IDANSK    TO SPAR-IDANSK                         
054200          MOVE K611-CLAG-IDINK     TO SPAR-IDINK                          
054300          MOVE K611-CLAG-BEFT      TO STYR-BEFT                           
054400          IF W-6005-IDDC     =  ZERO                                      
054500             MOVE WS-IDDC TO W-6005-IDDC                                  
054600*                            MOD-IDDC-UT                                  
054700          END-IF                                                          
054800                                                                          
054900          IF CDC                                                          
055000*           MOVE K611-CLAG-ADLAGOMR TO MOD-ADLAGOMR                       
055100            MOVE K611-CLAG-ADLAGOMR TO SPAR-ADLAGOMR                      
055200          ELSE                                                            
055300*           MOVE ARTS-SLAG-ADLAGOMR TO MOD-ADLAGOMR                       
055400            MOVE ARTS-SLAG-ADLAGOMR TO SPAR-ADLAGOMR                      
055500          END-IF                                                          
055600        END-IF                                                            
055700        IF W-IDLEVNR NOT = SPACE                                          
055800          MOVE W-IDLEVNR TO STYR-IDLEVNR                                  
055900        END-IF                                                            
056000                                                                          
056100        MOVE W-6005-IDDC     TO STYR-IDDC                                 
056200*-------CALL TILL SUBPROGRAM                                              
056300        CALL W611STYR USING STYR-W611STYR                                 
056400                            HANA-PCB PLAA-PCB                             
056500                                                                          
056600        IF STYR-KDSVAR-FEL                                                
056700*-FEL - 706                                                               
056800           MOVE ERR-OMR-MISSING    TO MED-IDMFSFEL                        
056900           CALL WMEDKONV USING     MED-WMEDAREA                           
057000           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
057100           PERFORM MFS-ERASE-FIELD-OUT                                    
057200           MOVE NEJ TO KEYS-SW                                            
057300        ELSE                                                              
057400*          MOVE STYR-ADINLOMR-FB   TO MOD-ADINLOMR-FB                     
057500*          MOVE STYR-ADINLOMR-FP   TO MOD-ADINLOMR-FP                     
057600           MOVE W-6005-IDDC        TO STYR-WS-IDDC                        
057700           IF (STYR-ADINLOMR-FB = 'INS?' OR 'FB? ') AND                   
057800              (STYR-CDC)                                                  
057900             IF W-IDLEVNR NOT = SPACE                                     
058000               MOVE W-IDLEVNR TO STYR-IDLEVNR                             
058100             END-IF                                                       
058200                                                                          
058300             IF STYR-CDC-SE                                               
058400               MOVE WC-CDC-TR   TO STYR-IDDC                              
058500             ELSE                                                         
058600               MOVE WC-CDC-SE   TO STYR-IDDC                              
058700             END-IF                                                       
058800*-------CALL TILL SUBPROGRAM                                              
058900             CALL W611STYR USING STYR-W611STYR                            
059000                                 HANA-PCB PLAA-PCB                        
059100             IF STYR-KDSVAR-FEL                                           
059200*-FEL - 706                                                               
059300                MOVE ERR-OMR-MISSING    TO MED-IDMFSFEL                   
059400                CALL WMEDKONV USING     MED-WMEDAREA                      
059500                MOVE MED-MFSFEL         TO MOD-TEMFSFEL                   
059600                PERFORM MFS-ERASE-FIELD-OUT                               
059700                MOVE NEJ TO KEYS-SW                                       
059800             ELSE                                                         
059900                IF STYR-ADINLOMR-FB = 'INS?' OR 'FB? '                    
060000                  MOVE W-6005-IDDC        TO STYR-IDDC                    
060100*                 MOVE MOD-ADINLOMR-FB    TO STYR-ADINLOMR-FB             
060200                ELSE                                                      
060300*                 MOVE STYR-ADINLOMR-FB   TO MOD-ADINLOMR-FB              
060400                  MOVE STYR-IDDC          TO W-6005-IDDC                  
060500                END-IF                                                    
060600             END-IF                                                       
060700          END-IF                                                          
060800        END-IF                                                            
060900                                                                          
061000     ELSE                                                                 
061100        IF SEGMENT-MISSING                                                
061200*-FEL - 017                                                               
061300           MOVE ERR-ART-MISSING    TO MED-IDMFSFEL                        
061400           CALL WMEDKONV USING     MED-WMEDAREA                           
061500           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
061600           PERFORM MFS-ERASE-FIELD-OUT                                    
061700           MOVE NEJ TO KEYS-SW                                            
061800        END-IF                                                            
061900     END-IF                                                               
062000*    PERFORM MFS-ERASE-FIELD-IN                                           
062100     .                                                                    
062200     EJECT                                                                
062300 FB-READ-BASICDATA-IDLEVNR SECTION.                                       
062400                                                                          
062500     MOVE ZERO            TO STYR-IDARTNR                                 
062600                             STYR-IDFKNGRP                                
062700                             STYR-BEFT                                    
062800     MOVE W-IDLEVNR       TO STYR-IDLEVNR                                 
062900     MOVE W-6005-IDDC     TO STYR-IDDC                                    
063000                                                                          
063100*----CALL TILL SUBPROGRAM                                                 
063200     CALL W611STYR USING STYR-W611STYR                                    
063300                         HANA-PCB PLAA-PCB                                
063400                                                                          
063500     IF STYR-KDSVAR-FEL                                                   
063600*-FEL - 706                                                               
063700        MOVE ERR-OMR-MISSING    TO MED-IDMFSFEL                           
063800        CALL WMEDKONV USING     MED-WMEDAREA                              
063900        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
064000        PERFORM MFS-ERASE-FIELD-OUT                                       
064100        MOVE NEJ TO KEYS-SW                                               
064200     ELSE                                                                 
064300*       MOVE STYR-ADINLOMR-FB   TO MOD-ADINLOMR-FB                        
064400        MOVE W-6005-IDDC        TO STYR-WS-IDDC                           
064500        IF (STYR-ADINLOMR-FB = 'FB? ' OR 'INS?') AND                      
064600           (STYR-CDC)                                                     
064700           MOVE ZERO            TO STYR-IDARTNR                           
064800                                   STYR-IDFKNGRP                          
064900                                   STYR-BEFT                              
065000           MOVE W-IDLEVNR       TO STYR-IDLEVNR                           
065100           IF STYR-CDC-SE                                                 
065200             MOVE WC-CDC-TR TO STYR-IDDC                                  
065300           ELSE                                                           
065400             MOVE WC-CDC-SE TO STYR-IDDC                                  
065500           END-IF                                                         
065600                                                                          
065700*----CALL TILL SUBPROGRAM                                                 
065800           CALL W611STYR USING STYR-W611STYR                              
065900                               HANA-PCB PLAA-PCB                          
066000                                                                          
066100           IF STYR-KDSVAR-FEL                                             
066200*-FEL - 706                                                               
066300              MOVE ERR-OMR-MISSING    TO MED-IDMFSFEL                     
066400              CALL WMEDKONV USING     MED-WMEDAREA                        
066500              MOVE MED-MFSFEL         TO MOD-TEMFSFEL                     
066600              PERFORM MFS-ERASE-FIELD-OUT                                 
066700              MOVE NEJ TO KEYS-SW                                         
066800           ELSE                                                           
066900              IF STYR-ADINLOMR-FB = 'FB? ' OR 'INS?'                      
067000                MOVE W-6005-IDDC        TO STYR-IDDC                      
067100*               MOVE MOD-ADINLOMR-FB    TO STYR-ADINLOMR-FB               
067200              ELSE                                                        
067300*               MOVE STYR-ADINLOMR-FB   TO MOD-ADINLOMR-FB                
067400                MOVE STYR-IDDC          TO W-6005-IDDC                    
067500              END-IF                                                      
067600           END-IF                                                         
067700        END-IF                                                            
067800     END-IF                                                               
067900                                                                          
068000*    MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-FP                              
068100*                            MOD-ADLAGOMR                                 
068200     .                                                                    
068300     EJECT                                                                
068400 FC-READ-NAME SECTION.                                                    
068500                                                                          
068600     IF SPAR-IDANSK > +0                                                  
068700       MOVE 'ANSK    '  TO W-KDARBTYP                                     
068800       MOVE SPAR-IDANSK TO W-IDPERSON                                     
068900       PERFORM IMS-GU-P311                                                
069000       IF SEGMENT-FOUND                                                   
069100         MOVE PERS-IDNAMN       TO MOD-IDNAMN-ANSK                        
069200*        MOVE PERS-IDTFN        TO MOD-IDTFN-ANSK                         
069300       ELSE                                                               
069400         MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-ANSK                        
069500*                                  MOD-IDTFN-ANSK                         
069600       END-IF                                                             
069700     ELSE                                                                 
069800       MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-ANSK                          
069900*                                MOD-IDTFN-ANSK                           
070000     END-IF                                                               
070100                                                                          
070200     IF SPAR-IDINK  > +0                                                  
070300       MOVE 'INK     '  TO W-KDARBTYP                                     
070400       MOVE SPAR-IDINK  TO W-IDPERSON                                     
070500       PERFORM IMS-GU-P311                                                
070600       IF SEGMENT-FOUND                                                   
070700         MOVE PERS-IDNAMN       TO MOD-IDNAMN-INK                         
070800*        MOVE PERS-IDTFN        TO MOD-IDTFN-INK                          
070900       ELSE                                                               
071000         MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-INK                         
071100*                                  MOD-IDTFN-INK                          
071200       END-IF                                                             
071300     ELSE                                                                 
071400       MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-INK                           
071500*                                MOD-IDTFN-INK                            
071600     END-IF                                                               
071700                                                                          
071800     IF SPAR-IDBERED > +0                                                 
071900       MOVE 'BER     '   TO W-KDARBTYP                                    
072000       MOVE SPAR-IDBERED TO W-IDPERSON                                    
072100       PERFORM IMS-GU-P311                                                
072200       IF SEGMENT-FOUND                                                   
072300         MOVE PERS-IDNAMN       TO MOD-IDNAMN-BEREDARE                    
072400*        MOVE PERS-IDTFN        TO MOD-IDTFN-BEREDARE                     
072500       ELSE                                                               
072600         MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-BEREDARE                    
072700*                                  MOD-IDTFN-BEREDARE                     
072800       END-IF                                                             
072900     ELSE                                                                 
073000       MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-BEREDARE                      
073100*                                MOD-IDTFN-BEREDARE                       
073200     END-IF                                                               
073300                                                                          
073400     MOVE STYR-ADINLOMR-FB   TO W-ADINLOMR                                
073500     PERFORM IMS-GU-PLAA11                                                
073600     IF SEGMENT-FOUND                                                     
073700       MOVE PLAA-6006-IDPERSON-ANSV  TO SPAR-IDPERSON-ANSV                
073800     ELSE                                                                 
073900       MOVE +0                       TO SPAR-IDPERSON-ANSV                
074000     END-IF                                                               
074100                                                                          
074200     IF SPAR-IDPERSON-ANSV  > +0                                          
074300       MOVE 'CDC     '                 TO W-KDARBTYP                      
074400       MOVE SPAR-IDPERSON-ANSV         TO W-IDPERSON                      
074500       PERFORM IMS-GU-P311                                                
074600*      IF SEGMENT-FOUND                                                   
074700*        MOVE PERS-IDNAMN       TO MOD-IDNAMN-INLEV                       
074800*        MOVE PERS-IDTFN        TO MOD-IDTFN-INLEV                        
074900*      ELSE                                                               
075000*        MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-INLEV                       
075100*                                  MOD-IDTFN-INLEV                        
075200*      END-IF                                                             
075300     ELSE                                                                 
075400*      MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-INLEV                         
075500*                                MOD-IDTFN-INLEV                          
075600       CONTINUE                                                           
075700     END-IF                                                               
075800                                                                          
075900     MOVE 'QUAL' TO W-KDARBTYP                                            
076000                    W-KDARBTYP-B                                          
076100     IF W-IDARTNR > ZERO                                                  
076200        PERFORM FCA-SOEK-IDARTNR                                          
076300     ELSE                                                                 
076400        PERFORM FCB-SOEK-IDLEVNR                                          
076500     END-IF                                                               
076600     IF SW-TRAEFF = 'J'                                                   
076700       MOVE WS-IDPERSON         TO W-IDPERSON                             
076800       PERFORM IMS-GU-P311                                                
076900       IF SEGMENT-FOUND                                                   
077000         MOVE PERS-IDNAMN       TO MOD-IDNAMN-KVAL                        
077100*        MOVE PERS-IDTFN        TO MOD-IDTFN-KVAL                         
077200       ELSE                                                               
077300         MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-KVAL                        
077400*                                  MOD-IDTFN-KVAL                         
077500       END-IF                                                             
077600     ELSE                                                                 
077700       MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-KVAL                          
077800*                                MOD-IDTFN-KVAL                           
077900     END-IF                                                               
078000                                                                          
078100     MOVE 'CDC' TO W-KDARBTYP                                             
078200                   W-KDARBTYP-B                                           
078300     IF W-IDARTNR > ZERO                                                  
078400        PERFORM FCA-SOEK-IDARTNR                                          
078500     ELSE                                                                 
078600        PERFORM FCB-SOEK-IDLEVNR                                          
078700     END-IF                                                               
078800     IF SW-TRAEFF = 'J'                                                   
078900       MOVE WS-IDPERSON         TO W-IDPERSON                             
079000       PERFORM IMS-GU-P311                                                
079100       IF SEGMENT-FOUND                                                   
079200         MOVE PERS-IDNAMN       TO MOD-IDNAMN-FORP                        
079300*        MOVE PERS-IDTFN        TO MOD-IDTFN-FORP                         
079400       ELSE                                                               
079500         MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-FORP                        
079600*                                  MOD-IDTFN-FORP                         
079700       END-IF                                                             
079800     ELSE                                                                 
079900       MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-FORP                          
080000*                                MOD-IDTFN-FORP                           
080100     END-IF                                                               
080200                                                                          
080300     IF SPAR-ADLAGOMR > +0                                                
080400       MOVE SPAR-ADLAGOMR    TO W-ADINLOMR                                
080500       PERFORM IMS-GU-PLAA11                                              
080600       IF SEGMENT-FOUND                                                   
080700         MOVE 'CDC     '                 TO W-KDARBTYP                    
080800         MOVE PLAA-6006-IDPERSON-ANSV    TO W-IDPERSON                    
080900         PERFORM IMS-GU-P311                                              
081000*        IF SEGMENT-FOUND                                                 
081100*          MOVE PERS-IDNAMN       TO MOD-IDNAMN-LO                        
081200*          MOVE PERS-IDTFN        TO MOD-IDTFN-LO                         
081300*        ELSE                                                             
081400*          MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-LO                        
081500*                                    MOD-IDTFN-LO                         
081600*        END-IF                                                           
081700         CONTINUE                                                         
081800       ELSE                                                               
081900*        MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-LO                          
082000*                                  MOD-IDTFN-LO                           
082100         CONTINUE                                                         
082200       END-IF                                                             
082300     ELSE                                                                 
082400*      MOVE MFS-RENSA-FAELT   TO MOD-IDNAMN-LO                            
082500*                                MOD-IDTFN-LO                             
082600       CONTINUE                                                           
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000 FCA-SOEK-IDARTNR SECTION.                                                
083100                                                                          
083200     MOVE NEJ TO SW-TRAEFF                                                
083300     PERFORM IMS-GU-WDP3A                                                 
083400     PERFORM UNTIL SEGMENT-MISSING OR SW-TRAEFF = 'J'                     
083500        IF SEQA-IDARTNR-TOM < W-IDARTNR                                   
083600           PERFORM IMS-GN-WDP3A                                           
083700        ELSE                                                              
083800           IF SEQA-IDARTNR-FOM <= W-IDARTNR                               
083900           AND SEQA-IDARTNR-TOM >= W-IDARTNR                              
084000               MOVE 'J' TO SW-TRAEFF                                      
084100           ELSE                                                           
084200              MOVE 'GE' TO STATUS-WS                                      
084300           END-IF                                                         
084400        END-IF                                                            
084500     END-PERFORM                                                          
084600     IF SW-TRAEFF = 'J'                                                   
084700        MOVE SEQA-IDPERSON TO WS-IDPERSON                                 
084800     ELSE                                                                 
084900        MOVE SPAR-IDLEVNR    TO W-IDLEVNR-B                               
085000        PERFORM IMS-GU-WDP3B                                              
085100        IF SEGMENT-FOUND                                                  
085200           MOVE 'J'          TO SW-TRAEFF                                 
085300           MOVE SEQB-IDPERSON                                             
085400                             TO WS-IDPERSON                               
085500        ELSE                                                              
085600           PERFORM IMS-GU-WDP3C                                           
085700           PERFORM UNTIL SEGMENT-MISSING OR SW-TRAEFF = 'J'               
085800              IF SEQC-IDFKNGRP-TOM < SPAR-IDFKNGRP                        
085900                 PERFORM IMS-GN-WDP3C                                     
086000              ELSE                                                        
086100                 IF SEQC-IDFKNGRP-FOM <= SPAR-IDFKNGRP                    
086200                 AND SEQC-IDFKNGRP-TOM >= SPAR-IDFKNGRP                   
086300                    MOVE 'J' TO SW-TRAEFF                                 
086400                 ELSE                                                     
086500                    MOVE 'GE' TO STATUS-WS                                
086600                 END-IF                                                   
086700              END-IF                                                      
086800           END-PERFORM                                                    
086900           IF SW-TRAEFF = 'J'                                             
087000              MOVE SEQC-IDPERSON TO WS-IDPERSON                           
087100           END-IF                                                         
087200        END-IF                                                            
087300     END-IF                                                               
087400     .                                                                    
087500     EJECT                                                                
087600 FCB-SOEK-IDLEVNR SECTION.                                                
087700                                                                          
087800     MOVE NEJ TO SW-TRAEFF                                                
087900     MOVE WS-IDLEVNR       TO W-IDLEVNR-B                                 
088000     PERFORM IMS-GU-WDP3B                                                 
088100     IF SEGMENT-FOUND                                                     
088200        MOVE 'J'             TO SW-TRAEFF                                 
088300        MOVE SEQB-IDPERSON   TO WS-IDPERSON                               
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700*----------------------------------------------------------------*        
088800 MFS-ERASE-FIELD-OUT SECTION.                                             
088900                                                                          
089000*    --- ALLA UTDATA-FÄLT                                                 
089100     MOVE MFS-RENSA-FAELT         TO                                      
089200*                                    MOD-ADINLOMR-FB                      
089300*                                    MOD-ADINLOMR-FP                      
089400*                                    MOD-ADLAGOMR                         
089500*                                    MOD-IDNAMN-INLEV                     
089600*                                    MOD-IDTFN-INLEV                      
089700*                                    MOD-IDNAMN-LO                        
089800*                                    MOD-IDTFN-LO                         
089900                                     MOD-IDNAMN-ANSK                      
090000*                                    MOD-IDTFN-ANSK                       
090100                                     MOD-IDNAMN-BEREDARE                  
090200*                                    MOD-IDTFN-BEREDARE                   
090300                                     MOD-IDNAMN-FORP                      
090400*                                    MOD-IDTFN-FORP                       
090500                                     MOD-IDNAMN-KVAL                      
090600*                                    MOD-IDTFN-KVAL                       
090700                                     MOD-IDNAMN-INK                       
090800*                                    MOD-IDTFN-INK                        
090900     .                                                                    
091000     EJECT                                                                
091100*----------------------------------------------------------------*        
091200*MFS-ERASE-FIELD-IN SECTION.                                              
091300*                                                                         
091400*    --- ALLA INDATA-FÄLT                                                 
091500*    MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR-IN                       
091600*                                    MOD-IDLEVNR-IN                       
091700*                                    MOD-IDDC-IN                          
091800*    .                                                                    
091900*    EJECT                                                                
092000*----------------------------------------------------------------*        
092100* --- IMS SECTIONS ---                                                    
092200*----------------------------------------------------------------*        
092300 IMS-GET-MSG SECTION.                                                     
092400     MOVE '  QC' TO GOOD-STATUSCODES                                      
092500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
092600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092700     PERFORM IMS-STATUSCHECK                                              
092800     .                                                                    
092900     SKIP2                                                                
093000*                                                                         
093100 IMS-INSERT-MSG SECTION.                                                  
093200*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
093300*      MOVE '0' TO MFS-KDHUVOMR                                           
093400*    END-IF                                                               
093500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
093600     MOVE SPACE TO GOOD-STATUSCODES                                       
093700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
093800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093900     PERFORM IMS-STATUSCHECK                                              
094000     .                                                                    
094100     SKIP2                                                                
094200*                                                                         
094300 IMS-GU-ARTC-K601 SECTION.                                                
094400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
094500          DELIMITED BY SIZE INTO SSA1                                     
094600     MOVE '  GE' TO GOOD-STATUSCODES                                      
094700     CALL CBLTDLI USING GU  ARTC-PCB                                      
094800                            DLI-IO-AREA                                   
094900                            SSA1                                          
095000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
095100     PERFORM IMS-STATUSCHECK                                              
095200     .                                                                    
095300     SKIP2                                                                
095400*                                                                         
095500 IMS-GNP-ARTC-K611 SECTION.                                               
095600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
095700          DELIMITED BY SIZE INTO SSA1                                     
095800     MOVE 'WLARTC11' TO SSA2                                              
095900     MOVE '  GE' TO GOOD-STATUSCODES                                      
096000     CALL CBLTDLI USING GNP ARTC-PCB                                      
096100                            DLI-IO-AREA                                   
096200                            SSA1                                          
096300                            SSA2                                          
096400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
096500     PERFORM IMS-STATUSCHECK                                              
096600     .                                                                    
096700     SKIP2                                                                
096800*                                                                         
096900 IMS-GU-ARTS-K711 SECTION.                                                
097000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
097100          DELIMITED BY SIZE INTO SSA1                                     
097200     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
097300          DELIMITED BY SIZE INTO SSA2                                     
097400     MOVE '  GE' TO GOOD-STATUSCODES                                      
097500     CALL CBLTDLI USING GU  ARTS-PCB                                      
097600                            DLI-IO-AREA-ARTS11                            
097700                            SSA1 SSA2                                     
097800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSCHECK                                              
098000     .                                                                    
098100     SKIP2                                                                
098200*                                                                         
098300 IMS-GU-P311      SECTION.                                                
098400     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
098500          DELIMITED BY SIZE INTO SSA1                                     
098600     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
098700          DELIMITED BY SIZE INTO SSA2                                     
098800     MOVE '  GE' TO GOOD-STATUSCODES                                      
098900     CALL CBLTDLI USING GU  WDP3-PCB                                      
099000                            DLI-IO-AREA-P311                              
099100                            SSA1 SSA2                                     
099200     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
099300     PERFORM IMS-STATUSCHECK                                              
099400     .                                                                    
099500     SKIP2                                                                
099600*                                                                         
099700 IMS-GU-PLAA11    SECTION.                                                
099800     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-X ')'                         
099900          DELIMITED BY SIZE INTO SSA1                                     
100000     STRING 'W6PLAA11(ADINLOMR =' W-ADINLOMR-X ')'                        
100100          DELIMITED BY SIZE INTO SSA2                                     
100200     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
100300     CALL CBLTDLI USING GU  PLAA-PCB                                      
100400                            DLI-IO-AREA-PLAA                              
100500                            SSA1 SSA2                                     
100600     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
100700     PERFORM IMS-STATUSCHECK                                              
100800     .                                                                    
100900     SKIP2                                                                
101000*                                                                         
101100 IMS-GU-WDP3A SECTION.                                                    
101200     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
101300                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
101400                    '&KDARBTYP =' W-KDARBTYP ')'                          
101500            DELIMITED BY SIZE INTO SSA1                                   
101600     MOVE '  GE' TO GOOD-STATUSCODES                                      
101700     CALL CBLTDLI USING GU WDP3A-PCB DLI-IO-AREA-WDP3A SSA1               
101800     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
101900     PERFORM IMS-STATUSCHECK                                              
102000     .                                                                    
102100     SKIP2                                                                
102200*                                                                         
102300 IMS-GN-WDP3A SECTION.                                                    
102400     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
102500                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
102600                    '&KDARBTYP =' W-KDARBTYP ')'                          
102700            DELIMITED BY SIZE INTO SSA1                                   
102800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
102900     CALL CBLTDLI USING GN WDP3A-PCB DLI-IO-AREA-WDP3A SSA1               
103000     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
103100     PERFORM IMS-STATUSCHECK                                              
103200     .                                                                    
103300     SKIP2                                                                
103400*                                                                         
103500 IMS-GU-WDP3B SECTION.                                                    
103600     STRING 'WDP3B1  (WDP3B1KY =' W-WDP3B1-X                              
103700                    '&KDARBTYP =' W-KDARBTYP ')'                          
103800            DELIMITED BY SIZE INTO SSA1                                   
103900     MOVE '  GE' TO GOOD-STATUSCODES                                      
104000     CALL CBLTDLI USING GU WDP3B-PCB DLI-IO-AREA-WDP3B SSA1               
104100     MOVE WDP3B-STATUS-CODE TO STATUS-WS                                  
104200     PERFORM IMS-STATUSCHECK                                              
104300     .                                                                    
104400     SKIP2                                                                
104500*                                                                         
104600 IMS-GU-WDP3C SECTION.                                                    
104700     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
104800                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
104900                    '&KDARBTYP =' W-KDARBTYP ')'                          
105000            DELIMITED BY SIZE INTO SSA1                                   
105100     MOVE '  GE' TO GOOD-STATUSCODES                                      
105200     CALL CBLTDLI USING GU WDP3C-PCB DLI-IO-AREA-WDP3C SSA1               
105300     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
105400     PERFORM IMS-STATUSCHECK                                              
105500     .                                                                    
105600     SKIP2                                                                
105700*                                                                         
105800 IMS-GN-WDP3C SECTION.                                                    
105900     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
106000                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
106100                    '&KDARBTYP =' W-KDARBTYP ')'                          
106200            DELIMITED BY SIZE INTO SSA1                                   
106300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
106400     CALL CBLTDLI USING GN WDP3C-PCB DLI-IO-AREA-WDP3C SSA1               
106500     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
106600     PERFORM IMS-STATUSCHECK                                              
106700     .                                                                    
106800     SKIP2                                                                
106900*                                                                         
107000 IMS-STATUSCHECK SECTION.                                                 
107100     SET STATUS-IX TO 1                                                   
107200     SEARCH GOOD-STATUS                                                   
107300       AT END                                                             
107400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
107500         DELIMITED BY SIZE INTO ERROR-TEXT                                
107600         CALL FELLOG                                                      
107700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
107800         CONTINUE                                                         
107900     END-SEARCH                                                           
108000     .                                                                    
