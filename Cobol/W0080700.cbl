000300     SKIP3                                                                
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W0080700.                                                
001000*AUTHOR.         ODD OLSEN.                                               
001100*DATE-WRITTEN.   AUG 84.                                                  
001200                                                                          
001400                                                                          
001500*    FUNKTION.                                                            
001600*       FRÅGE OCH UPPDATERINGSPROGRAM                                     
001700*       FÖR EMBALLAGETYPSTEXTER PÅ WDGX-47381                             
001800                                                                          
001900*       REGLER FÖR UPPDATERING:                                           
002000*       -LÄSNING SKER ALLTID FÖRE ÄNDRING OCH BORTTAG                     
002100*       -NYUPPLÄGG AV ROT OCH SEGMENT ÄR MÖJLIGT.                         
002200*       -BORTTAG AV ROT OCH SEGMENT ÄR MÖJLIGT.                           
002300                                                                          
002400                                                                          
002500                                                                          
002600*    INDATA.                                                              
002700*        TRANSAKTION: W0T807                                              
002800*                     W0T807U                                             
002900                                                                          
003000*        MID:         W0I80701                                            
003100                                                                          
003200*    UTDATA.                                                              
003300*        MOD:         W0O80701                                            
003400     EJECT                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP3                                                                
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W0080700'.            
004700 77    JA                        PIC X       VALUE 'J'.                   
004800 77    NEJ                       PIC X       VALUE 'N'.                   
004900 77    WS-KDEMBTYP               PIC X(3).                                
005000 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +0   COMP SYNC.        
005100 77    INDX                      PIC S9(9)   COMP SYNC.                   
005200 77    MAX-INDX                  PIC S9(9)   VALUE +6   COMP SYNC.        
005300 77    WS-IDTRANS                PIC X(4).                                
005400       88  WS-GODKAEND-BILD                  VALUE '0807'.                
005500 77    WS-KDUPPD                 PIC X.                                   
005600       88  AENDRA                            VALUE 'Ä' 'C'.               
005700       88  NY                                VALUE 'N'.                   
005800       88  BORTTAG                           VALUE 'B' 'D'.               
005900*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -        
006000     EJECT                                                                
006010                                                                          
006020 01  DYNAMISKA-SUBPROGRAM.                                                
006030   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006040   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006050                                                                          
006100 01    NYCKLAR-TILL-DLI.                                                  
006200   03    W-KDEMBTYP-4738-X.                                               
006300     05    IDHTYP                 PIC X(4)    VALUE '4738'.               
006400     05    W-KDEMBTYP-4738        PIC S9(3)   COMP-3 VALUE ZERO.          
006500     05    FILLER                 PIC X(24)   VALUE LOW-VALUE.            
006600     SKIP3                                                                
006700 01    MEDDELANDE.                                                        
006800   03    FEL-1                   PIC X(10).                               
006900   03    FEL-2                   PIC X(25).                               
007000   03    FEL-4                   PIC X(34).                               
007100   03    FEL-5                   PIC X(37).                               
007200   03    FEL-6                   PIC X(36).                               
007300   03    MED-1                   PIC X(29).                               
007400   03    MED-2                   PIC X(23).                               
007500   03    MED-3                   PIC X(25).                               
007600     EJECT                                                                
007700                                                                          
007800 01    SVENSKA-MEDDELANDE.                                                
007900   03    FILLER                  PIC X(10)   VALUE                        
008000             'FEL NYCKEL'.                                                
008100   03    FILLER                  PIC X(25)   VALUE                        
008200             'INMATNINGSFÄLT FEL IFYLLT'.                                 
008300   03    FILLER                  PIC X(34)  VALUE                         
008400             'EMBALLAGETYP FINNS EJ REGISTRERAD'.                         
008500   03    FILLER                  PIC X(37)  VALUE                         
008600             'EMBALLAGETYP FINNS REDAN REGISTRERAD'.                      
008700   03    FILLER                  PIC X(36)  VALUE                         
008800             'EMBALLAGETYPTEXT EJ IFYLLD         '.                       
008900   03    FILLER                  PIC X(30)  VALUE                         
009000             'EMBALLAGETYPTEXT UPPDATERAD '.                              
009100   03    FILLER                  PIC X(23)  VALUE                         
009200             'EMBALLAGETYP BORTTAGEN'.                                    
009300   03    FILLER                  PIC X(25)  VALUE                         
009400             'EMBALLAGETYP REGISTRERAD'.                                  
009500     EJECT                                                                
009600 01    ENGELSKA-MEDDELANDE.                                               
009700   03    FILLER                  PIC X(10)   VALUE                        
009800             'WRONG KEY'.                                                 
009900   03    FILLER                  PIC X(25)   VALUE                        
010000             'INPUT FIELD NOT CORRECT'.                                   
010100   03    FILLER                  PIC X(34)  VALUE                         
010200             'PACK.MAT. TYPE NOT REGISTERED    '.                         
010300   03    FILLER                  PIC X(37)  VALUE                         
010400             'PACK.MAT. TYPE ALREADY REGISTERED   '.                      
010500   03    FILLER                  PIC X(36)  VALUE                         
010600             'PACK.MAT. TYPE TEXT NOT REGISTERED '.                       
010700   03    FILLER                  PIC X(30)  VALUE                         
010800             'PACK.MAT. TYPE TEXT REGISTERED'.                            
010900   03    FILLER                  PIC X(23)  VALUE                         
011000             'PACK.MAT. TYPE DELETED'.                                    
011100   03    FILLER                  PIC X(25)  VALUE                         
011200             'PACK.MAT. TYPE REGISTERED'.                                 
011300     EJECT                                                                
011400******************************************************************        
011500*                                                                         
011600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
011700*                                                                         
011800 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
011900     SKIP3                                                                
012000*01    MID -COPY W0I80701 -PRE MID-.                                      
012200     EJECT                                                                
012300*01    -COPY WMSGAREA                                                     
012500     EJECT                                                                
012600*    03  MOD -COPY W0O80701 -PRE MOD- -RED MSG-AREA.                      
012800     EJECT                                                                
012900*01    -COPY WMFSAREA                                                     
013100     EJECT                                                                
013200******************************************************************        
013300*                                                                         
013400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500*                                                                         
013600 01    IMS-WS.                                                            
013700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
013800     SKIP3                                                                
013900*                        **** STATUS-KOD FRÅN IMS                         
014000   03    STATUS-WS               PIC XX.                                  
014100     88    SEGMENT-FINNS                     VALUE '  '.                  
014200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
014300     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
014400     SKIP3                                                                
014500   03    GODK-STATUSKODER.                                                
014600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
014700     SKIP3                                                                
014800 01    SSA1                      PIC X(64).                               
014900 01    SSA2                      PIC X(64).                               
015000     EJECT                                                                
015100*                            IMS FUNKTIONSKODER                           
015200*01    -COPY W0003                                                        
015400     EJECT                                                                
015500*- - - - - - - - - - - - - - -DLI INPUT-OUTPUT AREA                       
015600 01  DLI-IO-AREA.                                                         
015700    03 IO-AREA                  PIC X(100)    VALUE SPACE.                
015800     SKIP2                                                                
015900*   03 WL473801   -COPY WDGX473H   -RED IO-AREA                           
016100     SKIP3                                                                
016200*   03 WL473811   -COPY WDGX4738   -RED IO-AREA -PRE 4738-                
016400     EJECT                                                                
016500 LINKAGE SECTION.                                                         
016600*01    -COPY W0009     -PRE MSG-                                          
016800     EJECT                                                                
016900*01    -COPY W0008     -PRE WDG-                                          
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION USING MSG-PCB WDG-PCB.                                
017400     ENTRY 'DLITCBL' USING MSG-PCB WDG-PCB.                               
017500     SKIP2                                                                
017600     PERFORM IMS-GET-MSG                                                  
017700     IF SEGMENT-FINNS                                                     
017800       PERFORM A-INIT-SPARA-KOLLA-INPUT                                   
017900       IF WS-GODKAEND-BILD                                                
018000         IF WS-KDEMBTYP NUMERIC                                           
018100           MOVE WS-KDEMBTYP TO W-KDEMBTYP-4738                            
018200           IF MFS-UPDATE                                                  
018300             IF AENDRA                                                    
018400               PERFORM   B-AENDRA-4738                                    
018500             ELSE                                                         
018600               EVALUATE TRUE                                              
018700               WHEN BORTTAG                                               
018800                 PERFORM   C-BORTTAG-4738                                 
018900               WHEN NY                                                    
019000                 PERFORM   D-NY-4738                                      
019100                WHEN OTHER                                                
019200                 MOVE FEL-2 TO MOD-MESSAGE-RAD1                           
019300                 MOVE MFS-RENSA-FAELT TO MOD-KDUPPD-BEEMBTYP-UT           
019400                 PERFORM MFS-ROER-EJ-TILL-MOD                             
019500               END-EVALUATE                                               
019600             END-IF                                                       
019700           ELSE                                                           
019800             PERFORM E-LAS-4738                                           
019900           END-IF                                                         
020000         ELSE                                                             
020100           MOVE FEL-1 TO MOD-MESSAGE-RAD1                                 
020200           PERFORM MFS-RENSA-TILL-MOD                                     
020300         END-IF                                                           
020400       ELSE                                                               
020500         MOVE MFS-RENSA-FAELT TO MOD-KDEMBTYP-UT                          
020600                                 MOD-KDUPPD-BEEMBTYP-UT                   
020700       END-IF                                                             
020800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
020900       PERFORM IMS-INSERT-MSG                                             
021000     END-IF                                                               
021100     MOVE ZERO TO RETURN-CODE                                             
021200     GOBACK                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 A-INIT-SPARA-KOLLA-INPUT SECTION.                                        
021600     SKIP2                                                                
021700     IF MSG-DUBBLA-TRANSKODER                                             
021800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I80701                 
021900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022100       IF MSG-IDTRANS-2 = '0807'                                          
022200         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
022300       ELSE                                                               
022400         MOVE SPACE TO MFS-KDTRTYP                                        
022500       END-IF                                                             
022600     ELSE                                                                 
022700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I80701                  
022800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
022900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023000     END-IF                                                               
023100     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
023200                                                                          
023310     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W0O80701 + 4                  
023320                                                                          
023400     IF SWEDISH-TEXT                                                      
023500       MOVE SVENSKA-MEDDELANDE TO MEDDELANDE                              
023600     ELSE                                                                 
023700       MOVE ENGELSKA-MEDDELANDE TO MEDDELANDE                             
023800     END-IF                                                               
023900     IF MID-KDEMBTYP-IN = ALL '+'                                         
024000       MOVE MID-KDEMBTYP-UT TO WS-KDEMBTYP                                
024100       INSPECT WS-KDEMBTYP REPLACING ALL SPACE BY ZERO                    
024200     ELSE                                                                 
024300       MOVE MID-KDEMBTYP-IN TO WS-KDEMBTYP                                
024400       MOVE SPACE TO MFS-KDTRTYP                                          
024500     END-IF                                                               
024600     IF MID-KDUPPD-BEEMBTYP-IN = ALL '+'                                  
024700       MOVE MID-KDUPPD-BEEMBTYP-UT TO WS-KDUPPD                           
024800     ELSE                                                                 
024900       MOVE MID-KDUPPD-BEEMBTYP-IN TO WS-KDUPPD                           
025000       MOVE SPACE TO MFS-KDTRTYP                                          
025100     END-IF                                                               
025200     MOVE LOW-VALUE TO MSG-AREA                                           
025300     MOVE 'W0O80701' TO MFS-IDMOD                                         
025400     MOVE '0807' TO MOD-IDTRANS                                           
025500*                                                                         
025600     MOVE WS-KDUPPD TO MOD-KDUPPD-BEEMBTYP-UT                             
025700     MOVE WS-KDEMBTYP TO MOD-KDEMBTYP-UT                                  
025800     INSPECT MOD-KDEMBTYP-UT REPLACING LEADING ZERO BY SPACE              
025900     MOVE MFS-RENSA-FAELT TO MOD-KDEMBTYP-IN                              
026000                             MOD-KDUPPD-BEEMBTYP-IN                       
026100                             MOD-MESSAGE-RAD1                             
026200                             MOD-MESSAGE-RAD23                            
026300     .                                                                    
026400                                                                          
026500     EJECT                                                                
026600 B-AENDRA-4738 SECTION.                                                   
026700     SKIP2                                                                
026800     PERFORM IMS-GET-4738                                                 
026900     IF SEGMENT-FINNS                                                     
027000       MOVE +1 TO INDX                                                    
027100       PERFORM UNTIL INDX > MAX-INDX                                      
027300         IF MID-BEEMBTYP (INDX) NOT = ALL '+'                             
027400          MOVE MID-BEEMBTYP (INDX) TO 4738-EMBTYP-BEEMBTYP (INDX)         
027600          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEEMBTYP-ATTR (INDX)          
027700         END-IF                                                           
027800         ADD +1 TO INDX                                                   
027900       END-PERFORM                                                        
028000       PERFORM IMS-REPLACE-4738-BARN                                      
028100       MOVE MED-1 TO MOD-MESSAGE-RAD23                                    
028200       PERFORM MFS-ROER-EJ-TILL-MOD                                       
028300     ELSE                                                                 
028400       MOVE FEL-4 TO MOD-MESSAGE-RAD1                                     
028500       PERFORM MFS-RENSA-TILL-MOD                                         
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 C-BORTTAG-4738 SECTION.                                                  
029000     SKIP2                                                                
029100     PERFORM IMS-GET-4738-ROT                                             
029200     IF SEGMENT-FINNS                                                     
029300       PERFORM IMS-DELETE-4738-ROT                                        
029400       MOVE MED-2 TO MOD-MESSAGE-RAD23                                    
029500       PERFORM MFS-RENSA-TILL-MOD                                         
029600     ELSE                                                                 
029700       MOVE FEL-4 TO MOD-MESSAGE-RAD1                                     
029800       PERFORM MFS-ROER-EJ-TILL-MOD                                       
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 D-NY-4738 SECTION.                                                       
030300     SKIP2                                                                
030400     MOVE W-KDEMBTYP-4738-X TO 473H-WDGX473H                              
030500     PERFORM IMS-INSERT-4738-ROT                                          
030600     IF NOT SEGMENT-FINNS-REDAN                                           
030700       IF MID-BEEMBTYP-GRUPP NOT = ALL '+'                                
030800         MOVE '1' TO 4738-EMBTYP-KDSEGKEY                                 
030900         MOVE +1 TO INDX                                                  
031000         PERFORM UNTIL INDX > MAX-INDX                                    
031200           IF MID-BEEMBTYP (INDX) = ALL '+'                               
031300             MOVE SPACE TO 4738-EMBTYP-BEEMBTYP (INDX)                    
031400           ELSE                                                           
031500             MOVE MID-BEEMBTYP (INDX) TO                                  
031600             4738-EMBTYP-BEEMBTYP (INDX)                                  
031700             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
031800             MOD-BEEMBTYP-ATTR (INDX)                                     
031900           END-IF                                                         
032000           ADD +1 TO INDX                                                 
032100         END-PERFORM                                                      
032200         PERFORM IMS-INSERT-4738-BARN                                     
032300         MOVE MED-3 TO MOD-MESSAGE-RAD23                                  
032400         PERFORM MFS-ROER-EJ-TILL-MOD                                     
032500       ELSE                                                               
032600         PERFORM IMS-GET-4738-ROT                                         
032700         PERFORM IMS-DELETE-4738-ROT                                      
032800         MOVE FEL-6 TO MOD-MESSAGE-RAD1                                   
032900         MOVE MFS-ADD-SAETT-CURSOR TO MOD-BEEMBTYP-ATTR (1)               
033000         PERFORM MFS-RENSA-TILL-MOD                                       
033100       END-IF                                                             
033200     ELSE                                                                 
033300       MOVE FEL-5 TO MOD-MESSAGE-RAD1                                     
033400       PERFORM IMS-GET-4738                                               
033500       MOVE 4738-EMBTYP-BEEMBTYP (1) TO MOD-BEEMBTYP (1)                  
033600       MOVE 4738-EMBTYP-BEEMBTYP (2) TO MOD-BEEMBTYP (2)                  
033700       MOVE 4738-EMBTYP-BEEMBTYP (3) TO MOD-BEEMBTYP (3)                  
033800       MOVE 4738-EMBTYP-BEEMBTYP (4) TO MOD-BEEMBTYP (4)                  
033900       MOVE 4738-EMBTYP-BEEMBTYP (5) TO MOD-BEEMBTYP (5)                  
033910       MOVE 4738-EMBTYP-BEEMBTYP (6) TO MOD-BEEMBTYP (6)                  
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 E-LAS-4738 SECTION.                                                      
034400     SKIP2                                                                
034500     PERFORM IMS-GET-4738                                                 
034600     IF SEGMENT-FINNS                                                     
034700       MOVE 4738-EMBTYP-BEEMBTYP (1) TO MOD-BEEMBTYP (1)                  
034800       MOVE 4738-EMBTYP-BEEMBTYP (2) TO MOD-BEEMBTYP (2)                  
034900       MOVE 4738-EMBTYP-BEEMBTYP (3) TO MOD-BEEMBTYP (3)                  
035000       MOVE 4738-EMBTYP-BEEMBTYP (4) TO MOD-BEEMBTYP (4)                  
035100       MOVE 4738-EMBTYP-BEEMBTYP (5) TO MOD-BEEMBTYP (5)                  
035110       MOVE 4738-EMBTYP-BEEMBTYP (6) TO MOD-BEEMBTYP (6)                  
035200     ELSE                                                                 
035300       MOVE FEL-4 TO MOD-MESSAGE-RAD1                                     
035400       PERFORM MFS-RENSA-TILL-MOD                                         
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 MFS-RENSA-TILL-MOD SECTION.                                              
035900     SKIP2                                                                
036000     MOVE MFS-RENSA-FAELT TO MOD-BEEMBTYP (1)                             
036100                             MOD-BEEMBTYP (2)                             
036200                             MOD-BEEMBTYP (3)                             
036300                             MOD-BEEMBTYP (4)                             
036400                             MOD-BEEMBTYP (5)                             
036410                             MOD-BEEMBTYP (6)                             
036500     .                                                                    
036600*                                                                         
036700     EJECT                                                                
036800 MFS-ROER-EJ-TILL-MOD SECTION.                                            
036900     SKIP2                                                                
037000     MOVE MFS-ROER-EJ-FAELT TO MOD-BEEMBTYP (1)                           
037100                               MOD-BEEMBTYP (2)                           
037200                               MOD-BEEMBTYP (3)                           
037300                               MOD-BEEMBTYP (4)                           
037400                               MOD-BEEMBTYP (5)                           
037410                               MOD-BEEMBTYP (6)                           
037500     .                                                                    
037600*                                                                         
037700     EJECT                                                                
037800* IMS SEKTIONER                                                           
037900     SKIP3                                                                
038000 IMS-GET-MSG SECTION.                                                     
038100     MOVE '  QC' TO GODK-STATUSKODER                                      
038200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
038300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038400     PERFORM IMS-STATUSKONTROLL                                           
038500     .                                                                    
038600     SKIP3                                                                
038700     EJECT                                                                
038800 IMS-INSERT-MSG SECTION.                                                  
038900     IF ENGLISH-TEXT                                                      
039000       MOVE 'N' TO MFS-KDHUVOMR                                           
039100     END-IF                                                               
039200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
039300     MOVE SPACE TO GODK-STATUSKODER                                       
039400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
039500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039600     PERFORM IMS-STATUSKONTROLL                                           
039700     .                                                                    
039800     EJECT                                                                
039900 IMS-GET-4738 SECTION.                                                    
040000     STRING 'WL473801(WDGXKEY  =' W-KDEMBTYP-4738-X ')'                   
040100            DELIMITED BY SIZE INTO SSA1                                   
040200     MOVE 'WL473811' TO SSA2                                              
040300     MOVE '  GE' TO GODK-STATUSKODER                                      
040400     CALL CBLTDLI USING GHU WDG-PCB DLI-IO-AREA SSA1 SSA2                 
040500     MOVE WDG-STATUS-CODE TO STATUS-WS                                    
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     .                                                                    
040800     EJECT                                                                
040900 IMS-GET-4738-ROT SECTION.                                                
041000     STRING 'WL473801(WDGXKEY  =' W-KDEMBTYP-4738-X ')'                   
041100            DELIMITED BY SIZE INTO SSA1                                   
041200     MOVE '  GE' TO GODK-STATUSKODER                                      
041300     CALL CBLTDLI USING GHU WDG-PCB DLI-IO-AREA SSA1                      
041400     MOVE WDG-STATUS-CODE TO STATUS-WS                                    
041500     PERFORM IMS-STATUSKONTROLL                                           
041600     .                                                                    
041700     EJECT                                                                
041800 IMS-REPLACE-4738-BARN SECTION.                                           
041900     MOVE '  ' TO GODK-STATUSKODER                                        
042000     CALL CBLTDLI USING REPL WDG-PCB DLI-IO-AREA                          
042100     MOVE WDG-STATUS-CODE TO STATUS-WS                                    
042200     PERFORM IMS-STATUSKONTROLL                                           
042300     .                                                                    
042400     EJECT                                                                
042500 IMS-DELETE-4738-ROT SECTION.                                             
042600     MOVE '  ' TO GODK-STATUSKODER                                        
042700     CALL CBLTDLI USING DLET WDG-PCB DLI-IO-AREA                          
042800     MOVE WDG-STATUS-CODE TO STATUS-WS                                    
042900     PERFORM IMS-STATUSKONTROLL                                           
043000     .                                                                    
043100     EJECT                                                                
043200 IMS-INSERT-4738-ROT SECTION.                                             
043300     MOVE 'WL473801' TO SSA1                                              
043400     MOVE '  II' TO GODK-STATUSKODER                                      
043500     CALL CBLTDLI USING ISRT WDG-PCB DLI-IO-AREA SSA1                     
043600     MOVE WDG-STATUS-CODE TO STATUS-WS                                    
043700     PERFORM IMS-STATUSKONTROLL                                           
043800     .                                                                    
043900     EJECT                                                                
044000 IMS-INSERT-4738-BARN SECTION.                                            
044100     STRING 'WL473801(WDGXKEY  ='  W-KDEMBTYP-4738-X ')'                  
044200            DELIMITED BY SIZE INTO SSA1                                   
044300     MOVE 'WL473811' TO SSA2                                              
044400     MOVE '  ' TO GODK-STATUSKODER                                        
044500     CALL CBLTDLI USING ISRT WDG-PCB DLI-IO-AREA SSA1 SSA2                
044600     MOVE WDG-STATUS-CODE TO STATUS-WS                                    
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     EJECT                                                                
045000 IMS-STATUSKONTROLL SECTION.                                              
045100     SET STATUS-IX TO 1                                                   
045200     SEARCH GODK-STATUS                                                   
045210       AT END                                                             
045220         CALL FELLOG                                                      
045300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
045400     END-SEARCH                                                           
045500     .                                                                    
