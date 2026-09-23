000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1052400.                                                
000300 AUTHOR.         STEFAN ANDREASSON FRONTEC.                               
000400 DATE-WRITTEN.   NOV 1995.                                                
000500 DATE-COMPILED.                                                           
000600*                KOPIERAT PROGRAM W00717.                                 
000700*    FUNKTION.   TP-UPPDATERINGSPROGRAM.                                  
000800*                STARTAR SOP VIA W00606 FÖR BESTÄLLNING AV                
000900*                LISTOR                                                   
001000*                                                                         
001100*    ÄNDRING.....                                                         
001200*                INMATNING AV KDPRTVAL FÖR ATT STYRA OUTPUT               
001300*                TILL RÄTT PRINTER.                                       
001400*                                                                         
001500*    ÄNDRING.....920226                                                   
001600*                INMATNING AV TIERSDAT FÖR JOBTYP = 3 (ÄNDR.TJ.)          
001700*                (TIERSDAT BACKAS 32 VECKOR).                             
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W1T524         ENTER                                
002100*                     W1T524U        PF11                                 
002200*        MID:         W1I52401                                            
002300*    UTDATA.                                                              
002400*        MOD:         W1O52401                                            
002500*                                                                         
002600*        FELLOG                                                           
002700*                                                                         
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W1052400'.            
003500 77  INX                         PIC S9(9)   COMP SYNC.                   
003600*------------------------------- SPRÅK-INDEX                              
003700 77  S-IX                        PIC S9(9)   COMP SYNC VALUE +1.          
003800 01  FILLER                      PIC X(10)   VALUE 'ABENDTEXT='.          
003900 77  ABENDTEXT                   PIC X(72)   VALUE SPACE.                 
004000                                                                          
004100 01  W-FELFAELT.                                                          
004200     03  W-IDSKYLT-1             PIC X(3).                                
004300     03  FILLER                  PIC X       VALUE SPACE.                 
004400     03  W-IDSKYLT-2             PIC X(3).                                
004500     03  FILLER                  PIC X       VALUE SPACE.                 
004600     03  W-IDSKYLT-3             PIC X(3).                                
004700     03  FILLER                  PIC X       VALUE SPACE.                 
004800     03  W-IDSKYLT-4             PIC X(3).                                
004900     03  FILLER                  PIC X       VALUE SPACE.                 
005000     03  W-IDSKYLT-5             PIC X(3).                                
005100     03  FILLER                  PIC X       VALUE SPACE.                 
005200     03  W-IDSKYLT-6             PIC X(3).                                
005300     03  FILLER                  PIC X       VALUE SPACE.                 
005400     03  W-PUB                   PIC X(4).                                
005500     03  FILLER                  PIC X       VALUE SPACE.                 
005600     03  W-KDPRTVAL              PIC X.                                   
005700     03  FILLER                  PIC X       VALUE SPACE.                 
005800     03  W-TIERSDAT              PIC X(5).                                
005900                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006500                                                                          
006600     EJECT                                                                
006700*01  -COPY WDATAREA                                                       
006800     EJECT                                                                
006900*01  -COPY WDAGAREA                                                       
007000     EJECT                                                                
007100*- - - - - - - - - - - -   - - - PARAMETRAR TILL SOP                      
007200 01  W-PROG-TO-PROG-SW.                                                   
007300*03  -COPY WMSGSOP                                                        
007400                                                                          
007500                                                                          
007600 01  FILLER                      PIC X(16)   VALUE 'KONTROLLER'.          
007700 01  KONTROLL-FALT.                                                       
007800     03  INDATA-OK               PIC X       VALUE 'J'.                   
007900     03  W154JX93-SPRAK-FINNS    PIC X       VALUE 'N'.                   
008000                                                                          
008100 01  FILLER                      PIC X(16)   VALUE 'KONSTANTER'.          
008200 01  KONSTANTER.                                                          
008300     03  JA                      PIC X       VALUE 'J'.                   
008400     03  NEJ                     PIC X       VALUE 'N'.                   
008500     03  OK                      PIC X       VALUE 'O'.                   
008600     03  FEL                     PIC X       VALUE 'F'.                   
008700                                                                          
008800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
008900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
009000                                                                          
009100 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
009200 01  ARBETSFALT.                                                          
009300     03  A-DAGENS-DATUM.                                                  
009400         05  DATUM-AA            PIC 9(2).                                
009500         05  DATUM-MM            PIC 9(2).                                
009600         05  DATUM-DD            PIC 9(2).                                
009700     03  A-TID                   PIC 9(9).                                
009800                                                                          
009900     03  W-DAGENS-VECKA          PIC 9(6)     VALUE ZERO.                 
010000     03  W-INMATAD-VECKA         PIC X(4).                                
010100     03  W-AAR                   PIC 9(2).                                
010200     EJECT                                                                
010300 01  WS-XX-TIERSDAT-AAVVD        PIC X(5).                                
010400 01  FILLER REDEFINES WS-XX-TIERSDAT-AAVVD.                               
010500     03  WS-XX-AA                PIC X(2).                                
010600     03  WS-XX-VV                PIC X(2).                                
010700     03  WS-XX-D                 PIC X.                                   
010800     SKIP3                                                                
010900 01  WS-TIERSDAT-AAVVD           PIC 9(5).                                
011000 01  FILLER REDEFINES WS-TIERSDAT-AAVVD.                                  
011100     03  WS-TIERSDAT-AAVV        PIC 9(4).                                
011200     03  WS-TIERSDAT-D           PIC 9(1).                                
011300     SKIP3                                                                
011400 01  W-TESTAREA.                                                          
011500     03 W-TESTFAELT1             PIC X.                                   
011600     03 FILLER                   PIC X      VALUE '/'.                    
011700     03 W-TESTFAELT2             PIC X(5).                                
011800     03 FILLER                   PIC X      VALUE '/'.                    
011900     03 W-TESTFAELT3             PIC X(3).                                
012000     03 FILLER                   PIC X      VALUE '/'.                    
012100     03 W-TESTFAELT4             PIC X(3).                                
012200     03 FILLER                   PIC X      VALUE '/'.                    
012300     03 W-TESTFAELT5             PIC X(3).                                
012400     03 FILLER                   PIC X      VALUE '/'.                    
012500     03 W-TESTFAELT6             PIC X(3).                                
012600     03 FILLER                   PIC X      VALUE '/'.                    
012700     03 W-TESTFAELT7             PIC X(3).                                
012800     03 FILLER                   PIC X      VALUE '/'.                    
012900     03 W-TESTFAELT8             PIC X(3).                                
013000     03 FILLER                   PIC X      VALUE '/'.                    
013100     03 W-TESTFAELT9             PIC X.                                   
013200     03 FILLER                   PIC X      VALUE '/'.                    
013300     03 W-TESTFAELT10            PIC X(5).                                
013400     03 FILLER                   PIC X      VALUE '/'.                    
013500     SKIP3                                                                
013600 01  W009VADD-DATUM              PIC S9(5)   COMP-3.                      
013700 01  W009VADD-ANTAL              PIC S9(3)   COMP-3.                      
013800     EJECT                                                                
013900                                                                          
014000*               ******     DATA SOM SKICKAS TILL SOP                      
014100 01  PARM-TESYMBV.                                                        
014200     03  FILLER                  PIC X(4)    VALUE 'KAT('.                
014300     03  PARM-IDCATNR            PIC 9(5).                                
014400     03  FILLER                  PIC X       VALUE ')'.                   
014500     03  FILLER                  PIC X(4)    VALUE 'KOD('.                
014600     03  PARM-SPRAK-KOD          PIC X(18).                               
014700     03  FILLER                  PIC X       VALUE ')'.                   
014800     03  FILLER                  PIC X(4)    VALUE 'DAT('.                
014900     03  PARM-TIERSDAT           PIC S9(5).                               
015000     03  FILLER                  PIC X       VALUE ')'.                   
015100     03  FILLER                  PIC X(4)    VALUE 'PUB('.                
015200     03  PARM-KDCATPUB           PIC X(6).                                
015300     03  FILLER                  PIC X       VALUE ')'.                   
015400                                                                          
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)   VALUE 'NYCKLAR'.             
015700 01  NYCKLAR-TILL-DLI.                                                    
015800     03  W-IDJCLRAD-X.                                                    
015900         05  W-IDJCLRAD          PIC S9(5)   COMP-3.                      
016000     03  W-IDUSER-X.                                                      
016100         05  W-IDUSER            PIC X(8).                                
016200     03  W-WDN5ASEQ-MIN-X.                                                
016300         05  W-IDCATNR-MIN       PIC 9(5)   VALUE ZERO.                   
016400         05  W-IDCATGRP-MIN      PIC 9(2)   VALUE ZERO.                   
016500         05  W-IDCATAVS-MIN      PIC 9(4)   VALUE ZERO.                   
016600     03  W-WDN5ASEQ-MAX-X.                                                
016700         05  W-IDCATNR-MAX       PIC 9(5)   VALUE 99999.                  
016800         05  W-IDCATGRP-MAX      PIC 9(2)   VALUE 99.                     
016900         05  W-IDCATAVS-MAX      PIC 9(4)   VALUE 9999.                   
017000     03  W-IDCATNR-X.                                                     
017100         05  W-IDCATNR           PIC 9(5).                                
017200     EJECT                                                                
017300 01  FILLER                   PIC X(16) VALUE 'MEDDELANDEN'.              
017400 01  FELMEDDELANDE.                                                       
017500*                                                                         
017600     03 FEL1.                                                             
017700        05 FILLER                PIC X(26)                                
017800                  VALUE 'EJ AUKTORISERAD ANVÄNDARE '.                     
017900        05 FILLER                PIC X(26)                                
018000                  VALUE 'USER NOT AUTHORISED       '.                     
018100     03 F-TEXT-1 REDEFINES FEL1.                                          
018200        05 FEL-1 OCCURS 2        PIC X(26).                               
018300                                                                          
018400     03 FEL2.                                                             
018500        05 FILLER                PIC X(26)                                
018600                  VALUE 'UPPLYSTA FÄLT FEL         '.                     
018700        05 FILLER                PIC X(26)                                
018800                  VALUE 'HIGHLIGHTED FIELDS WRONG  '.                     
018900     03 F-TEXT-2 REDEFINES FEL2.                                          
019000        05 FEL-2 OCCURS 2        PIC X(26).                               
019100                                                                          
019200     03 FEL3.                                                             
019300        05 FILLER                PIC X(26)                                
019400                  VALUE 'RUTIN FINNS EJ            '.                     
019500        05 FILLER                PIC X(26)                                
019600                  VALUE 'ROUTINE DOES NOT EXIST    '.                     
019700     03 F-TEXT-3 REDEFINES FEL3.                                          
019800        05 FEL-3 OCCURS 2        PIC X(26).                               
019900                                                                          
020000     03 FEL4.                                                             
020100        05 FILLER                PIC X(26)                                
020200                  VALUE 'ENDAST 1 SPRÅK I W154J093 '.                     
020300        05 FILLER                PIC X(26)                                
020400                  VALUE 'ONLY 1 LANG. IN  W154J093 '.                     
020500     03 F-TEXT-4 REDEFINES FEL4.                                          
020600        05 FEL-4 OCCURS 2        PIC X(26).                               
020700                                                                          
020800     03 FEL5.                                                             
020900        05 FILLER                PIC X(26)                                
021000                  VALUE 'OIDENTIFIERAT JOB-NUMMER  '.                     
021100        05 FILLER                PIC X(26)                                
021200                  VALUE 'UNIDENTIFIED JOB NUMBER   '.                     
021300     03 F-TEXT-5 REDEFINES FEL5.                                          
021400        05 FEL-5 OCCURS 2        PIC X(26).                               
021500                                                                          
021600     03 FEL6.                                                             
021700        05 FILLER                PIC X(26)                                
021800                  VALUE 'PUBKOD SAKNAS             '.                     
021900        05 FILLER                PIC X(26)                                
022000                  VALUE 'TIMECODE MISSING          '.                     
022100     03 F-TEXT-6 REDEFINES FEL6.                                          
022200        05 FEL-6 OCCURS 2        PIC X(26).                               
022300                                                                          
022400     03 FEL7.                                                             
022500        05 FILLER                PIC X(26)                                
022600                  VALUE 'OMBRYTNING REDAN BESTÄLLD '.                     
022700        05 FILLER                PIC X(26)                                
022800                  VALUE 'PAGEMAKEUP ALREADY ORDERED'.                     
022900     03 F-TEXT-7 REDEFINES FEL7.                                          
023000        05 FEL-7 OCCURS 2        PIC X(26).                               
023100                                                                          
023200 01  MEDDELANDE.                                                          
023300     03  MED-1                  PIC X(25)                                 
023400         VALUE 'UPPDATERING GJORD       '.                                
023500                                                                          
023600     03  MED-2                  PIC X(25)                                 
023700         VALUE 'INGEN UPPDATERING GJORD '.                                
023800                                                                          
023900     03  MED-4                  PIC X(29)                                 
024000         VALUE 'TRYCK PF11 FÖR UPPDATERING'.                              
024100                                                                          
024200     EJECT                                                                
024300*01  LAND-KONTROLL   -COPY WWLAND01                                       
024400     EJECT                                                                
024500*                        ****    TP-AREOR                                 
024600 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
024700*01  MID-AREA -COPY W1I52401                                              
024800     EJECT                                                                
024900*01  -COPY WMSGAREA                                                       
025000     EJECT                                                                
025100*03  MOD-AREA -COPY W1O52401  -RED MSG-AREA.                              
025200     EJECT                                                                
025300*01  -COPY WMFSAREA.                                                      
025400     EJECT                                                                
025500*****                                                                     
025600*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025700*****                                                                     
025800 01  IMS-WS.                                                              
025900     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
026000     SKIP3                                                                
026100*****                    **** STATUS-KOD FRÅN IMS                         
026200     03  STATUS-WS               PIC X(2).                                
026300         88  SEGMENT-FINNS                   VALUE '  '.                  
026400         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
026500     SKIP3                                                                
026600     03  GODK-STATUSKODER.                                                
026700         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
026800     SKIP3                                                                
026900 01  SSA1                        PIC X(64).                               
027000 01  SSA2                        PIC X(64).                               
027100     EJECT                                                                
027200*                            IMS FUNKTIONSKODER                           
027300*01  -COPY W0003                                                          
027400                                                                          
027500     EJECT                                                                
027600 01  FILLER                      PIC X(16)   VALUE 'IO-AREA'.             
027700 01  DLI-IO-AREA.                                                         
027800     03  IO-AREA                 PIC X(500)  VALUE SPACE.                 
027900     SKIP3                                                                
028000*    03  WLKATH01  -COPY WDN501  -RED IO-AREA -PRE WLKATH-.               
028100     EJECT                                                                
028200     03  IO-AREA-2               PIC X(500)  VALUE SPACE.                 
028300     SKIP3                                                                
028400*    03  WLKATM01  -COPY WDN101  -RED IO-AREA-2 -PRE WLKATM-.             
028500 LINKAGE SECTION.                                                         
028600     SKIP2                                                                
028700*01  -COPY W0009     -PRE MSG-                                            
028800     EJECT                                                                
028900*01  -COPY W0009     -PRE ALT-                                            
029000     EJECT                                                                
029100*01  -COPY W0008     -PRE WLKATH-                                         
029200         05  FILLER              PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008     -PRE WLKATM-                                         
029500         05  FILLER              PIC X.                                   
029600     EJECT                                                                
029700 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB                                
029800                           WLKATH-PCB WLKATM-PCB.                         
029900 MAIN SECTION.                                                            
030000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
030100                           WLKATH-PCB WLKATM-PCB.                         
030200                                                                          
030300     PERFORM IMS-GET-MSG                                                  
030400     IF SEGMENT-FINNS                                                     
030500         PERFORM A-INIT-SPARA-INPUT                                       
030600         IF MFS-IDTRANS = '1524'                                          
030700           IF MFS-UPDATE                                                  
030800              IF MID-KDBEH-IDJOB = 1 OR 2 OR 3                            
030900                 PERFORM B-KOLLA-INDATA                                   
031000                 IF INDATA-OK = JA                                        
031100                    PERFORM C-UPPD-PARAMETER                              
031200                    PERFORM D-STARTA-JOB                                  
031300                    MOVE MED-1      TO MOD-MESSAGE-RAD23                  
031400                    PERFORM F-RENSA-BILD                                  
031500                 ELSE                                                     
031600                    MOVE MED-2      TO MOD-MESSAGE-RAD23                  
031700                    PERFORM G-VISA-BILD-IGEN                              
031800                 END-IF                                                   
031900              ELSE                                                        
032000                 MOVE FEL-5(S-IX) TO MOD-MESSAGE-RAD1                     
032100                 MOVE NEJ TO INDATA-OK                                    
032200                 PERFORM G-VISA-BILD-IGEN                                 
032300              END-IF                                                      
032400           ELSE                                                           
032500             MOVE MED-4 TO MOD-MESSAGE-RAD23                              
032600             PERFORM G-VISA-BILD-IGEN                                     
032700           END-IF                                                         
032800         ELSE                                                             
032900            PERFORM F-RENSA-BILD                                          
033000         END-IF                                                           
033100         MOVE LENGTH OF MOD-AREA TO MSG-KVLL                              
033200         ADD +4                  TO MSG-KVLL                              
033300         PERFORM IMS-INSERT-MSG                                           
033400     END-IF                                                               
033500                                                                          
033600     MOVE ZERO TO RETURN-CODE                                             
033700     GOBACK                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 A-INIT-SPARA-INPUT SECTION.                                              
034100                                                                          
034200     IF MSG-DUBBLA-TRANSKODER                                             
034300         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I52401               
034400         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
034500         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
034600         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
034700     ELSE                                                                 
034800         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I52401                
034900         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
035000         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
035100         MOVE ' ' TO MFS-KDTRTYP                                          
035200     END-IF                                                               
035300                                                                          
035400     MOVE LOW-VALUE TO MOD-W1O52401                                       
035500     MOVE 'W1O52401' TO MFS-IDMOD                                         
035600     MOVE '1524' TO MOD-IDTRANS                                           
035700                                                                          
035800     IF ENGLISH-TEXT                                                      
035900        MOVE +2 TO S-IX                                                   
036000     ELSE                                                                 
036100        MOVE +1 TO S-IX                                                   
036200     END-IF                                                               
036300                                                                          
036400     MOVE MFS-RENSA-FAELT TO MOD-MESSAGE-RAD1                             
036500                             MOD-MESSAGE-RAD23                            
036600     ACCEPT A-DAGENS-DATUM FROM DATE                                      
036700     ACCEPT A-TID FROM TIME                                               
036800                                                                          
036900     .                                                                    
037000     EJECT                                                                
037100 B-KOLLA-INDATA SECTION.                                                  
037200                                                                          
037300     MOVE JA    TO INDATA-OK                                              
037400     MOVE +1    TO INX                                                    
037500                                                                          
037600     IF MID-KDBEH-IDJOB = 1 OR 2 OR 3                                     
037700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBEH-IDJOB-ATTR                 
037800     ELSE                                                                 
037900        MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBEH-IDJOB-ATTR                 
038000        MOVE NEJ                  TO INDATA-OK                            
038100        MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                              
038200     END-IF                                                               
038300                                                                          
038400     IF MID-IDCATNR NOT = ALL '+'                                         
038500        IF MID-IDCATNR NUMERIC                                            
038600           MOVE MID-IDCATNR TO W-IDCATNR                                  
038700                               W-IDCATNR-MIN                              
038800                               W-IDCATNR-MAX                              
038900           PERFORM IMS-GET-WLKATH01                                       
039000           IF SEGMENT-FINNS                                               
039100              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATNR-ATTR                
039200              MOVE MID-IDCATNR TO W-IDCATNR                               
039300              PERFORM IMS-GET-WLKATM01                                    
039400              IF SEGMENT-FINNS                                            
039500                 CONTINUE                                                 
039600              ELSE                                                        
039700                 MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCATNR-ATTR             
039800                 MOVE NEJ TO INDATA-OK                                    
039900                 MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                     
040000              END-IF                                                      
040100           ELSE                                                           
040200              MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCATNR-ATTR                
040300              MOVE NEJ TO INDATA-OK                                       
040400              MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                        
040500           END-IF                                                         
040600        ELSE                                                              
040700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-ATTR                     
040800           MOVE NEJ               TO INDATA-OK                            
040900           MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                           
041000        END-IF                                                            
041100     ELSE                                                                 
041200        MOVE MFS-NUM-FAELT-FEL    TO MOD-IDCATNR-ATTR                     
041300        MOVE NEJ                  TO INDATA-OK                            
041400        MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                              
041500     END-IF                                                               
041600                                                                          
041700     IF MID-KDBEH-IDJOB = 3                                               
041800*SPRÅK 1                                                                  
041900        MOVE +1 TO INX                                                    
042000        IF MID-IDSKYLT(INX) = SPACE OR ALL '+'                            
042100           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR(INX)               
042200           MOVE NEJ TO INDATA-OK                                          
042300           MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                           
042400        ELSE                                                              
042500           MOVE MID-IDSKYLT(INX) TO IDSKYLT                               
042600           IF NOT TYSKA                                                   
042700            IF NOT ENGELSKA                                               
042800             IF NOT AMERIKANSKA                                           
042900              IF NOT FRANSKA                                              
043000               IF NOT SPANSKA                                             
043100                IF NOT ITALENSKA                                          
043200                 IF NOT HOLLANDSKA                                        
043300                  IF NOT PORTUGISISKA                                     
043400                   IF NOT FINSKA                                          
043500                    IF NOT SVENSKA                                        
043600                      MOVE MFS-ALFA-FAELT-FEL TO                          
043700                      MOD-IDSKYLT-ATTR(INX)                               
043800                      MOVE NEJ TO INDATA-OK                               
043900                      MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                
044000                    END-IF                                                
044100                   END-IF                                                 
044200                  END-IF                                                  
044300                 END-IF                                                   
044400                END-IF                                                    
044500               END-IF                                                     
044600              END-IF                                                      
044700             END-IF                                                       
044800            END-IF                                                        
044900           END-IF                                                         
045000           IF INDATA-OK = JA                                              
045100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR(INX)           
045200           END-IF                                                         
045300*SPRÅK 2                                                                  
045400           ADD 1 TO INX                                                   
045500           IF MID-IDSKYLT(INX) = '*  ' OR ALL '+'                         
045600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR(INX)           
045700           ELSE                                                           
045800             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR(INX)             
045900             MOVE NEJ TO INDATA-OK                                        
046000             MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                         
046100           END-IF                                                         
046200*SPRÅK 3-6                                                                
046300           ADD 1 TO INX                                                   
046400           PERFORM UNTIL INX > +6                                         
046500              IF MID-IDSKYLT(INX) = SPACE OR ALL '+'                      
046600                 MOVE MFS-ALFA-FAELT-RAETT                                
046700                           TO MOD-IDSKYLT-ATTR(INX)                       
046800              ELSE                                                        
046900                 MOVE MFS-ALFA-FAELT-FEL                                  
047000                           TO MOD-IDSKYLT-ATTR(INX)                       
047100                 MOVE NEJ TO INDATA-OK                                    
047200                 MOVE FEL-4(S-IX) TO MOD-MESSAGE-RAD1                     
047300              END-IF                                                      
047400              ADD 1 TO INX                                                
047500           END-PERFORM                                                    
047600        END-IF                                                            
047700        MOVE 1 TO INX                                                     
047800        PERFORM UNTIL INX > +6                                            
047900           INSPECT MID-IDSKYLT(INX) REPLACING ALL '+' BY SPACE            
048000           ADD 1 TO INX                                                   
048100        END-PERFORM                                                       
048200                                                                          
048300                                                                          
048400* PRINTNING STYRDES TIDIGARE MED KDPRTVAL TILL RÄTT PRINTER               
048500* ÄR NUMERA BORTTAGET MID- OCH MOD-FÄLT LIGGER DOCK KVAR I BILDEN         
048600*                                                                         
048700***                                                                       
048800     ELSE                                                                 
048900*****      IDJOBB 1 ELLER 2 *****************                             
049000                                                                          
049100        IF MID-IDSKYLT (1) = SPACE OR ALL '+'                             
049200           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-ATTR (1)                
049300           MOVE NEJ TO INDATA-OK                                          
049400           MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                           
049500        END-IF                                                            
049600        MOVE 1 TO INX                                                     
049700        PERFORM UNTIL INX > +6                                            
049800        OR MID-IDSKYLT(INX) = ALL '+'                                     
049900           MOVE MID-IDSKYLT (INX) TO IDSKYLT                              
050000           IF NOT TYSKA                                                   
050100            IF NOT ENGELSKA                                               
050200             IF NOT AMERIKANSKA                                           
050300              IF NOT FRANSKA                                              
050400               IF NOT SPANSKA                                             
050500                IF NOT ITALENSKA                                          
050600                 IF NOT HOLLANDSKA                                        
050700                  IF NOT PORTUGISISKA                                     
050800                   IF NOT FINSKA                                          
050900                    IF NOT SVENSKA                                        
051000                       MOVE MFS-ALFA-FAELT-FEL TO                         
051100                       MOD-IDSKYLT-ATTR (INX)                             
051200                       MOVE NEJ TO INDATA-OK                              
051300                       MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1               
051400                    END-IF                                                
051500                   END-IF                                                 
051600                  END-IF                                                  
051700                 END-IF                                                   
051800                END-IF                                                    
051900               END-IF                                                     
052000              END-IF                                                      
052100             END-IF                                                       
052200            END-IF                                                        
052300           END-IF                                                         
052400                                                                          
052500           IF INDATA-OK = JA                                              
052600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-ATTR(INX)          
052700           END-IF                                                         
052800           ADD 1 TO INX                                                   
052900        END-PERFORM                                                       
053000                                                                          
053100        PERFORM UNTIL INX > +6                                            
053200           INSPECT MID-IDSKYLT(INX) REPLACING ALL '+' BY SPACE            
053300           ADD +1 TO INX                                                  
053400        END-PERFORM                                                       
053500                                                                          
053600* PRINTNING STYRDES TIDIGARE MED KDPRTVAL TILL RÄTT PRINTER               
053700* ÄR NUMERA BORTTAGET MID- OCH MOD-FÄLT LIGGER DOCK KVAR I BILDEN         
053800***                                                                       
053900     END-IF                                                               
054000                                                                          
054100     IF MID-KDBEH-IDJOB = 1                                               
054200     AND INDATA-OK = JA                                                   
054300                                                                          
054400*******   KONTROLLERA MOT DATUMRUTIN ATT INTE                             
054500*******   KATALOGOMBRYTNING REDAN ÄR BESTÄLLD                             
054600                                                                          
054700        IF WLKATM-KAT-TIOMBRYT-ORD > ZERO                                 
054800              MOVE MFS-NUM-FAELT-FEL    TO MOD-IDCATNR-ATTR               
054900              MOVE NEJ                  TO INDATA-OK                      
055000              MOVE FEL-7(S-IX) TO MOD-MESSAGE-RAD1                        
055100        END-IF                                                            
055200                                                                          
055300*******   KONTROLLERA MOT DATUMRUTIN ATT KAT-KDCATPUB ÄR                  
055400*******   UPPDATERAD INFÖR NÄSTA KATALOGOMBRYTNING (GÖRS I 1525)          
055500                                                                          
055600        MOVE 'AAMMDD'       TO DAT-KDDATFORM                              
055700        MOVE A-DAGENS-DATUM TO DAT-I-TIDATUM                              
055800                                                                          
055900        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
056000                        DAT-O-TIDATUM DAT-KDSVAR                          
056100                                                                          
056200        IF DAT-KDSVAR-OK                                                  
056300          MOVE DAT-TISEKEL TO W-DAGENS-VECKA (1:2)                        
056400          MOVE DAT-TIAA-VECKA                                             
056500                           TO W-DAGENS-VECKA (3:2)                        
056600          MOVE DAT-TIVV    TO W-DAGENS-VECKA (5:2)                        
056700          IF WLKATM-KAT-KDCATPUB-FOM = SPACE                              
056800*         OR WLKATM-KAT-KDCATPUB-FOM < W-DAGENS-VECKA                     
056900              MOVE MFS-NUM-FAELT-FEL    TO MOD-IDCATNR-ATTR               
057000              MOVE NEJ                  TO INDATA-OK                      
057100              MOVE FEL-6(S-IX) TO MOD-MESSAGE-RAD1                        
057200          END-IF                                                          
057300                                                                          
057400        ELSE                                                              
057500            STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS             
057600            DELIMITED BY SIZE INTO FELTEXT                                
057700            CALL FELLOG                                                   
057800        END-IF                                                            
057900     END-IF                                                               
058000                                                                          
058100     IF MID-KDBEH-IDJOB = 3                                               
058200        IF MID-TIERSDAT = ALL '+'                                         
058300           MOVE MFS-NUM-FAELT-FEL TO MOD-TIERSDAT-ATTR                    
058400           MOVE NEJ TO INDATA-OK                                          
058500           MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                           
058600        ELSE                                                              
058700           MOVE MID-TIERSDAT TO WS-XX-TIERSDAT-AAVVD                      
058800           MOVE '1'          TO WS-XX-D                                   
058900           MOVE WS-XX-TIERSDAT-AAVVD TO WS-TIERSDAT-AAVVD                 
059000           IF WS-TIERSDAT-AAVVD NUMERIC                                   
059100              MOVE MFS-NUM-FAELT-RAETT TO MOD-TIERSDAT-ATTR               
059200           ELSE                                                           
059300              MOVE MFS-NUM-FAELT-FEL   TO MOD-TIERSDAT-ATTR               
059400              MOVE NEJ TO INDATA-OK                                       
059500              MOVE FEL-2(S-IX) TO MOD-MESSAGE-RAD1                        
059600           END-IF                                                         
059700        END-IF                                                            
059800     ELSE                                                                 
059900        MOVE MFS-RENSA-FAELT TO MOD-TIERSDAT                              
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300                                                                          
060400 C-UPPD-PARAMETER SECTION.                                                
060500     SKIP2                                                                
060600     MOVE MID-IDCATNR       TO PARM-IDCATNR                               
060700     MOVE MID-SPRAK-KOD     TO PARM-SPRAK-KOD                             
060800     MOVE WLKATM-KAT-KDCATPUB-FOM                                         
060900                            TO PARM-KDCATPUB                              
061000     IF MID-KDBEH-IDJOB = 3                                               
061100*      ---  FIXA TILL ETT 32-VECKORS TIDIGARE PARM-TIERSDAT               
061200       IF WS-TIERSDAT-AAVV(1:2) < 70                                      
061300         MOVE 20 TO DAT-TISEKEL                                           
061400       ELSE                                                               
061500         MOVE 19 TO DAT-TISEKEL                                           
061600       END-IF                                                             
061700                                                                          
061800       MOVE WS-TIERSDAT-AAVVD TO DAT-I-TIDATUM                            
061900       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
062000                                                                          
062100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
062200                           DAT-O-TIDATUM DAT-KDSVAR                       
062300       IF DAT-KDSVAR-FEL                                                  
062400         MOVE 'FELSVAR FRÅN WDATKONV I C-UPPD-' TO ABENDTEXT              
062500         CALL FELLOG                                                      
062600       ELSE                                                               
062700         MOVE DAT-TISEKEL   TO DAG-TISEKEL-TOM                            
062800         MOVE DAT-TIAAMMDD  TO DAG-TIAAMMDD-TOM                           
062900       END-IF                                                             
063000       MOVE 224           TO DAG-KVKALDAG                                 
063100*      --- 32 VECKOR BAKÅT - COMPUTE FOM = TOM - DAGAR                    
063200       MOVE 003 TO DAG-KDCALL                                             
063300                                                                          
063400                                                                          
063500       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR           
063600       IF DAG-KDSVAR = FEL                                                
063700         MOVE 'FELSVAR FRÅN WDAGKONV I C-UPPD-' TO ABENDTEXT              
063800         CALL FELLOG                                                      
063900       ELSE                                                               
064000         MOVE DAG-TISEKEL-FOM  TO DAT-TISEKEL                             
064100         MOVE DAG-TIAAMMDD-FOM TO DAT-I-TIDATUM                           
064200       END-IF                                                             
064300       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
064400                                                                          
064500                                                                          
064600       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
064700                           DAT-O-TIDATUM DAT-KDSVAR                       
064800       IF DAT-KDSVAR-OK                                                   
064900         MOVE DAT-TIAAVVD TO   PARM-TIERSDAT                              
065000       ELSE                                                               
065100         MOVE 88011       TO  PARM-TIERSDAT                               
065200       END-IF                                                             
065300     ELSE                                                                 
065400       MOVE ZERO          TO PARM-TIERSDAT                                
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
065800                                                                          
065900 D-STARTA-JOB SECTION.                                                    
066000                                                                          
066100     IF MID-KDBEH-IDJOB = 1                                               
066200       MOVE '1524'           TO MSGSOP-IDTRANS                            
066300       MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                           
066400       MOVE 'W154B1    '     TO MSGSOP-IDPROCESS                          
066500       MOVE 'O'              TO MSGSOP-KDSOPFUNK                          
066600       MOVE PARM-TESYMBV     TO MSGSOP-TESYMBV                            
066700       PERFORM IMS-INSERT-ALT-MSG                                         
066800                                                                          
066900       MOVE '1524'           TO MSGSOP-IDTRANS                            
067000       MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                           
067100       MOVE 'W154B1BLK '     TO MSGSOP-IDPROCESS                          
067200       MOVE 'O'              TO MSGSOP-KDSOPFUNK                          
067300       MOVE SPACE            TO MSGSOP-TESYMBV                            
067400       PERFORM IMS-INSERT-ALT-MSG                                         
067500     ELSE                                                                 
067600      IF MID-KDBEH-IDJOB = 2                                              
067700        MOVE '1524'           TO MSGSOP-IDTRANS                           
067800        MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                          
067900        MOVE 'W154B2    '     TO MSGSOP-IDPROCESS                         
068000        MOVE 'O'              TO MSGSOP-KDSOPFUNK                         
068100        MOVE PARM-TESYMBV     TO MSGSOP-TESYMBV                           
068200        PERFORM IMS-INSERT-ALT-MSG                                        
068300                                                                          
068400        MOVE '1524'           TO MSGSOP-IDTRANS                           
068500        MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                          
068600        MOVE 'W154B2BLK '     TO MSGSOP-IDPROCESS                         
068700        MOVE 'O'              TO MSGSOP-KDSOPFUNK                         
068800        MOVE SPACE            TO MSGSOP-TESYMBV                           
068900        PERFORM IMS-INSERT-ALT-MSG                                        
069000      ELSE                                                                
069100       IF MID-KDBEH-IDJOB = 3                                             
069200         MOVE '1524'           TO MSGSOP-IDTRANS                          
069300         MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                         
069400         MOVE 'W154B3    '     TO MSGSOP-IDPROCESS                        
069500         MOVE 'O'              TO MSGSOP-KDSOPFUNK                        
069600         MOVE PARM-TESYMBV     TO MSGSOP-TESYMBV                          
069700         PERFORM IMS-INSERT-ALT-MSG                                       
069800                                                                          
069900         MOVE '1524'           TO MSGSOP-IDTRANS                          
070000         MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                         
070100         MOVE 'W154B3BLK '     TO MSGSOP-IDPROCESS                        
070200         MOVE 'O'              TO MSGSOP-KDSOPFUNK                        
070300         MOVE SPACE            TO MSGSOP-TESYMBV                          
070400         PERFORM IMS-INSERT-ALT-MSG                                       
070500       END-IF                                                             
070600      END-IF                                                              
070700     END-IF                                                               
070800     .                                                                    
070900                                                                          
071000     EJECT                                                                
071100 F-RENSA-BILD SECTION.                                                    
071200                                                                          
071300     MOVE +1 TO INX                                                       
071400                                                                          
071500     MOVE MFS-RENSA-FAELT TO MOD-KDBEH-IDJOB                              
071600                             MOD-IDCATNR                                  
071700                             MOD-KDPRTVAL                                 
071800                             MOD-TIERSDAT                                 
071900     PERFORM UNTIL INX > +6                                               
072000        MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT(INX)                          
072100        ADD +1 TO INX                                                     
072200     END-PERFORM                                                          
072300     .                                                                    
072400     EJECT                                                                
072500 G-VISA-BILD-IGEN SECTION.                                                
072600                                                                          
072700     MOVE +1 TO INX                                                       
072800                                                                          
072900     MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEH-IDJOB                            
073000                               MOD-IDCATNR                                
073100                               MOD-KDPRTVAL                               
073200                               MOD-TIERSDAT                               
073300     PERFORM UNTIL INX > +6                                               
073400        MOVE MFS-ROER-EJ-FAELT TO MOD-IDSKYLT(INX)                        
073500        ADD +1 TO INX                                                     
073600     END-PERFORM                                                          
073700     .                                                                    
073800     EJECT                                                                
073900* IMS SEKTIONER                                                           
074000     SKIP3                                                                
074100 IMS-GET-MSG SECTION.                                                     
074200                                                                          
074300     MOVE '  QC' TO GODK-STATUSKODER                                      
074400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
074500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074600     PERFORM IMS-STATUSKONTROLL                                           
074700     .                                                                    
074800     SKIP3                                                                
074900 IMS-INSERT-MSG SECTION.                                                  
075000                                                                          
075100     IF ENGLISH-TEXT                                                      
075200        MOVE 'N' TO MFS-KDHUVOMR                                          
075300     END-IF                                                               
075400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
075500     MOVE SPACE TO GODK-STATUSKODER                                       
075600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
075700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075800     PERFORM IMS-STATUSKONTROLL                                           
075900     .                                                                    
076000     SKIP3                                                                
076100 IMS-INSERT-ALT-MSG SECTION.                                              
076200                                                                          
076300     MOVE SPACE TO GODK-STATUSKODER                                       
076400     CALL CBLTDLI USING PURG ALT-PCB W-PROG-TO-PROG-SW                    
076500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     EJECT                                                                
076900 IMS-GET-WLKATM01 SECTION.                                                
077000                                                                          
077100     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
077200             DELIMITED BY SIZE INTO SSA1                                  
077300     MOVE '  GE' TO GODK-STATUSKODER                                      
077400     CALL CBLTDLI USING GHU WLKATM-PCB IO-AREA-2 SSA1                     
077500     MOVE WLKATM-STATUS-CODE TO STATUS-WS                                 
077600     PERFORM IMS-STATUSKONTROLL                                           
077700     .                                                                    
077800     EJECT                                                                
077900 IMS-GET-WLKATH01 SECTION.                                                
078000                                                                          
078100     STRING 'WLKATH01(WDN5ASEQ=>' W-WDN5ASEQ-MIN-X                        
078200                    '&WDN5ASEQ<=' W-WDN5ASEQ-MAX-X ')'                    
078300                  DELIMITED BY SIZE INTO SSA1                             
078400     MOVE '  GE' TO GODK-STATUSKODER                                      
078500     CALL CBLTDLI USING GN WLKATH-PCB IO-AREA SSA1                        
078600     MOVE WLKATH-STATUS-CODE TO STATUS-WS                                 
078700     PERFORM IMS-STATUSKONTROLL                                           
078800     .                                                                    
078900     EJECT                                                                
079000 IMS-STATUSKONTROLL SECTION.                                              
079100     SET STATUS-IX TO 1                                                   
079200     SEARCH GODK-STATUS                                                   
079300       AT END                                                             
079400         CALL FELLOG                                                      
079500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
079600         CONTINUE                                                         
079700     END-SEARCH                                                           
079800     .                                                                    
