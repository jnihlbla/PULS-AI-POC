000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2727900.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   15/10/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        PROGRAM FÖR ATT LÄGGA UPP BUYER OCH KÖPTABELL                    
001000*        FÖR REFILLARTIKLAR TILL CDC                                      
001100*                                                                         
001200*                                                                         
001300                                                                          
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          ---                                                            
002200     SELECT W27277                     ASSIGN TO W27279D1.                
002300*          ---                                                            
002400     SELECT W27279                     ASSIGN TO W27279D2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W27277                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  POST -COPY W27277 -PRE  W27277-  -L.                                 
003500     SKIP3                                                                
003600 FD  W27279                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W27179 -PRE  UT-  -L.                                     
004100                                                                          
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2727900'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  W27277-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W27277                       VALUE 'J'.                   
005200                                                                          
005300 01  ARBETSAREOR.                                                         
005400     03 WS-DAGENS-TID            PIC 9(10)  VALUE ZERO.                   
005700     03 WS-IDPERSON-BUY          PIC 9(3)   VALUE ZERO.                   
005800                                                                          
005900     03 WS-IDPROJ                PIC X(4).                                
006000     03 WS-IDPROJ-REDEFINE       REDEFINES WS-IDPROJ.                     
006100        05 WS-IDPROJ-2POS        PIC X(2).                                
006200        05 WS-IDPROJ-FILLER      PIC X(2).                                
006300                                                                          
006400                                                                          
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600                                                                          
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STATUS          PIC X(2)    VALUE SPACE.                 
007000     03  FILLER                  PIC X       VALUE SPACE.                 
007100     03  FELTEXT-TEXT            PIC X(69)   VALUE SPACE.                 
007200                                                                          
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
008300     03  W271BUYR                PIC X(8)    VALUE 'W271BUYR'.            
008400                                                                          
008500     EJECT                                                                
008600 01  IN-AREA-START-W27277    PIC X(24)   VALUE                            
008700                                 'IN-AREA-START-W27277 '.                 
008800                                                                          
008900                                                                          
009000*01  AREA -COPY W27277     -PRE W27277-                                   
009100                                                                          
009200     EJECT                                                                
009300 01  UT-AREA-START           PIC X(24)   VALUE                            
009400                                 'UT-AREA-START  '.                       
009500                                                                          
009600                                                                          
009700*01  AREA -COPY W27179     -PRE UT-                                       
009800     EJECT                                                                
009900                                                                          
010000*    --- PARAMETRAR TILL W271BUYR                                         
010100*                                                                         
010200*01  -COPY W271BUYR                                                       
010300     EJECT                                                                
010400                                                                          
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900                                                                          
011000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100                                                                          
011200     SKIP3                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011600     03  W-IDDC-B6-X.                                                     
011700         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
011800                                                                          
011900*                                                                         
012000     SKIP2                                                                
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(64).                               
013000 01  SSA2                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013800 01   DLI-IO-AREA-B601.                                                   
013900*     03  -COPY WDB601                                                    
014000     EJECT                                                                
014100                                                                          
014200 LINKAGE SECTION.                                                         
014300                                                                          
014400     EJECT                                                                
014500*01  -COPY W0008      -PRE WDB6-                                          
014600     05  FILLER                  PIC X.                                   
014700                                                                          
014800 01  BUYR-WDB6-PCB                 PIC X.                                 
014900 01  BUYR-WDL7-PCB                 PIC X.                                 
015000 01  BUYR-WDL8-PCB                 PIC X.                                 
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING WDB6-PCB                                       
015300                           BUYR-WDB6-PCB BUYR-WDL7-PCB                    
015400                           BUYR-WDL8-PCB.                                 
015500     ENTRY 'DLITCBL' USING WDB6-PCB                                       
015600                           BUYR-WDB6-PCB BUYR-WDL7-PCB                    
015700                           BUYR-WDL8-PCB.                                 
015800                                                                          
015900 MAIN SECTION.                                                            
016000                                                                          
016100     PERFORM A-INIT                                                       
016200                                                                          
016300     MOVE '11'               TO W-IDDC-B6                                 
016400     PERFORM IMS-GU-WDB601                                                
016500                                                                          
016600     PERFORM S01-LAES-W27277                                              
016700     PERFORM UNTIL END-OF-W27277                                          
016800       MOVE W27277-IDARTNR   TO UT-IDARTNR                                
016900       MOVE '11'             TO UT-IDDC                                   
017000       MOVE ZERO             TO UT-IDREFTAB                               
017100                                UT-IDPERSON-BUY                           
017110       MOVE NEJ              TO BUYR-FLBUYER-CHANGED                      
017120                                BUYR-FLTABLE-CHANGED                      
017200                                                                          
017300       IF W27277-FLBUYUPD = 'N'                                           
017400         PERFORM B-BUYERTILLDELNING                                       
017500       END-IF                                                             
017600       IF W27277-FLTABUPD = 'N'                                           
017700         PERFORM C-KOEPTABELL                                             
017800       END-IF                                                             
017900                                                                          
018000       IF UT-IDREFTAB > ZERO                                              
018100       OR UT-IDPERSON-BUY > ZERO                                          
018120       OR BUYR-FLBUYER-CHANGED = JA                                       
018130       OR BUYR-FLTABLE-CHANGED = JA                                       
018200          MOVE W27277-FLBUYUPD  TO UT-FLBUYUPD                            
018300          MOVE W27277-FLTABUPD  TO UT-FLTABUPD                            
018310          MOVE DCS-KDDCSTYR-BUY TO UT-KDDCSTYR-BUY                        
018400          PERFORM S03-SKRIV-W27279                                        
018500       END-IF                                                             
018600                                                                          
018700       PERFORM S01-LAES-W27277                                            
018800     END-PERFORM                                                          
018900     PERFORM Z-FINIT                                                      
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400                                                                          
019500     EJECT                                                                
019600 A-INIT SECTION.                                                          
019700                                                                          
019800     OPEN INPUT  W27277                                                   
019900     OPEN OUTPUT W27279                                                   
020000                                                                          
020100     ACCEPT DAGENS-DATUM FROM DATE                                        
020200     ACCEPT WS-DAGENS-TID   FROM TIME                                     
020300     DISPLAY 'START TID : ' WS-DAGENS-TID                                 
020400                                                                          
020500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020600     .                                                                    
020700                                                                          
020800     EJECT                                                                
020900 B-BUYERTILLDELNING SECTION.                                              
021000                                                                          
021100***                                                                       
021200***                                                                       
021300     IF DCS-KDDCSTYR-BUY = 0                                              
021400        PERFORM S20-PREPARE-BUYR-LINK-AREA                                
021500        CALL W271BUYR USING BUYR-W271BUYR                                 
021600                            BUYR-WDB6-PCB BUYR-WDL7-PCB                   
021700                            BUYR-WDL8-PCB                                 
021800        IF  BUYR-KDSVAR-OK                                                
021900           IF BUYR-FLBUYER-CHANGED = JA                                   
022000              MOVE BUYR-IDPERSON-BUY    TO UT-IDPERSON-BUY                
022100           ELSE                                                           
022110              MOVE BUYR-IDPERSON-BUY-IN TO UT-IDPERSON-BUY                
022120           END-IF                                                         
022200        ELSE                                                              
022300           DISPLAY 'W271BUYR-ERROR1:' BUYR-TEXT                           
022400           CALL FELLOG                                                    
022500        END-IF                                                            
028900     END-IF                                                               
029000     .                                                                    
029100                                                                          
029200     EJECT                                                                
029300 C-KOEPTABELL SECTION.                                                    
029400                                                                          
029500     IF DCS-KDDCSTYR-REFTAB = 0                                           
029600        IF W27277-FLBUYUPD   = JA                                         
029700*-------- SINCE BUYER IS LOCKED W271BUYR WASNT CALLED IN B- .             
029800        OR BUYR-IDARTNR  NOT = W27277-IDARTNR                             
029900*-------- OR THE PART HAS KDDCSTYR-BUY <> 0                               
030000*-------- THEN MUST CALL W271BUYR HERE TO GET IDREFTAB.                   
030100           PERFORM S20-PREPARE-BUYR-LINK-AREA                             
030200           CALL W271BUYR USING BUYR-W271BUYR                              
030300                               BUYR-WDB6-PCB BUYR-WDL7-PCB                
030400                               BUYR-WDL8-PCB                              
030500           IF BUYR-KDSVAR-OK                                              
030600              CONTINUE                                                    
030700           ELSE                                                           
030800              DISPLAY 'W271BUYR-ERROR2:' BUYR-TEXT                        
030900              CALL FELLOG                                                 
031000           END-IF                                                         
031100        END-IF                                                            
031300        IF BUYR-FLTABLE-CHANGED = JA                                      
031400           MOVE BUYR-IDREFTAB     TO UT-IDREFTAB                          
031500        ELSE                                                              
031501           MOVE BUYR-IDREFTAB-IN  TO UT-IDREFTAB                          
031502        END-IF                                                            
032400     END-IF                                                               
032500                                                                          
032600     .                                                                    
032700                                                                          
032800     EJECT                                                                
032900 Z-FINIT SECTION.                                                         
033000     ACCEPT WS-DAGENS-TID   FROM TIME                                     
033100     DISPLAY 'SLUT  TID : ' WS-DAGENS-TID                                 
033200     CLOSE W27277                                                         
033300           W27279                                                         
033600     SKIP2                                                                
033700     MOVE 'S' TO POSTSUM-OPKOD                                            
033800     CALL POSTSUM USING POSTSUM-PARM                                      
033900     .                                                                    
034000                                                                          
034100     EJECT                                                                
034200 S01-LAES-W27277  SECTION.                                                
034300     READ W27277    INTO W27277-AREA                                      
034400     AT END                                                               
034500        SET END-OF-W27277 TO TRUE                                         
034600     NOT AT END                                                           
034800        MOVE 'W27277 '       TO POSTSUM-FDNAMN                            
034900        MOVE 'W27279D1'      TO POSTSUM-DDNAMN2                           
035000        MOVE SPACE           TO POSTSUM-TRANSTYP                          
035100        CALL POSTSUM USING POSTSUM-PARM                                   
035200                                                                          
035300     END-READ                                                             
035400     .                                                                    
035500                                                                          
035600     EJECT                                                                
035700 S03-SKRIV-W27279     SECTION.                                            
035800                                                                          
035900     WRITE UT-POST FROM UT-AREA                                           
036200     MOVE 'W27279 '          TO POSTSUM-FDNAMN                            
036300     MOVE 'W27279D2'         TO POSTSUM-DDNAMN2                           
036400     MOVE UT-IDDC            TO POSTSUM-TRANSTYP                          
036500     CALL POSTSUM USING POSTSUM-PARM                                      
036600     .                                                                    
036700     EJECT                                                                
036800* --- IMS SEKTIONER ---                                                   
036900     SKIP3                                                                
037000     EJECT                                                                
037100                                                                          
037200 S20-PREPARE-BUYR-LINK-AREA SECTION.                                      
037300                                                                          
037400     MOVE 002                        TO BUYR-KDCALL                       
037500     MOVE W27277-IDARTNR             TO BUYR-IDARTNR                      
037600     MOVE W27277-IDPERSON-BUY        TO BUYR-IDPERSON-BUY-IN              
037700     MOVE W27277-FLBUYUPD            TO BUYR-FLBUYUPD                     
037800     MOVE W27277-IDREFTAB            TO BUYR-IDREFTAB-IN                  
037900     MOVE W27277-FLTABUPD            TO BUYR-FLTABUPD                     
038000     MOVE W27277-KDPRODSL            TO BUYR-KDPRODSL                     
038100     MOVE W27277-IDFKNGRP            TO BUYR-IDFKNGRP                     
038200     MOVE W27277-KDUART              TO BUYR-KDUART                       
038300     MOVE '11'                       TO BUYR-IDDC                         
038400     MOVE W27277-IDDC-REF            TO BUYR-IDDC-REF                     
038500     MOVE W27277-FLFLYG              TO BUYR-FLFLYG                       
038600     MOVE W27277-KDFARLIG            TO BUYR-KDFARLIG                     
038700     MOVE W27277-VLARTNTO            TO BUYR-VLARTNTO                     
038800     MOVE W27277-KVPB-TOT            TO BUYR-KVPB-TOT                     
038900     MOVE W27277-TISOP               TO BUYR-TISOP                        
039000     MOVE W27277-TIURPROD            TO BUYR-TIURPROD                     
039010     MOVE W27277-FLBSNES             TO BUYR-FLBSNES                      
039020     MOVE W27277-KVEOP               TO BUYR-KVEOP                        
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 IMS-GU-WDB601    SECTION.                                                
039500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
039600          DELIMITED BY SIZE INTO SSA1                                     
039700     MOVE '  ' TO GODK-STATUSKODER                                        
039800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
039900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
040000     PERFORM IMS-STATUSKONTROLL                                           
040100     .                                                                    
040200     EJECT                                                                
040300 IMS-STATUSKONTROLL SECTION.                                              
040400                                                                          
040500     SET STATUS-IX TO 1                                                   
040600     SEARCH GODK-STATUS                                                   
040700       AT END                                                             
040800         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
040900           DELIMITED BY SIZE INTO FELTEXT-TEXT                            
041000         DISPLAY FELTEXT                                                  
041100         CALL FELLOG                                                      
041200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041300         CONTINUE                                                         
041400     END-SEARCH                                                           
041500     .                                                                    
