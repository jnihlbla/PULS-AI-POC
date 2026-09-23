000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4011600.                                                
000400 AUTHOR.         INGER NILSSON.                                           
000500 DATE-WRITTEN.   JULI  90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*        UPPDATERING AV FELKOD FÖR KONTROLLRAPPORT                        
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        UPPDATERING,NYUPPLÄGGNING,BORTTAG OCH FRÅGEPROGRAM               
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T116                                              
001500*        MID:         W4I11601                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O11601                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002401                                                                          
002410*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                   PIC X(8)    VALUE 'W4011600'.                
002600 77  JA                      PIC X       VALUE 'J'.                       
002700 77  NEJ                     PIC X       VALUE 'N'.                       
002800 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
002900 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
003000 77  MAX-RADER               PIC S9(9)   VALUE +10  COMP SYNC.            
003100 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +822 COMP SYNC.            
003200                                                                          
003300 77  INDATA-SW               PIC X       VALUE 'J'.                       
003400   88  INDATA-OK                         VALUE 'J'.                       
003500   88  INDATA-FEL                        VALUE 'N'.                       
003600                                                                          
003700 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
003800   88  NYCKLAR-OK                        VALUE 'J'.                       
003900   88  NYCKLAR-FEL                       VALUE 'N'.                       
004000                                                                          
004100 77  ALLT-SW                 PIC X       VALUE 'J'.                       
004200   88  ALLT-OK                           VALUE 'J'.                       
004300                                                                          
004400 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
004500   88  EGEN-MID                          VALUE '4116'.                    
004600   88  GODK-MID                          VALUE '4115'.                    
004610   88  HELP-MID                          VALUE '0551'.                    
004700     EJECT                                                                
004800 01  GENERELLA-SUBPROGRAM.                                                
004900   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
005000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
005100   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
005200     EJECT                                                                
005300*   -COPY WDECAREA                                                        
005500     EJECT                                                                
005600*   -COPY WMEDAREA                                                        
005800     EJECT                                                                
005900******************************************************************        
006000*                                                                         
006100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
006200*                                                                         
006300 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
006400     SKIP3                                                                
006500*01  MID -COPY W4I11601                                                   
006700     EJECT                                                                
006800*01  -COPY WMSGAREA                                                       
007000     EJECT                                                                
007100*  03  MOD -COPY W4O11601 -RED MSG-AREA.                                  
007300     EJECT                                                                
007400*01  -COPY WMFSAREA                                                       
007600     EJECT                                                                
007700******************************************************************        
007800*                                                                         
007900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008000*                                                                         
008100 01  IMS-WS.                                                              
008200   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008500   03  W-WDGX-4825-KEY-X.                                                 
008600     05  W-IDHTYP-4825       PIC  X(04)  VALUE '4825'.                    
008700     05  W-IDSKYLT           PIC  X(03)  VALUE 'GB '.                     
008800     05  FILLER              PIC  X(23)  VALUE LOW-VALUE.                 
008900   03  W-WDGX-4826-KEY-X.                                                 
009000     05  W-IDKRFEL           PIC  X(02)  VALUE SPACE.                     
009100     05  FILLER              PIC  X(08)  VALUE LOW-VALUE.                 
009200     SKIP3                                                                
009300*                        **** STATUS-KOD FRÅN IMS                         
009400   03  STATUS-WS             PIC XX.                                      
009500     88  SEGMENT-FINNS                   VALUE '  '.                      
009600     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
009700     88  SEGMENT-SLUT                    VALUE 'GB'.                      
009800     SKIP3                                                                
009900   03  GODK-STATUSKODER.                                                  
010000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010100     SKIP3                                                                
010200 01    SSA1                  PIC X(64).                                   
010300 01    SSA2                  PIC X(64).                                   
010400     EJECT                                                                
010500*                            IMS FUNKTIONSKODER                           
010600*01    -COPY W0003                                                        
010800     EJECT                                                                
010900*                            DLI INPUT-OUTPUT AREA                        
011000 01  DLI-IO-AREA.                                                         
011100   03  IO-AREA               PIC X(100)  VALUE SPACE.                     
011200     SKIP3                                                                
011300*  03  WLXXLA01  -COPY WDGX4825  -PRE XXLA-  -RED IO-AREA.                
011500     EJECT                                                                
011600*  03  WLXXLA11  -COPY WDGX4826  -PRE XXLA-  -RED IO-AREA.                
011800     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012000*01  -COPY W0009     -PRE MSG-                                            
012200     EJECT                                                                
012300*01  -COPY W0008     -PRE XXLA-                                           
012500     05  FILLER              PIC X(50).                                   
012600     EJECT                                                                
012700 PROCEDURE DIVISION USING MSG-PCB XXLA-PCB.                               
012800     ENTRY 'DLITCBL' USING MSG-PCB XXLA-PCB.                              
012900                                                                          
013000     PERFORM IMS-GET-MSG                                                  
013100     IF SEGMENT-FINNS                                                     
013200        PERFORM A-INIT                                                    
013300        PERFORM B-KOLLA-NYCKLAR                                           
013400        IF NYCKLAR-OK                                                     
013500           IF MFS-UPDATE                                                  
013600              PERFORM G-KOLLA-INPUT                                       
013700              IF INDATA-OK                                                
013800                PERFORM H-UPPDATERA                                       
013900              END-IF                                                      
014000           ELSE                                                           
014100              IF MFS-FIRST                                                
014200                 PERFORM C-FOERSTA-SIDA                                   
014300              ELSE                                                        
014400                 IF MFS-NEXT                                              
014500                    PERFORM D-NAESTA-SIDA                                 
014600                 ELSE                                                     
014700                    PERFORM E-SAMMA-SIDA                                  
014800                 END-IF                                                   
014900              END-IF                                                      
015000           END-IF                                                         
015100           IF ALLT-OK                                                     
015200              PERFORM F-LAES-VISA-FELKOD                                  
015300           END-IF                                                         
015400        END-IF                                                            
015500        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
015600        PERFORM IMS-INSERT-MSG                                            
015700     END-IF                                                               
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400     SKIP2                                                                
016500     IF MSG-DUBBLA-TRANSKODER                                             
016600        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I11601                
016700        MOVE MSG-IDTRANS-2     TO MFS-IDTRANS                             
016800        MOVE MSG-KDMFSFOR-2    TO MFS-KDMFSFOR                            
016900     ELSE                                                                 
017000        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I11601                  
017100        MOVE MSG-IDTRANS-1     TO MFS-IDTRANS                             
017200        MOVE MSG-KDMFSFOR-1    TO MFS-KDMFSFOR                            
017300     END-IF                                                               
017400                                                                          
017500     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
017600     MOVE MSG-IDPFK            TO MFS-IDPFK                               
017700     MOVE MFS-IDTRANS          TO W-IDTRANS                               
017800                                                                          
017900     MOVE LOW-VALUE            TO MSG-AREA                                
018000     MOVE 'W4O116N1'           TO MFS-IDMOD                               
018100     MOVE '4116'               TO MOD-IDTRANS                             
018200     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
018300                                                                          
018400     IF EGEN-MID OR HELP-MID                                              
018410       CONTINUE                                                           
018420     ELSE                                                                 
018500       MOVE SPACE              TO MFS-KDTRTYP                             
018600       MOVE '7'                TO MFS-IDPFK                               
018700     END-IF                                                               
018800     .                                                                    
018900     EJECT                                                                
019000 B-KOLLA-NYCKLAR SECTION.                                                 
019100     SKIP2                                                                
019200     MOVE JA                   TO NYCKLAR-SW                              
019300     MOVE MFS-RENSA-FAELT      TO MOD-IDKRFELI                            
019410     IF EGEN-MID OR HELP-MID                                              
019420        CONTINUE                                                          
019430     ELSE                                                                 
019600        MOVE SPACE             TO MID-IDKRFELI                            
019800     END-IF                                                               
019900                                                                          
020000     IF MID-IDKRFELI = ALL '+'                                            
020100        MOVE MID-IDKRFELU      TO W-IDKRFEL                               
020200     ELSE                                                                 
020300        IF MID-IDKRFELI = ALL '+' OR SPACE                                
020400           MOVE NEJ            TO NYCKLAR-SW                              
020500        ELSE                                                              
020600           MOVE MID-IDKRFELI   TO W-IDKRFEL                               
020700           MOVE '7'            TO MFS-IDPFK                               
020800           MOVE SPACE          TO MFS-KDTRTYP                             
020900        END-IF                                                            
021000     END-IF                                                               
021100                                                                          
024000     IF GODK-MID OR NYCKLAR-OK                                            
024100        MOVE W-IDKRFEL         TO MOD-IDKRFELU                            
024300     ELSE                                                                 
024400        MOVE MFS-RENSA-FAELT   TO MOD-IDKRFELU                            
024600     END-IF                                                               
024700                                                                          
024800     IF NYCKLAR-FEL                                                       
024900        MOVE '401'             TO MED-IDMFSFEL                            
025000        CALL WMEDKONV USING MED-WMEDAREA                                  
025100        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
025200        PERFORM MFS-RENSA-FAELT-IN                                        
025300        PERFORM MFS-RENSA-FAELT-UT                                        
025400     END-IF                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 C-FOERSTA-SIDA SECTION.                                                  
025800     SKIP2                                                                
025900     PERFORM MFS-RENSA-FAELT-IN                                           
026000     PERFORM MFS-RENSA-FAELT-UT                                           
026100     MOVE JA TO ALLT-SW                                                   
026200     .                                                                    
026300     EJECT                                                                
026400 D-NAESTA-SIDA SECTION.                                                   
026500     SKIP2                                                                
026600     MOVE MID-IDKRFEL-NX       TO W-IDKRFEL                               
026700     MOVE JA                   TO ALLT-SW                                 
026800     .                                                                    
026900     EJECT                                                                
027000 E-SAMMA-SIDA SECTION.                                                    
027100     SKIP2                                                                
027200     IF MID-INPUT = ALL '+'                                               
027300       MOVE MID-IDKRFEL-EN     TO W-IDKRFEL                               
027400       MOVE JA                 TO ALLT-SW                                 
027500     ELSE                                                                 
027600       MOVE NEJ                TO ALLT-SW                                 
027700       MOVE '003'              TO MED-IDMFSFEL                            
027800       CALL WMEDKONV USING MED-WMEDAREA                                   
027900       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
028000       PERFORM MFS-ROR-EJ-FAELT-IN                                        
028100       PERFORM MFS-ROR-EJ-FAELT-UT                                        
028200       PERFORM MFS-LAS-IN-IGEN                                            
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 F-LAES-VISA-FELKOD SECTION.                                              
028700     SKIP2                                                                
028800     PERFORM IMS-GU-WLXXLA01                                              
028900                                                                          
029000     PERFORM IMS-GNP-WLXXLA11                                             
029100                                                                          
029200     MOVE +1                     TO INDX                                  
029300                                                                          
029400     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-RADER                     
029500       IF INDX = +1                                                       
029600          MOVE XXLA-4826-IDKRFEL TO MOD-IDKRFEL-EN                        
029700       END-IF                                                             
029800                                                                          
029900       MOVE XXLA-4826-IDKRFEL    TO MOD-IDKRFEL-UT (INDX)                 
030000       MOVE XXLA-4826-BEKRFEL    TO MOD-BEKRFEL-UT (INDX)                 
030100                                                                          
030200       PERFORM IMS-GNP-WLXXLA11-OKVAL                                     
030300                                                                          
030400       IF SEGMENT-FINNS                                                   
030500          MOVE XXLA-4826-IDKRFEL TO MOD-IDKRFEL-NX                        
030600       ELSE                                                               
030700          MOVE MOD-IDKRFEL-EN    TO MOD-IDKRFEL-NX                        
030800       END-IF                                                             
030900                                                                          
031000       ADD +1                    TO INDX                                  
031100     END-PERFORM                                                          
031200                                                                          
031300     PERFORM UNTIL INDX > MAX-RADER                                       
031400       MOVE MFS-RENSA-FAELT      TO MOD-IDKRFEL-UT (INDX)                 
031500                                    MOD-BEKRFEL-UT (INDX)                 
031600       ADD +1                    TO INDX                                  
031700     END-PERFORM                                                          
031800                                                                          
031900     PERFORM MFS-RENSA-FAELT-IN                                           
032000                                                                          
032100     IF SEGMENT-FINNS                                                     
032200        MOVE '105'             TO MED-IDMFSINF                            
032300        CALL WMEDKONV USING MED-WMEDAREA                                  
032400        MOVE MED-MFSINF        TO MOD-TEMFSINF                            
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 G-KOLLA-INPUT SECTION.                                                   
032900     SKIP2                                                                
033000     MOVE JA                          TO INDATA-SW                        
033100                                                                          
033200     IF MID-INPUT = ALL '+'                                               
033300        MOVE '011'                    TO MED-IDMFSFEL                     
033400        CALL WMEDKONV USING MED-WMEDAREA                                  
033500        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
033600        PERFORM MFS-ROR-EJ-FAELT-IN                                       
033700        PERFORM MFS-ROR-EJ-FAELT-UT                                       
033800        MOVE NEJ                      TO INDATA-SW                        
033900        MOVE NEJ                      TO ALLT-SW                          
034000     ELSE                                                                 
034100       PERFORM MFS-LAS-IN-IGEN                                            
034200     END-IF                                                               
034300                                                                          
034400     IF INDATA-OK                                                         
034500        IF MID-IDKRFEL-IN = ALL '+' OR ' '                                
034700           MOVE NEJ TO INDATA-SW                                          
034800           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDKRFEL-IN-ATTR              
034900        ELSE                                                              
035000           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDKRFEL-IN-ATTR              
035100           MOVE MID-IDKRFEL-IN        TO W-IDKRFEL                        
035200        END-IF                                                            
035300                                                                          
035400        IF NOT MID-KDCMD-DELETE                                           
035500           IF MID-BEKRFEL-IN = ALL '+' OR ' '                             
035600              MOVE NEJ                TO INDATA-SW                        
035700              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEKRFEL-IN-ATTR              
035800           ELSE                                                           
035900              MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKRFEL-IN-ATTR            
036000           END-IF                                                         
036100        END-IF                                                            
036200                                                                          
037100        IF MID-KDCMD-DELETE AND INDATA-OK                                 
037200           PERFORM IMS-GU-WLXXLA11                                        
037300           IF SEGMENT-SAKNAS                                              
037400              MOVE NEJ                TO INDATA-SW                        
037500              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                
037600           ELSE                                                           
037700              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR              
037800           END-IF                                                         
037900        END-IF                                                            
038000     END-IF                                                               
038100                                                                          
038200     IF INDATA-FEL                                                        
038300        MOVE NEJ                      TO ALLT-SW                          
038400        MOVE '001'                    TO MED-IDMFSFEL                     
038500        CALL WMEDKONV USING MED-WMEDAREA                                  
038600        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
038700        PERFORM MFS-ROR-EJ-FAELT-IN                                       
038800        PERFORM MFS-ROR-EJ-FAELT-UT                                       
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 H-UPPDATERA SECTION.                                                     
039300     SKIP2                                                                
039400     PERFORM IMS-GHU-WLXXLA01                                             
039500     IF SEGMENT-SAKNAS                                                    
039600        MOVE W-WDGX-4825-KEY-X TO DLI-IO-AREA                             
039700        PERFORM IMS-ISRT-WLXXLA01                                         
039800     END-IF                                                               
039900                                                                          
040000     PERFORM IMS-GHU-WLXXLA11                                             
040100     IF SEGMENT-FINNS                                                     
040200        IF MID-KDCMD-DELETE                                               
040300           PERFORM IMS-DLET-WLXXLA11                                      
040400        ELSE                                                              
040500           MOVE MID-BEKRFEL-IN TO XXLA-4826-BEKRFEL                       
040600           PERFORM IMS-REPL-WLXXLA11                                      
040700        END-IF                                                            
040800     ELSE                                                                 
040900        MOVE MID-IDKRFEL-IN    TO XXLA-4826-IDKRFEL                       
041000        MOVE LOW-VALUE         TO XXLA-4826-LOW-VALUE                     
041100        MOVE MID-BEKRFEL-IN    TO XXLA-4826-BEKRFEL                       
041200        PERFORM IMS-ISRT-WLXXLA11                                         
041300     END-IF                                                               
041400                                                                          
041500     MOVE '101'                TO MED-IDMFSINF                            
041600     CALL WMEDKONV USING MED-WMEDAREA                                     
041700     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
041800     PERFORM MFS-FORM-ATTR                                                
041900     PERFORM MFS-RENSA-FAELT-IN                                           
042000     .                                                                    
042100     EJECT                                                                
042200 MFS-RENSA-FAELT-IN SECTION.                                              
042300     SKIP2                                                                
042400     MOVE MFS-RENSA-FAELT   TO MOD-IDKRFEL-IN                             
042500                               MOD-BEKRFEL-IN                             
042700                               MOD-KDCMD-IN                               
042800     .                                                                    
042900     SKIP3                                                                
043000 MFS-RENSA-FAELT-UT SECTION.                                              
043100     SKIP2                                                                
043200     MOVE MFS-RENSA-FAELT TO MOD-IDKRFEL-NX                               
043300                             MOD-IDKRFEL-EN                               
043400     PERFORM MFS-RENSA-FAELT-UT-RAD                                       
043500     .                                                                    
043600     SKIP3                                                                
043700 MFS-RENSA-FAELT-UT-RAD SECTION.                                          
043800     SKIP2                                                                
043900     MOVE +1 TO INDX                                                      
044000     PERFORM UNTIL INDX > MAX-RADER                                       
044100       MOVE MFS-RENSA-FAELT TO MOD-IDKRFEL-UT (INDX)                      
044200                               MOD-BEKRFEL-UT (INDX)                      
044300       ADD +1 TO INDX                                                     
044400     END-PERFORM                                                          
044500     .                                                                    
044600     EJECT                                                                
044700 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
044800     SKIP2                                                                
044900     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDKRFEL-NX                           
045000                                 MOD-IDKRFEL-EN                           
045100                                                                          
045200     MOVE +1 TO INDX                                                      
045300     PERFORM UNTIL INDX > MAX-RADER                                       
045400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKRFEL-UT (INDX)                    
045500                                 MOD-BEKRFEL-UT (INDX)                    
045600       ADD +1 TO INDX                                                     
045700     END-PERFORM                                                          
045800     .                                                                    
045900     SKIP3                                                                
046000 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
046100     SKIP2                                                                
046200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKRFEL-IN                             
046300                               MOD-BEKRFEL-IN                             
046500                               MOD-KDCMD-IN                               
046600     .                                                                    
046700     EJECT                                                                
046800 MFS-FORM-ATTR SECTION.                                                   
046900     SKIP2                                                                
047000     MOVE MFS-FORMATETS-ATTR TO MOD-IDKRFEL-IN-ATTR                       
047100                                MOD-BEKRFEL-IN-ATTR                       
047300                                MOD-KDCMD-IN-ATTR                         
047400     .                                                                    
047500     SKIP3                                                                
047600 MFS-LAS-IN-IGEN SECTION.                                                 
047700     SKIP2                                                                
047800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKRFEL-IN-ATTR                    
047900                                   MOD-BEKRFEL-IN-ATTR                    
048100                                   MOD-KDCMD-IN-ATTR                      
048200     .                                                                    
048300     EJECT                                                                
048400* IMS SEKTIONER                                                           
048500     SKIP3                                                                
048600 IMS-GET-MSG SECTION.                                                     
048700                                                                          
048800     MOVE '  QC' TO GODK-STATUSKODER                                      
048900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
049000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300     SKIP3                                                                
049400 IMS-INSERT-MSG SECTION.                                                  
049500                                                                          
049600     IF NOT ENGLISH-TEXT                                                  
049700       MOVE '0' TO MFS-KDHUVOMR                                           
049800     END-IF                                                               
049900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
050000     MOVE SPACE TO GODK-STATUSKODER                                       
050100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
050200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050300     PERFORM IMS-STATUSKONTROLL                                           
050400     .                                                                    
050500     EJECT                                                                
050600 IMS-GHU-WLXXLA01 SECTION.                                                
050700                                                                          
050800     STRING 'WLXXLA01(WDGXKEY  =' W-WDGX-4825-KEY-X ')'                   
050900          DELIMITED BY SIZE INTO SSA1                                     
051000     MOVE '  GE' TO GODK-STATUSKODER                                      
051100     CALL CBLTDLI USING GHU XXLA-PCB DLI-IO-AREA SSA1                     
051200     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
051300     PERFORM IMS-STATUSKONTROLL                                           
051400     .                                                                    
051500                                                                          
051600 IMS-GHU-WLXXLA11 SECTION.                                                
051700                                                                          
051800     STRING 'WLXXLA01(WDGXKEY  =' W-WDGX-4825-KEY-X ')'                   
051900          DELIMITED BY SIZE INTO SSA1                                     
052000     STRING 'WLXXLA11(WDGXKEY  =' W-WDGX-4826-KEY-X ')'                   
052100          DELIMITED BY SIZE INTO SSA2                                     
052200     MOVE '  GE' TO GODK-STATUSKODER                                      
052300     CALL CBLTDLI USING GHU XXLA-PCB DLI-IO-AREA SSA1 SSA2                
052400     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     .                                                                    
052700     EJECT                                                                
052800 IMS-GNP-WLXXLA11 SECTION.                                                
052900                                                                          
053000     STRING 'WLXXLA11(WDGXKEY =>' W-WDGX-4826-KEY-X ')'                   
053100          DELIMITED BY SIZE INTO SSA1                                     
053200     MOVE '  GE' TO GODK-STATUSKODER                                      
053300     CALL CBLTDLI USING GNP XXLA-PCB DLI-IO-AREA SSA1                     
053400     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
053500     PERFORM IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700                                                                          
053710 IMS-GNP-WLXXLA11-OKVAL SECTION.                                          
053720                                                                          
053730     MOVE 'WLXXLA11 ' TO SSA1                                             
053750     MOVE '  GE' TO GODK-STATUSKODER                                      
053760     CALL CBLTDLI USING GNP XXLA-PCB DLI-IO-AREA SSA1                     
053770     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
053780     PERFORM IMS-STATUSKONTROLL                                           
053790     .                                                                    
053791                                                                          
053800 IMS-GU-WLXXLA01 SECTION.                                                 
053900                                                                          
054000     STRING 'WLXXLA01(WDGXKEY  =' W-WDGX-4825-KEY-X ')'                   
054100          DELIMITED BY SIZE INTO SSA1                                     
054200     MOVE '  GE' TO GODK-STATUSKODER                                      
054300     CALL CBLTDLI USING GU XXLA-PCB DLI-IO-AREA SSA1                      
054400     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
054500     PERFORM IMS-STATUSKONTROLL                                           
054600     .                                                                    
054700                                                                          
054800 IMS-GU-WLXXLA11 SECTION.                                                 
054900                                                                          
055000     STRING 'WLXXLA01(WDGXKEY  =' W-WDGX-4825-KEY-X ')'                   
055100          DELIMITED BY SIZE INTO SSA1                                     
055200     STRING 'WLXXLA11(WDGXKEY  =' W-WDGX-4826-KEY-X ')'                   
055300          DELIMITED BY SIZE INTO SSA2                                     
055400     MOVE '  GE' TO GODK-STATUSKODER                                      
055500     CALL CBLTDLI USING GU  XXLA-PCB DLI-IO-AREA SSA1 SSA2                
055600     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
055700     PERFORM IMS-STATUSKONTROLL                                           
055800     .                                                                    
055900     EJECT                                                                
056000 IMS-ISRT-WLXXLA01 SECTION.                                               
056100                                                                          
056200     MOVE 'WLXXLA01 '         TO SSA1                                     
056300     MOVE '  '   TO GODK-STATUSKODER                                      
056400     CALL CBLTDLI USING ISRT XXLA-PCB DLI-IO-AREA SSA1                    
056500     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
056600     PERFORM IMS-STATUSKONTROLL                                           
056700     .                                                                    
056800                                                                          
056900 IMS-ISRT-WLXXLA11 SECTION.                                               
057000                                                                          
057100     STRING 'WLXXLA01(WDGXKEY  =' W-WDGX-4825-KEY-X ')'                   
057200          DELIMITED BY SIZE INTO SSA1                                     
057300     MOVE 'WLXXLA11 ' TO SSA2                                             
057400     MOVE '  '  TO GODK-STATUSKODER                                       
057500     CALL CBLTDLI USING ISRT XXLA-PCB DLI-IO-AREA SSA1 SSA2               
057600     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
057900                                                                          
058000 IMS-REPL-WLXXLA11 SECTION.                                               
058100                                                                          
058200     MOVE '  ' TO GODK-STATUSKODER                                        
058300     CALL CBLTDLI USING REPL XXLA-PCB DLI-IO-AREA                         
058400     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
058500     PERFORM IMS-STATUSKONTROLL                                           
058600     .                                                                    
058700                                                                          
058800 IMS-DLET-WLXXLA11 SECTION.                                               
058900                                                                          
059000     MOVE '  ' TO GODK-STATUSKODER                                        
059100     CALL CBLTDLI USING DLET XXLA-PCB DLI-IO-AREA                         
059200     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     EJECT                                                                
059600 IMS-STATUSKONTROLL SECTION.                                              
059700                                                                          
059800     SET STATUS-IX TO 1                                                   
059900     SEARCH GODK-STATUS                                                   
060000       AT END CALL FELLOG                                                 
060100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
060200     END-SEARCH                                                           
060300     .                                                                    
